;;; gitter.el --- An Emacs Gitter client  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; URL: https://github.com/xuchunyang/gitter.el
;; Package-Requires: ((emacs "24.4") (let-alist "1.0.4"))
;; Keywords: Gitter, chat, client, Internet
;; Version: 1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO fix flag unread in thread buffer

;; TODO fix jump to unread when message in thread

;; TODO fix flag mentions

;; TODO make-buttons (including actions) of avatar and message header

;; TODO make-buttons (including actions) of unread and mentions
;; indicators (on input-buffer header line)

;; See https://github.com/xuchunyang/gitter.el

;;; Code:

(require 'json)
(require 'subr-x)
(require 'ewoc)
(require 'shr)
(require 'shr-tag-pre-highlight)

(eval-when-compile (require 'let-alist))
(eval-when-compile (require 'evil nil t))


;;; Customization

(defgroup gitter nil
  "An Emacs Gitter client."
  :group 'comm)

(defcustom gitter-token nil
  "Your Gitter Personal Access Token.

To get your token:
1) Visit URL `https://developer.gitter.im'
2) Click Sign in (top right)
3) You will see your personal access token at
   URL `https://developer.gitter.im/apps'

DISCLAIMER
When you save this variable, DON'T WRITE IT ANYWHERE PUBLIC."
  :group 'gitter
  :type '(choice (string :tag "Token")
                 (const :tag "Not set" nil)))

(defcustom gitter-curl-program-name "curl"
  "Name/path by which to invoke the curl program."
  :group 'gitter
  :type 'string)


;;; Variable

(defvar gitter--debug nil
  "When non-nil, print debug information.")

(defconst gitter--root-endpoint "https://api.gitter.im"
  "The Gitter API endpoint.

For its documentation, refer to
URL `https://developer.gitter.im/docs/welcome'.")

(defconst gitter--stream-endpoint "https://stream.gitter.im"
  "The Gitter Streaming API endpoint.

For its documentation, refer to
URL `https://developer.gitter.im/docs/streaming-api'.")

(defvar-local gitter--output-marker nil
  "The marker where process output (i.e., message) should be insert.")

(defvar-local gitter--input-marker nil
  "The markder where input (i.e., composing a new message) begins.")

(defvar-local gitter--messages nil)

(defvar-local gitter--ewoc nil)

(defvar-local gitter--last-message nil
  "The last message has been inserted.")

(defvar-local gitter--room-data nil)
(defvar-local gitter--room-name nil)
(defvar-local gitter--room-id nil)
(defvar-local gitter--unread-items nil)
(defvar-local gitter--process-buffer nil)
(defvar-local gitter--input-buffer nil)
(defvar-local gitter--node nil)

(defvar gitter--prompt-function #'gitter--default-prompt
  "function called with message JSON object to return a prompt for chatting logs.")

(defvar gitter--user-id nil)

(defvar gitter--user-rooms nil
  "JSON object of requesing user rooms API.")

(defvar gitter--known-users nil)

(defvar gitter--avatar-dir (file-name-as-directory
                            (concat (temporary-file-directory) "gitter")))

(defvar-local gitter--window-start 0)
(defvar-local gitter--timer nil)

(defvar gitter--markup-text-functions '(string-trim
                                        gitter--markup-fenced-code)
  "A list of functions to markup text. They will be called in order.

The functions should take a string as argument and return a string.
The functions are called in the Gitter buffer, you can examine some buffer
local variables etc easily, but you should not modify the buffer or change the
current buffer.")

(defvar gitter--formatting-library 'shr)

;; To distinguish different buttons (by char-property), we define separate
;; face(s)(-names)
(defface gitter-prompt
  '((((background light))
     :background "LightSalmon")
    (((background dark))
     :background "#212225")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'gitter-faces)

(defface gitter-prompt-unread
  '((((background dark)) :background "darkolivegreen")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'gitter-faces)

(defface gitter-prompt-mention
  '((((background dark)) :background "orange4")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'gitter-faces)

(add-to-list 'shr-external-rendering-functions
             '(pre . shr-tag-pre-highlight))

;; (defun shr-tag-code (dom)
;;   (let ((lang (alist-get 'class (cadr dom))))
;;     (insert (concat "\n\n" (propertize (concat "#+BEGIN_SRC " lang) 'face 'highlight) "\n"))
;;     (if lang
;;         (insert (gitter--fontify-code (with-temp-buffer
;;                                         (shr-generic dom)
;;                                         (buffer-string))
;;                                       (pcase lang
;;                                         ("lisp" 'emacs-lisp-mode)
;;                                         ("python" 'python-mode))))
;;       (shr-generic dom)))
;;   (insert (concat "\n" (propertize "#+END_SRC" 'face 'highlight) "\n\n")))

(defun shr-tag-p (dom)
  (shr-generic dom)
  (shr-ensure-paragraph))

(defun shr-tag-code (dom)
  (let* ((shr-current-font 'fixed-pitch)
         (text (with-temp-buffer
                 (shr-generic dom)
                 (buffer-string))))
    (insert (propertize text 'face 'org-code))))

;;; Utility

(defmacro gitter--debug (format-string &rest args)
  "When `gitter--debug', print debug information almost like `message'."
  `(when gitter--debug
     (message ,(concat "[Gitter] " format-string) ,@args)))

(defun gitter--request (method resource &optional params data _noerror)
  "Request URL at RESOURCE with METHOD.
If PARAMS or DATA is provided, it should be alist."
  (with-current-buffer (generate-new-buffer " *curl*")
    (let* ((p (and params (concat "?" (gitter--url-encode-params params))))
           (d (and data (json-encode-list data)))
           (url (concat gitter--root-endpoint resource p))
           (headers
            (append (and d '("Content-Type: application/json"))
                    (list "Accept: application/json"
                          (format "Authorization: Bearer %s" gitter-token))))
           (args (gitter--curl-args url method headers d)))
      (gitter--debug "Calling curl with %S" args)
      (if (zerop (apply #'call-process gitter-curl-program-name nil t nil args))
          (progn (goto-char (point-min))
                 (gitter--read-response))
        (error "curl failed")
        (display-buffer (current-buffer))))))

(defun gitter--url-encode-params (params)
  "URI-encode and concatenate PARAMS.
PARAMS is an alist."
  (mapconcat
   (lambda (pair)
     (pcase-let ((`(,key . ,val) pair))
       (concat (url-hexify-string (symbol-name key)) "="
               (url-hexify-string val))))
   params "&"))

(defun gitter--curl-args (url method &optional headers data)
  "Return curl command line options/arguments as a list."
  (let ((args ()))
    (push "-s" args)
    ;; (push "-i" args)
    (push "-X" args)
    (push method args)
    (dolist (h headers)
      (push "-H" args)
      (push h args))
    (when data
      (push "-d" args)
      (push data args))
    (nreverse (cons url args))))

(defun gitter--read-response ()
  "Customized `json-read' by using native Emacs Lisp types."
  (let ((json-object-type 'alist)
        (json-array-type  'list)
        (json-key-type    'symbol)
        (json-false       nil)
        (json-null        nil))
    (json-read)))

(defun gitter--current-rooms ()
  (let (rooms)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (eq major-mode 'gitter-mode)
          (push (buffer-name) rooms))))
    rooms))

(defun gitter--room-id ()
  (alist-get 'id (seq-find (lambda (r)
                             (string= (alist-get 'name r) (buffer-name)))
                           gitter--user-rooms)))

(defun gitter--search ()
  (let ((prev-messages (gitter--request "GET"
                                        (format "/v1/rooms/%s/chatMessages" (gitter--room-id))
                                        '((limit . "500") (q . "dalanicolai")))))
    (completing-read "Select message" (mapcar (lambda (m) (alist-get 'text m)) prev-messages))))

(defun gitter--last-non-whitespace ()
  (save-excursion
    (goto-char (point-max))
    (search-backward-regexp "[^[:blank:]]")))

(defun gitter--ewoc-message-id (node)
  (alist-get 'id (ewoc-data node)))

(defun gitter--ewoc-collect (ewoc predicate &rest args)
  "Like `ewoc-collect' but passes the node itself to the predicate."
  (ewoc--set-buffer-bind-dll-let* ewoc
      ((header (ewoc--header ewoc))
       (node (ewoc--node-nth dll -2))
       result)
    (while (not (eq node header))
      (if (apply predicate node args)
	        (push node result))
      (setq node (ewoc--node-prev dll node)))
    result))

(defun gitter--ewoc-find (ewoc predicate &rest args)
  (ewoc--set-buffer-bind-dll-let* ewoc
                                  ((header (ewoc--header ewoc))
                                   (node (ewoc--node-nth dll -2))
                                   result)
                                  (while (not (eq node header))
                                    (if (apply predicate (ewoc--node-data node) args)
	                                      (push node result))
                                    (setq node (ewoc--node-prev dll node)))
                                  result))

(defun gitter--currently-displayed-messages ()
  ;; We first collect all nodes with point between window, and reverse it
  (let* ((displayed-nodes (reverse
                              (gitter--ewoc-collect gitter--ewoc
                                                    (lambda (r)
                                                      (or (<= (window-start) (ewoc-location r))
                                                          (< (ewoc-location r) (window-end)))))))
         ;; Subsequently we check if the last node (car of displayed-nodes) is
         ;; fully displayed otherwise we do not include it (in the returned
         ;; list)

         ;; If `displayed-nodes' nodes includes the last node must use a
         ;; different way to check if it fully displayed
         (last (car displayed-nodes))
         (next (ewoc-next gitter--ewoc last)))
    (mapcar (lambda (n)
              (alist-get 'id (ewoc--node-data n)))
            (if next
                (if (= (ewoc-location next) (window-end))
                    displayed-nodes
                  (cdr displayed-nodes))
              (if (> (window-end) (gitter--last-non-whitespace))
                  displayed-nodes
                (cdr displayed-nodes))))))

;; (defun gitter--currently-displayed-messages ()
;;   (let* ((first (ewoc-locate gitter--ewoc (window-start)))
;;          (first-full (if (print (= (window-start) (ewoc-location first)))
;;                          first
;;                        (ewoc-next gitter--ewoc first)))
;;          (last (ewoc-locate gitter--ewoc (window-end)))
;;          (last-full (if-let (l (ewoc-next gitter--ewoc last))
;;                         (if (= (window-end) (ewoc-location l))
;;                             last
;;                           (ewoc-prev gitter--ewoc last))
;;                       (if (> (window-end) (gitter--last-non-whitespace))
;;                           last
;;                         (ewoc-prev gitter--ewoc last))))
;;          messages)
;;     (push (gitter--ewoc-message-id first-full) messages)
;;     (let ((next-node (ewoc-next gitter--ewoc first-full)))
;;       (while (not (eq next-node last-full))
;;         (push (gitter--ewoc-message-id next-node) messages)
;;         (setq next-node (ewoc-next gitter--ewoc next-node)))
;;       (push (gitter--ewoc-message-id last-full) messages))
;;     messages))

(defun gitter-cycle-formatting-library ()
  (interactive)
  (let ((options '(shr markdown-mode original)))
    (print (setq gitter--formatting-library
                 (if-let (x(cdr (member gitter--formatting-library options)))
                     (car x)
                   'shr)))))

(defun gitter--ewoc-pp-message (response)
  (let-alist response
             (if (and gitter--last-message
                      (string= .fromUser.username
                               (let-alist gitter--last-message
                                 .fromUser.username)))
                 ;; Delete one newline
                 (progn (unless .editedAt (delete-char -1))
                        (insert-text-button (make-string 78 (string-to-char " "))
                                            'face (list (gitter--prompt-face response))
                                            'action (lambda (b) (pp (gitter--button-get-data b))))
                        (insert-text-button "···"
                                            'face (list (gitter--prompt-face response))
                                            'type 'gitter-edit
                                            'action (lambda (b)
                                                      (gitter-edit (ewoc-locate gitter--ewoc b))))
                        (insert "\n"))
               (funcall gitter--prompt-function response))
             (let ((beg (point)))
               ;; (insert (concat (apply #'propertize
               ;;                        " "
               ;;                        (append (list 'display `(space . (:width (,(line-pixel-height)))))
                               ;;                (when .unread (list 'face
                               ;;                                    (cons 'background-color (if .mentions
                               ;;                                                                "orange3"
                               ;;                                                              "green3"))))))
                               ;; " "))
               (pcase gitter--formatting-library
                 ('shr (let* ((shr-max-width 80)
                              (html .html)
                              (formatted-text
                               (with-temp-buffer (insert html)
                                                 (libxml-parse-html-region (point-min) (point-max)))))
                         (shr-insert-document formatted-text)))
                 ('markdown-mode (insert (with-temp-buffer
                                           (insert .text)
                                           ;; (markdown-mode)
                                           (gitter--fontify-code (buffer-string) 'markdown-mode))))
                 ('original (insert (let ((text .text))
                                      (dolist (fn gitter--markup-text-functions)
                                        (setq text (funcall fn text)))
                                      text))))
                (insert "\n")
               ;; (fill-region beg (point))
                )
             (when .threadMessageCount
               (insert (propertize " " 'display `(space . (:width (,(line-pixel-height))))))
               (insert " ")
               (insert-text-button "replies"
                                   'action (lambda (_)
                                             ;; (pop-to-buffer "thread" '(display-buffer-in-direction . ((direction . right))))
                                             (let ((id gitter--room-id))
                                               (switch-to-buffer-other-window "thread")
                                               (erase-buffer)
                                               (gitter-mode)
                                               (setq gitter--room-id id)
                                               (gitter--insert-messages
                                                (gitter--request "GET"
                                                                       (format "/v1/rooms/%s/chatMessages/%s/thread" id .id)
                                                                       '((limit . "100")))))))
               (insert "\n"))))

(defun gitter--insert-messages (messages &optional id)
  (setq gitter--ewoc
        (ewoc-create #'gitter--ewoc-pp-message nil nil t))
  (dolist (r messages) (ewoc-enter-last gitter--ewoc r))
  (goto-char (point-max)))

(defun gitter--flag-messages-read (room-id unread-items &optional get)
  (apply #'gitter--request (if get "GET" "POST")
         (format "/v1/user/%s/rooms/%s/unreadItems" gitter--user-id room-id)
         (unless get
           (list nil
                 (list (cons 'chat unread-items))))))

(defun gitter--unread-and-mentions ()
  (gitter--request "GET"
                         (format "/v1/user/%s/rooms/%s/unreadItems" gitter--user-id gitter--room-id)))

(defun gitter-flag-displayed-read (room-id)
  (interactive)
  (when (and gitter--room-id
             (string= gitter--room-id room-id))
    (let ((messages (gitter--currently-displayed-messages)))
      (gitter--flag-messages-read gitter--room-id messages)
      (setq gitter--room-data (gitter--request "GET" (format "/v1/rooms/%s" room-id)))
      ;; (setq gitter--messages (print (gitter--request "GET"
      ;;                                                (print (format "/v1/rooms/%s/chatMessages"
      ;;                                                               room-id
      ;;                                                               ))
      ;;                                                (print (list (cons 'limit "2") ;(number-to-string (print (length messages))))
      ;;                                                             (cons 'beforeId (car messages)))))))
      ;; (let ((inhibit-read-only t))
      ;;   ;; (point (point)))
      ;;   (erase-buffer)
      ;;   (gitter--insert-messages gitter--messages)
      ;;   ;; (goto-char point)
      (ewoc-map (lambda (data)
                  (when (and (alist-get 'unread data)
                             (member (alist-get 'id data)
                                     (gitter--currently-displayed-messages)))
                    (setf (alist-get 'unread data) nil) t))
                  gitter--ewoc))
    (setq-local global-mode-string (gitter--mode-line-buttons (alist-get 'unreadItems gitter--room-data)
                                                              (alist-get 'mentions gitter--room-data)))
    (force-mode-line-update)))

(defun gitter-flag-all-read ()
  (interactive)
  (gitter--flag-messages-read gitter--room-id (alist-get 'chat (gitter--unread-and-mentions))))

(defun gitter--update-timer ()
  (unless (= gitter--window-start (window-start)) 
    (when (timerp gitter--timer)
          (cancel-timer gitter--timer))
    (setq gitter--timer (run-at-time "1 sec" nil #'gitter-flag-displayed-read gitter--room-id)))
  (setq gitter--window-start (window-start)))

(defun gitter--input-window-resize (&optional _ _ _)
  (fit-window-to-buffer))
  ;; (window-resize (selected-window) (- (+ 2 (count-lines (point-min) (point-max))) (window-height))))

(defun gitter--unread-button-action ()
  (interactive)
  (gitter--goto-unread-item 'chat))

(defun gitter--mentions-button-action ()
  (interactive)
  (gitter--goto-unread-item 'mention))

(defconst gitter-unread-button-map (let ((map (make-sparse-keymap)))
                                     (define-key map [mode-line down-mouse-1] #'gitter--unread-button-action)
                                     map))

(defconst gitter-mentions-button-map (let ((map (make-sparse-keymap)))
                                      (define-key map [mode-line down-mouse-1] #'gitter--mentions-button-action)
                                      map))

(defun gitter--goto-unread-item (type)
  (let* ((id (car (alist-get type gitter--unread-items)))
         (node (car (gitter--ewoc-find gitter--ewoc
                                      (lambda (m)
                                        (string= (alist-get 'id m)
                                                 id))))))
    (if node
        (ewoc-goto-node gitter--ewoc node)
      ;; if `id' is not found in the ewoc then it is probably a reply (in a
      ;; thread)
      (let* ((room-id gitter--room-id)
             (data (gitter--request "GET" (format "/v1/rooms/%s/chatMessages/%s" room-id id)))
             (parent-id (alist-get 'parentId data)))
        (switch-to-buffer-other-window "thread")
        (erase-buffer)
        (gitter-mode)
        (setq gitter--room-id room-id)
        (gitter--insert-messages
         (gitter--request "GET" (format "/v1/rooms/%s/chatMessages/%s/thread" room-id parent-id) '((limit . "100"))))
        (add-hook 'post-command-hook #'gitter--update-timer nil t)))))

(defun gitter--mode-line-buttons (&optional unread mentions)
  (let* ((unread-items (when (or (not unread) (or (/= unread 0) (/= mentions 0)))
                        (gitter--unread-and-mentions)))
         (unread (length (alist-get 'chat unread-items)))
         (mentions (length (alist-get 'mention unread-items))))
    (when unread-items
      (setq gitter--unread-items unread-items))
    (concat (unless (= unread 0)
              (propertize (format "unread: %s" unread)
                          'face '(:foreground "darkolivegreen")
                          'mouse-face 'mode-line-highlight
                          'keymap gitter-unread-button-map))
            " "
            (unless (= mentions 0)
              (propertize (format "mentions: %s" mentions)
                          'face '(:foreground "orange4")
                          'mouse-face 'mode-line-highlight
                          'keymap gitter-mentions-button-map)))))

;; TODO See if this function can replace part of `gitter--open-room'
;; (defun gitter--create-message-buffer (buffer-data messages)
;;   (let* ((name (alist-get 'name data))
;;          (id (alist-get 'id data))
;;          (process-buffer (get-buffer-create name))
;;          (input-buffer (concat name "-input")))
;;     (auto-fill-mode)
;;     (gitter-mode)
;;     (setq cursor-type nil)
;;     (setq gitter--room-data data)
;;     (setq gitter--room-id id)
;;     (setq gitter--room-name name)
;;     (setq gitter--process-buffer process-buffer)
;;     (setq gitter--input-buffer input-buffer)

;;     ;; (setq-local global-mode-string (gitter--mode-line-buttons (alist-get 'unreadItems data)
;;     ;;                                                           (alist-get 'mentions data)))

;;     (add-hook 'post-command-hook #'gitter--update-timer nil t)
;;     ;; (add-hook 'window-configuration-change-hook #'gitter--display-input-buffer nil t)
;;     ;; (add-hook 'kill-buffer-hook #'gitter--kill-input-buffer nil t)
;;     ;; (setq buffer-quit-function #'gitter--bury)
;;     (gitter--insert-messages messages id)))

(defun gitter--open-room (room-data)
  (let* ((name (alist-get 'name room-data))
         (id (alist-get 'id room-data))
         (process-buffer (get-buffer-create name))
         (input-buffer (concat name "-input")))
    (with-current-buffer process-buffer
      (unless (process-live-p (get-buffer-process (current-buffer)))
        (auto-fill-mode)
        (gitter-mode)
        (setq cursor-type nil)
        (setq gitter--room-data room-data)
        (setq gitter--room-id id)
        (setq gitter--room-name name)
        (setq gitter--process-buffer process-buffer)
        (setq gitter--input-buffer input-buffer)

        (setq-local global-mode-string (gitter--mode-line-buttons (alist-get 'unreadItems room-data)
                                                                        (alist-get 'mentions room-data)))

        (add-hook 'post-command-hook #'gitter--update-timer nil t)
        ;; (add-hook 'window-configuration-change-hook #'gitter--display-input-buffer nil t)
        ;; (add-hook 'kill-buffer-hook #'gitter--kill-input-buffer nil t)
        ;; (setq buffer-quit-function #'gitter--bury)
        (let (
              ;; (fill-column 80)
              ;; (inhibit-read-only t)
              (prev-messages (gitter--request "GET" (format "/v1/rooms/%s/chatMessages" id) '((limit . "100")))))
          (gitter--insert-messages prev-messages id))
        (let* ((url (concat gitter--stream-endpoint
                            (format "/v1/rooms/%s/chatMessages" id)))
               (headers
                (list "Accept: application/json"
                      (format "Authorization: Bearer %s" gitter-token)))
               (proc
                ;; NOTE According to (info "(elisp) Asynchronous Processes")
                ;; we should use a pipe by let-binding `process-connection-type'
                ;; to nil, however, it doesn't working very well on my system
                (apply #'start-process
                       (concat "curl-streaming-process-" name)
                       (current-buffer)
                       gitter-curl-program-name
                       (gitter--curl-args url "GET" headers)))
               ;; Parse response (json) incrementally
               ;; Use a scratch buffer to accumulate partial output
               (parse-buf (generate-new-buffer
                           (concat " *Gitter search parse for " (buffer-name)))))
          (process-put proc 'room-id id)
          (process-put proc 'parse-buf parse-buf)
          (set-process-filter proc #'gitter--output-filter)))

        ;; (with-current-buffer (get-buffer-create input-buffer)
        ;;   (gitter-input-mode)
        ;;   (setq-local gitter--process-buffer process-buffer)
        ;;   (setq buffer-quit-function #'gitter--bury)
        ;;   (setq mode-line-format nil)
        ;;   (setq header-line-format (propertize (format "Press %s to send
        ;;   message." (substitute-command-keys "\\[gitter-send-message]")) 'face
        ;;   'bold))
        ;;   (setq header-line-format (format "Input. Press %s to send message."
        ;;                                    (substitute-command-keys "\\[gitter-send-message]")))
        ;;   (setq gitter--input-marker (point-max-marker))
        ;;   (add-hook 'after-change-functions #'gitter--input-window-resize nil t)))

      (switch-to-buffer (current-buffer))
      ;; (setq mode-line-format nil)
      (recenter -1))))
        ;;   (pop-to-buffer gitter--input-buffer
        ;;                  '(display-buffer-below-selected . ((dedicated . t)))))
        ;; ;; (setq header-line-format "Input:\t\t\tthreads")
        ;; (fit-window-to-buffer)))

(defun gitter-input (&optional node)
  (interactive)
  (let ((process-buffer gitter--process-buffer))
    (with-current-buffer (get-buffer-create gitter--input-buffer)
      (cond (node (gitter-edit-mode)
                  (setq gitter--node node))
            (t (gitter-input-mode)))
      (setq-local gitter--process-buffer process-buffer)
      ;; (setq buffer-quit-function #'gitter--bury)
      (setq mode-line-format nil)
      ;; (setq header-line-format (propertize (format "Press %s to send
      ;;       message." (substitute-command-keys "\\[gitter-send-message]")) 'face
      ;;       'bold))
      ;; (setq header-line-format (format "Input. Press %s to send message."
      ;;                                  (substitute-command-keys "\\[gitter-send-message]")))
      (setq gitter--input-marker (point-max-marker))
      (when node (insert (alist-get 'text (ewoc-data node))))
      (add-hook 'after-change-functions #'gitter--input-window-resize nil t))
    (pop-to-buffer gitter--input-buffer
                   '(display-buffer-below-selected . ((dedicated . t))))
    (fit-window-to-buffer)))

(defun gitter-edit (node)
  (gitter-input node))

(when (featurep 'evil)
  (add-to-list 'evil-insert-state-modes 'gitter-input-mode))

;; (defun gitter--header-line ()
;;   (let* ((text (format "Input. Press %s to send message."
;;                        (substitute-command-keys "\\[gitter-send-message]")))
;; 	 (room-data (gitter--request "GET" (format "/v1/rooms/%s" (gitter--room-id))))
;; 	 (unread (alist-get 'unreadItems room-data))
;; 	 (mentions (alist-get 'mentions room-data))
;; 	 (buttons-text (concat
;; 			(unless (= unread 0) (format "unread: %s" unread))
;; 			"   "
;; 			(unless (= mentions 0) (format "mentions: %s" mentions))))
;; 	 (spaces (make-string (- 80 (length (concat "Input:" buttons-text))) (string-to-char " "))))
;;     (print (concat "Input:" spaces buttons-text))))

;; (defun gitter--display-input-buffer ()
;;   (when (buffer-live-p gitter--process-buffer)
;;     (pop-to-buffer gitter--input-buffer
;;                    '(display-buffer-below-selected . ((dedicated . t)))))
;;   (setq header-line-format "Input:\t\t\tthreads")
;;   (fit-window-to-buffer))

(defun gitter--output-filter (process output)
  (when gitter--debug
    (with-current-buffer (get-buffer-create "*gitter log*")
      (goto-char (point-max))
      (insert output "\n\n")))

  (let ((results-buf (process-buffer process))
        (parse-buf (process-get process 'parse-buf)))
    (when (buffer-live-p results-buf)
      (with-current-buffer parse-buf
        ;; Insert new data
        (goto-char (point-max))
        (insert output)
        (condition-case err
            (progn
              (goto-char (point-min))
              ;; `gitter--read-response' moves point
              (let ((response (gitter--read-response)))
                (with-current-buffer results-buf
                  (ewoc-enter-last gitter--ewoc response)
                ;; (if (and gitter--last-message
                ;;          (string= .fromUser.username
                ;;                   (let-alist gitter--last-message
                ;;                     .fromUser.username)))
                ;;     ;; Delete one newline
                ;;     (delete-char -1)
                ;;   (insert (funcall gitter--prompt-function response)))
                ;; (insert
                ;;  (concat (propertize " " 'display `(space . (:width (,(line-pixel-height)))))
                ;;          " "
                ;;          (let ((text .text))
                ;;            (dolist (fn gitter--markup-text-functions)
                ;;              (setq text (funcall fn text)))
                ;;            text))
                ;;  "\n"
                ;;  "\n")
                  (setq gitter--last-message response)))
              (delete-region (point-min) (point)))
          (error
           ;; FIXME
           (with-current-buffer (get-buffer-create "*Debug Gitter Log")
             (goto-char (point-max))
             (insert (format "The error was: %s" err)
                     "\n"
                     output))))))))

;;; Prompt
(define-button-type 'gitter-avatar)
(define-button-type 'gitter-display-name)
(define-button-type 'gitter-username)
(define-button-type 'gitter-data)
(define-button-type 'gitter-edit)

(defun gitter--button-get-data (button)
  "Get message data from ewoc.
This function should only be used within the action property of
buttons located within the ewoc."
  (ewoc-data (ewoc-locate gitter--ewoc button)))

(defvar gitter-avatar-map
  (print (let ((map (make-sparse-keymap "Select option: ")))
           (define-key map "a" '("THIS" . test))
           (define-key map "b" '("THAT" . test))
     map)))

(defun gitter--prompt-face (response)
  (let-alist response
          (if .unread
              (if .mentions 'gitter-prompt-mention 'gitter-prompt-unread)
            'gitter-prompt)))

(defun gitter--default-prompt (response)
  "Default function to make prompt by using the JSON object MESSAGE."
  (let-alist response
    (unless (member .fromUser.username (mapcar (lambda (x)
                                                 (alist-get 'username x))
                                               gitter--known-users))
      (push .fromUser gitter--known-users)
      (unless (member .fromUser.username (directory-files gitter--avatar-dir))
        (url-copy-file (or .fromUser.avatarUrlSmall
                           (let* ((splitname (split-string .fromUser.displayName))
                                  (url-name (mapconcat #'identity splitname "+")))
                             (concat "https://ui-avatars.com/api/?name=%s" url-name)))
                         (concat gitter--avatar-dir .fromUser.username))))
    (let* ((text (format "%s @%s %s".fromUser.displayName .fromUser.username .sent))
           (whitespace (make-string (- 76 (length text)) (string-to-char " "))))
      (when window-system
        (insert-text-button " "
                            'type 'gitter-avatar
                            'display (create-image (concat gitter--avatar-dir .fromUser.username)
                                                   nil
                                                   nil
                                                   :height (line-pixel-height)
                                                   :ascent 100)
                            'action (lambda (b)
                                      (let-alist (ewoc-data (ewoc-locate gitter--ewoc b))
                                        (pp .fromUser.id))))
        ;; (pcase-let ((`(,x . ,y) (window-absolute-pixel-position b)))
        ;;   (x-popup-menu (list (list x y) (selected-window))
        ;;                       (list gitter-avatar-map)))))

        ;; '("user" ("choice" ("line" . 3) ("circ" . 4))))))))
        (insert "\n"))
      (insert-text-button .fromUser.displayName
                          'type 'gitter-display-name
                          'face (list (gitter--prompt-face response) 'bold)
                          'action (lambda (b)
                                    (let-alist (gitter--button-get-data b)
                                      (pp .fromUser.id))))
      (insert (propertize " " 'face (gitter--prompt-face response)))
      (insert-text-button (concat "@" .fromUser.username)
                          'type 'gitter-username
                          'face (list (gitter--prompt-face response) 'bold)
                          'action (lambda (b)
                                    (let ((label (button-label b)))
                                      (if-let (buf (get-buffer gitter--input-buffer))
                                          (switch-to-buffer buf)
                                        (gitter-input))
                                      (insert label))))
      (insert (propertize " " 'face (gitter--prompt-face response)))
      (insert-text-button .sent 
                          'type 'gitter-data
                          'face (list (gitter--prompt-face response)))
      ;; (insert (propertize (concat text whitespace)
      ;;                     'face (test response)))
      (insert (propertize " " 'face (gitter--prompt-face response)))
      (insert-text-button whitespace
                          'face (list (gitter--prompt-face response))
                          'action (lambda (b) (pp (gitter--button-get-data b))))
      (insert (propertize " " 'face (gitter--prompt-face response)))
      (insert-text-button "···"
                          'type 'gitter-edit
                          'face (gitter--prompt-face response)
                          'action (lambda (b)
                                    (gitter-edit (ewoc-locate gitter--ewoc b))))
      (insert "\n"))))

;; The result produced by `markdown-mode' was not satisfying
;;
;; (defun gitter--fontify-markdown (text)
;;   (with-temp-buffer
;;     ;; Work-around for `markdown-mode'. It looks like markdown-mode treats ":"
;;     ;; specially (I don't know the reason), this strips the specificity (I don't
;;     ;; know how either)
;;     (insert "\n\n")
;;     (insert text)
;;     (delay-mode-hooks (markdown-mode))
;;     (if (fboundp 'font-lock-ensure)
;;         (font-lock-ensure)
;;       (with-no-warnings
;;         (font-lock-fontify-buffer)))
;;     (buffer-substring 3 (point-max))))

(defun gitter--fontify-code (code mode)
  "Fontify CODE in major-mode MODE."
  (with-temp-buffer
    (insert code)
    (delay-mode-hooks (funcall mode))
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings
        (font-lock-fontify-buffer)))
    (buffer-string)))

(defun gitter--markup-fenced-code (text)
  "Markup Github-flavored fenced code block.

For reference, see URL
`https://help.github.com/articles/creating-and-highlighting-code-blocks/'."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Assuming there is only one code block
    (let* ((beg-inner (and (re-search-forward "^\\s-*```\\(.*\\)$" nil t)
                           (line-end-position)))
           (lang (and beg-inner
                      (string-trim (match-string 1))))
           (beg-outter (and lang
                            (line-beginning-position)))
           (end-outter (and beg-outter
                            (re-search-forward "^\\s-*```\\s-*$" nil t)
                            (line-end-position)))
           (end-inner (and end-outter
                           (line-beginning-position)))
           (mode (and end-inner
                      (not (string-empty-p lang))
                      (intern (format "%s-mode" lang)))))
      (when (and mode (fboundp mode))
        (let ((code (buffer-substring beg-inner end-inner)))
          (gitter--debug "Markup code in %s mode" mode)
          (delete-region beg-outter end-outter)
          (insert (gitter--fontify-code code mode)))))
    (buffer-string)))


;;; Major mode


;; FIXME Maybe it is better to use a major mode
(define-derived-mode gitter-mode special-mode "Gitter"
  "Minor mode which is enabled automatically in Gitter buffers.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Ustually you don't need to call it interactively, it is
interactive because of the cost of using `define-minor-mode'.
Sorry to make your M-x more chaotic (yes, I think M-x is already
chaotic), that's not my intention but I don't want to bother with
learning how to make commandsnon-interactive."
  :group 'gitter)

;; (evil-define-key 'normal gitter-mode-map
;;   "i" #'gitter-input
;;   "C-j" #'gitter-goto-next-message
;;   "C-k" #'gitter-goto-prev-message
;;   (kbd "<tab>") #'gitter-switch-buffer)

(define-derived-mode gitter-input-mode fundamental-mode "Gitter input"
  "Minor mode which is enabled automatically in Gitter buffers.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Ustually you don't need to call it interactively, it is
interactive because of the cost of using `define-minor-mode'.
Sorry to make your M-x more chaotic (yes, I think M-x is already
chaotic), that's not my intention but I don't want to bother with
learning how to make commandsnon-interactive."
  :group 'gitter)

(define-key gitter-input-mode-map
            "\C-c\C-c" #'gitter-send-message)

(define-derived-mode gitter-edit-mode fundamental-mode "Gitter edit"
  "Minor mode which is enabled automatically in Gitter buffers.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Ustually you don't need to call it interactively, it is
interactive because of the cost of using `define-minor-mode'.
Sorry to make your M-x more chaotic (yes, I think M-x is already
chaotic), that's not my intention but I don't want to bother with
learning how to make commandsnon-interactive."
  :group 'gitter)

(define-key gitter-edit-mode-map
            "\C-c\C-c" #'gitter-update-message)

;; (evil-define-key 'normal gitter-input-mode-map
;;   "C-j"   #'gitter-goto-next-message
;;   "C-k"   #'gitter-goto-prev-message
;;   (kbd "<tab>") #'gitter-switch-buffer)

;;; Commands

;; (defun gitter-goto-next-message ()
;;   (interactive)
;;   (ewoc-goto-next gitter--ewoc 1))

;; (defun gitter-goto-prev-message ()
;;   (interactive)
;;   (ewoc-goto-prev gitter--ewoc 1))

(when (featurep 'evil)
  (evil-define-motion gitter-goto-next-message (count)
    :type block
    (with-selected-window (get-buffer-window gitter--process-buffer)
      (ewoc-goto-next gitter--ewoc (or count 1))))

  (evil-define-motion gitter-goto-prev-message (count)
    :type block
    (with-selected-window (get-buffer-window gitter--process-buffer)
      (ewoc-goto-prev gitter--ewoc (or count 1)))))

(defun gitter-switch-buffer ()
  (interactive)
  (pop-to-buffer
   (if (get-buffer-process (current-buffer))
       gitter--input-buffer
     gitter--process-buffer)))

(defun gitter-print-ewoc ()
  (interactive)
  (if gitter--ewoc
      (ewoc-map (lambda (x) (pp x) (princ "\n")) gitter--ewoc)
    (user-error "Buffer does not contain a gitter-ewoc")))

;;;###autoload
(defun gitter ()
  "Open a room.
When ARG is non-nil, refresh `gitter--user-rooms' list."
  (interactive)
  (unless (stringp gitter-token)
    (let* ((plist (car (auth-source-search :max 1 :host "gitter.im")))
           (k (plist-get plist :secret)))
      (if (functionp k)
          (setq gitter-token (funcall k))
        (user-error "`gitter-token' is not set.  \
Please put this line in your ~/.authinfo or ~/.authinfo.gpg
machine gitter.im password here-is-your-token"))))
  (setq gitter--user-id (let-alist (car (gitter--request "GET" "/v1/user")) .id))
  (setq gitter--user-rooms (gitter--request "GET" "/v1/rooms"))
  ;; FIXME Assuming room name is unique because of `completing-read'
  (let* ((rooms (mapcar (lambda (alist)
                          (alist-get 'name alist))
                        gitter--user-rooms))
         (completion-extra-properties '(:annotation-function
                                        (lambda (name)
                                          (let* ((room (seq-find (lambda (r)
                                                                   (rassoc name r))
                                                                 gitter--user-rooms))
                                                 (unread   (alist-get 'unreadItems room))
                                                 (mentions (alist-get 'mentions room)))
                                            (concat (when (/= unread 0)
                                                      (propertize
                                                       (format " unread: %s" unread)
                                                       'face '(:foreground "darkolivegreen")))
                                                     (when (/= mentions 0)
                                                       (propertize
                                                       (format " mentions %s" mentions)
                                                       'face '(:foreground "orange4"))))))))
         (name (completing-read "Open room: " rooms nil t))
         (room-data (seq-find (lambda (r)
                                (rassoc name r))
                              gitter--user-rooms)))
    (unless (file-directory-p gitter--avatar-dir)
      (make-directory gitter--avatar-dir))
    (gitter--open-room room-data)))

(defun gitter-send-message (&optional node)
  "Send message in the current Gitter buffer."
  (interactive)
  (let ((proc (get-buffer-process gitter--process-buffer)))
    (when (and proc (process-live-p proc))
      (let* ((id (process-get proc 'room-id))
             (resource (format "/v1/rooms/%s/chatMessages" id))
             (msg (string-trim
                   (buffer-substring
                    (marker-position gitter--input-marker)
                    (point-max)))))
        (if (string-empty-p msg)
            (error "Can't send empty message")
          (gitter--request (if node "PUT" "POST")
                           (if node
                                     (concat resource "/" (alist-get 'id (ewoc-data node)))
                                   resource)
                           nil `((text . ,msg)))
          (kill-buffer))
        (when node
          (ewoc-set-data node
                         (gitter--request "GET"
                                                (concat resource "/" (alist-get 'id (ewoc-data node)))
                                                nil `((text . ,msg))))
          (ewoc-invalidate gitter--ewoc node))))))
      ;; (delete-region (marker-position gitter--input-marker)
      ;;                (point-max)))))))

(defun gitter-update-message ()
  (interactive)
  (gitter-send-message gitter--node))

(provide 'gitter)
;;; gitter.el ends here
