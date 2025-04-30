;;; Tab-bar-replacements --- define replacement of items in "tab-bar.el". ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 's)
(require 'nerd-icons)
(require 'thattem-tab-bar-faces)

;;; History items

(defvar thattem-tab-bar-back-button
  (nerd-icons-faicon "nf-fa-caret_left"
                     :face 'thattem-tab-bar/big-face-2
                     :v-adjust 0)
  "Replacement for \\='tab-bar-back-button\\='.")

(defvar thattem-tab-bar-forward-button
  (nerd-icons-faicon "nf-fa-caret_right"
                     :face 'thattem-tab-bar/big-face-2
                     :v-adjust 0)
  "Replacement for \\='tab-bar-forward-button\\='.")

(defun thattem-tab-bar-format-history ()
  "Replacement for \\='tab-bar-format-history\\='."
  (when tab-bar-history-mode
    `((history-back
       menu-item ,thattem-tab-bar-back-button
       tab-bar-history-back
       :help "Click to go back in tab history")
      (history-forward
       menu-item ,thattem-tab-bar-forward-button
       tab-bar-history-forward
       :help "Click to go forward in tab history"))))

;;; Tabs items

(defun thattem-tab-bar-tab-face (tab)
  "Replacement for \\='tab-bar-tab-face-default\\='.
Choose face for TAB."
  (if (eq (car tab) 'current-tab)
      'thattem-tab-bar/highlight-face-1
    'thattem-tab-bar/face-1))

(defcustom thattem-tab-bar-tab-face-function
  #'thattem-tab-bar-tab-face
  "Replacement for \\='tab-bar-tab-face-function\\='."
  :type 'function
  :group 'thattem-tab-bar)

(defvar thattem-tab-bar-close-button
  (propertize
   (nerd-icons-codicon "nf-cod-close"
                       :face 'thattem-tab-bar/face-1)
   'close-tab t
   'help-echo "Click to close tab")
  "Replacement for \\='tab-bar-close-button\\='.")

;; Helper function
(defun thattem-tab-bar/icon-for-buffer-name (buffer-name tab)
  "Return a nerd icon for the major mode of the BUFFER-NAME buffer.
Or return a question mark if the buffer does not exists.
The face is determined by TAB."
  (if (get-buffer buffer-name)
      (nerd-icons-icon-for-mode
       (with-current-buffer buffer-name major-mode)
       :face
       (funcall thattem-tab-bar-tab-face-function tab))
    (nerd-icons-codicon
     "nf-cod-question"
     :face
     (funcall thattem-tab-bar-tab-face-function tab))))

(defun thattem-tab-bar-name-format (tab i)
  "Replacement for \\='tab-bar-tab-name-format-default\\='.
Build the string shown in tab bar for No.I TAB."
  (let ((current-p (eq (car tab) 'current-tab)))
    (concat (propertize
             (if tab-bar-tab-hints (format "%d " i) "")
             'face (funcall thattem-tab-bar-tab-face-function tab))
            (thattem-tab-bar/icon-for-buffer-name
             (alist-get 'name tab) tab)
            (propertize
             (s-truncate 6 (alist-get 'name tab) " ")
             'face (funcall thattem-tab-bar-tab-face-function tab))
            (mapconcat
             (lambda (buffer)
               (thattem-tab-bar/icon-for-buffer-name
                buffer tab))
             (cdr(delete-dups (cons
                               (if current-p
                                   (current-buffer)
                                 (alist-get 'name tab))
                               (window-state-buffers
                                (if current-p
                                    (window-state-get)
                                  (alist-get 'ws tab))))))
             (propertize
              " " 'face (funcall thattem-tab-bar-tab-face-function tab)))
            (propertize
             " "'face (funcall thattem-tab-bar-tab-face-function tab))
            (or (and tab-bar-close-button-show
                     (not (eq tab-bar-close-button-show
                              (if current-p 'non-selected 'selected)))
                     thattem-tab-bar-close-button)
                ""))))

(defcustom thattem-tab-bar-name-format-function
  #'thattem-tab-bar-name-format
  "Replacement for \\='tab-bar-tab-name-format-function\\='."
  :type 'function
  :group 'thattem-tab-bar)

(defun thattem-tab-bar--format-tab (tab i)
  "Replacement for \\='tab-bar--format-tab\\='.
Format TAB using its index I."
  (append
   `((,(intern (format "left-sep-%i" i))
      menu-item
      ,(nerd-icons-powerline "nf-pl-left_hard_divider"
                             :face 'thattem-tab-bar/big-face-1)
      ignore :help ""))
   (cond
    ((eq (car tab) 'current-tab)
     `((current-tab
        menu-item
        ,(funcall thattem-tab-bar-name-format-function tab i)
        ignore
        :help "Current tab")))
    (t
     `((,(intern (format "tab-%i" i))
        menu-item
        ,(funcall thattem-tab-bar-name-format-function tab i)
        ,(alist-get 'binding tab)
        :help "Click to visit tab"))))
   `((,(intern (format "right-sep-%i" i))
      menu-item
      ,(nerd-icons-powerline "nf-pl-left_hard_divider"
                             :face 'thattem-tab-bar/big-face-2)
      ignore :help ""))
   (when (alist-get 'close-binding tab)
     `((,(if (eq (car tab) 'current-tab)
             'C-current-tab
           (intern (format "C-tab-%i" i)))
        menu-item ""
        ,(alist-get 'close-binding tab))))))

(defun thattem-tab-bar-format-tabs ()
  "Replacement for \\='tab-bar-format-tabs\\='."
  (let ((i 0))
    (mapcan
     (lambda (tab)
       (setq i (1+ i))
       (thattem-tab-bar--format-tab tab i))
     (funcall tab-bar-tabs-function))))

;;; Add tab button

(defvar thattem-tab-bar-new-button
  (nerd-icons-codicon "nf-cod-add"
                      :face 'thattem-tab-bar/highlight-face-2)
  "Replacement for \\='tab-bar-new-button\\='.")

(defun thattem-tab-bar-format-add-tab ()
  "Replacement for \\='tab-bar-format-add-tab\\='."
  (when thattem-tab-bar-new-button
    `((add-tab menu-item ,thattem-tab-bar-new-button tab-bar-new-tab
               :help "New tab"))))

;;; Align item

(defun thattem-tab-bar-format-align-right ()
  "Replacement for \\='tab-bar-format-align-right\\='."
  (let* ((rest (cdr (memq 'thattem-tab-bar-format-align-right tab-bar-format)))
         (rest (tab-bar-format-list rest))
         (rest (mapconcat (lambda (item) (nth 2 item)) rest ""))
         (hpos (progn
                 (add-face-text-property 0 (length rest) 'tab-bar t rest)
                 (string-pixel-width rest)))
         (str (propertize " " 'display
                          (if (window-system)
                              `(space :align-to (- right (,hpos)))
                            `(space :align-to (,(- (frame-inner-width)
                                                   hpos))))
                          'face 'thattem-tab-bar/face-2)))
    `((align-right menu-item ,str ignore))))

;;; Global information
(defun thattem-tab-bar-format-global ()
  "Replacement for \\='tab-bar-format-global\\='."
  `((global menu-item
            ,(format-mode-line global-mode-string
                               'thattem-tab-bar/highlight-face-2)
            ignore)))


(provide 'thattem-tab-bar-replacements)
;;; thattem-tab-bar-replacements.el ends here
