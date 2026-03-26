;;; Tab-bar-new-items --- define some new tab bar items  -*- lexical-binding: t; -*-

;; Author: That Temperature <2719023332@qq.com>
;; URL: https://github.com/thattemperature/thattem-tab-bar

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tab-bar-new-items defines some new tab bar items.

;;; Code:

(require 'thattem-tab-bar-replacements)

;;; Executable and library path configurations

(eval-and-compile
  (defcustom thattem-tab-bar-wmctrl-executable "wmctrl"
    "The wmctrl executable on your system to be used by thattem-tab-bar."
    :type 'string
    :group 'thattem-tab-bar)

  (defcustom thattem-tab-bar-thattem-library-path
    "/usr/local/lib/libthattem_emacs_library.so"
    "The path of thattem-emacs-library."
    :type 'string
    :group 'thattem-tab-bar)

  (module-load thattem-tab-bar-thattem-library-path))

;;; Align item

(defun thattem-tab-bar-format-align-middle ()
  "Align the rest of tab bar items to the middle.

It will keep items after \\='thattem-tab-bar-format-align-right\\='
align to the right.  And if the \"right part\" is long, the
\"middle part\" will be pushed to the left."
  (let* ((middle (cdr (memq 'thattem-tab-bar-format-align-middle
                            tab-bar-format)))
         (middle (cl-set-difference
                  middle (memq 'thattem-tab-bar-format-align-right
                               tab-bar-format)))
         (middle (cl-set-difference
                  middle thattem-tab-bar-not-eval-item-list))
         (middle (tab-bar-format-list middle))
         (middle (mapconcat (lambda (item) (nth 2 item)) middle ""))
         (hpos-middle (progn
                        (add-face-text-property
                         0 (length middle) 'tab-bar t middle)
                        (string-pixel-width middle)))
         (rest (cdr (memq 'thattem-tab-bar-format-align-right
                          tab-bar-format)))
         (rest (cl-set-difference
                rest thattem-tab-bar-not-eval-item-list))
         (rest (tab-bar-format-list rest))
         (rest (mapconcat (lambda (item) (nth 2 item)) rest ""))
         (hpos-rest (progn
                      (add-face-text-property
                       0 (length rest) 'tab-bar t rest)
                      (string-pixel-width rest)))
         (str (propertize " " 'display
                          `(space :align-to
                                  (,(min(/ (- (frame-inner-width)
                                              hpos-middle)
                                           2)
                                        (- (frame-inner-width)
                                           hpos-middle
                                           hpos-rest))))
                          'face `(thattem-tab-bar/face-2))))
    `((align-right menu-item ,str ignore))))

;;; Workspace control

(defun thattem-tab-bar-switch-workspace (id)
  "Switch to workspace with ID."
  (ignore-errors
    (process-lines thattem-tab-bar-wmctrl-executable "-s" id)))

(defun thattem-tab-bar-get-workspace-list ()
  "Get the list of workspace by `wmctrl` shell command."
  (ignore-errors
    (process-lines thattem-tab-bar-wmctrl-executable "-d")))

(defun thattem-tab-bar-get-workspace-id (workspace)
  "Get the id of the WORKSPACE."
  (car (split-string workspace "[ \t\v]+")))

(defun thattem-tab-bar-current-workspace-p (workspace)
  "Return non-nil if WORKSPACE is current workspace.
WORKSPACE should be a line of `wmctrl -d` command."
  (string= "*" (cadr (split-string workspace "[ \t\v]+"))))

(defun thattem-tab-bar--format-workspace (workspace)
  "Format WORKSPACE and return the result as a keymap."
  (let ((current-p (thattem-tab-bar-current-workspace-p workspace))
        (id (thattem-tab-bar-get-workspace-id workspace))
        (space (propertize
                " " 'face
                `(thattem-tab-bar/thin-face-2
                  (:height ,thattem-tab-bar-small-font-height)))))
    (cond
     (current-p
      `((current-workspace
         menu-item
         ,(propertize
           (concat
            space
            (nerd-icons-mdicon
             (format "nf-md-numeric_%d_circle"
                     (1+ (% (string-to-number id) 10)))
             :face `(thattem-tab-bar/thin-face-2
                     (:height ,thattem-tab-bar-big-font-height)))
            space)
           'type 'workspace)
         ignore
         :help "Current workspace")))
     (t
      `((,(intern (format "workspace-%s" id))
         menu-item
         ,(propertize
           (concat
            space
            (nerd-icons-mdicon
             (format "nf-md-numeric_%d_circle_outline"
                     (1+ (% (string-to-number id) 10)))
             :face `(thattem-tab-bar/thin-face-2
                     (:height ,thattem-tab-bar-big-font-height)))
            space)
           'type 'workspace
           'id id)
         ignore
         :help "Click to change workspace"))))))

(defun thattem-tab-bar-format-workspaces--before-helper ()
  "A helper function to get workspace information and save as frame \
parameter."
  (let* ((workspace-list (thattem-tab-bar-get-workspace-list))
         (list-length (length workspace-list))
         (current-workspace (cl-find-if
                             #'thattem-tab-bar-current-workspace-p
                             workspace-list))
         (specialized-current-id (when current-workspace
                                   (+
                                    (string-to-number
                                     (thattem-tab-bar-get-workspace-id
                                      current-workspace))
                                    list-length)))
         (previous-id (when current-workspace
                        (number-to-string
                         (% (1- specialized-current-id)
                            list-length))))
         (next-id (when current-workspace
                    (number-to-string
                     (% (1+ specialized-current-id)
                        list-length)))))
    (set-frame-parameter nil 'workspace-list workspace-list)
    (set-frame-parameter nil 'previous-workspace-id previous-id)
    (set-frame-parameter nil 'next-workspace-id next-id)))

(add-to-list 'thattem-tab-bar-not-eval-item-list
             #'thattem-tab-bar-format-workspaces--before-helper)

(defun thattem-tab-bar-format-workspaces ()
  "Produce workspace control items for the tab bar."
  (let ((workspace-list (frame-parameter nil 'workspace-list)))
    (if workspace-list
        (mapcan
         #'thattem-tab-bar--format-workspace
         workspace-list)
      `((workspace-error
         menu-item
         ,(propertize
           "Cannot get workspace information!"
           'face `(thattem-tab-bar/highlight-face-2
                   (:height ,thattem-tab-bar-big-font-height)))
         ignore)))))

;; Since mouse wheel event on tab bar cannot get the `posn-string`,
;; we cannot use text property to judge whether scroll workspace or
;; scroll tab, so I save the X coordinate of the workspace item as
;; frame parameter and use it as the judgment.
(defun thattem-tab-bar-format-workspaces--after-helper ()
  "A helper function to set workspace item's X coordinate as frame \
parameter."
  (let* ((super #'thattem-tab-bar-format-workspaces)
         (before (cl-set-difference
                  tab-bar-format (memq super tab-bar-format)))
         (rest (cdr (memq super tab-bar-format)))
         (super (tab-bar-format-list (list super)))
         (super (mapconcat (lambda (item) (nth 2 item)) super ""))
         (super-width (progn
                        (add-face-text-property
                         0 (length super) 'tab-bar t super)
                        (string-pixel-width super))))
    (if (memq #'thattem-tab-bar-format-align-right before)
        (let* ((rest (cl-set-difference
                      rest thattem-tab-bar-not-eval-item-list))
               (rest (tab-bar-format-list rest))
               (rest (mapconcat (lambda (item) (nth 2 item)) rest ""))
               (rest-width (progn
                             (add-face-text-property
                              0 (length rest) 'tab-bar t rest)
                             (string-pixel-width rest)))
               (frame-width (frame-inner-width)))
          (set-frame-parameter nil 'workspace-x-bound
                               (cons (- frame-width
                                        rest-width super-width)
                                     (- frame-width rest-width))))
      (let* ((before (cl-set-difference
                      before thattem-tab-bar-not-eval-item-list))
             (before (tab-bar-format-list before))
             (before (mapconcat (lambda (item) (nth 2 item)) before ""))
             (before-width (progn
                             (add-face-text-property
                              0 (length before) 'tab-bar t before)
                             (string-pixel-width before))))
        (set-frame-parameter nil 'workspace-x-bound
                             (cons before-width
                                   (+ before-width super-width)))))))

(add-to-list 'thattem-tab-bar-not-eval-item-list
             #'thattem-tab-bar-format-workspaces--after-helper)

;;; System monitor

(defcustom thattem-tab-bar-threshold-high 80
  "The threshold of system monitor item in percentage.
If the (cpu/memory/swap) usage is greater than this,
that item will be shown with different face."
  :type 'integer
  :group 'thattem-tab-bar)

(defvar thattem-tab-bar-cpu-percentage 0
  "The system CPU usage percentage.")

(defvar thattem-tab-bar-mem-percentage 0
  "The system memory usage percentage.")

(defvar thattem-tab-bar-swap-percentage 0
  "The system swap usage percentage.")

(defvar thattem-tab-bar-upload-speed ""
  "The system network upload speed.")

(defvar thattem-tab-bar-download-speed ""
  "The system network download speed.")

(defun thattem-tab-bar-update-system-monitor ()
  "Update system monitor data."
  (setq thattem-tab-bar-cpu-percentage
        (round (* (thattem-cpu-usage) 100)))
  (setq thattem-tab-bar-mem-percentage
        (round (* (thattem-mem-usage) 100)))
  (setq thattem-tab-bar-swap-percentage
        (round (* (thattem-swap-usage) 100)))
  (let ((net (thattem-net-speed)))
    (setq thattem-tab-bar-download-speed
          (file-size-human-readable (car net) 'si nil "B"))
    (setq thattem-tab-bar-upload-speed
          (file-size-human-readable (cadr net) 'si nil "B"))))

(defvar thattem-tab-bar-system-monitor-timer nil
  "The timer object to update system monitor.")

(defun thattem-tab-bar-format-system-monitor ()
  "Produce system monitor items for the tab bar."
  (append
   `((system-monitor-left-sep
      menu-item
      ,(nerd-icons-powerline
        "nf-ple-pixelated_squares_small_mirrored"
        :face `(thattem-tab-bar/thin-face-2
                :height ,thattem-tab-bar-big-font-height))
      ignore :help ""))
   `((system-monitor-cpu
      menu-item
      ,(concat
        (nerd-icons-octicon
         "nf-oct-cpu"
         :face `(,(if (< thattem-tab-bar-cpu-percentage
                         thattem-tab-bar-threshold-high)
                      'thattem-tab-bar/highlight-face-1
                    'thattem-tab-bar/warning-face-1)
                 (:height ,thattem-tab-bar-middle-font-height)))
        (propertize
         (format "%3d%% " thattem-tab-bar-cpu-percentage)
         'face `(,(if (< thattem-tab-bar-cpu-percentage
                         thattem-tab-bar-threshold-high)
                      'thattem-tab-bar/highlight-face-1
                    'thattem-tab-bar/warning-face-1)
                 (:height ,thattem-tab-bar-middle-font-height))))
      ignore
      :help "CPU usage"))
   `((system-monitor-mem
      menu-item
      ,(concat
        (nerd-icons-faicon
         "nf-fa-memory"
         :face `(,(if (< thattem-tab-bar-mem-percentage
                         thattem-tab-bar-threshold-high)
                      'thattem-tab-bar/highlight-face-1
                    'thattem-tab-bar/warning-face-1)
                 (:height ,thattem-tab-bar-middle-font-height)))
        (propertize
         (format "%3d%% " thattem-tab-bar-mem-percentage)
         'face `(,(if (< thattem-tab-bar-mem-percentage
                         thattem-tab-bar-threshold-high)
                      'thattem-tab-bar/highlight-face-1
                    'thattem-tab-bar/warning-face-1)
                 (:height ,thattem-tab-bar-middle-font-height))))
      ignore
      :help "Memory usage"))
   `((system-monitor-swap
      menu-item
      ,(concat
        (nerd-icons-mdicon
         "nf-md-swap_horizontal_bold"
         :face `(,(if (< thattem-tab-bar-swap-percentage
                         thattem-tab-bar-threshold-high)
                      'thattem-tab-bar/highlight-face-1
                    'thattem-tab-bar/warning-face-1)
                 (:height ,thattem-tab-bar-middle-font-height)))
        (propertize
         (format "%3d%% " thattem-tab-bar-swap-percentage)
         'face `(,(if (< thattem-tab-bar-swap-percentage
                         thattem-tab-bar-threshold-high)
                      'thattem-tab-bar/highlight-face-1
                    'thattem-tab-bar/warning-face-1)
                 (:height ,thattem-tab-bar-middle-font-height))))
      ignore
      :help "Swap usage"))
   `((system-monitor-upload
      menu-item
      ,(concat
        (nerd-icons-mdicon
         "nf-md-upload"
         :face `(thattem-tab-bar/highlight-face-1
                 (:height ,thattem-tab-bar-middle-font-height)))
        (propertize
         (format "%5s " thattem-tab-bar-upload-speed)
         'face `(thattem-tab-bar/highlight-face-1
                 (:height ,thattem-tab-bar-middle-font-height))))
      ignore
      :help "Upload speed"))
   `((system-monitor-download
      menu-item
      ,(concat
        (nerd-icons-mdicon
         "nf-md-download"
         :face `(thattem-tab-bar/highlight-face-1
                 (:height ,thattem-tab-bar-middle-font-height)))
        (propertize
         (format "%5s " thattem-tab-bar-download-speed)
         'face `(thattem-tab-bar/highlight-face-1
                 (:height ,thattem-tab-bar-middle-font-height))))
      ignore
      :help "Download speed"))
   `((system-monitor-right-sep
      menu-item
      ,(nerd-icons-powerline
        "nf-ple-pixelated_squares_big"
        :face `(thattem-tab-bar/thin-face-2
                :height ,thattem-tab-bar-big-font-height))
      ignore :help ""))))


(provide 'thattem-tab-bar-new-items)
;;; thattem-tab-bar-new-items.el ends here
