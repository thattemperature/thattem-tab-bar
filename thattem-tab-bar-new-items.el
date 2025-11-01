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

;;; Workspace control

(defvar thattem-tab-bar-current-workspace-icon
  (nerd-icons-octicon "nf-oct-dot_fill"
                      :face 'thattem-tab-bar/big-face-2
                      :v-adjust 0)
  "Icon of current workspace shows in the thattem-tab-bar.")

(defvar thattem-tab-bar-workspace-icon
  (nerd-icons-octicon "nf-oct-dot"
                      :face 'thattem-tab-bar/big-face-2
                      :v-adjust 0)
  "Icon of common workspace shows in the thattem-tab-bar.")

(defcustom thattem-tab-bar-wmctrl-executable "wmctrl"
  "The wmctrl executable on your system to be used by thattem-tab-bar."
  :type 'string
  :group 'thattem-tab-bar)

(defun thattem-tab-bar-switch-workspace (id)
  "Switch to workspace with ID."
  (ignore-errors
    (shell-command-to-string
     (concat thattem-tab-bar-wmctrl-executable " -s " id))))

(defun thattem-tab-bar-get-workspace-list ()
  "Get the list of workspace by `wmctrl` shell command."
  (ignore-errors
    (split-string
     (shell-command-to-string
      (concat thattem-tab-bar-wmctrl-executable " -d"))
     "[\n\r]+" t)))

(defun thattem-tab-bar-get-workspace-id (workspace)
  "Get the id of the WORKSPACE."
  (car (split-string workspace "[ \t\v]+")))

(defun thattem-tab-bar-current-workspace-p (workspace)
  "Return non-nil if WORKSPACE is current workspace.
WORKSPACE should be a line of `wmctrl -d` command."
  (string= "*" (cadr (split-string workspace "[ \t\v]+"))))

(defun thattem-tab-bar--format-workspace (workspace)
  "Format WORKSPACE and return the result as a keymap."
  (let ((current-p (thattem-tab-bar-current-workspace-p workspace)))
    (cond
     (current-p
      `((current-workspace
         menu-item
         ,(propertize
           thattem-tab-bar-current-workspace-icon
           'type 'workspace)
         ignore
         :help "Current workspace")))
     (t
      (let ((id (thattem-tab-bar-get-workspace-id workspace)))
        `((,(intern (format "workspace-%s" id))
           menu-item
           ,(propertize
             thattem-tab-bar-workspace-icon
             'type 'workspace
             'id id)
           ignore
           :help "Click to change workspace")))))))

(defun thattem-tab-bar-format-workspaces--before-helpler ()
  "A helpler function to get workspace information and save as frame \
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
             #'thattem-tab-bar-format-workspaces--before-helpler)

(defun thattem-tab-bar-format-workspaces ()
  "Produce workspace control items for the tab bar."
  (let ((workspace-list (frame-parameter nil 'workspace-list)))
    (if workspace-list
        (mapcan
         #'thattem-tab-bar--format-workspace
         workspace-list)
      (propertize "Cannot get workspace information!"
                  'face 'thattem-tab-bar/highlight-face-2))))

;; Since mouse wheel event on tab bar cannot get the `posn-string`,
;; we cannot use text property to judge whether scroll workspace or
;; scroll tab, so I save the X coordinate of the workspace item as
;; frame parameter and use it as the judgment.
(defun thattem-tab-bar-format-workspaces--after-helper ()
  "A helper function to set workspace item's X coordinate as frame \
parameter."
  (let* ((super #'thattem-tab-bar-format-workspaces)
         (position (seq-position tab-bar-format super))
         (before (seq-take tab-bar-format position))
         (super (funcall super))
         (super (mapconcat (lambda (item) (nth 2 item)) super ""))
         (super-width (progn
                        (add-face-text-property
                         0 (length super) 'tab-bar t super)
                        (string-pixel-width super))))
    (if (memq #'thattem-tab-bar-format-align-right before)
        (let* ((rest (cdr (memq super tab-bar-format)))
               (rest (cl-set-difference
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


(provide 'thattem-tab-bar-new-items)
;;; thattem-tab-bar-new-items.el ends here
