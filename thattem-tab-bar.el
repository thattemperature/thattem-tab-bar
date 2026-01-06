;;; Thattem-tab-bar --- a simple tab bar  -*- lexical-binding: t; -*-

;; Author: That Temperature <2719023332@qq.com>
;; Package-Requires: ((emacs "30.1") nerd-icons)
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

;; Thattem-tab-bar is a package that change the appearance of the
;; default tab bar.  It DO NOT change the items used in tab bar,
;; like "tab-bar-format-tabs".  Instead, it defines replacement
;; for the items, like "thattem-tab-bar-format-tabs".
;;
;; The package will set "tab-bar-format" to use those replacement
;; as default.  You can also customize this variable.

;;; Code:

(defgroup thattem-tab-bar nil
  "Modified tab bar."
  :group 'convenience)

(require 'thattem-tab-bar-advices)

(defcustom thattem-tab-bar-format-default
  '(thattem-tab-bar-format-workspaces--before-helpler
    thattem-tab-bar-format-workspaces
    thattem-tab-bar-format-workspaces--after-helper
    thattem-tab-bar-format-history
    thattem-tab-bar-format-tabs
    thattem-tab-bar-format-add-tab
    thattem-tab-bar-format-align-right
    thattem-tab-bar-format-global)
  "New value for \\='tab-bar-format\\='."
  :type '(repeat symbol)
  :group 'thattem-tab-bar)

(defvar thattem-tab-bar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-bar wheel-up]
                #'thattem-tab-bar-deal-mouse-wheel-up)
    (define-key map [tab-bar wheel-down]
                #'thattem-tab-bar-deal-mouse-wheel-down)
    (define-key map [down-mouse-2]
                #'thattem-tab-bar-deal-down-mouse-2)
    map)
  "Keymap used by \\='thattem-tab-bar-mode\\='.")

(define-minor-mode thattem-tab-bar-mode
  "Toggle thattem tab bar mode."
  :global t

  (when thattem-tab-bar-mode
    (setq tab-bar-format thattem-tab-bar-format-default))
  (when thattem-tab-bar-mode
    (advice-add 'tab-bar-mouse-1 :around
                #'thattem-tab-bar--advice-around--tab-bar-mouse-1))
  (unless thattem-tab-bar-mode
    (advice-remove 'tab-bar-mouse-1
                   #'thattem-tab-bar--advice-around--tab-bar-mouse-1)))


(provide 'thattem-tab-bar)
;;; thattem-tab-bar.el ends here
