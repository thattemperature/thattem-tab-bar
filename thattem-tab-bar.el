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

(require 'thattem-tab-bar-replacements)

(setq tab-bar-format
      '(thattem-tab-bar-format-history
        thattem-tab-bar-format-tabs
        thattem-tab-bar-format-add-tab
        thattem-tab-bar-format-align-right
        thattem-tab-bar-format-global))

(provide 'thattem-tab-bar)
;;; thattem-tab-bar.el ends here
