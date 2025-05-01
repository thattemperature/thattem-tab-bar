;;; Thattem-tab-bar --- a simple tab bar. ;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Thattem-tab-bar is a package that change the appearance of the
;; default tab bar.  It DO NOT change the items used in tab bar,
;; like "tab-bar-format-tabs".  Instead, it defines replacement
;; for the items, like "thattem-tab-bar-format-tabs".
;;
;; The package will set "tab-bar-format" to use those replacement
;; as default.  You can also customize this variable.

;;; Code:

;; Dependencies
(require 'nerd-icons)

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
