;;; Thattem-tab-bar -- a simple tab bar. ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Dependencies
(require 's)
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
