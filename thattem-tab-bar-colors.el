;;; Tab-bar-colors --- define colors used in tab bar. ;; -*- lexical-binding: t; -*-

;;; Commentary:
;; Tab-bar-colors defines the default colors used in Thattem-tab-bar.
;; You can customize these colors.

;;; Code:

(defcustom thattem-tab-bar/light-theme/bright-color "#ebebeb"
  "The default bright color for tab bar in light theme."
  :type 'string
  :group 'thattem-tab-bar)

(defcustom thattem-tab-bar/light-theme/bright-highlight-color "#ffffff"
  "The default highlight bright color for tab bar in light theme."
  :type 'string
  :group 'thattem-tab-bar)

(defcustom thattem-tab-bar/light-theme/dark-color "#b0d8f4"
  "The default dark color for tab bar in light theme."
  :type 'string
  :group 'thattem-tab-bar)

(defcustom thattem-tab-bar/light-theme/dark-highlight-color "#c0d0f0"
  "The default highlight dark color for tab bar in light theme."
  :type 'string
  :group 'thattem-tab-bar)


(provide 'thattem-tab-bar-colors)
;;; thattem-tab-bar-colors.el ends here
