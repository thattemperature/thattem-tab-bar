;;; Tab-bar-faces --- define faces used in tab bar. ;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'thattem-tab-bar-colors)

;;; Define font height

(defcustom thattem-tab-bar/small-font-height
  160
  "Font height of small part in tab bar."
  :type 'integer
  :group 'thattem-tab-bar)

(defcustom thattem-tab-bar/big-font-height
  300
  "Font height of big part in tab bar."
  :type 'integer
  :group 'thattem-tab-bar)

;;; Define faces

(defface thattem-tab-bar/face-1
  `((((class color) (background dark))
     ;; TODO
     )
    (((class color) (background light))
     :background
     ,thattem-tab-bar/light-theme/dark-color
     :foreground
     ,thattem-tab-bar/light-theme/bright-color
     :height
     ,thattem-tab-bar/small-font-height
     :weight bold)
    (t :inverse-video t))
  "First face for tab bar.")

(defface thattem-tab-bar/face-2
  `((((class color) (background dark))
     ;; TODO
     )
    (((class color) (background light))
     :background
     ,thattem-tab-bar/light-theme/bright-color
     :foreground
     ,thattem-tab-bar/light-theme/dark-color
     :height
     ,thattem-tab-bar/small-font-height
     :weight bold)
    (t :inverse-video t))
  "Second face for tab bar.")

(defface thattem-tab-bar/big-face-1
  `((((class color) (background dark))
     ;; TODO
     )
    (((class color) (background light))
     :background
     ,thattem-tab-bar/light-theme/dark-color
     :foreground
     ,thattem-tab-bar/light-theme/bright-color
     :height
     ,thattem-tab-bar/big-font-height)
    (t :inverse-video t))
  "First face for tab bar big part.")

(defface thattem-tab-bar/big-face-2
  `((((class color) (background dark))
     ;; TODO
     )
    (((class color) (background light))
     :background
     ,thattem-tab-bar/light-theme/bright-color
     :foreground
     ,thattem-tab-bar/light-theme/dark-color
     :height
     ,thattem-tab-bar/big-font-height)
    (t :inverse-video t))
  "Second face for tab bar big part.")

(defface thattem-tab-bar/highlight-face-1
  `((((class color) (background dark))
     ;; TODO
     )
    (((class color) (background light))
     :background
     ,thattem-tab-bar/light-theme/dark-color
     :foreground
     ,thattem-tab-bar/light-theme/bright-highlight-color
     :height
     ,thattem-tab-bar/big-font-height
     :weight bold)
    (t :inverse-video t))
  "First face for highlighted part on tab bar.")

(defface thattem-tab-bar/highlight-face-2
  `((((class color) (background dark))
     ;; TODO
     )
    (((class color) (background light))
     :background
     ,thattem-tab-bar/light-theme/bright-color
     :foreground
     ,thattem-tab-bar/light-theme/dark-highlight-color
     :height
     ,thattem-tab-bar/big-font-height
     :weight bold)
    (t :inverse-video t))
  "Second face for highlighted part on tab bar.")


(provide 'thattem-tab-bar-faces)
;;; thattem-tab-bar-faces.el ends here
