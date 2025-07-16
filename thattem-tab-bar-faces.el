;;; Tab-bar-faces --- define faces used in tab bar  -*- lexical-binding: t; -*-

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

;; Tab-bar-faces defines the text face used in Thattem-tab-bar.

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
     :background
     ,thattem-tab-bar/dark-theme/bright-color
     :foreground
     ,thattem-tab-bar/dark-theme/dark-color
     :height
     ,thattem-tab-bar/small-font-height
     :weight bold)
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
     :background
     ,thattem-tab-bar/dark-theme/dark-color
     :foreground
     ,thattem-tab-bar/dark-theme/bright-color
     :height
     ,thattem-tab-bar/small-font-height
     :weight bold)
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
     :background
     ,thattem-tab-bar/dark-theme/bright-color
     :foreground
     ,thattem-tab-bar/dark-theme/dark-color
     :height
     ,thattem-tab-bar/big-font-height)
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
     :background
     ,thattem-tab-bar/dark-theme/dark-color
     :foreground
     ,thattem-tab-bar/dark-theme/bright-color
     :height
     ,thattem-tab-bar/big-font-height)
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
     :background
     ,thattem-tab-bar/dark-theme/bright-color
     :foreground
     ,thattem-tab-bar/dark-theme/dark-highlight-color
     :height
     ,thattem-tab-bar/big-font-height
     :weight bold)
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
     :background
     ,thattem-tab-bar/dark-theme/dark-color
     :foreground
     ,thattem-tab-bar/dark-theme/bright-highlight-color
     :height
     ,thattem-tab-bar/big-font-height
     :weight bold)
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
