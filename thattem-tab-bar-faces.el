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

;;; Define font height

(defcustom thattem-tab-bar-small-font-height
  0.8
  "The height factor of small part in tab bar."
  :type 'float
  :group 'thattem-tab-bar)

(defcustom thattem-tab-bar-middle-font-height
  1.2
  "The height factor of middle part in tab bar."
  :type 'float
  :group 'thattem-tab-bar)

(defcustom thattem-tab-bar-big-font-height
  1.5
  "The height factor of big part in tab bar."
  :type 'float
  :group 'thattem-tab-bar)

;;; Define faces

(defface thattem-tab-bar/face-1
  '((t
     :background "black"
     :foreground "white"))
  "First face for tab bar.")

(defface thattem-tab-bar/face-2
  '((t
     :background "white"
     :foreground "black"))
  "Second face for tab bar.")

(defface thattem-tab-bar/thin-face-1
  '((t
     :background "black"
     :foreground "white"))
  "First face with lighter weight for tab bar.")

(defface thattem-tab-bar/thin-face-2
  '((t
     :background "white"
     :foreground "black"))
  "Second face with lighter weight for tab bar.")

(defface thattem-tab-bar/highlight-face-1
  '((t
     :background "black"
     :foreground "white"))
  "First face for highlighted part on tab bar.")

(defface thattem-tab-bar/highlight-face-2
  '((t
     :background "white"
     :foreground "black"))
  "Second face for highlighted part on tab bar.")

(defface thattem-tab-bar/warning-face-1
  '((t
     :background "black"
     :foreground "white"))
  "First face for warning item on tab bar.")

(defface thattem-tab-bar/warning-face-2
  '((t
     :background "white"
     :foreground "black"))
  "Second face for warning item on tab bar.")


(provide 'thattem-tab-bar-faces)
;;; thattem-tab-bar-faces.el ends here
