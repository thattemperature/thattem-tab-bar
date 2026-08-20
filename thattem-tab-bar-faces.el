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

(defmacro thattem-tab-bar--define-face (name attributes usage)
  "Define face used in thattem-tab-bar.

The name of the face will be \"thattem-tab-bar/{NAME}\".
The face is defined with ATTRIBUTES, and the docstring will be
\"Face for {USAGE} in thattem-tab-bar.\"."
  (declare (doc-string 3)
           (indent defun))
  `(defface ,(intern (format "thattem-tab-bar/%s"
                             (symbol-name name)))
     ,attributes
     ,(format "Face for %s in thattem-tab-bar."
              (string-trim usage
                           "[^[:alpha:]]+"
                           "[^[:alpha:]]+"))))

(defvar thattem-tab-bar--default-attribute-bright
  '((t
     :background "white"
     :foreground "black"))
  "Default face attributes of bright part.")

(defvar thattem-tab-bar--default-attribute-dark
  '((t
     :background "black"
     :foreground "white"))
  "Default face attributes of dark part.")


(thattem-tab-bar--define-face
  bright
  thattem-tab-bar--default-attribute-bright
  "Bright part.")

(thattem-tab-bar--define-face
  dark
  thattem-tab-bar--default-attribute-dark
  "Dark part.")

(thattem-tab-bar--define-face
  bright-thin
  thattem-tab-bar--default-attribute-bright
  "Bright part with thin font.")

(thattem-tab-bar--define-face
  dark-thin
  thattem-tab-bar--default-attribute-dark
  "Dark part with thin font.")

(thattem-tab-bar--define-face
  bright-highlight
  thattem-tab-bar--default-attribute-bright
  "Highlighted bright part.")

(thattem-tab-bar--define-face
  dark-highlight
  thattem-tab-bar--default-attribute-dark
  "Highlighted dark part.")

(thattem-tab-bar--define-face
  bright-warning
  thattem-tab-bar--default-attribute-bright
  "Bright warning symbols.")

(thattem-tab-bar--define-face
  dark-warning
  thattem-tab-bar--default-attribute-dark
  "Dark warning symbols.")


(provide 'thattem-tab-bar-faces)
;;; thattem-tab-bar-faces.el ends here
