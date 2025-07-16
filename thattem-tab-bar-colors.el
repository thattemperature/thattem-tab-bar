;;; Tab-bar-colors --- define colors used in tab bar  -*- lexical-binding: t; -*-

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

(defcustom thattem-tab-bar/dark-theme/bright-color "#784c74"
  "The default bright color for tab bar in dark theme."
  :type 'string
  :group 'thattem-tab-bar)

(defcustom thattem-tab-bar/dark-theme/bright-highlight-color "#d89cb4"
  "The default highlight bright color for tab bar in dark theme."
  :type 'string
  :group 'thattem-tab-bar)

(defcustom thattem-tab-bar/dark-theme/dark-color "#303030"
  "The default dark color for tab bar in dark theme."
  :type 'string
  :group 'thattem-tab-bar)

(defcustom thattem-tab-bar/dark-theme/dark-highlight-color "#000000"
  "The default highlight dark color for tab bar in dark theme."
  :type 'string
  :group 'thattem-tab-bar)


(provide 'thattem-tab-bar-colors)
;;; thattem-tab-bar-colors.el ends here
