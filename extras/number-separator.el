;;; number-separator.el --- Separate long integers -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jeff Filipovits

;; Author: Jeff Filipovits <jrfilipovits@gmail.com>
;; Url: https://github.com/legalnonsense/number-separator.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;;
;; Keywords: interger, readability, number

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Separate long integers with commas or periods using font-lock
;; E.g., 3000000 becomes 3,000,000

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;; Installation and usage

;;; Put number-separator.el into your load path and:
;;; (require 'number-seprator)
;;; (number-separator-mode)

;;;; Tips

;; Customize variables in the number-separator group

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'font-lock)

;;;; Custom variables

(defcustom number-separator ","
  "Character to separate integers. Default: \",\"."
  :type 'string)

(defcustom number-separator-interval 3
  "Number of numbers per group. Default: 3 (i.e., United States system.)"
  :type 'integer)

(defcustom number-separator-ignore-threshold 4
  "Ignore numbers with this many digits. Default: 4. 
This prevents separating four digit years."
  :type 'integer)

(defcustom number-separator-decimal-char "."
  "Character to separate an integer from its factional/decimal value.
Default: \".\""
  :type 'string)

;;;; Face

(defface number-separator-face
  '((t (:foreground "red")))
  "Face applied to the number separators.")

(defun number-separator-convert-to-regexp (string)
  "Add escape characters to a period."
  (if (string= "." string)
      "\\."
    string))

;;;; Variable
(defvar number-separator--font-lock-keyword  
  `((,(concat
       (number-separator-convert-to-regexp number-separator-decimal-char)
       "?[^"
       number-separator
       "[:space:] ][[:digit:]]+")
     (0 (list
	 'face nil
	 'display (number-separator)))))
  "Font lock keyword to find decimal numbers.")

;;;; Function

(defun number-separator ()  
  "Return a new string representing NUMBER with digit group separates added.
If NUMBER is nil, use the current `match-string'."
  (let ((number (match-string 0)))
    (if (or (string-prefix-p number-separator-decimal-char number)
	    (<= (length number) number-separator-ignore-threshold))
	nil
      (cl-loop for x from (- (length number) number-separator-interval)
	       downto 1
	       by number-separator-interval
	       do (setq number
			(concat (substring number 0 x)
				number-separator
				(substring number x (length number))))
	       finally return number))))

;;;; Minor mode

(define-minor-mode number-separator-mode
  "Separate long numbers."
  nil
  " numsep"
  nil  
  (if number-separator-mode
      (progn
	(add-face-text-property 0 1 'number-separator-face nil number-separator)
	(push 'display font-lock-extra-managed-props)
	(font-lock-add-keywords nil number-separator--font-lock-keyword)
	(font-lock-flush (point-min) (point-max)))
    (font-lock-remove-keywords nil number-separator--font-lock-keyword)
    (font-lock-flush (point-min) (point-max))))

;;;; Footer

(provide 'number-separator)

;; number-separator.el ends here
