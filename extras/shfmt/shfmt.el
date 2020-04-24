;;; shfmt.el --- Autoformat shell scripts -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/shfmt.el
;; Package-Requires: ((emacs "25.1") (reformatter "0.4"))
;; Keywords: languages

;;; Commentary:

;; Shell script autoformatting using shfmt; see https://github.com/mvdan/sh

;;; Code:

(require 'shfmt-common)
(require 'reformatter)

(defgroup shfmt nil
  "Auto-formatting for shell scripts"
  :group 'languages
  :prefix "shfmt-"
  :link '(url-link :tag "Site" "https://github.com/amake/shfmt.el")
  :link '(url-link :tag "Repository" "https://github.com/amake/shfmt.el.git"))

(defcustom shfmt-executable "shfmt"
  "The executable to run when autoformatting."
  :group 'shfmt
  :type 'file
  :safe #'stringp)

(defcustom shfmt-arguments nil
  "The args to supply to `shfmt-executable' when autoformatting."
  :group 'shfmt
  :type '(repeat string)
  :safe #'shfmt--list-of-strings-p)

(defun shfmt--list-of-strings-p (arg)
  "Check that ARG is a list of strings."
  (seq-every-p #'stringp arg))

(defun shfmt--build-argument-list ()
  "Build args list based on `shfmt-arguments' and user settings."
  (let ((indent (when (boundp 'sh-basic-offset)
                  `("-i" ,(number-to-string sh-basic-offset)))))
    `(,@indent ,@shfmt-arguments ,@(shfmt-common-get-parser-opts))))

;;;###autoload (autoload 'shfmt-buffer "current-file" nil t)
;;;###autoload (autoload 'shfmt-region "current-file" nil t)
;;;###autoload (autoload 'shfmt-on-save-mode "current-file" nil t)
(reformatter-define shfmt
  :program shfmt-executable
  :args (shfmt--build-argument-list)
  :lighter " shfmt"
  :group 'shfmt)

(provide 'shfmt)

;;; shfmt.el ends here
