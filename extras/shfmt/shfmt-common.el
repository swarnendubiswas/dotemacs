;;; shfmt-common.el --- Common utils for shfmt -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/shfmt.el
;; Package-Requires: ((emacs "24"))
;; Keywords: languages

;;; Commentary:

;; Shell script autoformatting using shfmt; see https://github.com/mvdan/sh

;;; Code:

(require 'sh-script)

(defun shfmt-common-get-parser-opts ()
  "Get the appropriate parser options for the current buffer."
  (cond ((eq sh-shell 'bash) '("--ln" "bash"))
        ((eq sh-shell 'mksh) '("--ln" "mksh"))
        ((eq sh-shell 'sh) '("--ln" "posix"))))

(provide 'shfmt-common)
;;; shfmt-common.el ends here
