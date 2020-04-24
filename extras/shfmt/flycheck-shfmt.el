;;; flycheck-shfmt.el --- Lint shell scripts -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/shfmt.el
;; Package-Requires: ((emacs "24") (flycheck "0.25"))
;; Keywords: languages

;;; Commentary:

;; Shell script linting using shfmt; see https://github.com/mvdan/sh

;;; Code:

(require 'shfmt-common)
(require 'flycheck)
(require 'sh-script)

(flycheck-define-checker sh-shfmt
  "A shell script syntax checker using shfmt.

See URL `https://github.com/mvdan/sh'."
  :command ("shfmt"
            (eval (shfmt-common-get-parser-opts)))
  :standard-input t
  :error-patterns
  ((error line-start
          ;; filename:line:column: message
          "<standard input>:" line ":" column ":" (zero-or-more " ") (message)
          line-end))
  :modes sh-mode
  :predicate (lambda () (memq sh-shell '(bash sh mksh)))
  :next-checkers ((warning . sh-bash)
                  (warning . sh-posix-bash)))

(defun flycheck-shfmt-setup ()
  "Set up the flycheck-shfmt checker."
  (interactive)
  (add-to-list 'flycheck-checkers 'sh-shfmt))

(provide 'flycheck-shfmt)

;;; flycheck-shfmt.el ends here
