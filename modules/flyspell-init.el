;;; flyspell-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup spell check.

;;; Code:

(use-package flyspell
  :preface
  (defun activate-flyspell ()
    "Turn on flyspell-mode and call flyspell-buffer."
    (interactive)
    ;; This next line REALLY slows buffer switching.
    (flyspell-mode)
    (flyspell-buffer))

  (defvar customised-hooks-alist
    '(text-mode-hook org-mode-hook LaTeX-mode-hook)
    "An alist of hooks that require customisations.")

  ;; (unless noninteractive
  ;;   ;; Activate flyspell for various major modes.
  ;;   (add-hook-list customised-hooks-alist 'activate-flyspell))

  :init
  (use-package flyspell-lazy
    :disabled t
    :ensure t
    :config
    (flyspell-lazy-mode 1))
  (setq-default ispell-program-name "/usr/bin/aspell")
  ;; speed up aspell: ultra | fast | normal | bad-spellers
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
  (setq flyspell-sort-corrections t
        flyspell-check-region-doublons t
        flyspell-issue-message-flag nil)
  (add-hook 'find-file-hooks #'turn-on-flyspell)
  ;; this is useful but slow
  ;;(add-hook 'before-save-hook 'flyspell-buffer)
  ;; (eval-after-load "flyspell"
  ;;   '(diminish 'flyspell-mode))
  :diminish flyspell-mode
  :bind
  ("C-c i f" . flyspell-mode)
  ("C-c i b" . flyspell-buffer)
  ;; another alternative is M-$
  ("C-c i w" . ispell-word))

(provide 'flyspell-init)

;;; flyspell-init.el ends here
