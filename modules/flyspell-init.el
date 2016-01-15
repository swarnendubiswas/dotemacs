;;; flyspell-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup spell check.

;;; Code:

(use-package flyspell
  :if (eq system-type 'gnu/linux)
  :preface
  (defun dotemacs--activate-flyspell ()
    "Turn on flyspell-mode and call flyspell-buffer."
    (interactive)
    ;; This next line REALLY slows buffer switching.
    (flyspell-mode)
    (flyspell-buffer))

  (defvar customised-hooks-alist
    '(text-mode-hook)
    "An alist of hooks that require customizations.")

  :init
  (use-package ispell
    :init
    (setq-default ispell-program-name "/usr/bin/aspell")
    (setq ispell-dictionary "english"
          ;; speed up aspell: ultra | fast | normal | bad-spellers
          ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))

  (setq flyspell-sort-corrections t
        flyspell-check-region-doublons t
        flyspell-issue-message-flag nil)

  ;; this is to turn on spell check in *scratch* buffer, which is in text-mode.
  (dolist (hook '(text-mode-hook find-file-hooks))
    (add-hook hook #'turn-on-flyspell))

  ;; TODO: Is this slowing down editing Python?
  ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)

  ;; this is useful but slow
  ;; (add-hook 'before-save-hook 'flyspell-buffer)

  ;; ;; Activate flyspell for various major modes.
  ;; (unless noninteractive
  ;;   (add-hook-list customised-hooks-alist 'dotemacs--activate-flyspell))

  :config
  ;; use this package if there are performance issues with flyspell, note that this package disables spell checks for
  ;; certain special buffers, including *scratch*
  (use-package flyspell-lazy
    :ensure t
    :disabled t
    :init (flyspell-lazy-mode 1))

  (use-package helm-flyspell
    :ensure t
    :if (bound-and-true-p dotemacs-use-helm-p)
    :config (bind-key "M-$" #'helm-flyspell-correct flyspell-mode-map))

  (use-package helm-ispell
    :ensure t
    :if (bound-and-true-p dotemacs-use-helm-p))

  (use-package flyspell-popup
    :ensure t
    :config (bind-key "C-;" #'flyspell-popup-correct flyspell-mode-map))


  :diminish flyspell-mode
  :bind
  (("C-c f f" . flyspell-mode)
   ("C-c f b" . flyspell-buffer)
   ;; another alternative is M-$
   ("C-c f w" . ispell-word)))

(provide 'flyspell-init)

;;; flyspell-init.el ends here
