;;; flyspell-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup spell check.  Assume aspell is available.

;;; Code:

(use-package flyspell
  :if (eq system-type 'gnu/linux)
  :preface
  (defun dotemacs--activate-flyspell ()
    "Turn on flyspell-mode and call flyspell-buffer."
    (interactive)
    (flyspell-mode) ; This REALLY slows buffer switching
    (flyspell-buffer))

  :init
  (use-package ispell
    :init
    (setq-default ispell-program-name "/usr/bin/aspell")
    (setq ispell-dictionary "english"
          ;; speed up aspell: ultra | fast | normal | bad-spellers
          ispell-extra-args '("--sug-mode=ultra"
                              "--lang=en_US")
          ;; Save a new word to personal dictionary without asking
          ispell-silently-savep t))
  
  (setq flyspell-sort-corrections t
        flyspell-issue-message-flag nil)

  ;; This is to turn on spell check in *scratch* buffer, which is in text-mode.
  (dolist (hook '(text-mode-hook find-file-hooks))
    (add-hook hook #'turn-on-flyspell))
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)

  ;; This is useful but slow
  ;; (add-hook 'before-save-hook #'flyspell-buffer)

  :config
  (use-package helm-flyspell
    :ensure t
    :if (eq dotemacs-selection 'helm)
    :config (bind-key "M-$" #'helm-flyspell-correct flyspell-mode-map))

  (use-package flyspell-popup
    :ensure t
    :config (bind-key "C-;" #'flyspell-popup-correct flyspell-mode-map))

  :diminish flyspell-mode
  :bind
  (("C-c f f" . flyspell-mode)
   ("C-c f b" . flyspell-buffer)
   ("C-c f w" . ispell-word)))

(provide 'flyspell-init)

;;; flyspell-init.el ends here
