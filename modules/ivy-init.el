;;; ivy-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup ivy mode as a replacement for ido.

;;; Code:

(use-package ivy
  :ensure swiper
  :if (bound-and-true-p dotemacs-prefer-ivy-over-ido-p)
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full
        ivy-wrap t
        ivy-height 10
        ivy-display-style 'fancy)
  (global-set-key (kbd "<f12>") 'ivy-resume)
  (use-package counsel
    :ensure t
    :bind
    (("C-h f" . counsel-describe-function)
     ("C-h v" . counsel-describe-variable)
     ("M-x" . counsel-M-x)
     ("<f3>" . counsel-find-file)
     ("C-x C-f" . counsel-find-file)
     ("M-y" . counsel-yank-pop))
    :config
    (setq counsel-find-file-at-point t)

    ;; (global-set-key [remap execute-extended-command] #'counsel-M-x)
    ;; (bind-key "M-x" #'counsel-M-x)

    ;; (global-set-key [remap describe-function] #'counsel-describe-function)
    ;; (global-set-key [remap describe-variable] #'counsel-describe-variable)

    ;; (global-set-key [remap find-file] #'counsel-find-file)
    ;; (bind-key "<f3>" #'counsel-find-file)

    ;; (bind-key "M-y" #'counsel-yank-pop)
    )
  :diminish ivy-mode)

(provide 'ivy-init)
;;; ivy-init.el ends here
