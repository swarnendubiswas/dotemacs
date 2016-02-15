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
        ivy-height 20
        ivy-display-style 'fancy
        ivy-re-builders-alist '((t . ivy--regex-plus))
        ivy-count-format "(%d/%d) ")
  (global-set-key (kbd "<f12>") 'ivy-resume)
  :config
  (use-package counsel
    :ensure t
    :bind
    (([remap describe-function] . counsel-describe-function)
     ;;("C-h f" . counsel-describe-function)
     ([remap describe-variable] . counsel-describe-variable)
     ;;("C-h v" . counsel-describe-variable)
     ([remap execute-extended-command] . counsel-M-x)
     ;; ("M-x" . counsel-M-x)
     ([remap find-file] . counsel-find-file)
     ;;("C-x C-f" . counsel-find-file)
     ("<f3>" . counsel-find-file)
     ("M-y" . counsel-yank-pop))
    :config
    ;; (global-set-key [remap describe-function] #'counsel-describe-function)
    ;; (global-set-key [remap describe-variable] #'counsel-describe-variable)
    ;; (global-set-key [remap execute-extended-command] #'counsel-M-x)
    ;; (global-set-key [remap find-file] #'counsel-find-file)
    (setq counsel-find-file-at-point t))
  :diminish ivy-mode)

(provide 'ivy-init)
;;; ivy-init.el ends here
