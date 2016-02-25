;;; indent-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup indentation in Emacs.

;;; Code:

(setq-default fill-column dotemacs-fill-column
              standard-indent 2 ; Set standard indent to 2 rather that 4
              tab-width 2
              ;; Spaces instead of tabs by default
              indent-tabs-mode nil)

(use-package aggressive-indent
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (aggressive-indent-mode 1)))
  :diminish aggressive-indent-mode)

(use-package electric ; Intelligent indentation, on by default from Emacs 24.4
  :init (electric-indent-mode 1))

;; If you wish to use this with autopairs and yasnippet, load this library first.
(use-package auto-indent-mode
  :ensure t
  :disabled t
  :diminish auto-indent-mode
  :init
  (auto-indent-global-mode 1)
  (setq auto-indent-on-visit-file t
        ;; Options: aggressive
        auto-indent-indent-style 'conservative))

(use-package highlight-indentation
  :ensure t
  :diminish highlight-indentation-mode
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (highlight-indentation-mode 1)
              (highlight-indentation-current-column-mode -1))))

;; Doesn't seem to work well with company-mode, auto-complete-mode, and fci-mode.
(use-package indent-guide
  :ensure t
  :disabled t
  :diminish indent-guide-mode
  :init
  (setq indent-guide-delay 1.0 ; Show guide lines only in idle-time
        indent-guide-recursive t)
  (add-hook 'prog-mode-hook #'indent-guide-mode))

(use-package smart-tabs ; Indent with tabs, align with spaces
  :ensure t
  :disabled t
  :init (global-smart-tab-mode 1))

(use-package dtrt-indent
  :ensure t
  :disabled t
  :diminish dtrt-indent-mode
  :config
  (setq dtrt-indent-verbosity 0)
  (add-hook 'prog-mode-hook
            (lambda ()
              (dtrt-indent-mode 1))))

(provide 'indent-init)

;;; indent-init.el ends here
