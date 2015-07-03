;;; indent-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup indentation in Emacs.

;;; Code:

(setq-default fill-column 120
              standard-indent 2 ; set standard indent to 2 rather that 4
              tab-width 2
              ;; spaces instead of tabs by default
              indent-tabs-mode nil)

(use-package aggressive-indent
  :ensure t
  :init (global-aggressive-indent-mode 1)
  :diminish aggressive-indent-mode)

;; intelligent indentation, on by default from Emacs 24.4
(use-package electric
  :init (electric-indent-mode 1))

;; If you wish to use this with autopairs and yasnippet, please load this library first.
(use-package auto-indent-mode
  :disabled t
  :ensure t
  :diminish auto-indent-mode
  :init (auto-indent-global-mode 1)
  (setq auto-indent-on-visit-file t
        ;; other option: aggressive
        auto-indent-indent-style 'conservative))

(use-package highlight-indentation
  :disabled t
  :ensure t
  :init
  (highlight-indentation-mode 1)
  (highlight-indentation-current-column-mode 1))

;; indentation guide: doesn't seem to work well with company-mode, auto-complete-mode, and fci-mode as well
(use-package indent-guide
  :disabled t
  :ensure t
  :diminish indent-guide-mode
  :init
  (setq indent-guide-delay 1.0 ; show guide lines only in idle-time
        indent-guide-recursive t)
  (add-hook 'prog-mode-hook #'indent-guide-mode))

;; smart tabs (indent with tabs, align with spaces)
(use-package smart-tabs
  :disabled t
  :ensure t
  :init
  ;; (autoload 'smart-tabs-mode "smart-tabs-mode"
  ;; (autoload 'smart-tabs-mode-enable "smart-tabs-mode")
  ;; (autoload 'smart-tabs-advice "smart-tabs-mode")
  ;; (autoload 'smart-tabs-insinuate "smart-tabs-mode")
  ;; (smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml)
  (global-smart-tab-mode 1))

(provide 'indent-init)

;;; indent-init.el ends here
