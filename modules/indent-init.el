;;; indent-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup indentation in emacs.

;;; Code:

(setq-default fill-column 120
              standard-indent 2 ; set standard indent to 2 rather that 4
              tab-width 2
              indent-tabs-mode nil) ; spaces instead of tabs by default

(use-package aggressive-indent
  :ensure t
  :config (global-aggressive-indent-mode 1)
  :diminish aggressive-indent-mode)

;; intelligent indentation, on by default from Emacs 24.4
(use-package electric
  :disabled t
  :config
  ;;(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))
  (electric-indent-mode -1))

(use-package auto-indent-mode
  :ensure t
  :defer 5
  :config (auto-indent-global-mode 1))

(use-package highlight-indentation
  :ensure t
  :disabled t
  :config (highlight-indentation-mode 1))

;; indentation guide: doesn't seem to work well with company-mode and auto-complete-mode
(use-package indent-guide
  :ensure t
  :disabled t
  :init (setq indent-guide-delay 0.1) ; show guide lines only in idle-time
  :config (indent-guide-global-mode 1))

;; smart tabs (indent with tabs, align with spaces)
(use-package smart-tabs
  :ensure t
  :disabled t
  :config (global-smart-tab-mode 1))

;;(autoload 'smart-tabs-mode "smart-tabs-mode"
;;  "Intelligently indent with tabs, align with spaces!")
;;(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
;;(autoload 'smart-tabs-advice "smart-tabs-mode")
;;(autoload 'smart-tabs-insinuate "smart-tabs-mode")
;;(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml)

(provide 'indent-init)

;;; indent-init.el ends here
