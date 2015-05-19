;;; prog-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Programming mode specific.

;;; Code:

;; show the name of the function in the modeline
(use-package which-func
  :config
  (setq which-func-modes t)
  (add-hook 'prog-mode-hook
            (lambda ()
              (which-function-mode 1))))

(use-package semantic
  :disabled t
  :config
  ;; (require 'semantic-ia)
  ;; (require 'semantic-loaddefs)
  ;; (require 'semanticdb)
  ;; SemanticDB files
  (setq semanticdb-default-save-directory (concat dotemacs-temp-directory "semanticdb"))
  (global-semanticdb-minor-mode 1)
  (global-semantic-highlight-func-mode 1)
  (add-hook 'prog-mode-hook #'semantic-mode)
  (semantic-mode 1))

(use-package idle
  :disabled t
  :config
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-idle-completions-mode 1)
  (global-semantic-idle-breadcrumbs-mode 1)
  (global-semantic-idle-summary-mode 1))

(use-package mode
  :disabled t
  :config
  (global-semantic-decoration-mode 1)
  ;; Enable SRecode (Template management) minor-mode.
  (global-srecode-minor-mode 1))

;; this hides the tabbar
(use-package stickyfunc-enhance
  :disabled t
  :ensure t
  :init (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode))

(use-package electric
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (electric-layout-mode 1))))

(add-hook 'prog-mode-hook
          (lambda ()
            (abbrev-mode -1)))

;; add makefile.rules to makefile-gmake-mode for Intel Pin
(add-to-list 'auto-mode-alist '("makefile\\.rules\\'" . makefile-gmake-mode))

;; shell script mode
(use-package sh-script
  :defer t
  :config
  (setq sh-basic-offset 4
        sh-indent-comment t
        sh-indentation 4))

;; enable speed bar
(use-package speedbar
  :config
  (setq speedbar-use-images nil
        speedbar-show-unknown-files t)
  (use-package sr-speedbar
    :ensure t
    :config
    (setq sr-speedbar-right-side nil
          sr-speedbar-max-width 40
          sr-speedbar-width 30
          sr-speedbar-default-width 30
          sr-speedbar-skip-other-window-p t))
  ;; (add-hook 'prog-mode-hook
  ;;           '(lambda ()
  ;;              (when (window-system)
  ;;                (sr-speedbar-open))))
  )

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config
  ;;(add-hook 'c-mode-common-hook 'ws-butler-mode)
  (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package dtrt-indent
  :ensure t
  :config
  (setq dtrt-indent-verbosity 0)
  (add-hook 'prog-mode-hook
            (lambda()
              (dtrt-indent-mode 1))))

(provide 'prog-init)

;;; prog-init.el ends here
