;;; prog-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Programming mode specific.

;;; Code:

;; show the name of the function in the modeline
(use-package which-func
  :config
  (add-to-list 'which-func-modes '(java-mode c++-mode c-mode python-mode))
  (add-hook 'prog-mode-hook 'which-func-mode))

(use-package electric
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (electric-layout-mode 1))))

(add-hook 'prog-mode-hook
          (lambda ()
            (abbrev-mode -1)))

(add-hook 'prog-mode-hook #'semantic-mode)

;; tags

(use-package ctags
  :ensure t
  :defer t)

(use-package ctags-update
  :ensure t
  :defer t)

(use-package etags
  :bind ("M-T" . tags-search))

(use-package gtags
  :ensure t
  :defer t)

(use-package helm-gtags
  :ensure t
  :diminish helm-gtags-mode
  :config (add-hook 'prog-mode-hook #'helm-gtags-mode))

(use-package ggtags
  :ensure t
  :diminish ggtags-mode
  :config (add-hook 'prog-mode-hook #'ggtags-mode))

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

(provide 'prog-init)

;;; prog-init.el ends here
