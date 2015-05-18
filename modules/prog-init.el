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

(setq tags-revert-without-query t
      ;; t=case-insensitive, nil=case-sensitive
      tags-case-fold-search nil)

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

(use-package ctags-update
  :ensure t
  :diminish ctags-auto-update-mode
  :config
  (setq ctags-update-delay-seconds (* 30 60)) ; every 1/2 hour
  (add-hook 'prog-mode-hook #'turn-on-ctags-auto-update-mode))

;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-gtags.el
;; front end to gnu global, use gtags -v
(use-package ggtags
  :ensure t
  :diminish ggtags-mode
  :config
  (setq ggtags-navigation-mode-lighter nil
        ggtags-oversize-limit (* 30 1024 1024)
        ;; use helm for completion
        ggtags-completing-read-function nil)
  (add-hook 'prog-mode-hook #'ggtags-mode))

(defhydra hydra-ggtags (:color blue)
  "ggtags"
  ("s" 'ggtags-find-other-symbol "find other symbol")
  ("h" 'ggtags-view-tag-history "view tag history")
  ("r" 'ggtags-find-reference "find reference")
  ("f" 'ggtags-find-file "find file")
  ("c" 'ggtags-create-tags "create tags")
  ("u" 'ggtags-update-tags "update tags"))
(bind-key "C-c g" 'hydra-ggtags/body)

;; http://wikemacs.org/wiki/C-ide
;; http://tuhdo.github.io/c-ide.html
;; use M-n to move to next candidate and M-p to move back previous candidate. Use M-g s to invoke Isearch on candidate buffer list 
(use-package helm-gtags
  :ensure t
  :diminish helm-gtags-mode
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-c g"
        helm-gtags-suggested-key-mapping t)
  (add-hook 'prog-mode-hook #'helm-gtags-mode)
  (bind-key "M-." 'helm-gtags-dwim helm-gtags-mode-map)
  (bind-key "M-," 'helm-gtags-pop-stack helm-gtags-mode-map)
  (bind-key "M-s" 'helm-gtags-select helm-gtags-mode-map))

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
