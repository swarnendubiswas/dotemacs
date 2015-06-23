;;; prog-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Programming mode specific.

;;; Code:

;; show the name of the function in the modeline
(use-package which-func
  :config
  (set-face-attribute 'which-func nil :foreground "black")
  (setq which-func-modes t)
  (add-hook 'prog-mode-hook
            (lambda ()
              (which-function-mode 1))))

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

;; lisp and variants

(use-package prog-mode
  :config
  ;;(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
  (global-prettify-symbols-mode 1))

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  :diminish eldoc-mode)

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
  :defer t
  :config
  (setq speedbar-use-images nil
        speedbar-show-unknown-files t)

  (use-package sr-speedbar
    :ensure t
    :commands sr-speedbar-open
    :init (defalias 'speedbar 'sr-speedbar-open)
    :config
    (setq sr-speedbar-right-side nil
          sr-speedbar-max-width 40
          sr-speedbar-width 30
          sr-speedbar-default-width 30
          sr-speedbar-skip-other-window-p t))

  ;; (add-hook 'prog-mode-hook
  ;;           (lambda ()
  ;;              (when (window-system)
  ;;                (sr-speedbar-open))))
  )

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :config (add-hook 'prog-mode-hook #'ws-butler-mode))

(use-package dtrt-indent
  :ensure t
  :config
  (setq dtrt-indent-verbosity 0)
  (add-hook 'prog-mode-hook
            (lambda()
              (dtrt-indent-mode 1))))

(use-package web-mode
  :ensure t
  :defer t
  :commands (web-beautify-css web-beautify-html web-beautify-js)
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.jsp\\'" . web-mode)
   ("\\.html\\'" . web-mode)
   ("\\.hb\\.html\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode))
  :config
  ;; everything is indented 2 spaces
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)

  (use-package web-beautify
    :ensure t
    :config
    (with-eval-after-load "js2-mode"
      (add-hook 'js2-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

    (with-eval-after-load "json-mode"
      (add-hook 'json-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

    (with-eval-after-load "sgml-mode"
      (add-hook 'html-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

    (with-eval-after-load "css-mode"
      (add-hook 'css-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))))

(provide 'prog-init)

;;; prog-init.el ends here
