;;; prog-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Programming mode specific.

;;; Code:

(use-package prog-mode
  :defer t
  :config
  (global-prettify-symbols-mode 1)

  (when (eq dotemacs-completion 'auto-complete)
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (add-to-list 'ac-sources 'ac-source-symbols)
                (add-to-list 'ac-sources 'ac-source-variables)
                (add-to-list 'ac-sources 'ac-source-functions))))

  (use-package which-func ; show the name of the function in the modeline
    :config
    (setq which-func-modes t)
    (add-hook 'prog-mode-hook
              (lambda ()
                (which-function-mode 1)))
    (set-face-attribute 'which-func nil :foreground "black"))

  (use-package electric-operator
    :ensure t
    :disabled t
    :init (add-hook 'prog-mode-hook #'electric-operator-mode))

  (use-package electric
    :init
    (add-hook 'prog-mode-hook
              (lambda ()
                (electric-layout-mode 1))))

  (use-package stickyfunc-enhance ; this hides the tabbar
    :ensure t
    :disabled t
    :init (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode))

  (use-package eldoc
    :config
    (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
    (add-hook 'ielm-mode-hook #'eldoc-mode)
    (add-hook 'python-mode-hook #'eldoc-mode)
    (use-package c-eldoc
      :ensure t
      :config (add-hook 'c-mode-hook #'eldoc-mode))
    :diminish eldoc-mode)

  (use-package make-mode
    :init
    (add-to-list 'auto-mode-alist '("\\Makefile\\'" . makefile-mode))
    ;; add makefile.rules to makefile-gmake-mode for Intel Pin
    (add-to-list 'auto-mode-alist '("makefile\\.rules\\'" . makefile-gmake-mode)))

  (use-package sh-script ; shell script mode
    :mode ("\\.zsh\\'" . sh-mode)
    :config
    (setq sh-basic-offset 4
          sh-indent-comment t
          sh-indentation 4))

  (use-package fish-mode
    :ensure t
    :mode ("\\.fish$" . fish-mode))

  (use-package speedbar
    :disabled t
    :config
    (setq speedbar-use-images nil
          speedbar-show-unknown-files t)

    (use-package sr-speedbar
      :ensure t
      :commands sr-speedbar-open
      :init
      (defalias 'speedbar 'sr-speedbar-open)
      (setq sr-speedbar-right-side nil
            sr-speedbar-max-width 40
            sr-speedbar-width 30
            sr-speedbar-default-width 30
            sr-speedbar-skip-other-window-p t)))

  (use-package ws-butler
    :ensure t
    :diminish ws-butler-mode
    :init (add-hook 'prog-mode-hook #'ws-butler-mode))

  (use-package dtrt-indent
    :ensure t
    :disabled t
    :diminish dtrt-indent-mode
    :config
    (setq dtrt-indent-verbosity 0)
    (add-hook 'prog-mode-hook
              (lambda ()
                (dtrt-indent-mode 1)))))

(use-package web-mode
  :functions (web-beautify-css web-beautify-html web-beautify-js)
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

  (when (eq dotemacs-completion 'auto-complete)
    (use-package ac-html
      :ensure t)

    (use-package ac-html-bootstrap
      :ensure t)

    (use-package ac-html-csswatcher
      :ensure t
      :config (ac-html-csswatcher-setup)))

  (when (eq dotemacs-completion 'company)
    (use-package ac-html-csswatcher
      :ensure t
      :config (company-web-csswatcher-setup))
    
    (use-package company-web
      :ensure t
      :preface
      (defun company-web--setup ()
        (setq-local company-backends
                    (append '(company-web-html company-web-jade company-web-slim)
                            company-backends)))
      :config (add-hook 'web-mode-hook #'company-web--setup)))

  (use-package web-beautify
    :ensure t
    :init
    (with-eval-after-load "js2-mode"
      (add-hook 'js2-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook #'web-beautify-js-buffer t t))))

    (with-eval-after-load "json-mode"
      (add-hook 'json-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook #'web-beautify-js-buffer t t))))

    (with-eval-after-load "sgml-mode"
      (add-hook 'html-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook #'web-beautify-html-buffer t t)))

      (with-eval-after-load "css-mode"
        (add-hook 'css-mode-hook
                  (lambda ()
                    (add-hook 'before-save-hook #'web-beautify-css-buffer t t)))))))

(use-package nxml-mode
  :defer t
  :config (setq nxml-slash-auto-complete-flag t
                nxml-auto-insert-xml-declaration-flag t)
  (when (eq dotemacs-completion 'auto-complete)
    (use-package auto-complete-nxml
      :ensure t)))

(provide 'prog-init)

;;; prog-init.el ends here
