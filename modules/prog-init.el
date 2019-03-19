;;; prog-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Programming mode specific.

;;; Code:

(defvar dotemacs-temp-directory)
(defvar dotemacs-modeline-theme)

(use-package semantic
  :init
  (setq semanticdb-default-save-directory (concat dotemacs-temp-directory "semanticdb"))
  (add-hook 'prog-mode-hook #'semantic-mode)
  :config
  ;; (semantic-mode 1)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-summary-mode 1)
  ;;https://emacs.stackexchange.com/questions/32268/can-semantic-and-company-coexist
  ;; (global-semantic-idle-completions-mode 1)
  (global-semantic-highlight-func-mode 1))

(use-package prog-mode
  :config
  (when (>= emacs-major-version 25)
    (setq prettify-symbols-unprettify-at-point 'right-edge)
    (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)))

(use-package make-mode
  :mode (("\\Makefile\\'" . makefile-mode)
         ("makefile\\.rules\\'" . makefile-gmake-mode)))

(use-package web-mode ; http://web-mode.org/
  :ensure t
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.phtml\\'" . web-mode)
   ("\\.hb\\.html\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-indent-style 4
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t
        web-mode-enable-css-colorization t
        web-mode-enable-block-face t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t))

(use-package ac-html-angular ; Required by ac-html and company-web
  :ensure t
  :config (ac-html-angular+))

(use-package company-web
  :ensure t
  :preface
  (defun sb/company-web--setup ()
    (setq-local company-backends
                (append '(company-web-html)
                        company-backends)))
  :config (add-hook 'web-mode-hook #'sb/company-web--setup))

(use-package web-beautify
  :ensure t
  :init
  (with-eval-after-load "js2-mode"
    (add-hook 'js2-mode-hook
              (lambda ()
                (add-hook 'before-save-hook #'web-beautify-js-buffer t t))))
  (with-eval-after-load "js"
    (add-hook 'js-mode-hook
              (lambda ()
                (add-hook 'before-save-hook #'web-beautify-js-buffer t t))))
  (with-eval-after-load "json-mode"
    (add-hook 'json-mode-hook
              (lambda ()
                (add-hook 'before-save-hook #'web-beautify-js-buffer t t))))
  (with-eval-after-load "web-mode"
    (add-hook 'web-mode-hook
              (lambda ()
                (add-hook 'before-save-hook #'web-beautify-html-buffer t t))))
  (with-eval-after-load "sgml-mode"
    (add-hook 'html-mode-hook
              (lambda ()
                (add-hook 'before-save-hook #'web-beautify-html-buffer t t)))
    (with-eval-after-load "css-mode"
      (add-hook 'css-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook #'web-beautify-css-buffer t t))))))

(use-package nxml-mode
  :init (defalias 'xml-mode 'nxml-mode)
  :config (setq nxml-slash-auto-complete-flag t
                nxml-auto-insert-xml-declaration-flag t)
  (add-hook 'nxml-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           'company-nxml))))

(use-package which-func ; Show the name of the function in the modeline
  :after prog-mode
  :disabled t
  :hook (c-mode-common . which-function-mode)
  ;; :init (setq which-func-modes '(java-mode c++-mode python-mode emacs-lisp-mode lisp-mode))
  :config
  (which-function-mode 1)
  (when (eq dotemacs-modeline-theme 'sml)
    (set-face-attribute 'which-func nil
                        :foreground "black"
                        :weight 'light))
  (when (or (eq dotemacs-modeline-theme 'powerline) (eq dotemacs-modeline-theme 'spaceline))
    (set-face-attribute 'which-func nil
                        ;; :foreground "white"
                        :weight 'light)))

(use-package electric
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (electric-layout-mode 1))))

(use-package eldoc
  :after prog-mode
  :if (eq system-type 'gnu/linux)
  :disabled t
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode python-mode) . eldoc-mode)
  :diminish eldoc-mode)

(use-package eldoc-overlay
  :ensure t
  :after eldoc
  :disabled t ; Too intrusive
  :diminish eldoc-overlay-mode
  :config (eldoc-overlay-mode 1))

(use-package octave
  :mode ("\\.m\\'" . octave-mode))

(use-package ess
  :ensure t
  :config
  (setq inferior-R-args "--quiet --no-restore-history --no-save"
        ess-indent-offset 4
        ess-indent-from-lhs 4)
  (use-package ess-smart-underscore
    :ensure t))

(use-package ini-mode
  :ensure t
  :mode ("\\.ini\\'" . ini-mode))

(add-hook 'after-save-hook
          (lambda ()
            (when (or (string-equal major-mode "lisp-mode") (string-equal major-mode "emacs-lisp-mode"))
              (check-parens))))

(provide 'prog-init)

;;; prog-init.el ends here
