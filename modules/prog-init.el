;;; prog-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Programming mode specific.

;;; Code:

(defvar dotemacs-completion-in-buffer)
(defvar dotemacs-temp-directory)
(defvar dotemacs-mode-line-theme)

(use-package semantic
  :defer 5
  :init
  (setq semanticdb-default-save-directory (concat dotemacs-temp-directory "semanticdb"))
  (add-hook 'prog-mode-hook #'semantic-mode)
  :config
  (semantic-mode 1)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-summary-mode 1)
  ;;https://emacs.stackexchange.com/questions/32268/can-semantic-and-company-coexist
  ;; (global-semantic-idle-completions-mode 1)
  (global-semantic-highlight-func-mode 1))

(use-package prog-mode
  :defer t
  :config
  (when (>= emacs-major-version 25)
    (setq prettify-symbols-unprettify-at-point 'right-edge)
    (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)))

(use-package make-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\Makefile\\'" . makefile-mode))
  ;; Add makefile.rules to makefile-gmake-mode for Intel Pin
  (add-to-list 'auto-mode-alist '("makefile\\.rules\\'" . makefile-gmake-mode)))

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
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indent-style 2
        ;; web-mode-style-padding 1
        ;; web-mode-script-padding 1
        ;; web-mode-block-padding 0
        ;; web-mode-comment-style 2
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t
        web-mode-enable-css-colorization t
        ;; web-mode-enable-comment-keywords t
        ;; web-mode-enable-current-element-highlight t
        ;; web-mode-enable-current-column-highlight t
        )

  (use-package ac-html-angular ; Required by ac-html and company-web
    :ensure t
    :config (ac-html-angular+))

  (use-package company-web
    :ensure t
    :if (bound-and-true-p dotemacs-completion-in-buffer)
    :preface
    (defun sb/company-web--setup ()
      (setq-local company-backends
                  (append '(company-web-html)
                          company-backends)))
    :config (add-hook 'web-mode-hook #'sb/company-web--setup)))

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
                  (add-hook 'before-save-hook #'web-beautify-css-buffer t t))))))

(use-package nxml-mode
  :defer t
  :init (defalias 'xml-mode 'nxml-mode)
  :config (setq nxml-slash-auto-complete-flag t
                nxml-auto-insert-xml-declaration-flag t)
  (when (bound-and-true-p dotemacs-completion-in-buffer)
    (add-hook 'nxml-mode-hook
              (lambda ()
                (add-to-list (make-local-variable 'company-backends)
                             'company-nxml)))))

(use-package which-func ; Show the name of the function in the modeline
  :after prog-mode
  :disabled t
  ;; :init (setq which-func-modes '(java-mode c++-mode python-mode emacs-lisp-mode lisp-mode))
  :hook (c-mode-common . which-function-mode)
  :config
  (which-function-mode 1)
  (when (eq dotemacs-mode-line-theme 'sml)
    (set-face-attribute 'which-func nil
                        :foreground "black"
                        :weight 'light))
  (when (or (eq dotemacs-mode-line-theme 'powerline) (eq dotemacs-mode-line-theme 'spaceline))
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
  :hook ((c-mode-common emacs-lisp-mode lisp-interaction-mode ielm-mode python-mode) . eldoc-mode)
  ;; :init
  ;; (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  ;; (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  ;; (add-hook 'ielm-mode-hook #'eldoc-mode)
  ;; (add-hook 'python-mode-hook #'eldoc-mode)
  :diminish eldoc-mode)

(use-package eldoc-overlay
  :ensure t
  :after eldoc
  :disabled t ; Too intrusive
  :diminish eldoc-overlay-mode
  :config (eldoc-overlay-mode 1))

(use-package octave
  :mode ("\\.m\\'" . octave-mode))

(use-package matlab-mode
  :ensure t
  :defer t
  :config
  ;; Can optionally setup CEDET support with (matlab-cedet-setup)
  (setq matlab-indent-function t))

(use-package ess
  :ensure t
  :defer t
  :config
  (setq inferior-R-args "--quiet --no-restore-history --no-save"
        ess-indent-offset 4
        ess-indent-from-lhs 4)
  (use-package ess-smart-underscore
    :ensure t))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" "\\.cmake\\'")
  :config
  (use-package cmake-font-lock
    :ensure t
    :hook (cmake-mode . cmake-font-lock-activate)))

(use-package cmake-ide
  :ensure t
  :defer t
  :config
  (setq cmake-ide-flags-c++ (append '("-std=c++11")))
  (cmake-ide-setup))

(use-package ini-mode
  :ensure t
  :mode ("\\.ini\\'" . ini-mode))

(add-hook 'after-save-hook
          (lambda ()
            (when (or (string-equal major-mode "lisp-mode") (string-equal major-mode "emacs-lisp-mode"))
              (check-parens))))

(provide 'prog-init)

;;; prog-init.el ends here
