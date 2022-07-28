;;; init-python.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(use-package python
  :straight (:type built-in)
  :mode
  (("SCon\(struct\|script\)$" . python-mode)
   ("[./]flake8\\'" . conf-mode)
   ("/Pipfile\\'" . conf-mode))
  :hook
  (python-mode-hook . lsp-deferred)
  :bind
  ;; Assigning a keybinding such as "C-[" is involved, `[' is treated as `meta'
  ;; https://emacs.stackexchange.com/questions/64839/assign-a-keybinding-with-c
  ;; TODO: Bind other keys suitably
  ;; python-nav-beginning-of-block
  ;; python-nav-end-of-block
  ;; python-nav-beginning-of-defun
  ;; python-nav-end-of-defun
  ;; python-nav-backward-defun
  ;; python-nav-forward-defun
  ;; python-nav-backward-statement
  ;; python-nav-forward-statement
  (:map python-mode-map
        ("C-c C-d")
        ("M-a"   . python-nav-backward-block)
        ("M-e"   . python-nav-forward-block)
        ("C-c <" . python-indent-shift-left)
        ("C-c >" . python-indent-shift-right))
  :custom
  (python-shell-completion-native-enable nil "Disable readline based native completion")
  (python-fill-docstring-style 'django)
  (python-indent-guess-indent-offset-verbose nil "Remove guess indent python message")
  (python-indent-guess-indent-offset nil)
  (python-indent-offset 4)
  (python-shell-exec-path "python3")
  (python-shell-interpreter "python3")
  :config
  (setenv "PYTHONPATH" "python3")

  ;; (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (python-pylint))))))

  ;; (setq auto-mode-alist (append
  ;;                        '(("SCon\(struct\|script\)$" . python-mode)
  ;;                          ("SConscript\\'" . python-mode)
  ;;                          ("[./]flake8\\'" . conf-mode)
  ;;                          ("/Pipfile\\'" . conf-mode))
  ;;                        auto-mode-alist))

  (with-eval-after-load "lsp-mode"
    (defvar lsp-pylsp-configuration-sources)
    (defvar lsp-pylsp-plugins-autopep8-enable)
    (defvar lsp-pylsp-plugins-mccabe-enabled)
    (defvar lsp-pylsp-plugins-pycodestyle-enabled)
    (defvar lsp-pylsp-plugins-pycodestyle-max-line-length)
    (defvar lsp-pylsp-plugins-pydocstyle-convention)
    (defvar lsp-pylsp-plugins-pydocstyle-enabled)
    (defvar lsp-pylsp-plugins-pydocstyle-ignore)
    (defvar lsp-pylsp-plugins-pyflakes-enabled)
    (defvar lsp-pylsp-plugins-pylint-args)
    (defvar lsp-pylsp-plugins-pylint-enabled)
    (defvar lsp-pylsp-plugins-yapf-enabled)
    (defvar lsp-pyright-langserver-command-args)
    (defvar lsp-pylsp-plugins-preload-modules)
    (defvar lsp-pylsp-plugins-flake8-enabled)
    (defvar lsp-pylsp-plugins-jedi-use-pyenv-environment)

    (when (eq sb/python-langserver 'pylsp)
      (setq lsp-pylsp-configuration-sources ["setup.cfg"]
            lsp-pylsp-plugins-mccabe-enabled nil
            ;; We can also set this per-project
            lsp-pylsp-plugins-preload-modules ["numpy", "csv", "pandas", "statistics", "json"]
            lsp-pylsp-plugins-pycodestyle-enabled nil
            lsp-pylsp-plugins-pycodestyle-max-line-length sb/fill-column
            lsp-pylsp-plugins-pydocstyle-convention "pep257"
            lsp-pylsp-plugins-pydocstyle-ignore (vconcat (list "D100" "D101" "D103" "D213"))
            lsp-pylsp-plugins-pyflakes-enabled nil
            lsp-pylsp-plugins-pylint-args (vconcat
                                           (list "-j 2"
                                                 (concat "--rcfile="
                                                         (expand-file-name ".config/pylintrc"
                                                                           sb/user-home-directory))))
            lsp-pylsp-plugins-pylint-enabled t ; Pylint can be expensive
            lsp-pylsp-plugins-yapf-enabled t
            lsp-pylsp-plugins-flake8-enabled nil
            lsp-pylsp-plugins-jedi-use-pyenv-environment t))))

(use-package python-docstring
  :after python-mode
  :demand t
  :commands
  (python-docstring-mode python-docstring-install)
  :config
  (python-docstring-install)
  :diminish)

(use-package pip-requirements
  :commands
  (pip-requirements-mode))

(use-package pyvenv
  :commands
  (pyvenv-mode pyvenv-tracking-mode)
  :hook
  (python-mode-hook . pyvenv-mode)
  :custom
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name (" [venv:"
                                                         pyvenv-virtual-env-name "] ")))
  (pyvenv-post-activate-hooks (list
                               (lambda ()
                                 (setq python-shell-interpreter
                                       (concat pyvenv-virtual-env "bin/python")))))
  (pyvenv-post-deactivate-hooks (list
                                 (lambda ()
                                   (setq python-shell-interpreter "python3")))))

;; FIXME: There is an error in passing options to isort
(use-package py-isort
  :if (and (executable-find "isort") (eq sb/python-langserver 'pyright))
  :disabled t
  :commands
  (py-isort-before-save py-isort-buffer py-isort-region)
  :hook
  (python-mode-hook . (lambda ()
                        (add-hook 'before-save-hook #'py-isort-before-save)))
  :custom
  (py-isort-options '("-lines=100"
                      "--up" ; Use parentheses
                      "--tc" ; Use a trailing comma on multiline imports
                      )))

(use-package python-isort
  :straight (python-isort :type git :host github :repo "wyuenho/emacs-python-isort")
  :if (and (executable-find "isort") (eq sb/python-langserver 'pyright))
  :hook
  (python-mode-hook . python-isort-on-save-mode)
  :custom
  (python-isort-arguments '("--stdout" "--atomic" "-lines=100" "--up" "--tc" "-")))

;; Yapfify works on the original file, so that any project settings supported by YAPF itself are
;; used. We do not use `lsp-format-buffer' since `pyright' does not support document formatting.
(use-package yapfify
  :if (executable-find "yapf")
  :hook
  (python-mode-hook . yapf-mode)
  :diminish yapf-mode)

(provide 'init-python)

;;; init-python.el ends here
