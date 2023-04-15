;;; init-python.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

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
  (python-mode-hook
    .
    (lambda ()
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  :bind
  ;; Assigning a keybinding such as "C-[" is involved, "[" is treated as `meta'
  ;; https://emacs.stackexchange.com/questions/64839/assign-a-keybinding-with-c

  ;; TODO: Bind other functions suitably: python-nav-beginning-of-block, python-nav-end-of-block,
  ;; python-nav-backward-defun, python-nav-forward-defun, python-nav-backward-statement,
  ;; python-nav-forward-statement
  (:map
    python-mode-map
    ("C-c C-d")
    ("C-M-a" . python-nav-beginning-of-defun)
    ("C-M-e" . python-nav-end-of-defun)
    ("M-a" . python-nav-backward-block)
    ("M-e" . python-nav-forward-block)
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
  :config (setenv "PYTHONPATH" "python3")

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

    (setq
      lsp-pylsp-configuration-sources ["setup.cfg"]
      lsp-pylsp-plugins-mccabe-enabled nil
      ;; We can also set this per-project
      lsp-pylsp-plugins-preload-modules ["numpy" , "csv" , "pandas" , "statistics" , "json"]
      lsp-pylsp-plugins-pycodestyle-enabled nil
      lsp-pylsp-plugins-pycodestyle-max-line-length sb/fill-column
      lsp-pylsp-plugins-pydocstyle-convention "pep257"
      lsp-pylsp-plugins-pydocstyle-ignore (vconcat (list "D100" "D101" "D103" "D213"))
      lsp-pylsp-plugins-pyflakes-enabled nil
      lsp-pylsp-plugins-pylint-args
      (vconcat
        (list
          "-j 2"
          (concat "--rcfile=" (expand-file-name ".config/pylintrc" sb/user-home-directory))))
      lsp-pylsp-plugins-pylint-enabled t
      lsp-pylsp-plugins-yapf-enabled t
      lsp-pylsp-plugins-flake8-enabled nil
      lsp-pylsp-plugins-black-enabled nil
      lsp-pylsp-plugins-jedi-use-pyenv-environment t)))

(use-package python-docstring
  :after python-mode
  :demand t
  :commands (python-docstring-mode python-docstring-install)
  :config (python-docstring-install)
  :diminish)

(use-package pip-requirements
  :commands (pip-requirements-mode))

(use-package pyvenv
  :commands (pyvenv-mode pyvenv-tracking-mode)
  :hook (python-mode-hook . pyvenv-mode)
  :custom
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name (" [venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-post-activate-hooks
    (list (lambda () (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (pyvenv-post-deactivate-hooks (list (lambda () (setq python-shell-interpreter "python3")))))

(use-package python-isort
  :straight (:host github :repo "wyuenho/emacs-python-isort")
  :if (and (executable-find "isort") (eq sb/python-langserver 'pyright))
  :hook (python-mode-hook . python-isort-on-save-mode)
  :custom
  (python-isort-arguments
    '
    ("--stdout" "--atomic" "-l 100"
      "--up" ; Use parentheses
      "--tc" ; Use a trailing comma on multiline imports
      "-")))

;; Yapfify works on the original file, so that any project settings supported by YAPF itself are
;; used. We do not use `lsp-format-buffer' or `eglot-format-buffer' since `pyright' does not support
;; document formatting.
(use-package yapfify
  :if (executable-find "yapf")
  :hook (python-mode-hook . yapf-mode)
  :diminish yapf-mode)

;; Install with "python3 -m pip install -U pyright --user". Create stubs for a package with "pyright
;; --createstub pandas".
(use-package lsp-pyright
  :if
  (and (eq sb/lsp-provider 'lsp-mode)
    (eq sb/python-langserver 'pyright)
    (executable-find "pyright"))
  :commands (lsp-pyright-locate-python lsp-pyright-locate-venv)
  :hook (python-mode-hook . (lambda () (require 'lsp-pyright)))
  :custom
  (lsp-pyright-python-executable-cmd "python3")
  (lsp-pyright-typechecking-mode "basic")
  (lsp-pyright-auto-import-completions t)
  (lsp-pyright-auto-search-paths t)
  :config
  (lsp-register-client
    (make-lsp-client
      :new-connection
      (lsp-tramp-connection
        (lambda () (cons "pyright-langserver" lsp-pyright-langserver-command-args)))
      :major-modes '(python-mode)
      :remote? t
      :server-id 'pyright-r
      :multi-root lsp-pyright-multi-root
      :priority 3
      :initialization-options
      (lambda ()
        (ht-merge (lsp-configuration-section "pyright") (lsp-configuration-section "python")))
      :initialized-fn
      (lambda (workspace)
        (with-lsp-workspace
          workspace
          (lsp--set-configuration
            (ht-merge (lsp-configuration-section "pyright") (lsp-configuration-section "python")))))
      :download-server-fn
      (lambda (_client callback error-callback _update?)
        (lsp-package-ensure 'pyright callback error-callback))
      :notification-handlers
      (lsp-ht
        ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
        ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
        ("pyright/endProgress" 'lsp-pyright--end-progress-callback)))))

(provide 'init-python)

;;; init-python.el ends here
