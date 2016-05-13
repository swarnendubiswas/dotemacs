;;; python-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure Python programming mode.  These settings are possibly a bit undercooked currently.

;;; Code:

;; Use either python-mode or elpy or anaconda-mode.

;; Set PYTHONPATH
(setenv "PYTHONPATH" "python3")

(use-package python ; Emacs built-in python mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config (setq python-shell-completion-native-enable nil))

(defun dotemacs--python-setup ()
  "Helper function for configuring python mode."
  (setq-default fill-column 78
                python-indent-offset 4)
  (setq python-shell-interpreter "python3")

  (turn-on-auto-fill)
  (flymake-mode-off)
  (run-python (python-shell-parse-command) nil nil)

  (when (eq dotemacs-completion-in-buffer 'auto-complete)
    (use-package auto-complete-chunk
      :ensure t
      :init
      (add-hook 'python-mode
                (lambda ()
                  ;; Make sure ac-source-chunk-list comes first.
                  (setq ac-sources (append '(ac-source-chunk-list) ac-sources))
                  (setq ac-chunk-list
                        '("os.path.abspath" "os.path.altsep" "os.path.basename"))))))

  (with-eval-after-load "flycheck"
    (use-package flycheck-pyflakes
      :ensure t
      :disabled t
      :config
      (add-to-list 'flycheck-disabled-checkers 'python-flake8)
      (add-to-list 'flycheck-disabled-checkers 'python-pylint))))

;; sudo <path-to-pip3> install --upgrade pip numpy scipy psutil django setuptools jedi paramiko cffi rope importmagic yapf pyflakes flake8 importmagic autopep8 pep8
;; FIXME: It would be good to disable flymake mode, since it becomes slow if there are a lot of guideline errors.
(use-package elpy
  :ensure t
  :diminish elpy-mode
  :preface
  (defun dotemacs--elpy-setup ()
    "Setup elpy and python configurations."
    (dotemacs--python-setup)
    (use-package pyvenv
      :ensure t
      :init (pyvenv-mode 1))
    (elpy-enable))

  (defun elpy-goto-definition-or-rgrep ()
    "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    (condition-case nil (elpy-goto-definition)
      (error (elpy-rgrep-symbol
              (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))
  :config
  (setq elpy-modules '(elpy-module-company
                       elpy-module-eldoc
                       elpy-module-pyvenv
                       elpy-module-highlight-indentation
                       elpy-module-yasnippet
                       elpy-module-sane-defaults))
  (add-hook 'python-mode-hook #'dotemacs--elpy-setup)
  (unbind-key "M-<left>" elpy-mode-map)
  (unbind-key "M-<right>" elpy-mode-map))

(use-package pydoc
  :ensure t
  :disabled t)

(use-package python-docstring
  :ensure t
  :disabled t
  :commands python-docstring-mode
  :config (python-docstring-mode 1))

(use-package py-autopep8
  :ensure t
  :disabled t
  :config
  (setq py-autopep8-options '("--max-line-length=120"))
  (add-hook 'python-mode-hook #'py-autopep8-enable-on-save))

(use-package py-import-check
  :ensure t
  :disabled t)

(use-package py-isort
  :ensure t
  :disabled t
  :config (add-hook 'before-save-hook 'py-isort-before-save nil t))

(provide 'python-init)

;;; python-init.el ends here
