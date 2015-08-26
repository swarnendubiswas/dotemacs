;;; python-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure Python programming mode.

;;; Code:

(use-package python
  :defer t
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq fill-column 78)
              (turn-on-auto-fill)))

  (use-package jedi
    :ensure t
    :config
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)

    (use-package company-jedi
      :ensure t
      :if (eq dotemacs-completion 'company)))

  (use-package pyenv-mode
    :ensure t
    :config (add-hook 'python-mode-hook #'pyenv-mode))

  (use-package pyvenv
    :ensure t
    :config (add-hook 'python-mode-hook #'pyvenv-mode))

  (use-package python-environment
    :ensure t)

  (use-package pyvirtualenv
    :ensure t
    :config (add-hook 'python-mode-hook #'pyvirtualenv-mode))

  (use-package anaconda-mode
    :ensure t
    :disabled t
    :diminish anaconda-mode
    :init
    (add-hook 'python-mode-hook #'anaconda-mode)

    (use-package company-anaconda
      :ensure t
      :if (eq dotemacs-completion 'company)
      :init
      (with-eval-after-load "company"
        (add-to-list 'company-backends 'company-anaconda))))

  (use-package pydoc
    :ensure t
    :config
    (use-package helm-pydoc
      :ensure t))

  (use-package python-docstring
    :ensure t
    :commands python-docstring-mode
    :config (python-docstring-mode 1))

  (use-package pip-requirements
    :ensure t
    :defer t)

  (use-package py-autopep8
    :ensure t
    :config
    (setq py-autopep8-options '("--max-line-length=100"))
    (add-hook 'python-mode-hook #'py-autopep8-enable-on-save))

  (use-package py-import-check
    :ensure t)

  (use-package py-isort
    :ensure t)

  (use-package pycomplete
    :ensure t
    :disabled t)

  ;; Useful packages with pip: autopep8, pyflakes, setuptools, psutil. pip is bundled with python >= 3.4.
  ;; sudo /usr/local/bin/python3.4 -m pip install [--upgrade] pyflakes
  (use-package elpy
    :ensure t
    :diminish elpy-mode
    :config
    (elpy-enable))

  (use-package flymake-python-pyflakes
    :ensure t
    :config
    (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
    ;; (setq flymake-python-pyflakes-executable "flake8")
    ;; (setq flymake-python-pyflakes-extra-arguments '("--ignore=W806")))
    )

  ;; rope does not work with python3
  (use-package pyde
    :ensure t
    :disabled t
    :config (pyde-enable)))

(provide 'python-init)

;;; python-init.el ends here
