;;; python-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure Python programming mode.  These settings are possibly a bit undercooked currently.

;;; Code:

;; Use either python-mode or elpy or anaconda-mode.

(use-package python ; Emacs built-in python mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(defun dotemacs--python-setup ()
  "Helper function for configuring python mode."
  (setq-default fill-column 78
                python-indent-offset 4)
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
      :config
      (add-to-list 'flycheck-disabled-checkers 'python-flake8)
      (add-to-list 'flycheck-disabled-checkers 'python-pylint))))

(use-package python-mode
  :ensure t
  :disabled t
  :mode ("\\.py\\'" . python-mode) ; implies ":defer t"
  :interpreter ("python" . python-mode)
  :config (add-hook 'python-mode-hook #'dotemacs--python-setup))

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
  :config
  (setq py-autopep8-options '("--max-line-length=120"))
  (add-hook 'python-mode-hook #'py-autopep8-enable-on-save))

(use-package py-import-check
  :ensure t
  :disabled t)

(use-package py-isort
  :ensure t
  :config (add-hook 'before-save-hook 'py-isort-before-save nil t))

;; Useful packages with pip: autopep8, pyflakes, setuptools, psutil. pip is bundled with python >= 3.4.
;; sudo /usr/local/bin/python3.4 -m pip install [--upgrade] pyflakes flake8 importmagic jedi autopep8
;; sudo pip install [--upgrade] pyflakes flake8 importmagic jedi autopep8
;; FIXME: It would be good to disable flymake mode, since it becomes slow if there are a lot of guideline errors.
(use-package elpy
  :ensure t
  :disabled t
  :diminish elpy-mode
  :preface
  (defun dotemacs--elpy-setup ()
    "Setup elpy and python configurations."
    (dotemacs--python-setup)
    (use-package pyvenv
      :ensure t
      :init (pyvenv-mode 1))
    (elpy-enable))
  :config (add-hook 'python-mode-hook #'dotemacs--elpy-setup))

(provide 'python-init)

;;; python-init.el ends here
