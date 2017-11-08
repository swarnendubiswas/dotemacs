;;; python-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure Python programming mode.  These settings are possibly a bit undercooked currently.

;;; Code:

;; Install the following packages: sudo -H pip3 install --upgrade pip setuptools jedi rope importmagic yapf pyflakes
;; flake8 autopep8 pep8 pylint flake8-docstring pydocstyle wheel isort

(defvar dotemacs-completion-in-buffer)

(setenv "PYTHONPATH" "python3")

(defun dotemacs--python-setup ()
  "Helper function for configuring python mode."
  (setq-default python-indent-offset 4
                python-indent-guess-indent-offset nil)
  ;; (setq python-shell-completion-native-enable nil)
  ;; (setq python-shell-interpreter "python3"
  ;;       python-shell-unbuffered nil)
  ;; (turn-on-auto-fill)
  ;; (run-python (python-shell-parse-command) nil nil)
  )

(use-package elpy
  :ensure t
  :diminish elpy-mode
  :preface
  (defun dotemacs--elpy-setup ()
    "Setup elpy and python configurations."
    (dotemacs--python-setup)
    (setq elpy-modules '(elpy-module-company
                         elpy-module-eldoc
                         elpy-module-pyvenv
                         elpy-module-highlight-indentation
                         elpy-module-yasnippet
                         elpy-module-sane-defaults)
          elpy-rpc-python-command "python3"
          elpy-rpc-backend "jedi"
          elpy-syntax-check-command "flake8")
    (use-package pyvenv
      :ensure t
      :config (pyvenv-mode 1))
    (use-package company-jedi
      :ensure t
      :ensure company
      :after company
      :if (eq dotemacs-completion-in-buffer 'company)
      :config (add-to-list 'company-backends '(company-jedi elpy-company-backend)))
    (add-hook 'before-save-hook #'elpy-format-code nil t)
    (elpy-mode 1))

  :init (add-hook 'python-mode-hook #'dotemacs--elpy-setup)
  :config
  (add-hook 'elpy-mode-hook #'flycheck-mode)
  ;; http://www.wilfred.me.uk/.emacs.d/init.html
  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list 'flycheck-disabled-checkers 'python-pylint)))

  (use-package py-autopep8
    :ensure t
    :disabled t
    :config
    (setq py-autopep8-options '("--max-line-length=120"))
    (add-hook 'python-mode-hook #'py-autopep8-enable-on-save)
    (add-hook 'elpy-mode-hook #'py-autopep8-enable-on-save))

  (use-package python-docstring
    :ensure t
    :diminish python-docstring-mode
    :config
    (add-hook 'python-mode-hook #'python-docstring-mode))
  :bind (:map elpy-mode-map
              ("C-c c e" . python-nav-forward-defun)
              ("C-c c a" . python-nav-backward-defun)
              ("C-c c i" . elpy-importmagic-fixup)
              ("M-<left>" . nil)
              ("M-<right>" . nil)
              ("M-." . nil)
              ("C-c C-d" . nil)
              ("C-c C-r i" . nil)))

(defun dotemacs--company-python-backends ()
  "Add backends for Python completion in company mode."
  (make-local-variable 'company-backends)
  (setq company-backends
        '((;; Generic backends
           company-files
           company-keywords
           company-dabbrev-code
           company-gtags
           company-capf
           ;; Python specific backends
           company-jedi
           elpy-company-backend))))
(add-hook 'python-mode-hook 'dotemacs--company-python-backends)

(defhydra hydra-python-indent (global-map "C-c c n")
  "indent"
  ("l" elpy-nav-indent-shift-left "left")
  ("r" elpy-nav-indent-shift-right "right"))

;; (add-hook 'before-save-hook
;;           (lambda ()
;;             (when (string-equal major-mode "python-mode")
;;               (elpy-format-code))))

(provide 'python-init)

;;; python-init.el ends here
