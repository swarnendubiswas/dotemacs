;;; python-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure Python programming mode.  These settings are possibly a bit undercooked currently.

;;; Code:

;; Install the following packages: sudo -H pip3 install --upgrade pip setuptools jedi rope importmagic yapf pylint pydocstyle isort

(defvar dotemacs-completion-in-buffer)

(defun sb/python-setup ()
  "Helper function for configuring python mode."
  (setq-default python-indent-offset 4
                python-indent-guess-indent-offset nil)
  (setq python-shell-completion-native-enable nil)
  (setq python-shell-interpreter "python3"
        python-shell-unbuffered nil))

(use-package elpy
  :ensure t
  :diminish elpy-mode
  :preface
  (defun sb/elpy-setup ()
    "Setup elpy and python configurations."
    (sb/python-setup)
    (setq elpy-modules '(elpy-module-company
                         elpy-module-eldoc
                         elpy-module-pyvenv
                         elpy-module-highlight-indentation
                         elpy-module-yasnippet
                         elpy-module-sane-defaults)
          elpy-rpc-python-command "python3"
          elpy-rpc-backend "jedi"
          elpy-syntax-check-command "pylint")
    (use-package pyvenv
      :ensure t
      :config (pyvenv-mode 1))
    (use-package company-jedi
      :ensure t
      :ensure company
      :after company
      :if (bound-and-true-p dotemacs-completion-in-buffer)
      :config (add-to-list 'company-backends '(company-jedi elpy-company-backend)))
    (elpy-mode 1))

  :init (add-hook 'python-mode-hook #'sb/elpy-setup)
  :config
  (add-hook 'elpy-mode-hook #'flycheck-mode)
  ;; ;; http://www.wilfred.me.uk/.emacs.d/init.html
  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;;             (add-to-list 'flycheck-disabled-checkers 'python-pylint)))

  (use-package python-docstring
    :ensure t
    :diminish python-docstring-mode
    :hook (python-mode . python-docstring-mode))

  (use-package pyimport
    :ensure t)

  (use-package py-isort
    :ensure t)
  :bind (:map elpy-mode-map
              ("C-c c e" . python-nav-forward-defun)
              ("C-c c a" . python-nav-backward-defun)
              ("M-<left>" . nil)
              ("M-<right>" . nil)
              ("M-." . nil)
              ("C-c C-d" . nil)
              ("C-c C-r i" . nil)))

(defun sb/company-python-backends ()
  "Add backends for Python completion in company mode."
  (make-local-variable 'company-backends)
  (setq company-backends
        '((;; Generic backends
           company-files
           company-keywords
           company-capf
           company-dabbrev
           company-dabbrev-code
           company-gtags
           ;; Python specific backends
           company-jedi
           elpy-company-backend))))
(add-hook 'python-mode-hook #'sb/company-python-backends)

(defhydra sb/hydra-python-indent (global-map "C-c c n")
  "indent"
  ("l" elpy-nav-indent-shift-left "left")
  ("r" elpy-nav-indent-shift-right "right"))

;; FIXME: One of the following three works.
;; (add-hook 'before-save-hook
;;           (lambda ()
;;             (when (string-equal major-mode "python-mode")
;;               (progn
;;                 (pyimport-remove-unused)
;;                 (elpy-yapf-fix-code)))))

(add-hook 'before-save-hook
          (lambda ()
            (when (string-equal major-mode "python-mode")
              (py-isort-before-save)
              ;; (pyimport-remove-unused) ; This can be irritating if you are yet to use the imports.
              (elpy-yapf-fix-code))))

;; (add-hook 'before-save-hook
;;           (lambda ()
;;             (when (string-equal major-mode "elpy-mode")
;;               (pyimport-remove-unused)
;;               (elpy-yapf-fix-code))))

(provide 'python-init)

;;; python-init.el ends here
