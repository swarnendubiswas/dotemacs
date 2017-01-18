;;; python-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure Python programming mode.  These settings are possibly a bit undercooked currently.

;;; Code:

;; Install the following packages
;; sudo -H pip3 install --upgrade pip numpy scipy psutil django setuptools jedi paramiko cffi rope importmagic yapf pyflakes flake8 importmagic autopep8 pep8 pylint overrides ggplot matplotlib ordered_set cpplint epc

(setenv "PYTHONPATH" "python3")

(defun dotemacs--python-setup ()
  "Helper function for configuring python mode."
  (setq-default fill-column 78
                python-indent-offset 4)
  (setq python-shell-interpreter "python3"
        python-shell-completion-native-enable nil
        python-shell-unbuffered nil)
  (turn-on-auto-fill)
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
                        '("os.path.abspath" "os.path.altsep" "os.path.basename")))))))

(use-package elpy
  :ensure t
  :diminish elpy-mode
  :preface
  (defun dotemacs--elpy-setup ()
    "Setup elpy and python configurations."
    (dotemacs--python-setup)
    (use-package pyvenv
      :ensure t
      :config (pyvenv-mode 1))
    (use-package company-jedi
      :ensure t
      :if (eq dotemacs-completion-in-buffer 'company)
      :config (add-to-list 'company-backends '(company-jedi elpy-company-backend)))
    (elpy-enable))

  (defun elpy-goto-definition-or-rgrep ()
    "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (ring-insert find-tag-marker-ring (point-marker))
    (condition-case nil (elpy-goto-definition)
      (error (elpy-rgrep-symbol
              (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))
  :init (add-hook 'python-mode-hook #'dotemacs--elpy-setup)
  :config
  (setq elpy-modules '(elpy-module-company
                       elpy-module-eldoc
                       elpy-module-pyvenv
                       elpy-module-highlight-indentation
                       elpy-module-yasnippet
                       elpy-module-sane-defaults)
        elpy-rpc-python-command "python3"
        elpy-rpc-backend "jedi")
  ;; Disable flymake mode, since it becomes slow if there are a lot of guideline errors.
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook #'flycheck-mode)

  (use-package py-autopep8
    :ensure t
    :config
    (setq py-autopep8-options '("--max-line-length=80"))
    (add-hook 'elpy-mode-hook #'py-autopep8-enable-on-save))

  (use-package python-docstring
    :ensure t
    :diminish python-docstring-mode
    :config
    (add-hook 'python-mode-hook #'python-docstring-mode))
  :bind (("C-c c l" . elpy-nav-indent-shift-left)
         ("C-c c r" . elpy-nav-indent-shift-right)
         ("C-c c e" . python-nav-forward-defun)
         ("C-c c a" . python-nav-backward-defun)
         ("C-c c i" . elpy-importmagic-fixup)
         :map elpy-mode-map
         ("M-<left>" . nil)
         ("M-<right>" . nil)
         ("M-." . nil)
         ("C-c C-d" . nil)
         ("C-c C-r i" . nil)))

(provide 'python-init)

;;; python-init.el ends here
