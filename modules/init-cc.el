;;; init-cc.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(use-package cc-mode
  :straight (:type built-in)
  :defines (c-electric-brace c-enable-auto-newline c-set-style)
  :commands (c-fill-paragraph c-end-of-defun c-beginning-of-defun c++-mode)
  :mode
  (("\\.h\\'" . c++-mode)
   ("\\.c\\'" . c++-mode))
  :hook
  (c++-mode-hook . (lambda ()
                     (setq-local c-set-style "cc-mode"
                                 c-basic-offset 2)
                     (lsp-deferred)))
  :config
  (defvar c-electric-indent)

  ;; Disable electric indentation and on-type formatting
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq-local c-auto-newline nil
                          c-electric-brace nil
                          c-electric-flag nil
                          c-electric-indent nil
                          c-enable-auto-newline nil
                          c-syntactic-indentation nil)))

  (unbind-key "C-M-a" c-mode-map)

  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection "clangd")
  ;;   :major-modes '(c-mode c++-mode)
  ;;   :remote? t
  ;;   :server-id 'clangd-r))
  :bind
  (:map c-mode-base-map
        ("C-c c a" . c-beginning-of-defun)
        ("C-c c e" . c-end-of-defun)
        ("M-q"     . c-fill-paragraph)
        ("C-c C-d" . nil)))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook
  (c++-mode-hook . modern-c++-font-lock-mode))

(use-package cuda-mode
  :commands cuda-mode
  :mode
  (("\\.cu\\'"  . c++-mode)
   ("\\.cuh\\'" . c++-mode)))

(use-package opencl-mode
  :commands opencl-mode
  :mode "\\.cl\\'")

(use-package cmake-mode
  :if (executable-find "cmake")
  :commands cmake-mode
  :mode "\(CMakeLists\.txt|\.cmake\)$"
  :hook
  (cmake-mode-hook . (lambda ()
                       (make-local-variable 'lsp-disabled-clients)
                       (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                       (spell-fu-mode -1)
                       (flyspell-mode -1)
                       (lsp-deferred)))
  ;; :config
  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection "cmake-language-server")
  ;;   :major-modes '(cmake-mode)
  ;;   :remote? t
  ;;   :server-id 'cmakels-r))
  )

(use-package cmake-font-lock
  :hook
  (cmake-mode-hook . cmake-font-lock-activate))

(provide 'init-cc)

;;; init-cc.el ends here
