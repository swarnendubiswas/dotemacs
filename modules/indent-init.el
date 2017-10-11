;;; indent-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup indentation in Emacs.

;;; Code:

(defvar dotemacs-fill-column)

(setq-default fill-column dotemacs-fill-column
              standard-indent 2
              tab-width 2
              tab-always-indent 'complete
              ;; Spaces instead of tabs by default
              indent-tabs-mode nil)

(use-package aggressive-indent
  :ensure t
  :init (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  :diminish aggressive-indent-mode)

(use-package electric ; Intelligent indentation, on by default from Emacs 24.4
  :config (electric-indent-mode 1))

(use-package highlight-indentation ; TODO: Face color does not match well with leuven theme
  :ensure t
  :diminish (highlight-indentation-mode highlight-indentation-current-column-mode)
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (highlight-indentation-mode 1)
              (highlight-indentation-current-column-mode 1)))
  (set-face-background 'highlight-indentation-face "WhiteSmoke")
  (set-face-background 'highlight-indentation-current-column-face "wheat"))

(provide 'indent-init)

;;; indent-init.el ends here
