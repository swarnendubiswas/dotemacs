;;; whitespace-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure whitespace.

;;; Code:

(when (bound-and-true-p dotemacs-enable-whitespace-module-p)
  (use-package whitespace
    :disabled t
    :diminish global-whitespace-mode
    :init
    ;;(global-whitespace-mode 1)
    (setq-default show-trailing-whitespace nil
                  whitespace-line-column 'dotemacs-fill-column
                  ;; '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab
                  ;; space-mark tab-mark newline-mark)
                  whitespace-style '(faces trailing empty lines-tail)))

  ;; Use the whitespace-cleanup-mode package instead, it is more comprehensive
  ;; (add-hook 'before-save-hook #'delete-trailing-whitespace)

  (use-package whitespace-cleanup-mode
    :ensure t
    :diminish whitespace-cleanup-mode
    :init (whitespace-cleanup-mode 1)))

(provide 'whitespace-init)

;;; whitespace-init.el ends here
