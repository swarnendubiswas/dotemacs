;;; whitespace-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure whitespace.

;;; Code:

(when (bound-and-true-p dotemacs-enable-whitespace-module)
  (add-hook 'before-save-hook #'delete-trailing-whitespace)

  (use-package whitespace
    :defer t
    :diminish global-whitespace-mode
    :init
    ;;(global-whitespace-mode 1)
    (setq-default show-trailing-whitespace t
                  whitespace-line-column 'dotemacs-fill-column
                  ;; whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty
                  ;;                         space-after-tab space-mark tab-mark newline-mark)
                  whitespace-style '(faces trailing empty lines-tail))
    ;;(set-face-attribute 'whitespace-line nil :background "red1" :foreground "yellow" :weight 'bold)
    )

  (use-package whitespace-cleanup-mode
    :ensure t
    :diminish whitespace-cleanup-mode
    :init
    ;; (dolist (hook '(prog-mode-hook))
    ;;   (add-hook hook #'whitespace-cleanup-mode))
    (whitespace-cleanup-mode 1)))

(provide 'whitespace-init)

;;; whitespace-init.el ends here
