;;; fci-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup fci.

;;; Code:

(use-package fill-column-indicator
  :if (bound-and-true-p dotemacs-fci-p)
  :preface
  (defun dotemacs-auto-fci-mode (&optional unused)
    (if (> frame-width dotemacs-fill-column)
        (fci-mode 1)
      (fci-mode 0)))

  (define-globalized-minor-mode dotemacs-global-fci-mode fci-mode
    (lambda ()
      (fci-mode 1)))

  :config
  ;; Turn off fci-mode when popups are activated
  ;; https://github.com/alpaker/Fill-Column-Indicator/issues/21#issuecomment-6959718
  (with-eval-after-load 'popup
    (defvar sanityinc/fci-mode-suppressed nil)

    (defun sanityinc/suppress-fci-mode (&rest args)
      "Suspend fci-mode while popups are visible"
      (setq-local sanityinc/fci-mode-suppressed fci-mode)
      (when fci-mode
        (turn-off-fci-mode)))
    (advice-add 'popup-create :before #'sanityinc/suppress-fci-mode)

    (defun sanityinc/restore-fci-mode (&rest args)
      "Restore fci-mode when all popups have closed"
      (when (and sanityinc/fci-mode-suppressed
                 (null popup-instances))
        (setq-local sanityinc/fci-mode-suppressed nil)
        (turn-on-fci-mode)))

    (advice-add 'popup-delete :after #'sanityinc/restore-fci-mode))

  (dotemacs-global-fci-mode 1)

  ;; (add-hook 'after-change-major-mode-hook #'dotemacs-auto-fci-mode)
  ;; (add-hook 'window-size-change-functions #'dotemacs-auto-fci-mode)

  (setq-default fci-rule-column dotemacs-fill-column)
  (setq fci-handle-truncate-lines nil
        fci-rule-width 1
        fci-rule-color "grey40"))

(provide 'fci-init)

;;; fci-init.el ends here
