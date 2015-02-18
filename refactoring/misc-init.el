;;; misc-init.el --- Part of emacs initialization

;;; Commentary:
;; Miscellaneous configurations

;;; Code:
(use-package smooth-scroll
             :ensure t
             :defer t
             :init (smooth-scroll-mode 1)
             )

(use-package auto-compile
             :ensure t
             :init ((auto-compile-on-load-mode 1)
                    (auto-compile-on-save-mode 1)
                    )
             :config
             progn (
                    (setq auto-compile-display-buffer nil
                          auto-compile-mode-line-counter t
                          )
                    (setq load-prefer-newer t)
                    )
             )

(provide 'misc-init)

;;; misc-init.el ends here
