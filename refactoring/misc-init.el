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

(use-package achievements-mode
             :ensure t
             :defer t
             :init (achievements-mode 1)
             :config (setq achievements-idle-time 600) ; seconds
             )

;; speed up emacs for large files
(use-package vlf
             :ensure t
             :defer t
             :config (setq large-file-warning-threshold 50000000) ; warn when opening files bigger than 50MB
             )
(require 'vlf-setup)


(provide 'misc-init)

;;; misc-init.el ends here
