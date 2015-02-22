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

(use-package achievements
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

(use-package tabbar
             :ensure t
             :init (tabbar-mode 1)
             :config (setq tabbar-use-images nil) ; speed up by not using images
             )

(use-package jgraph-mode
             :ensure
             :defer t
             )

(use-package hungry-delete
             :ensure t
             :defer t
             :init (global-hungry-delete-mode 1)
             )

(use-package fixme-mode
             :ensure t
             :defer t
             :config (fixme-mode 1)
             )

(use-package fish-mode
             :ensure t
             :disabled t
             )

(provide 'misc-init)

;;; misc-init.el ends here
