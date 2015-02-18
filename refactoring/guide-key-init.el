;; guide-key
(use-package guide-key
             :ensure t
             :defer t
             :config (setq guide-key/guide-key-sequence t
                           guide-key/recursive-key-sequence-flag t
                           guide-key/popup-window-position 'bottom
                           )
             :idle (guide-key-mode 1)
             )
(use-package guide-key-tip
             :ensure t
             :defer t
             :config (setq guide-key-tip/enabled t)
             )
