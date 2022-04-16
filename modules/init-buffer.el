(use-package ibuffer
  :straight nil
  :config
  (defalias 'list-buffers 'ibuffer)
  (setq ibuffer-display-summary nil
        ibuffer-default-sorting-mode 'alphabetic ; Options: `major-mode', `recency'
        ibuffer-use-header-line t)
  :bind ("C-x C-b" . ibuffer))

(use-package ibuf-ext
  :straight nil
  :commands ibuffer-auto-mode
  :config
  ;; Do not show filter groups if there are no buffers in that group
  (setq ibuffer-show-empty-filter-groups nil)
  :hook (ibuffer-hook . ibuffer-auto-mode))

(use-package ibuffer-projectile ; Group buffers by projectile project
  :straight t
  :commands ibuffer-projectile-set-filter-groups
  :hook (ibuffer-hook . ibuffer-projectile-set-filter-groups))

(use-package all-the-icons-ibuffer
  :straight t
  :if (display-graphic-p)
  :commands all-the-icons-ibuffer-mode
  :hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode)
  :config (setq all-the-icons-ibuffer-icon-size 0.8))

(provide 'init-buffer)
