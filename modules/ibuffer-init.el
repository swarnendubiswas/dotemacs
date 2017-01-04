;;; ibuffer-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; IBuffer configurations.

;;; Code:

(use-package ibuffer
  :commands ibuffer
  :config
  (defalias 'list-buffers 'ibuffer) ; Turn on ibuffer by default
  (setq ibuffer-expert t
        ibuffer-always-show-last-buffer nil
        ibuffer-default-sorting-mode 'alphabetic ; Options: major-mode
        ibuffer-use-header-line t
        ibuffer-display-summary t
        ;; Ignore case when searching
        ibuffer-case-fold-search t)

  (use-package ibuf-ext
    :config (setq ibuffer-show-empty-filter-groups nil))

  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))

  (add-hook 'ibuffer-hook #'ibuffer-auto-mode)
  :bind ([remap list-buffers] . ibuffer))

(use-package ibuffer-projectile ; Group buffers by projectile project
  :ensure t
  :after ibuffer
  :config (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

(provide 'ibuffer-init)

;;; ibuffer-init.el ends here
