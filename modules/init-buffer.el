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

(use-package counsel-fd
  :straight t
  :if (and (eq sb/minibuffer-completion 'ivy) (executable-find "fd"))
  :bind
  (("C-x d" . counsel-fd-dired-jump) ; Jump to a directory below the current directory
   ;; Jump to a file below the current directory
   ("C-x f" . counsel-fd-file-jump)))

(use-package vlf ; Speed up Emacs for large files: "M-x vlf <PATH-TO-FILE>"
  :straight t
  :commands vlf
  :defines vlf-application
  :init
  (setq vlf-application 'dont-ask)
  (require 'vlf-setup))

(use-package immortal-scratch
  :straight t
  :commands immortal-scratch-mode
  ;; :init (run-with-idle-timer 2 nil #'immortal-scratch-mode)
  :hook (after-init-hook . immortal-scratch-mode))

;; I use the "*scratch*" buffer for taking notes, this package helps to make the data persist
(use-package persistent-scratch
  :straight t
  :commands persistent-scratch-setup-default
  :hook (after-init-hook . persistent-scratch-setup-default)
  :config
  (advice-add 'persistent-scratch-setup-default :around #'sb/inhibit-message-call-orig-fun))

(use-package consult-dir
  :straight t
  :bind
  (([remap list-directory] . consult-dir)
   ("C-x C-d" . consult-dir)
   :map minibuffer-local-completion-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

(provide 'init-buffer)
