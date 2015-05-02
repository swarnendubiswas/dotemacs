;;; helm-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure helm.

;;; Code:

(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode 1)
  :config
  (setq helm-quick-update t ; do not display invisible candidates
        helm-M-x-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-lisp-fuzzy-completion t
        ;; helm-split-window-default-side 'other ;; open helm buffer in another window
        ;; open helm buffer inside current window, not occupy whole other window
        helm-split-window-in-side-p t
        ido-use-virtual-buffers t
        helm-completion-in-region-fuzzy-match t
        ;; move to end or beginning of source when reaching top or bottom of source
        helm-move-to-line-cycle-in-source t
        helm-display-header-line t
        helm-idle-delay 0.1 ; be idle for this many seconds, before updating in delayed sources
        ;; be idle for this many seconds, before updating candidate buffer
        helm-input-idle-delay 0.1
        helm-follow-mode-persistent t
        helm-always-two-windows nil)
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-dired-recent-dirs
                                    ;;helm-source-bookmarks
                                    helm-source-buffer-not-found))
  (use-package helm-buffers
    :config
    ;; fuzzy matching buffer names when non--nil
    (setq helm-buffers-fuzzy-matching t))
  (use-package helm-utils
    :config (setq helm-highlight-number-lines-around-point 10
                  helm-yank-symbol-first t))
  (use-package helm-files
    :config (setq helm-file-cache-fuzzy-match t
                  helm-ff-transformer-show-only-basename nil
                  helm-ff-file-name-history-use-recentf t
                  helm-ff-auto-update-initial-value t
                  helm-recentf-fuzzy-match t))
  (use-package helm-dabbrev
    :config (setq helm-dabbrev-cycle-threshold 2))
  (use-package helm-org
    :config (setq helm-org-headings-fontify t))
  (use-package helm-dired-recent-dirs
    :ensure t
    :config (setq helm-dired-recent-dirs-max 50))
  (use-package helm-adaptive
    :config
    (setq helm-adaptive-history-file (concat dotemacs-temp-directory "helm-adaptive-history"))
    (helm-adaptive-mode 1))
  (use-package helm-descbinds
    :ensure t
    :config (helm-descbinds-mode 1))
  (use-package helm-words
    :disabled t
    :ensure t)
  (use-package helm-bibtex
    :ensure t)
  (use-package helm-orgcard
    :ensure t)
  (use-package helm-mode-manager
    :ensure t)
  (use-package helm-themes
    :ensure t)
  (use-package helm-helm-commands
    :ensure t)
  (use-package helm-swoop
    :ensure t
    :config
    (setq helm-multi-swoop-edit-save t
          helm-swoop-speed-or-color t
          helm-swoop-split-direction #'split-window-vertically
          helm-swoop-split-with-multiple-windows nil
          helm-swoop-use-line-number-face t))
  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  :bind
  ("M-x" . helm-M-x)
  ("C-x b" . helm-mini)
  ("C-x C-b" . helm-buffers-list)
  ;; Starting helm-find-files with C-u will show you a little history of the last visited directories.
  ("C-x C-f" . helm-find-files)
  ("<f8>" . helm-recentf) ;; not really required, can instead use 'helm-mini
  ("C-x C-l" . helm-locate)
  ("M-y" . helm-show-kill-ring)
  ;;("<tab>" . helm-execute-persistent-action) ; do not rebind <tab> globally
  ("C-z" . helm-select-action)
  ("M-i" . helm-swoop)
  :diminish helm-mode)

(provide 'helm-init)

;;; helm-init.el ends here
