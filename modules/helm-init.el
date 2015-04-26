;;; helm-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure helm.

;;; Code:

(use-package helm
  :ensure t
  :demand t
  :init
  (require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode 1)
  :config
  (setq helm-ff-transformer-show-only-basename nil
        helm-yank-symbol-first t
        helm-quick-update t
        helm-buffers-fuzzy-matching t
        helm-M-x-fuzzy-match t
        helm-ff-file-name-history-use-recentf nil
        helm-ff-auto-update-initial-value t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-split-window-in-side-p t
        ido-use-virtual-buffers t
        helm-completion-in-region-fuzzy-match t
        helm-move-to-line-cycle-in-source t
        helm-org-headings-fontify t
        helm-display-header-line t
        helm-dabbrev-cycle-threshold 3)
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-buffer-not-found))
  (use-package helm-adaptive
    :config
    (setq helm-adaptive-history-file (concat emacs-temp-directory "helm-adaptive-history"))
    (helm-adaptive-mode 1))
  (use-package helm-company
    :ensure t)
  (use-package helm-dired-recent-dirs
    :ensure t)
  (use-package helm-descbinds
    :ensure t
    :config (helm-descbinds-mode 1))
  (use-package helm-flyspell
    :ensure t)
  (use-package helm-flycheck
    :ensure t)
  (use-package helm-words
    :ensure t)
  (use-package helm-bibtex
    :ensure t)
  (use-package helm-orgcard
    :ensure t)
  (use-package helm-c-yasnippet
    :ensure t)
  (use-package helm-j-cheatsheet
    :ensure t
    :commands helm-j-cheatsheet)
  :bind
  ("M-x" . helm-M-x)
  ("C-x b" . helm-mini)
  ("C-x C-b" . helm-buffers-list)
  ;; Starting helm-find-files with C-u will show you a little history of the last visited directories.
  ("C-x C-f" . helm-find-files)
  ("<f8>" . helm-recentf)
  ("C-x C-l" . helm-locate)
  ("M-y" . helm-show-kill-ring)
  :diminish helm-mode)

(provide 'helm-init)

;;; helm-init.el ends here
