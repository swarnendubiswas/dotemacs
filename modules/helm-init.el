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
        helm-candidate-number-limit 80
        helm-M-x-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-apropos-fuzzy-match t
        helm-split-window-default-side 'right
        ;; open helm buffer inside current window, not occupy whole other window
        helm-split-window-in-side-p t
        ido-use-virtual-buffers 'auto
        helm-completion-in-region-fuzzy-match t
        ;; move to end or beginning of source when reaching top or bottom of source
        helm-move-to-line-cycle-in-source t
        helm-display-header-line t
        helm-idle-delay 0.1 ; be idle for this many seconds, before updating in delayed sources
        ;; be idle for this many seconds, before updating candidate buffer
        helm-input-idle-delay 0.1
        helm-follow-mode-persistent t
        helm-always-two-windows nil
        ;; both the min and max height are set to be equal on purpose
        helm-autoresize-max-height 60
        helm-autoresize-min-height 60
        helm-case-fold-search 'smart)
  (setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-dired-recent-dirs
                                    helm-source-buffer-not-found))
  (use-package helm-plugin
    :config
    ;; http://stackoverflow.com/questions/19949212/emacs-helm-completion-how-to-turn-off-persistent-help-line
    (defadvice helm-display-mode-line (after undisplay-header activate)
      (setq header-line-format nil)))
  (use-package helm-buffers
    :config
    ;; fuzzy matching buffer names when non--nil
    (setq helm-buffers-fuzzy-matching t))
  (use-package helm-utils
    :config
    (setq helm-highlight-number-lines-around-point 10
          helm-yank-symbol-first t))
  (use-package helm-files
    :config
    (setq helm-file-cache-fuzzy-match t
          helm-ff-transformer-show-only-basename nil
          helm-ff-file-name-history-use-recentf t
          helm-ff-auto-update-initial-value t
          helm-recentf-fuzzy-match t
          helm-ff-skip-boring-files t
          helm-boring-file-regexp-list (append helm-boring-file-regexp-list
                                               '("\\.undo$"
                                                 "\\.elc$"
                                                 "\\#$"
                                                 "\\~$"))))
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
    :init (helm-descbinds-mode 1))
  (use-package helm-words
    :disabled t
    :ensure t)
  (use-package helm-bibtex
    :ensure t
    :config (setq helm-bibtex-bibliography "~/workspace/bib/plass.bib"))
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
  ;; http://tuhdo.github.io/c-ide.html
  (use-package helm-gtags
    :ensure t
    :defer t
    :init
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)
    :config
    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'prog-mode-hook 'helm-gtags-mode))
  (use-package helm-make
    :ensure t)

  (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
  (bind-key "C-z" 'helm-select-action helm-map)

  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)

  :bind
  (("M-x" . helm-M-x)
   ("C-c h f" . helm-find-files)
   ;; Starting helm-find-files with C-u will show you a little history of the last visited directories.
   ("<f9>" . helm-find-files)
   ("<f6>" . helm-mini)
   ("<f7>" . helm-buffers-list)
   ("C-c h r" . helm-recentf) ;; not really required, can instead use 'helm-mini
   ("C-c h l" . helm-locate)
   ("C-c h y" . helm-show-kill-ring)
   ;;("<tab>" . helm-execute-persistent-action) ; do not rebind <tab> globally
   ("C-c h s" . helm-swoop)
   ("C-c h a" . helm-apropos)
   ("C-c h g" . helm-do-grep))

  :diminish helm-mode)

;; http://ericjmritz.name/2015/04/06/organizing-key-bindings-in-gnu-emacs-using-hydra/
(defhydra hydra-helm (:color blue)
  "helm commands"
  ("x" helm-M-x "helm-M-x")
  ("b" helm-mini "helm-mini")
  ("i" helm-buffers-list "helm-buffers-list")
  ("f" helm-find-files "helm-find-files")
  ("r" helm-recentf "helm-recentf")
  ("l" helm-locate "helm-locate")
  ("y" helm-show-kill-ring "helm-show-kill-ring")
  ("s" helm-swoop "helm-swoop")
  ("a" helm-apropos "helm-apropos")
  ("g" helm-do-grep "helm-do-grep"))
(global-unset-key (kbd "C-b"))
(bind-key "C-b" 'hydra-helm/body)

(provide 'helm-init)

;;; helm-init.el ends here
