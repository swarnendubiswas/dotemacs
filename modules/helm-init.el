;;; helm-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure helm.

;;; Code:

(use-package helm
  :ensure helm-core
  :if (bound-and-true-p dotemacs-use-helm-p)
  :init
  (use-package helm-flx ;; recommended to load before helm
    :ensure t
    :config (helm-flx-mode 1))

  (setq helm-quick-update t ; do not display invisible candidates
        helm-candidate-number-limit 100
        helm-M-x-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-apropos-fuzzy-match t
        helm-split-window-default-side 'below
        helm-split-window-in-side-p t ; open helm buffer inside current window, not occupy whole other window
        ido-use-virtual-buffers 'auto
        helm-completion-in-region-fuzzy-match t
        helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source
        helm-display-header-line t
        helm-echo-input-in-header-line t
        helm-idle-delay 0.1 ; be idle for this many seconds, before updating in delayed sources
        helm-input-idle-delay 0.1 ; be idle for this many seconds, before updating candidate buffer
        helm-follow-mode-persistent t
        ;; helm-always-two-windows nil
        ;; both the min and max height are set to be equal on purpose
        ;; helm-autoresize-max-height 60
        ;; helm-autoresize-min-height 60
        helm-case-fold-search 'smart)

  (use-package helm-mode
    :diminish helm-mode
    :init
    (setq helm-completion-in-region-fuzzy-match t)
    (helm-mode 1))

  (helm-autoresize-mode 1)

  (use-package helm-dired-recent-dirs
    :ensure t
    :disabled t
    :init
    (setq shell-file-name "/usr/bin/fish"
          helm-dired-recent-dirs-max 50))

  (use-package helm-buffers
    :bind
    (("C-c h m" . helm-mini)
     ;; ([remap switch-to-buffer] . helm-mini)
     ;; ([remap list-buffers] . helm-buffers-list)
     ("<f6>" . helm-buffers-list)
     ("C-c h b" . helm-buffers-list))
    :config
    (setq helm-buffers-fuzzy-matching t
          helm-buffer-skip-remote-checking t
          helm-buffer-max-length 45
          helm-mini-default-sources '(helm-source-buffers-list
                                      helm-source-recentf
                                      ;; helm-source-dired-recent-dirs
                                      helm-source-buffer-not-found)))

  (use-package helm-command
    :bind
    (([remap execute-extended-command] . helm-M-x)
     ("M-x" . helm-M-x)
     ;; convenient since it is a single keypress
     ("<f1>" . helm-M-x)))

  (use-package helm-utils
    :init
    (setq helm-highlight-number-lines-around-point 10
          helm-yank-symbol-first t))

  (use-package helm-files
    :config
    (setq helm-file-cache-fuzzy-match t
          helm-ff-transformer-show-only-basename nil
          helm-ff-file-name-history-use-recentf t
          helm-ff-search-library-in-sexp t
          helm-ff-auto-update-initial-value t
          helm-recentf-fuzzy-match t
          helm-ff-skip-boring-files t
          helm-boring-file-regexp-list (append helm-boring-file-regexp-list
                                               '("\\.undo$" "\\.elc$" "\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\#$" "\\~$")))
    :bind
    (;; Starting helm-find-files with C-u will show you a little history of the last visited directories.
     ("<f4>" . helm-find-files)
     ("C-c h f" . helm-find-files)
     ;; ([remap find-file] . helm-find-files)
     ("<f5>" . helm-for-files)
     ("C-c h j" . helm-for-files)
     ("C-c h r" . helm-recentf)))

  (use-package helm-adaptive
    :init
    (setq helm-adaptive-history-file (concat dotemacs-temp-directory "helm-adaptive-history"))
    (helm-adaptive-mode 1))

  (use-package helm-descbinds
    :ensure t
    :init
    (fset 'describe-bindings 'helm-descbinds)
    (helm-descbinds-mode 1))

  ;; "C-c C-e" to go into edit mode
  (use-package helm-swoop
    :ensure t
    :bind
    (("C-c h s" . helm-swoop)
     ("C-c h /" . helm-multi-swoop))
    :config
    (setq helm-multi-swoop-edit-save t ; save buffer when helm-multi-swoop-edit complete
          helm-swoop-speed-or-color nil
          helm-swoop-split-direction #'split-window-vertically
          helm-swoop-split-with-multiple-windows nil
          helm-swoop-move-to-line-cycle t ; go to the opposite side of line from the end or beginning of line
          helm-swoop-use-line-number-face t))

  (use-package helm-make
    :ensure t
    :bind ("C-c h k" . helm-make-projectile))

  (use-package helm-grep
    :init
    ;; http://stackoverflow.com/questions/28316688/how-to-bind-helm-do-grep-1-to-a-key-in-emacs
    (global-set-key [f12]
                    (lambda ()
                      (interactive)
                      (let ((current-prefix-arg 't))
                        (call-interactively 'helm-do-grep)))))

  (use-package helm-ring
    :bind (([remap yank-pop] . helm-show-kill-ring))
    :init (helm-push-mark-mode 1))

  (use-package helm-elisp-package
    :bind ("C-c h p" . helm-list-elisp-packages))

  (use-package helm-fuzzier
    :ensure t
    :init (helm-fuzzier-mode 1))

  (bind-keys :map helm-map
             ("<tab>" . helm-execute-persistent-action) ; do not rebind <tab> globally
             ("C-z" . helm-select-action))

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
    ("/" helm-multi-swoop "helm-multi-swoop")
    ("a" helm-apropos "helm-apropos")
    ("g" helm-do-grep "helm-do-grep")
    ("u" helm-resume "helm-resume")
    ;; swoop is better than occur
    ("o" helm-occur "helm-occur"))
  (global-unset-key (kbd "C-b"))
  (bind-key "C-b" 'hydra-helm/body)

  :bind
  (("C-c h l" . helm-locate)
   ("C-c h y" . helm-show-kill-ring)
   ("C-c h a" . helm-apropos)
   ("C-c h g" . helm-do-grep)
   ("<f7>" . helm-resume)
   ("C-c h u" . helm-resume)
   ;; swoop is better than occur
   ("C-c h o" . helm-occur)))

(provide 'helm-init)

;;; helm-init.el ends here
