;;; helm-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure helm.

;;; Code:

(use-package helm
  :ensure helm-core
  :if (eq dotemacs-selection 'helm)
  :init
  (use-package helm-flx ;; Recommended to load before helm
    :ensure t
    :config (helm-flx-mode 1))

  (setq helm-quick-update nil
        helm-candidate-number-limit 100
        helm-apropos-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-lisp-fuzzy-completion t
        ;; I prefer to open helm buffers in full frame since it gives more vertical space. Right side is bad since long
        ;; lines can get truncated.
        helm-full-frame t ; Make the helm buffer occupy the full frame
        ;; helm-split-window-default-side 'right
        ;; helm-split-window-in-side-p nil ; Open helm buffer inside current window, not occupy whole other window
        ;; helm-always-two-windows nil
        helm-move-to-line-cycle-in-source t ; Move to end or beginning of source when reaching top or bottom of source
        helm-display-header-line t
        helm-echo-input-in-header-line t
        ;; helm-idle-delay 0.1 ; Be idle for this many seconds, before updating in delayed sources
        ;; helm-input-idle-delay 0.1 ; Be idle for this many seconds, before updating candidate buffer
        helm-follow-mode-persistent t
        ;; Both the min and max height are set to be equal on purpose
        ;; helm-autoresize-max-height 60
        ;; helm-autoresize-min-height 60
        ;; Default is 'smart, searches and matches should ignore case
        helm-case-fold-search t)

  (use-package helm-mode
    :diminish helm-mode
    :init
    (setq helm-completion-in-region-fuzzy-match t
          helm-mode-fuzzy-match t)
    (helm-mode 1))

  (helm-autoresize-mode -1) ; Distracting

  (use-package helm-dired-recent-dirs
    :ensure t
    :disabled t
    :init
    (setq shell-file-name "/usr/bin/fish"
          helm-dired-recent-dirs-max 50))

  (use-package helm-buffers
    :bind
    (([remap switch-to-buffer] . helm-mini)
     ([remap list-buffers] . helm-buffers-list))
    :config
    (setq helm-buffers-fuzzy-matching t
          helm-buffer-skip-remote-checking t
          helm-buffer-max-length 45
          helm-mini-default-sources '(helm-source-buffers-list
                                      helm-source-recentf
                                      ;; helm-source-dired-recent-dirs
                                      helm-source-buffer-not-found)))

  (use-package helm-command
    :config (setq helm-M-x-fuzzy-match t)
    :bind
    (([remap execute-extended-command] . helm-M-x)
     ;; Convenient since it is a single keypress
     ("<f1>" . helm-M-x)))

  (use-package helm-utils
    :init
    (setq helm-highlight-number-lines-around-point 10
          helm-yank-symbol-first t)
    (helm-popup-tip-mode 1))

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
     ([remap find-file] . helm-find-files)
     ("<f3>" . helm-find-files)
     ("<f4>" . helm-for-files)
     ("<f8>" . helm-recentf)))

  (use-package helm-adaptive
    :init
    (setq helm-adaptive-history-file (concat dotemacs-temp-directory "helm-adaptive-history"))
    (helm-adaptive-mode 1))

  (use-package helm-descbinds
    :ensure t
    :init
    (fset 'describe-bindings 'helm-descbinds)
    (helm-descbinds-mode 1))

  (use-package helm-describe-modes
    :ensure t
    :config (global-set-key [remap describe-mode] #'helm-describe-modes))

  ;; "C-c C-e" to go into edit mode
  (use-package helm-swoop
    :ensure t
    :bind
    (("C-c h s" . helm-swoop)
     ("C-c h /" . helm-multi-swoop))
    :config
    (setq helm-multi-swoop-edit-save t ; Save buffer when helm-multi-swoop-edit complete
          helm-swoop-speed-or-color nil
          helm-swoop-split-direction #'split-window-vertically
          helm-swoop-split-with-multiple-windows nil
          helm-swoop-move-to-line-cycle t ; go to the opposite side of line from the end or beginning of line
          helm-swoop-use-line-number-face t))

  (use-package helm-make
    :ensure t
    :bind ("C-c h k" . helm-make-projectile)
    :config
    (when (eq dotemacs-selection 'ivy)
      (setq helm-make-completion-method 'ivy)))

  (use-package helm-grep
    :disabled t
    :init
    ;; http://stackoverflow.com/questions/28316688/how-to-bind-helm-do-grep-1-to-a-key-in-emacs
    (global-set-key [f12]
                    (lambda ()
                      (interactive)
                      (let ((current-prefix-arg 't))
                        (call-interactively 'helm-do-grep)))))

  (use-package helm-ring
    :bind ([remap yank-pop] . helm-show-kill-ring)
    :init (helm-push-mark-mode 1))

  (use-package helm-elisp-package
    :bind ("C-c h p" . helm-list-elisp-packages))

  (use-package helm-fuzzier
    :ensure t
    :disabled t ; FIXME: Enable after https://github.com/EphramPerdition/helm-fuzzier/issues/12 is fixed.
    :init (helm-fuzzier-mode 1))

  (bind-keys :map helm-map
             ("<tab>" . helm-execute-persistent-action) ; Do not rebind <tab> globally
             ("C-z" . helm-select-action))

  :bind
  (("C-c h l" . helm-locate)
   ("C-c h a" . helm-apropos)
   ("C-c h g" . helm-do-grep)
   ("<f7>" . helm-resume)
   ;; swoop is better than occur
   ([remap occur] . helm-occur)
   ("C-c h o" . helm-occur)))

(provide 'helm-init)

;;; helm-init.el ends here
