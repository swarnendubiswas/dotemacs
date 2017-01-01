;;; helm-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure helm.

;;; Code:

(defvar dotemacs-selection)
(defvar dotemacs-temp-directory)

(use-package helm-mode
  :ensure helm
  :if (eq dotemacs-selection 'helm)
  :diminish helm-mode
  :config
  (setq helm-completion-in-region-fuzzy-match t
        helm-mode-fuzzy-match t)
  (helm-mode 1))

(use-package helm
  :ensure helm-core
  :if (eq dotemacs-selection 'helm)
  :config
  (setq helm-candidate-number-limit 100
        helm-locate-fuzzy-match t
        helm-ag-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-etags-fuzzy-match t
        ;; I now prefer to open helm buffers in full frame since it gives more vertical space. Right side is bad since
        ;; long lines can get truncated.
        helm-full-frame nil ; Make the helm buffer occupy the full frame
        ;; helm-split-window-default-side 'right
        ;; helm-split-window-in-side-p nil ; Open helm buffer inside current window, not occupy whole other window
        helm-always-two-windows t
        helm-move-to-line-cycle-in-source t ; Move to end or beginning of source when reaching top or bottom of source
        helm-display-header-line nil
        helm-echo-input-in-header-line nil
        helm-case-fold-search t ; Default is 'smart, searches and matches should ignore case
        helm-follow-mode-persistent t)

  (helm-autoresize-mode -1) ; Distracting

  (use-package helm-buffers
    :preface
    ;; https://github.com/zeltak/.emacs.d/blob/master/settings.el
    (defun dotemacs--sort-dired-buffers (buffers)
      "Sort BUFFERS by moving all Dired buffers to the end."
      (let (dired-buffers other-buffers)
        (dolist (buf buffers)
          (if (with-current-buffer buf
                (eq major-mode 'dired-mode))
              (push buf dired-buffers)
            (push buf other-buffers)))
        (nreverse (append dired-buffers other-buffers))))

    (defun dotemacs--helm-buffers-sort-dired-buffers (orig-fun &rest args)
      (dotemacs--sort-dired-buffers (apply orig-fun args)))

    :bind
    (([remap switch-to-buffer] . helm-mini)
     ([remap list-buffers] . helm-buffers-list))
    :init (advice-add 'helm-buffers-sort-transformer :around #'dotemacs--helm-buffers-sort-dired-buffers)
    :config
    (setq helm-buffers-fuzzy-matching t
          helm-buffer-skip-remote-checking t
          helm-buffer-max-length 45 ; When disabled (nil) use the longest buffer-name length found
          helm-buffer-details-flag t
          helm-buffers-truncate-lines nil
          helm-mini-default-sources '(helm-source-buffers-list
                                      helm-source-recentf
                                      helm-source-dired-recent-dirs
                                      helm-source-buffer-not-found)))

  (use-package helm-command
    :config (setq helm-M-x-fuzzy-match t)
    :bind
    ;; Convenient since it is a single key press
    ("<f1>" . helm-M-x))

  (use-package helm-utils
    :config
    (setq helm-highlight-matches-around-point-max-lines 10
          helm-yank-symbol-first t)
    (helm-popup-tip-mode 1))

  (use-package helm-files
    :config
    (setq helm-ff-transformer-show-only-basename t ; Do not show the complete path
          helm-ff-file-name-history-use-recentf t
          helm-ff-search-library-in-sexp t
          helm-ff-auto-update-initial-value nil ; Auto update when only one candidate directory is matched
          helm-ff-skip-boring-files t
          helm-ff-fuzzy-matching t
          helm-ff-tramp-not-fancy nil
          helm-ff-newfile-prompt-p t
          helm-ff-guess-ffap-urls nil
          helm-ff-guess-ffap-filenames t
          helm-file-cache-fuzzy-match t
          helm-recentf-fuzzy-match t
          helm-for-files-preferred-list '(helm-source-buffers-list
                                          helm-source-file-cache
                                          helm-source-files-in-current-dir
                                          helm-source-recentf))

    (unless (bound-and-true-p dotemacs-use-ignoramus-p)
      (setq helm-boring-file-regexp-list (append helm-boring-file-regexp-list
                                                 '("\\.elc$"
                                                   "\\.fdb_latexmk$"
                                                   "\\.fls$"
                                                   "\\.git$"
                                                   "\\.hg$"
                                                   "\\.la$"
                                                   "\\.log$"
                                                   "\\.o$"
                                                   "\\.out$"
                                                   "\\.pdf$"
                                                   "\\.ps$"
                                                   "\\.pyc$"
                                                   "\\.rel$"
                                                   "\\.rip$"
                                                   "\\.undo$"
                                                   "\\#$"
                                                   "\\~$"))))
    :bind
    (;; Starting helm-find-files with C-u will show you a little history of the last visited directories.
     ([remap find-file] . helm-find-files)
     ("<f2>" . helm-find-files)
     ("<f3>" . helm-for-files)
     ("<f9>" . helm-recentf)))

  (use-package helm-adaptive
    :config
    (setq helm-adaptive-history-file (concat dotemacs-temp-directory "helm-adaptive-history"))
    (helm-adaptive-mode 1))

  (use-package helm-dabbrev
    :config (setq helm-dabbrev-case-fold-search t)
    :bind ([remap dabbrev-expand] . helm-dabbrev))

  (use-package helm-elisp-package
    :bind ("C-c h p" . helm-list-elisp-packages))

  :bind
  (([remap locate] . helm-locate)
   ("C-c h l" . helm-locate)
   ([remap apropos] . helm-apropos)
   ("C-c h a" . helm-apropos)
   ("C-c h g" . helm-do-grep)
   ("C-c h r" . helm-resume)
   ("C-c r" . helm-resume)
   ([remap occur] . helm-occur)
   ("C-c h o" . helm-occur)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action) ; Do not rebind <tab> globally
   ("C-z" . helm-select-action)))

(use-package helm-smex
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :after helm
  :bind
  (([remap execute-extended-command] . helm-smex)
   ("M-X" . helm-smex-major-mode-commands)))

(use-package helm-fuzzier
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :after helm
  :config (helm-fuzzier-mode 1))

(use-package helm-flx
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :after helm
  :config (helm-flx-mode 1))

(use-package helm-descbinds
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :after helm
  :config
  (fset 'describe-bindings 'helm-descbinds)
  (helm-descbinds-mode 1))

(use-package helm-describe-modes
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :after helm
  :bind ([remap describe-mode] . helm-describe-modes))

(use-package helm-ls-svn
  :ensure t)

(use-package helm-ls-git
  :ensure t)

(provide 'helm-init)

;;; helm-init.el ends here
