;;; dired-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Dired configurations.

;;; Code:

(use-package dired
  :preface
  (defun dired-go-home ()
    (interactive)
    (dired "~/"))

  (defun dired-jump-to-top ()
    (interactive)
    (goto-char (point-min)) ; Faster than (beginning-of-buffer)
    (dired-next-line 2))

  (defun dired-jump-to-bottom ()
    (interactive)
    (goto-char (point-max)) ; Faster than (end-of-buffer)
    (dired-next-line -1))

  :bind (:map dired-mode-map
              ("M-<home>" . dired-go-home)
              ("i" . find-file)
              ("M-<up>" . dired-jump-to-top)
              ("M-<down>" . dired-jump-to-bottom))
  :config
  (setq dired-auto-revert-buffer t ; Revert each dired buffer automatically when you "revisit" it
        dired-recursive-deletes 'always ; Single prompt for all n directories
        dired-recursive-copies 'always
        dired-listing-switches "-ABhl --si --group-directories-first" ; Check `ls' for additional options
        dired-ls-F-marks-symlinks t ; -F marks links with @
        dired-dwim-target t)
  ;; Auto refresh dired when files change
  (add-hook 'dired-mode-hook 'auto-revert-mode))

(use-package dired-x
  :commands dired-jump
  :config
  (setq dired-bind-jump t
        ;; Do not show messages when omitting files
        dired-omit-verbose nil)
  (unless (bound-and-true-p dotemacs-use-ignoramus-p)
    (add-hook 'dired-mode-hook #'dired-omit-mode))

  ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Make sure to remove \"Omit\" from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map)
  :bind ("C-x C-j" . dired-jump))

(use-package dired+
  :disabled t
  :after dired
  :load-path "extras"
  :init
  ;; Set this flag before dired+ is loaded: http://irreal.org/blog/?p=3341
  (setq-default diredp-hide-details-initially-flag nil
                diredp-hide-details-propagate-flag nil)
  :config
  (toggle-diredp-find-file-reuse-dir 1)
  ;;(diredp-toggle-find-file-reuse-dir 1)
  )

(use-package diredfl
  :ensure t
  :after dired
  :config (diredfl-global-mode))

(use-package dired-efap
  :ensure t
  :after dired
  :commands dired-efap
  :config (setq dired-efap-initial-filename-selection nil)
  :bind (:map dired-mode-map
              ("r" . dired-efap )))

(use-package dired-narrow ; Narrow dired to match filter
  :ensure t
  :after dired
  :commands dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-quick-sort
  :ensure t
  :after dired
  :config (dired-quick-sort-setup))

(use-package ecb
  :ensure t
  :if (bound-and-true-p dotemacs-use-ecb)
  :config
  (ecb-layout-define "swarna1" left nil
                     (ecb-split-ver 0.5 t)
                     (if (fboundp (quote ecb-set-sources-buffer)) (ecb-set-sources-buffer) (ecb-set-default-ecb-buffer))
                     (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
                     (if (fboundp (quote ecb-set-methods-buffer)) (ecb-set-methods-buffer) (ecb-set-default-ecb-buffer))
                     (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
                     (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
                     )
  (setq ecb-examples-bufferinfo-buffer-name nil
        ecb-create-layout-file (concat dotemacs-temp-directory "ecb-user-layouts.el")
        ecb-tip-of-the-day nil
        ecb-tree-buffer-style 'ascii-guides
        ecb-show-sources-in-directories-buffer 'always
        ecb-layout-name "swarna1"
        ecb-compile-window-height nil)
  (ecb-activate)
  (add-hook 'compilation-finish-functions (lambda (buf strg) (kill-buffer buf))))

(use-package sr-speedbar
  :ensure t
  :disabled t
  :config
  (setq sr-speedbar-right-side nil
        sr-speedbar-width 15
        sr-speedbar-default-width 15
        sr-speedbar-max-width 20))

(use-package treemacs
  :ensure t
  :commands (treemacs treemacs-toggle)
  :demand t
  :config
  (setq treemacs-follow-after-init t
        treemacs-width 35
        treemacs-indentation 2
        treemacs-git-integration t
        treemacs-collapse-dirs 3
        treemacs-silent-refresh t
        treemacs-change-root-without-asking t
        treemacs-sorting 'alphabetic-desc
        treemacs-show-hidden-files t
        treemacs-never-persist nil
        treemacs-is-never-other-window nil
        treemacs-goto-tag-strategy 'refetch-index
        treemacs-no-png-images nil
        treemacs-recenter-after-file-follow t
        treemacs-recenter-after-tag-follow  t
        treemacs-silent-filewatch t
        treemacs-silent-refresh t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'extended)
  :bind ("C-j" . treemacs-toggle))

;; Delays loading of known projectile projects, not sure why!
(use-package treemacs-projectile
  :ensure t
  :disabled t
  :defer t
  :after treemacs
  :config (setq treemacs-header-function #'treemacs-projectile-create-header))

(provide 'dired-init)

;;; dired-init ends here
