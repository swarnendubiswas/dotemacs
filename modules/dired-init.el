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
  :after dired
  :load-path "extras"
  :init
  ;; Set this flag before dired+ is loaded: http://irreal.org/blog/?p=3341
  (setq-default diredp-hide-details-initially-flag nil
                diredp-hide-details-propagate-flag nil)
  (diredp-toggle-find-file-reuse-dir 1))

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

(provide 'dired-init)

;;; dired-init ends here
