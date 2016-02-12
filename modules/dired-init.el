;;; dired-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Dired configurations.

;;; Code:

;; Use "C-x d", or "M-x dired". Kill whole dired buffer with "C-u q".
(use-package dired
  :functions (dired-mark dired-unmark dired-unmark-all-marks dired-toggle-marks dired-prev-marked-file dired-next-marked-file dired-copy-filename-as-kill)
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

  :config
  (setq dired-auto-revert-buffer t ; Revert each dired buffer automatically when you visit it
        dired-recursive-deletes 'always ; Single prompt for all n directories
        dired-recursive-copies 'always
        ;; Check ls for additional options
        dired-listing-switches "-ABhl --si --group-directories-first"
        dired-ls-F-marks-symlinks t ; -F marks links with @
        dired-dwim-target t)

  (bind-keys
   :map dired-mode-map
   ("M-<home>" . dired-go-home)
   ("i" . ido-find-file)
   ("M-<up>" . dired-jump-to-top)
   ("M-<down>" . dired-jump-to-bottom))

  (use-package dired-x
    :functions (dired-jump dired-omit-mode)
    :init
    (setq dired-bind-jump t
          ;; Do not show messages when omitting files
          dired-omit-verbose nil)
    (add-hook 'dired-mode-hook #'dired-omit-mode)

    ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
    (defadvice dired-omit-startup (after diminish-dired-omit activate)
      "Make sure to remove \"Omit\" from the modeline."
      (diminish 'dired-omit-mode) dired-mode-map)

    :bind*
    ;; open dired with the cursor right on the file you're editing
    ("C-x C-j" . dired-jump))

  (use-package dired+
    :ensure t
    ;; Set this flag before dired+ is loaded: http://irreal.org/blog/?p=3341
    :init
    (setq-default diredp-hide-details-initially-flag nil
                  diredp-hide-details-propagate-flag nil)
    (diredp-toggle-find-file-reuse-dir 1))

  (use-package dired-efap
    :ensure t
    :config
    (setq dired-efap-initial-filename-selection nil) ; Options: t, nil, no-extension
    (bind-key "<f2>" #'dired-efap dired-mode-map))

  (use-package diredful
    :ensure t
    :init (diredful-mode 1)))

(provide 'dired-init)

;;; dired-init ends here
