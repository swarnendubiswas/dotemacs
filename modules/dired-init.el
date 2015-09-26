;;; dired-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Dired configurations.

;;; Code:

;; Use "C-x d", or "M-x dired". Kill whole dired buffer with "C-u q".
(use-package dired
  :defer t
  :functions (dired-mark dired-unmark dired-unmark-all-marks dired-toggle-marks dired-prev-marked-file dired-next-marked-file dired-copy-filename-as-kill)
  :preface
  (defun dired-go-home ()
    (interactive)
    (dired "~/"))

  (defun dired-jump-to-top ()
    (interactive)
    (goto-char (point-min)) ; faster than (beginning-of-buffer)
    (dired-next-line 2))

  (defun dired-jump-to-bottom ()
    (interactive)
    (goto-char (point-max)) ; faster than (end-of-buffer)
    (dired-next-line -1))

  :config
  (setq dired-auto-revert-buffer t ; revert each dired buffer automatically when you visit it
        dired-recursive-deletes 'always ; single prompt for all n directories
        dired-recursive-copies 'always
        ;; check ls for options
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
    :config
    (setq dired-bind-jump t
          ;; do not show messages when omitting files
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
    (setq dired-efap-initial-filename-selection nil) ; options: t, nil, no-extension
    (bind-key "<f2>" #'dired-efap dired-mode-map))

  ;; http://oremacs.com/2015/02/21/hydra-docstring-sexp
  (defhydra hydra-dired-marked (dired-mode-map "" :color pink)
    "Number of marked items: %(length (dired-get-marked-files))"
    ("m"   dired-mark                      "mark")
    ("u"   dired-unmark                    "unmark")
    ("U"   dired-unmark-all-marks          "unmark ALL")
    ("t"   dired-toggle-marks              "toggle marks")
    ("P"   dired-prev-marked-file          "prev marked")
    ("M-{" dired-prev-marked-file          "prev marked")
    ("N"   dired-next-marked-file          "next marked")
    ("M-}" dired-next-marked-file          "next marked")
    ("w"   dired-copy-filename-as-kill     "copy file name(s)")
    ("W"   (dired-copy-filename-as-kill 0) "copy file name(s) - full path")
    ("C-g" nil                             "cancel" :color blue))
  (bind-key "." 'hydra-dired-marked/body dired-mode-map))

(provide 'dired-init)

;;; dired-init ends here
