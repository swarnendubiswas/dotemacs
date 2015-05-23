;;; dired-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Dired configurations.

;;; Code:

;; Use "C-x d", or "M-x dired". Kill whole dired buffer with "C-u q".
(use-package dired
  :preface
  (defun dired--go-home ()
    (interactive)
    (dired "~/"))
  
  ;; M-<up> is nicer in dired if it moves to the first file
  (defun dired--jump-to-top ()
    (interactive)
    (beginning-of-buffer)
    (dired-next-line 2))
  
  ;; M-<down> is nicer in dired if it moves to the last file
  (defun dired--jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))
  
  :config
  (setq dired-auto-revert-buffer t ; revert each dired buffer automatically when you visit it
        dired-recursive-deletes 'always ; single prompt for all n directories
        dired-recursive-copies 'always
        ;;dired-listing-switches "-ABhltc --si --group-directories-first"
        dired-listing-switches "-ABhl --si --group-directories-first"
        dired-ls-F-marks-symlinks t ; -F marks links with @
        dired-dwim-target t)
  
  ;; jump to home directory
  ;; (global-set-key (kbd "M-<home>")
  ;;                 (lambda ()
  ;;                   (interactive)
  ;;                   (dired "~/")))
  (bind-key "M-<home>" 'dired--go-home dired-mode-map)
  ;; (with-eval-after-load "dired"
  ;;   (define-key dired-mode-map (kbd "i") 'ido-find-file))
  (bind-key "i" 'ido-find-file dired-mode-map)
  ;; (with-eval-after-load "dired"
  ;;   (define-key dired-mode-map (kbd "M-<up>") 'dired-back-to-top))
  (bind-key "M-<up>" 'dired-back-to-top dired-mode-map)
  ;; (with-eval-after-load "dired"
  ;;   (define-key dired-mode-map (kbd "M-<down>") 'dired-jump-to-bottom))
  (bind-key "M-<down>" 'dired-jump-to-bottom dired-mode-map))

;; Jump to dired buffer corresponding to current buffer.
(use-package dired-x
  :commands dired-jump
  :config
  (setq dired-bind-jump t
        ;; do not show messages when omitting files
        dired-omit-verbose nil)
  (add-hook 'dired-mode-hook #'dired-omit-mode)

  ;; SB: This does not work.
  ;; (eval-after-load "dired-omit-mode"
  ;;   '(diminish dired-omit-mode))
  ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Make sure to remove \"Omit\" from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map)
  
  :bind*
  ;; C-x C-j opens dired with the cursor right on the file you're editing
  ("C-x C-j" . dired-jump))

(use-package dired+
  :ensure t
  ;; Set this flag before dired+ is loaded: http://irreal.org/blog/?p=3341
  :init (setq-default diredp-hide-details-initially-flag nil
                      diredp-hide-details-propagate-flag nil)
  :config (diredp-toggle-find-file-reuse-dir 1))

;; direx:jump-to-directory is a good explorer
(use-package direx
  :ensure t
  :defer t)

(use-package direx-grep
  :ensure t
  :defer t)

(use-package dired-efap
  :ensure t
  :config
  ;; (when (dired-mode)
  ;;   (define-key dired-mode-map (kbd "<f2>") nil))
  ;;(define-key dired-mode-map [f2] 'dired-efap)
  (bind-key "<f2>" 'dired-efap dired-mode-map))

;; Not required starting from Emacs 24.4
;; (use-package dired-details
;;   :ensure t
;;   :config (setq dired-details-hide-link-targets nil))

;; (use-package dired-details+
;;   :ensure t)

(use-package dired-rainbow
  :ensure t)

(use-package dired-hacks-utils
  :ensure t
  :defer t)

(use-package nav
  :disabled t
  :load-path "lisp/emacs-nav-49/"
  :config
  (nav-mode)
  ;;(nav-disable-overeager-window-splitting)
  :bind ("<f6>" . nav-toggle))

(use-package dired-nav-enhance
  :ensure t
  :defer t)

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

(provide 'dired-init)

;;; dired-init ends here
