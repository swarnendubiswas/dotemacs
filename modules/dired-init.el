;;; dired-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Dired configurations.

;;; Code:

;; Use "C-x d", or "M-x dired". Kill whole dired buffer with "C-u q".
(use-package dired
  :config
  (setq dired-auto-revert-buffer t ; revert each dired buffer automatically when you visit it
        dired-recursive-deletes 'always ; single prompt for all n directories
        dired-recursive-copies 'always
        ;;delete-by-moving-to-trash t
        ;;dired-listing-switches "-ABhltc --si --group-directories-first"
        dired-listing-switches "-ABhl --si --group-directories-first"
        dired-ls-F-marks-symlinks t
        dired-dwim-target t))

;; Jump to dired buffer corresponding to current buffer.
(use-package dired-x
  :commands (dired-jump)
  :config
  (setq dired-bind-jump t)
  ;;(setq-default dired-omit-mode t)
  ;; (add-hook 'dired-mode-hook
  ;;           (lambda ()
  ;;             (dired-omit-mode 1)))
  :bind ("C-x C-j" . dired-jump)) ;; C-x C-j opens dired with the cursor right on the file you're editing

(use-package dired+
  :ensure t
  ;; Set this flag before dired+ is loaded: http://irreal.org/blog/?p=3341
  :init (setq-default diredp-hide-details-initially-flag nil))

;; direx:jump-to-directory is a good explorer
(use-package direx
  :ensure t)

;; (when (dired-mode)
;;   (define-key dired-mode-map (kbd "<f2>") nil))

(use-package dired-efap
  :ensure t
  :config
  ;;(define-key dired-mode-map [f2] 'dired-efap)
  (bind-key "<f2>" 'dired-efap dired-mode-map))

;; Not required starting from Emacs 24.4
;; (use-package dired-details
;;   :ensure t
;;   :config (setq dired-details-hide-link-targets nil))

;; (use-package dired-details+
;;   :ensure t
;;   :defer 5)

(use-package dired-rainbow
  :ensure t)

(use-package dired-hacks-utils
  :ensure t)

(use-package nav
  :disabled t
  :load-path "~/.emacs.d/lisp/emacs-nav-49/"
  :config
  (nav-mode)
  ;;(nav-disable-overeager-window-splitting)
  :bind ("<f6>" . nav-toggle))

(provide 'dired-init)

;;; dired-init ends here
