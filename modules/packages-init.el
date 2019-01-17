;;; packages-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup packages.

;;; Code:

(defvar dotemacs-emacs-custom-file)

(setq load-prefer-newer t)

(require 'package)
(setq package-user-dir (expand-file-name "~/.emacs.d/elpa/")
      ;; Avoid loading packages twice
      package-enable-at-startup nil)
(package-initialize)

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/") t)

;; Marmalade repo often does not work reliably
(when (bound-and-true-p dotemacs-use-marmalade-repo-p)
  (add-to-list 'package-archives
               '("marmalade" . "https://marmalade-repo.org/packages/") t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(setq use-package-check-before-init t
      use-package-always-ensure nil
      use-package-always-defer t
      use-package-verbose t)
;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(defun package--save-selected-packages (&rest opt) nil)

(use-package bind-key
  :ensure t
  :bind ("C-c d k" . describe-personal-keybindings))

(use-package diminish
  :ensure t)

(use-package paradox
  :ensure t
  :bind (("C-c d l" . paradox-list-packages)
         ("C-c d u" . paradox-upgrade-packages))
  :config
  (use-package async
    :ensure t)
  (setq paradox-execute-asynchronously t
        ;; paradox-github-token t
        paradox-spinner-type 'random)
  (paradox-enable))

;; www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(use-package cus-edit
  :defer 2
  :config
  (setq custom-file dotemacs-emacs-custom-file)
  (when (file-exists-p custom-file)
    (load custom-file :noerror)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'packages-init)

;;; packages-init.el ends here
