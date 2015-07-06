;;; packages-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup packages. Inspired from http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html

;;; Code:

(require 'package)
(setq package-user-dir (expand-file-name "~/.emacs.d/elpa/")
      package-enable-at-startup nil)

;; elpa ("gnu" . "http://elpa.gnu.org/packages/") is already preconfigured
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

;; setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure nil
      use-package-verbose t)

;; SB: :init always happens before package load, whether :config has been deferred or not. This implies :init is never
;; deferred.

;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
;; (eval-and-compile
;;   (mapc
;;    #'(lambda (path)
;;        (push (expand-file-name path user-emacs-directory) load-path))
;;    '("site-lisp" "override" "lisp" "lisp/use-package" "lisp/bind-key" "lisp/diminish")))

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t
  :bind ("C-c d k" . describe-personal-keybindings))

;; this only *recompiles* ELisp source files.
(use-package auto-compile
  :disabled t
  :ensure t
  :init
  ;; prefer newer files
  (setq load-prefer-newer t)
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter nil)
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

(use-package paradox
  :ensure t
  :defer 5
  :bind (("C-c d p" . paradox-list-packages)
         ("C-c d u" . paradox-upgrade-packages))
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(use-package hydra
  :defer t
  :ensure t)

(provide 'packages-init)

;;; packages-init.el ends here
