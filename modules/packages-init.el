;;; packages-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup packages.  Inspired from http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html

;;; Code:

(require 'package)
(setq package-user-dir (expand-file-name "~/.emacs.d/elpa/")
      package-enable-at-startup nil)

;; elpa ("gnu" . "http://elpa.gnu.org/packages/") is already preconfigured
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/") t)

(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup use-package: https://github.com/jwiegley/use-package                                       ;;
;; :init always happens before package load, whether :config has been deferred or not. This implies ;;
;; :init is never deferred.                                                                         ;;
;; :load-path: if the path is relative, it is expanded within user-emacs-directory                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure nil
      use-package-check-before-init t
      use-package-verbose t)

(use-package bind-key
  :ensure t
  :bind ("C-c d k" . describe-personal-keybindings))

(use-package diminish
  :ensure t)

(use-package auto-compile ; This only *recompiles* elisp source files.
  :ensure t
  :disabled t
  :init (auto-compile-on-save-mode 1)
  :config
  (setq load-prefer-newer t
        auto-compile-display-buffer nil
        auto-compile-mode-line-counter nil)
  (auto-compile-on-load-mode 1))

;; Paradox installed from older Emacs versions gets stuck refreshing with >= Emacs 25 because of a *problem with
;; macros*. The solution is to reinstall the package on >= Emacs 25.
;; https://github.com/Malabarba/paradox/issues/102
;; https://github.com/Malabarba/paradox/issues/107
(use-package paradox
  :ensure t
  :bind (("C-c d p" . paradox-list-packages)
         ("C-c d u" . paradox-upgrade-packages)
         ("C-c d P" . package-list-packages))
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t
        paradox-spinner-type 'random)
  (paradox-enable))

(provide 'packages-init)

;;; packages-init.el ends here
