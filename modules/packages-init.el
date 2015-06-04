;;; packages-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup packages. Inspired from http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html

;;; Code:

(require 'package)
(setq package-user-dir (expand-file-name "~/.emacs.d/elpa/")
      package-enable-at-startup nil
      use-package-verbose t)

;; elpa ("gnu" . "http://elpa.gnu.org/packages/") is already preconfigured
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; always referesh contents so that new packages added to the repositories are available
(package-refresh-contents)
;; setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(require 'diminish)
(require 'bind-key)

;; prefer newer files
(setq load-prefer-newer t)

(use-package auto-compile
  :disabled t
  :ensure t
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter nil)
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

(use-package paradox
  :ensure t
  :defer 5
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(use-package hydra
  :ensure t)

(provide 'packages-init)

;;; packages-init.el ends here
