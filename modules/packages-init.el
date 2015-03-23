;;; packages-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup packages. Inspired from http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html

;;; Code:

(require 'package)
(setq package-user-dir (expand-file-name "~/.emacs.d/elpa/")
      package-enable-at-startup nil)

;; elpa ("gnu" . "http://elpa.gnu.org/packages/") is already preconfigured
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile 
	(require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package paradox
  :ensure t
  :init
  (setq paradox-execute-asynchronously t
        paradox-github-token t))

(provide 'packages-init)

;;; packages-init.el ends here