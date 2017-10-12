;;; tags-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup tags for different projects.

;;; Code:

(defvar dotemacs-selection)
(defvar dotemacs-completion-in-buffer)

(setq tags-revert-without-query t
      tags-case-fold-search nil)

;; Exuberant ctags is better than etags: https://www.emacswiki.org/emacs/BuildTags
(setq path-to-ctags "ctags") ;; <- your ctags path here
(defun create-ctags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name))))

(defun dotemacs-create-latex-ctags () ; (dir-name))
  "Create ctags for the current latex project."
  (interactive)
  (compile "find . -type f -name \"*.tex\" -print | xargs ctags -o TAGS"))

(use-package ctags-update
  :ensure t
  :defer t)

;; Front end to GNU Global, use `gtags -v -c`.
;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-gtags.el
;; http://tuhdo.github.io/c-ide.html
(use-package ggtags
  :ensure t
  :if (and (eq system-type 'gnu/linux) (or (eq dotemacs-selection 'ido) (eq dotemacs-selection 'none)))
  :diminish ggtags-mode
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1))))
  (add-hook 'python-mode-hook #'ggtags-mode)
  :config
  (setq ggtags-navigation-mode-lighter nil
        ggtags-oversize-limit (* 50 1024 1024)
        ggtags-completing-read-function nil)
  :bind (:map ggtags-mode-map
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)
              ("M-." . ggtags-find-tag-dwim)
              ("M-," . pop-tag-mark)))

;; http://wikemacs.org/wiki/C-ide
;; http://tuhdo.github.io/c-ide.html
;; Use M-n to move to next candidate and M-p to move back previous candidate. Use "M-g s" to invoke Isearch on candidate
;; buffer list.
(use-package helm-gtags
  :ensure t
  :diminish helm-gtags-mode
  :if (and (eq system-type 'gnu/linux) (eq dotemacs-selection 'helm))
  :config
  (setq helm-gtags-ignore-case nil
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-suggested-key-mapping t
        helm-gtags-fuzzy-match t
        helm-gtags-maximum-candidates 1000
        helm-gtags-cache-select-result t
        helm-gtags-display-style 'detail
        helm-gtags-update-interval-second 60)
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (helm-gtags-mode 1))))
  (add-hook 'python-mode-hook #'helm-gtags-mode)
  :bind (:map helm-gtags-mode-map
              ("M-." . helm-gtags-dwim)
              ("M-," . helm-gtags-pop-stack)
              ("M-'" . helm-gtags-select)
              ("M-t" . helm-gtags-find-tag)))

(use-package counsel-gtags
  :ensure t
  :if (and (eq system-type 'gnu/linux) (eq dotemacs-selection 'ivy))
  :diminish counsel-gtags-mode
  :commands (counsel-gtags-find-definition
             counsel-gtags-find-reference
             counsel-gtags-find-symbol
             counsel-gtags-find-file
             counsel-gtags-create-tags
             counsel-gtags-update-tags
             counsel-gtags-dwim)
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (counsel-gtags-mode 1))))
  (add-hook 'python-mode-hook #'counsel-gtags-mode)
  :config
  (setq counsel-gtags-ignore-case nil
        counsel-gtags-auto-update t)
  :bind (:map counsel-gtags-mode-map
              ("M-." . counsel-gtags-dwim)
              ("M-," . counsel-gtags-go-backward)
              ("C-c g s" . counsel-gtags-find-symbol)
              ("C-c g r" . counsel-gtags-find-reference)
              ("C-c g c" . counsel-gtags-create-tags)
              ("C-c g u" . counsel-gtags-update-tags)))

;; https://vxlabs.com/2016/04/11/step-by-step-guide-to-c-navigation-and-completion-with-emacs-and-the-clang-based-rtags/

(use-package rtags
  :ensure t
  :config
  (setq rtags-completions-enabled t
        rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings)

  (use-package ivy-rtags
    :ensure t)

  (use-package helm-rtags
    :ensure t)

  (use-package company-rtags
    :ensure t
    :after company
    :config (add-to-list 'company-backends 'company-rtags))

  (use-package flycheck-rtags
    :ensure t
    :init (add-hook 'c-mode-common-hook #'setup-flycheck-rtags)))

(provide 'tags-init)

;;; tags-init.el ends here
