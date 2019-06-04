;;; tags-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup tags for different projects.

;;; Code:

(defvar dotemacs-cc-tags)

(use-package counsel-gtags
  :ensure t
  :if (eq system-type 'gnu/linux)
  :diminish counsel-gtags-mode
  :commands (counsel-gtags-find-definition
             counsel-gtags-find-reference
             counsel-gtags-find-symbol
             counsel-gtags-find-file
             counsel-gtags-create-tags
             counsel-gtags-update-tags
             counsel-gtags-dwim)
  :init
  (add-hook 'java-mode-hook #'counsel-gtags-mode)
  (add-hook 'python-mode-hook #'counsel-gtags-mode)
  (when (eq dotemacs-cc-tags 'gtags)
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode)
                  (counsel-gtags-mode 1)))))
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

(use-package counsel-etags
  :ensure t
  :bind(:map counsel-etags-mode-map
             ("M-." . counsel-etags-find-tag-at-point)
             ;; ("M-t" . counsel-etags-grep-symbol-at-point)
             ;; ("M-s" . counsel-etags-find-tag)
             )
  :config
  (add-to-list 'counsel-etags-ignore-directories '".vscode")
  (add-to-list 'counsel-etags-ignore-filenames '".clang-format")
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; How many seconds to wait before rerunning tags for auto-update
  (setq counsel-etags-update-interval 180))

(provide 'tags-init)

;;; tags-init.el ends here
