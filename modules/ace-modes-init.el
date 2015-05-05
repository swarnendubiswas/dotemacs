;;; ace-modes-init.el --- Part of emacs initialization   -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup ace-xxx (jump/buffer/isearch) modes.

;;; Code:

;; (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(use-package ace-jump-mode
  :ensure t
  :bind
  ;;("C-c SPC" . ace-jump-mode)
  ("C-'" . ace-jump-mode)
  :config
  ;;(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
  (ace-jump-mode-enable-mark-sync))

;; leave out certain buffers based on file name patterns
;; http://scottfrazersblog.blogspot.com/2010/01/emacs-filtered-buffer-switching.html
(use-package ace-jump-buffer
  :ensure t
  :preface
  (defvar my-bs-always-show-regexps '("\\*\\(scratch\\)\\*")
    "*Buffer regexps to always show when buffer switching.")
  (defvar my-bs-never-show-regexps '("^\\s-" "^\\*" "TAGS$")
    "*Buffer regexps to never show when buffer switching.")
  (defvar my-ido-ignore-dired-buffers nil
    "*If non-nil, buffer switching should ignore dired buffers.")
  (defun my-bs-str-in-regexp-list (str regexp-list)
    "Return non-nil if str matches anything in regexp-list."
    (let ((case-fold-search nil))
      (catch 'done
        (dolist (regexp regexp-list)
          (when (string-match regexp str)
            (throw 'done t))))))
  (defun my-bs-ignore-buffer (name)
    "Return non-nil if the named buffer should be ignored."
    (or (and (not (my-bs-str-in-regexp-list name my-bs-always-show-regexps))
             (my-bs-str-in-regexp-list name my-bs-never-show-regexps))
        (and my-ido-ignore-dired-buffers
             (save-excursion
               (set-buffer name)
               (equal major-mode 'dired-mode)))))
  :config
  (setq bs-configurations
        '(("all" nil nil nil nil nil)
          ("files" nil nil nil (lambda (buf) (my-bs-ignore-buffer (buffer-name buf))) nil)))
  (setq bs-cycle-configuration-name "files")
  (setq-default ajb-bs-configuration "files")
  ;;(global-set-key (kbd "M-b") 'ace-jump-buffer-with-configuration)
  ;;(global-set-key (kbd "M-b") 'ace-jump-buffer)
  :bind
  ;;("M-b" . ace-jump-buffer)
  ("<f6>" . ace-jump-buffer))

(use-package ace-jump-helm-line
  :ensure t
  :if (eq dotemacs-helm-or-ido 'helm)
  :config
  ;; style: avy-jump and ace-jump-mode-style
  (setq ace-jump-helm-line-use-avy-style nil)
  (bind-key "C-'" 'ace-jump-helm-line helm-map))

(provide 'ace-modes-init)

;;; ace-modes-init.el ends here
