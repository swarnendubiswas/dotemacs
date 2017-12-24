;;; tags-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup tags for different projects.

;;; Code:

(defvar dotemacs-selection)
(defvar dotemacs-completion-in-buffer)
(defvar dotemacs-cc-helper)

;; Front end to GNU Global, use `gtags -v -c`.
;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-gtags.el
;; http://tuhdo.github.io/c-ide.html
(use-package ggtags
  :ensure t
  :if (and (eq system-type 'gnu/linux) (or (eq dotemacs-selection 'ido) (eq dotemacs-selection 'none)))
  :diminish ggtags-mode
  :init
  (add-hook 'java-mode-hook #'ggtags-mode)
  (add-hook 'python-mode-hook #'ggtags-mode)
  (when (eq dotemacs-cc-helper 'gtags)
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode)
                  (ggtags-mode 1)))))
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
  (add-hook 'java-mode-hook #'counsel-gtags-mode)
  (add-hook 'python-mode-hook #'counsel-gtags-mode)
  (when (eq dotemacs-cc-helper 'gtags)
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

;; https://vxlabs.com/2016/04/11/step-by-step-guide-to-c-navigation-and-completion-with-emacs-and-the-clang-based-rtags/
;; c-C r [ rtags-location-stack-back Jumps to last visited tag.
;; C-c r ] rtags-location-stack-forward Moves forward in location stack
(use-package rtags
  :ensure t
  :if (eq dotemacs-cc-helper 'rtags)
  :bind (:map c++-mode-map
              ("M-." . rtags-find-symbol-at-point)
              ("M-," . rtags-location-stack-back)
              ("C-c C-j" . rtags-imenu))
  :init
  (when (eq dotemacs-cc-helper 'rtags)
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode)
                  (rtags-mode 1)))))
  :config
  (add-hook 'c-mode-common-hook #'rtags-start-process-unless-running)
  (setq rtags-completions-enabled t
        rtags-find-file-case-insensitive t
        rtags-find-file-prefer-exact-match nil
        rtags-autostart-diagnostics t)

  (when (eq dotemacs-selection 'ivy)
    (setq rtags-display-result-backend 'ivy))

  (use-package ivy-rtags
    :ensure t
    :if (eq dotemacs-selection 'ivy))

  (use-package flycheck-rtags
    :ensure t
    :disabled t
    :preface
    ;; https://github.com/Andersbakken/rtags/blob/7e6b6f21935eedbe4678ba91c5531ac162b51a5a/src/flycheck-rtags.el
    (defun sb/flycheck-rtags-setup ()
      "Configure flycheck-rtags for better experience."
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-check-syntax-automatically nil)
      (setq-local flycheck-highlighting-mode nil)
      (add-hook 'c-mode-hook #'sb/flycheck-rtags-setup)
      (add-hook 'c++-mode-hook #'sb/flycheck-rtags-setup))
    :init (add-hook 'c-mode-common-hook #'sb/flycheck-rtags-setup)))

(use-package company-rtags
  :ensure t
  :if (eq dotemacs-cc-helper 'rtags)
  :after company
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (add-to-list 'company-backends 'company-rtags))))

(use-package ivy-xref
  :ensure t
  :config (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(provide 'tags-init)

;;; tags-init.el ends here
