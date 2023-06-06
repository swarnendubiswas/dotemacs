;;; init-buffer.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)

(declare-function sb/inhibit-message-call-orig-fun "init-core.el")

(use-package ibuffer
  :straight (:type built-in)
  :hook (ibuffer-hook . ibuffer-auto-mode)
  :bind ("C-x C-b" . ibuffer-jump)
  :custom
  (ibuffer-display-summary nil)
  (ibuffer-default-sorting-mode 'alphabetic)
  (ibuffer-use-header-line t)
  (ibuffer-show-empty-filter-groups nil "Do not show empty groups if there are no buffers")
  :config (defalias 'list-buffers 'ibuffer))

;; Provides ibuffer filtering and sorting functions to group buffers by function or regexp applied
;; to `default-directory'. By default buffers are grouped by `project-current' or by
;; `default-directory'.

(use-package ibuffer-project
  :after project
  :hook
  (ibuffer-hook
    .
    (lambda ()
      (unless (eq ibuffer-sorting-mode 'project-file-relative)
        (ibuffer-do-sort-by-project-file-relative))))
  :custom
  (ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
  (ibuffer-project-use-cache t "Avoid calculating project root, use cache")
  :config
  ;; Remote buffers will be grouped by protocol and host
  (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote")))

(use-package ibuffer-projectile ; Group buffers by Projectile project
  :after projectile
  :hook (ibuffer-hook . ibuffer-projectile-set-filter-groups))

;; (use-package vlf ; Speed up Emacs for large files: "M-x vlf <PATH-TO-FILE>"
;;   :demand t
;;   :defines vlf-application
;;   :commands vlf
;;   :init
;;   (setq vlf-application 'dont-ask)
;;   (require 'vlf-setup))

;; When the *scratch* buffer is killed, immediately respawn it.
(use-package immortal-scratch
  :hook (emacs-startup-hook . immortal-scratch-mode))

;; Helps to make the data in the "*scratch*" buffer persist.
(use-package persistent-scratch
  :hook (emacs-startup-hook . persistent-scratch-setup-default)
  :config (advice-add 'persistent-scratch-setup-default :around #'sb/inhibit-message-call-orig-fun))

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
;; https://github.com/dakrone/eos/blob/master/eos-core.org

(use-package popwin
  :hook (emacs-startup-hook . popwin-mode)
  :config (defvar popwin:special-display-config-backup popwin:special-display-config)

  ;;   (push '("*Help*"              :noselect t)   popwin:special-display-config)
  ;;   (push '(compilation-mode      :noselect t)   popwin:special-display-config)
  ;;   (push '("*Compile-Log*"       :noselect t)   popwin:special-display-config)
  ;;   (push '("*manage-minor-mode*" :noselect t)   popwin:special-display-config)
  ;;   (push '("*Paradox Report*"    :noselect t)   popwin:special-display-config)
  ;;   (push '("*Selection Ring:")                  popwin:special-display-config)
  ;;   (push '("*Flycheck checkers*" :noselect nil) popwin:special-display-config)
  ;;   (push '(flycheck-error-list-mode :noselect nil) popwin:special-display-config)
  ;;   (push '("*ripgrep-search*"    :noselect nil) popwin:special-display-config)
  ;;   (push '("^\*magit:.+\*$"      :noselect nil) popwin:special-display-config)
  ;;   (push '("*xref*"              :noselect nil) popwin:special-display-config)
  (push '(helpful-mode :noselect t :position bottom :height 20) popwin:special-display-config)
  ;;   (push "*Shell Command Output*"               popwin:special-display-config)
  ;;   (add-to-list 'popwin:special-display-config '("*Completions*" :stick t :noselect t))
  ;;   (add-to-list 'popwin:special-display-config '("*Occur*" :noselect nil))
  ;;   (add-to-list 'popwin:special-display-config '("*Backtrace*"))
  ;;   (add-to-list 'popwin:special-display-config '("*Apropos*"))
  ;;   (add-to-list 'popwin:special-display-config '("*Warnings*"))
  ;;   (add-to-list 'popwin:special-display-config '("*prettier errors*"))
  ;;   (add-to-list 'popwin:special-display-config '("*explain-pause-top*"))
  ;;   (add-to-list 'popwin:special-display-config '(ivy-occur-grep-mode))
  (add-to-list 'popwin:special-display-config '(deadgrep-mode :noselect nil))
  ;;   (add-to-list 'popwin:special-display-config '("*lsp session*"))
  (add-to-list 'popwin:special-display-config '(comint-mode :noselect t)))

;; TODO: Try Shackle
(use-package shackle
  :disabled t
  :init (shackle-mode)
  :config
  (setq shackle-inhibit-window-quit-on-same-windows t)
  (setq shackle-default-alignment 'below)
  (setq shackle-default-size 0.4)
  (setq shackle-rules
    '
    ((helpful-mode :align t :select t)
      (help-mode :align t :select t)
      (compilation-mode :align t :select t)
      (apropos-mode :align t :select t)
      ("^\\*eldoc" :regexp t :align below :select t)
      ("^\\*Occur" :regexp t :align t :select t)))
  (setq shackle-default-rule nil))

;; `ace-window' replaces `other-window' by assigning each window a short, unique label.
(use-package ace-window
  :bind (([remap other-window] . ace-window) ("M-o" . ace-window))
  :config
  (add-to-list 'aw-ignored-buffers "*toc*")
  (ace-window-display-mode 1))

;; The keybinding will be hidden if we use tmux with its default prefix key, and we will need to
;; press twice.
(use-package ace-jump-buffer
  :bind ("C-b" . ace-jump-buffer)
  :custom
  (ajb-bs-configuration "files-and-scratch")
  (ajb-max-window-height 30)
  (ajb-sort-function 'bs--sort-by-filename "Always predictable"))

;; Save buffers when Emacs loses focus. This causes additional saves which triggers the
;; `before-save-hook' and `after-save-hook' and leads to auto-formatters being invoked more
;; frequently.
(use-package super-save
  :defines (super-save-remote-files super-save-triggers super-save-hook-triggers)
  :hook (emacs-startup-hook . super-save-mode)
  :custom (super-save-remote-files nil "Ignore remote files, can cause Emacs to hang")
  :config (add-to-list 'super-save-triggers 'ace-window)
  :diminish)

(provide 'init-buffer)

;;; init-buffer.el ends here
