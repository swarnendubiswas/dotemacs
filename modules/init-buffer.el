;;; init-buffer.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)

(declare-function sb/inhibit-message-call-orig-fun "init-core.el")

(use-package ibuffer
  :straight (:type built-in)
  :hook
  (ibuffer-hook . ibuffer-auto-mode)
  :bind
  ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-display-summary nil)
  (ibuffer-default-sorting-mode 'alphabetic) ; Options: `major-mode', `recency'
  (ibuffer-use-header-line t)
  ;; Do not show groups if there are no buffers in that group
  (ibuffer-show-empty-filter-groups nil)
  :config
  (defalias 'list-buffers 'ibuffer))

;; Provides ibuffer filtering and sorting functions to group buffers by function or regexp applied
;; to `default-directory'. By default buffers are grouped by `project-current' or by
;; `default-directory'.
(use-package ibuffer-project
  :if (eq sb/project-handler 'project)
  :hook
  (ibuffer-hook . (lambda ()
                    (unless (eq ibuffer-sorting-mode 'project-file-relative)
                      (ibuffer-do-sort-by-project-file-relative))))
  :custom
  (ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
  (ibuffer-project-use-cache t "Avoid calculating project root, use cache")
  :config
  ;; Remote buffers will be grouped by protocol and host
  (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote")))

(use-package ibuffer-projectile ; Group buffers by Projectile project
  :if (eq sb/project-handler 'projectile)
  :after projectile
  :hook
  (ibuffer-hook . ibuffer-projectile-set-filter-groups))

;; Display icons for all buffers in ibuffer.
(use-package all-the-icons-ibuffer
  :when (display-graphic-p)
  :hook
  (ibuffer-mode-hook . all-the-icons-ibuffer-mode)
  :custom
  (all-the-icons-ibuffer-icon-size 0.8))

(use-package counsel-fd ; Counsel interface for fd
  :when
  (and (eq sb/minibuffer-completion 'ivy) (executable-find "fd"))
  :bind
  (("C-x d" . counsel-fd-dired-jump) ; Jump to a directory below the current directory
   ;; Jump to a file below the current directory
   ("C-x f" . counsel-fd-file-jump)))

;; (use-package vlf ; Speed up Emacs for large files: "M-x vlf <PATH-TO-FILE>"
;;   :demand t
;;   :defines vlf-application
;;   :commands vlf
;;   :init
;;   (setq vlf-application 'dont-ask)
;;   (require 'vlf-setup))

;; When the *scratch* buffer is killed, immediately respawn it.
(use-package immortal-scratch
  :hook
  (after-init-hook . immortal-scratch-mode))

;; I use the "*scratch*" buffer for taking notes, this package helps to make the data persist.
(use-package persistent-scratch
  :hook
  (after-init-hook . persistent-scratch-setup-default)
  :config
  (advice-add 'persistent-scratch-setup-default :around #'sb/inhibit-message-call-orig-fun))

;; ;; Hooks into to `find-file-hook' to add all visited files and directories to `fasd'
;; (use-package fasd
;;   :when (executable-find "fasd")
;;   :defines fasd-enable-initial-prompt
;;   :hook
;;   (after-init-hook . global-fasd-mode)
;;   :bind* ("C-c /" . fasd-find-file)
;;   :custom
;;   (fasd-enable-initial-prompt nil "Narrow easily with Ivy/Vertico"))

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
;; https://github.com/dakrone/eos/blob/master/eos-core.org

(use-package popwin
  :hook
  (after-init-hook . popwin-mode)
  :config
  (defvar popwin:special-display-config-backup popwin:special-display-config)

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
  (push '(helpful-mode :noselect t :position bottom :height 20)
        popwin:special-display-config)
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

;; ;; Learn about display actions, see [[info:elisp#Display Action Functions]]
;; ;; https://emacs.stackexchange.com/questions/22499/how-can-i-tell-emacs-to-always-open-help-buffers-in-the-current-window
;; (add-to-list 'display-buffer-alist '("*Faces*"                  display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("*Flycheck checkers*"      display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("*Flycheck errors*"        display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("*Help*"                   display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("*Bufler*"                 display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("*manage-minor-mode*"      display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("*use-package statistics*" display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("^\\*deadgrep*"            display-buffer-same-window))
;; ;; Open shell in same window.
;; (add-to-list 'display-buffer-alist `(,(regexp-quote "*shell")   display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("^\\*Compile-Log\\*"       display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("^\\*Warnings\\*"          display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("^\\*Backtrace\\*"         display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("*Async Shell Command*"    display-buffer-no-window))

;; `ace-window' replaces `other-window' by assigning each window a short, unique label.
(use-package ace-window
  :bind
  (([remap other-window] . ace-window)
   ("M-o" . ace-window))
  :custom
  (aw-minibuffer-flag t "Display ace-window is active in the minibuffer")
  :config
  (add-to-list 'aw-ignored-buffers "*toc*")
  (ace-window-display-mode 1))

;; The keybinding will be hidden if we use tmux, and we will need to press twice.
(use-package ace-jump-buffer
  :bind
  ("C-b" . ace-jump-buffer)
  :custom
  (ajb-bs-configuration "files-and-scratch")
  (ajb-max-window-height 30)
  (ajb-sort-function 'bs--sort-by-filename "Always predictable"))

;; Save buffers when Emacs loses focus. This causes additional saves which triggers the
;; `before-save-hook' and `after-save-hook' and leads to auto-formatters being invoked more
;; frequently.
(use-package super-save
  :defines
  (super-save-remote-files super-save-triggers super-save-hook-triggers)
  :hook
  (after-init-hook . super-save-mode)
  :custom
  (super-save-remote-files nil "Ignore remote files, can cause Emacs to hang")
  (super-save-triggers '(other-window windmove-up windmove-down
                                      windmove-left windmove-right ace-window))
  :diminish)

(provide 'init-buffer)

;;; init-buffer.el ends here
