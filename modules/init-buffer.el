;;; init-buffer.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)

(declare-function sb/inhibit-message-call-orig-fun "init-core.el")

(progn
  (unless (fboundp 'ibuffer)
    (autoload #'ibuffer "ibuffer" nil t))

  (bind-keys :package ibuffer
             ("C-x C-b" . ibuffer))

  (defalias 'list-buffers 'ibuffer)

  (with-eval-after-load "ibuffer"
    (defvar ibuffer-display-summary)
    (defvar ibuffer-default-sorting-mode)
    (defvar ibuffer-use-header-line)

    (setq ibuffer-display-summary nil
          ibuffer-default-sorting-mode 'alphabetic ; Options: `major-mode', `recency'
          ibuffer-use-header-line t)))

(progn
  (declare-function ibuffer-auto-mode "ibuffer-ext")

  (unless (fboundp 'ibuffer-auto-mode)
    (autoload #'ibuffer-auto-mode "ibuf-ext" nil t))

  (add-hook 'ibuffer-hook #'ibuffer-auto-mode)

  (with-eval-after-load "ibuf-ext"
    ;; Do not show filter groups if there are no buffers in that group
    (defvar ibuffer-show-empty-filter-groups)

    (setq ibuffer-show-empty-filter-groups nil)))

(use-package ibuffer-projectile ; Group buffers by Projectile project
  :hook (ibuffer-hook . ibuffer-projectile-set-filter-groups))

(use-package all-the-icons-ibuffer
  :if (display-graphic-p)
  :hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode)
  :custom (all-the-icons-ibuffer-icon-size 0.8))

(use-package counsel-fd
  :if (and (eq sb/minibuffer-completion 'ivy) (executable-find "fd"))
  :bind
  (("C-x d" . counsel-fd-dired-jump) ; Jump to a directory below the current directory
   ;; Jump to a file below the current directory
   ("C-x f" . counsel-fd-file-jump)))

(use-package vlf ; Speed up Emacs for large files: "M-x vlf <PATH-TO-FILE>"
  :commands vlf
  :defines vlf-application
  :init
  (setq vlf-application 'dont-ask)
  (require 'vlf-setup))

(use-package immortal-scratch
  ;; :init (run-with-idle-timer 2 nil #'immortal-scratch-mode)
  :hook (after-init-hook . immortal-scratch-mode))

;; I use the "*scratch*" buffer for taking notes, this package helps to make the data persist
(use-package persistent-scratch
  :hook (after-init-hook . persistent-scratch-setup-default)
  :config
  (advice-add 'persistent-scratch-setup-default :around #'sb/inhibit-message-call-orig-fun))

;; Hooks into to `find-file-hook' to add all visited files and directories to `fasd'
(use-package fasd
  :defines fasd-enable-initial-prompt
  :if (executable-find "fasd")
  ;; :init (run-with-idle-timer 3 nil #'global-fasd-mode)
  :hook (after-init-hook . global-fasd-mode)
  :custom (fasd-enable-initial-prompt nil)
  :bind* ("C-c /" . fasd-find-file))

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
;; https://github.com/dakrone/eos/blob/master/eos-core.org
(use-package popwin
  :hook (after-init-hook . popwin-mode)
  :config
  (defvar popwin:special-display-config-backup popwin:special-display-config)

  (push '("*Help*"              :noselect t)   popwin:special-display-config)
  (push '(compilation-mode      :noselect t)   popwin:special-display-config)
  (push '("*Compile-Log*"       :noselect t)   popwin:special-display-config)
  (push '("*manage-minor-mode*" :noselect t)   popwin:special-display-config)
  (push '("*Paradox Report*"    :noselect t)   popwin:special-display-config)
  (push '("*Selection Ring:")                  popwin:special-display-config)
  (push '("*Flycheck checkers*" :noselect nil) popwin:special-display-config)
  (push '(flycheck-error-list-mode :noselect nil) popwin:special-display-config)
  (push '("*ripgrep-search*"    :noselect nil) popwin:special-display-config)
  (push '("^\*magit:.+\*$"      :noselect nil) popwin:special-display-config)
  (push '("*xref*"              :noselect nil) popwin:special-display-config)
  (push '(helpful-mode          :noselect t)   popwin:special-display-config)
  (push "*Shell Command Output*"               popwin:special-display-config)
  (add-to-list 'popwin:special-display-config '("*Completions*" :stick t :noselect t))
  (add-to-list 'popwin:special-display-config '("*Occur*" :noselect nil))
  (add-to-list 'popwin:special-display-config '("*Backtrace*"))
  (add-to-list 'popwin:special-display-config '("*Apropos*"))
  (add-to-list 'popwin:special-display-config '("*Warnings*"))
  (add-to-list 'popwin:special-display-config '("*prettier errors*"))
  (add-to-list 'popwin:special-display-config '("*explain-pause-top*"))
  (add-to-list 'popwin:special-display-config '(ivy-occur-grep-mode))
  (add-to-list 'popwin:special-display-config '(deadgrep-mode))
  (add-to-list 'popwin:special-display-config '("*lsp session*")))

;; Learn about display actions, see [[info:elisp#Display Action Functions]]
;; https://emacs.stackexchange.com/questions/22499/how-can-i-tell-emacs-to-always-open-help-buffers-in-the-current-window
(add-to-list 'display-buffer-alist '("*Faces*"                  display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Flycheck checkers*"      display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Flycheck errors*"        display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Help*"                   display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Bufler*"                 display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*manage-minor-mode*"      display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*use-package statistics*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*deadgrep*"            display-buffer-same-window))
;; Open shell in same window.
(add-to-list 'display-buffer-alist `(,(regexp-quote "*shell")   display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*Compile-Log\\*"       display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*Warnings\\*"          display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*Backtrace\\*"         display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Async Shell Command*"    display-buffer-no-window))

;; ;; Do not popup the *Async Shell Command* buffer
;; (add-to-list 'display-buffer-alist
;;              (cons "\\*Async Shell Command\\*.*"
;;                    (cons #'display-buffer-no-window nil)))

(provide 'init-buffer)

;;; init-buffer.el ends here
