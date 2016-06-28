;;; keybindings-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Custom keybindings.  Use M-x describe-personal-keybindings to see modifications.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bind-key*, bind* overrides all minor mode bindings. The kbd macro is not required with bind-key variants. With ;;
;; bind-key, you do not need an explicit "(kbd ...)".                                                             ;;
;; Other variants: (global-set-key (kbd "RET") 'newline-and-indent)                                               ;;
;; (define-key global-map (kbd "RET") 'newline-and-indent)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-key "RET" #'newline-and-indent)
(bind-key "C-l" #'goto-line)
(bind-key "C-c z" #'repeat)
(bind-key "C-z" #'undo)

;; In a line with comments, "C-u M-;" removes the comments altogether. That means deleting the comment, NOT UNCOMMENTING
;; but removing all commentted text and the comment marker itself.
(bind-key "C-c n" #'comment-region)
(bind-key "C-c m" #'uncomment-region)
(bind-key* "C-c ;" #'dotemacs-comment-line)
(bind-key* "C-c b" #'comment-box) ; Overrides bib-cite keys

(bind-key "<f10>" #'other-window) ; Switch to the other buffer
(bind-key "<f11>" #'delete-other-windows)

(bind-key "<f12>" #'dotemacs-kill-other-buffers) ; Kill all non-special buffers
(bind-key* "C-s" #'save-buffer)
(bind-key* "C-S-s" #'dotemacs-save-all-buffers)
(unbind-key "C-x s") ; Bound to save-some-buffers
(bind-key "C-x s" #'dotemacs-switch-to-scratch)

(bind-key "C-+" #'text-scale-increase)
(bind-key "C--" #'text-scale-decrease)

(bind-key "C-c d b" #'dotemacs-byte-compile-current-file)
(bind-key "C-c d i" #'dotemacs-byte-compile-init-dir)
(bind-key "C-c d n" #'package-list-packages-no-fetch)

(use-package which-key ; Show help popups for prefix keys
  :ensure t
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 1.0
        which-key-popup-type 'side-window
        which-key-side-window-location '(right bottom) ; Try to use the right, switch to use the bottom if there is no space
        which-key-use-C-h-commands t)
  (which-key-setup-side-window-right-bottom)
  :diminish which-key-mode)

(provide 'keybindings-init)

;;; keybindings-init.el ends here
