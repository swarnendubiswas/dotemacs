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

(defvar dotemacs-use-ecb)

(bind-keys
 ("RET" . newline-and-indent)
 ("C-l" . goto-line)
 ("C-c z" . repeat)
 ("C-z" . undo))

;; In a line with comments, "C-u M-;" removes the comments altogether. That means deleting the comment, NOT UNCOMMENTING
;; but removing all commented text and the comment marker itself.
(bind-keys*
 ("C-c n" . comment-region)
 ("C-c m" . uncomment-region)
 ("C-c ;" . sb/comment-line)
 ("C-c b" . comment-box))

(bind-keys
 ("<f10>" . other-window) ; Switch to the other buffer
 ("<f11>" . delete-other-windows)
 ("C-x k" . kill-this-buffer)
 ("<f12>" . sb/kill-other-buffers))

(bind-keys*
 ("C-s" . save-buffer)
 ("C-S-s" . sb/save-all-buffers))
(unbind-key "C-x s") ; Bound to save-some-buffers
(bind-key "C-x s" #'sb/switch-to-scratch)

(bind-keys
 ("C-+" . #'text-scale-increase)
 ("C--" . #'text-scale-decrease))

;; (bind-keys
;;  ("C-c d b" . sb/byte-compile-current-file)
;;  ("C-c d i" . sb/byte-compile-init-dir))

(use-package which-key ; Show help popups for prefix keys
  :ensure t
  :if (and (not (bound-and-true-p dotemacs-use-ecb)) (and (version<= "24.4.0" emacs-version)))
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 1.0
        which-key-popup-type 'side-window
        ;; Try to use the right, switch to use the bottom if there is no space
        which-key-side-window-location '(right bottom)
        which-key-use-C-h-commands t)
  (which-key-setup-side-window-right-bottom)
  :diminish which-key-mode)

(use-package hydra
  :ensure t)

(provide 'keybindings-init)

;;; keybindings-init.el ends here
