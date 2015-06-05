;;; keybindings-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Custom keybindings.

;;; Code:

;; bind-key* overrides all minor mode bindings

;;(global-set-key (kbd "RET") 'newline-and-indent)
;;(define-key global-map (kbd "RET") 'newline-and-indent)
(bind-key "RET" 'newline-and-indent)

;;(global-set-key (kbd "C-l") 'goto-line)
;;(define-key global-map (kbd "C-l") 'goto-line)
(bind-key "C-l" 'goto-line)

;;(global-set-key (kbd "C-c z") 'repeat)
(bind-key "C-c z" 'repeat)

;;(global-set-key (kbd "C-z") 'undo)
(bind-key "C-z" 'undo)

;; SB: Not useful
;;(global-set-key [f1] 'shell)
;;(global-set-key [f2] 'split-window-vertically)
;;(global-set-key [f3] 'split-window-horizontally)
;; switch to the other buffer
(bind-key "<f9>" 'other-window)
;;(global-set-key [f4] 'delete-other-windows)
(bind-key "<f11>" 'delete-other-windows)

;;(global-set-key (kbd "M-/") 'hippie-expand) ;; replace dabbrev-expand
(bind-key "M-/" 'hippie-expand)

(global-unset-key (kbd "C-s")) ; isearch-forward-regexp
;;(global-set-key (kbd "C-f") 'isearch-forward-regexp)
(bind-key "C-f" 'isearch-forward-regexp)
;;(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(bind-key "C-f" 'isearch-repeat-forward isearch-mode-map)

;;(global-set-key (kbd "C-c n") #'comment-region)
(bind-key "C-c n" 'comment-region)
;;(global-set-key (kbd "C-c m") #'uncomment-region)
(bind-key "C-c m" 'uncomment-region)
;;(global-set-key (kbd "C-c ;") #'comment-line)
(bind-key "C-c ;" 'dotemacs--comment-line)

;; buffers
;;(global-set-key (kbd "C-c k") #'kill-other-buffers) ; kill all non-special buffers
(bind-key "<f10>" 'dotemacs--kill-other-buffers)
(global-unset-key (kbd "C-x C-s")) ; save-buffer
;;(global-set-key (kbd "C-s") 'save-buffer)
(bind-key "C-s" 'save-buffer)
(bind-key "C-S-s" 'dotemacs--save-all-buffers)

;; ;; M-<left>/<right> is overwritten by 'ahs-backward/forward, which is not useful
;; (when (auto-highlight-symbol-mode)
;;   (define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
;;   (define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-<left>") #'tabbar-backward-tab)
            (local-set-key (kbd "M-<right>") #'tabbar-forward-tab)))
;;(global-set-key (kbd "M-<left>") 'tabbar-backward-tab)
(bind-key* "M-<left>" 'tabbar-backward-tab)
;;(global-set-key (kbd "M-<right>") 'tabbar-forward-tab)
(bind-key* "M-<right>" 'tabbar-forward-tab)

;; with bind-key, you do not need an explicit "(kbd ...)"
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

;; globally unset M-x
;;(global-unset-key (kbd "M-x"))

;; the command `key-chord-describe' lists currently defined key chords.
(use-package key-chord
  :disabled t
  :ensure t
  :config
  ;; good choices in English: hj
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "uu" 'undo-tree-visualize)
  (key-chord-define-global "xx" 'smex)
  ;; (key-chord-define c++-mode-map ";;"  "\C-e;")
  ;; (key-chord-define c++-mode-map "{}"  "{\n\n}\C-p\t")
  :init (key-chord-mode 1))

(use-package keyfreq
  :disabled t
  :ensure t
  :config
  (setq keyfreq-file (concat dotemacs-temp-directory "keyfreq"))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


(provide 'keybindings-init)

;;; keybindings-init.el ends here
