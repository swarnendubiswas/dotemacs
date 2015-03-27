;;; keybindings-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Keybindings

;;; Code:

;;(global-set-key (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "RET") 'newline-and-indent)
;;(global-set-key (kbd "C-l") 'goto-line)
(define-key global-map (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-c z") 'repeat)
(global-set-key (kbd "C-z") 'undo)

(global-set-key [f1] 'shell)

(global-set-key [f2] 'split-window-vertically)
(global-set-key [f3] 'split-window-horizontally)
(global-set-key [f4] 'delete-other-windows)
(global-set-key [f5] 'other-window) ; switch to the other buffer

(global-set-key (kbd "M-/") 'hippie-expand)

(global-unset-key (kbd "C-s")) ; isearch-forward-regexp
(global-set-key (kbd "C-f") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(global-unset-key (kbd "C-x C-s")) ; save-buffer
(global-set-key (kbd "C-s") 'save-buffer)

(global-set-key (kbd "C-c n") #'comment-region)
(global-set-key (kbd "C-c m") #'uncomment-region)
(global-set-key (kbd "C-c ;") #'comment-line)

;; buffers
(global-set-key (kbd "C-c k") 'kill-other-buffers) ; kill all non-special buffers

;; dired
;; jump to home directory
(global-set-key (kbd "M-<home>")
                (lambda () 
                  (interactive)
                  (dired "~/")))
;; M-<up> is nicer in dired if it moves to the fourth line - the first file
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

;; M-<down> is nicer in dired if it moves to the last file
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "i") 'ido-find-file))
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "M-<up>") 'dired-back-to-top))
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "M-<down>") 'dired-jump-to-bottom))

;; M-<left>/<right> is overwritten by 'ahs-backward/forward, which is not useful
(when (auto-highlight-symbol-mode)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil))
(add-hook 'org-mode-hook 
          (lambda ()
            (local-set-key (kbd "M-<left>") #'tabbar-backward-tab)
            (local-set-key (kbd "M-<right>") #'tabbar-forward-tab)))
(global-set-key (kbd "M-<left>") 'tabbar-backward-tab)
(global-set-key (kbd "M-<right>") 'tabbar-forward-tab)

(provide 'keybindings-init)

;;; keybindings-init.el ends here
