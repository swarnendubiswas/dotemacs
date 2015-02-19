;;; keybindings-init.el --- Part of emacs initialization

;;; Commentary:
;; Keybindings

;;; Code:

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-c z") 'repeat)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-\\") 'goto-last-change) ; goto-last-change

(global-set-key [f1] 'shell)

(global-set-key [f2] 'split-window-vertically)
(global-set-key [f3] 'split-window-horizontally)
(global-set-key [f4] 'delete-other-windows)
(global-set-key [f7] 'other-window) ; switch to the other buffer

(global-set-key [f6] 'nav-toggle) ; set up a quick key to toggle nav

(global-set-key (kbd "M-/") 'hippie-expand)

(global-unset-key (kbd "C-s")) ; isearch-forward-regexp
(global-set-key (kbd "C-f") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(global-unset-key (kbd "C-x C-s")) ; save-buffer
(global-set-key (kbd "C-s") 'save-buffer)

(global-set-key (kbd "C-c n") 'comment-region)
(global-set-key (kbd "C-c m") 'uncomment-region)
(global-set-key (kbd "C-c ;") #'comment-line)

;; setting up writegood-mode, identify weasel words, passive voice, and duplicate words
(global-set-key (kbd "C-c g") 'writegood-mode)

;; define a keyboard shortcut for duplicating lines
(global-set-key (kbd "C-c C-d") 'duplicate-thing)

;; buffers
(global-set-key (kbd "C-c k") 'kill-other-buffers) ; kill all non-special buffers
(global-set-key (kbd "C-x C-b") 'ibuffer) ; use ibuffer for buffer list
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)

(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
;;(global-set-key (kbd "M-b") 'ace-jump-buffer-with-configuration)
(global-set-key (kbd "M-b") 'ace-jump-buffer)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; dired
(global-set-key (kbd "C-x C-j") #'dired-jump)
(define-key dired-mode-map (kbd "i") 'ido-find-file)
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
(define-key dired-mode-map (kbd "M-<up>") 'dired-back-to-top)

;; M-<down> is nicer in dired if it moves to the last file
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))
(define-key dired-mode-map (kbd "M-<down>") 'dired-jump-to-bottom)

;; M-<left>/<right> is overwritten by 'ahs-backward/forward, which is not useful
(when (auto-highlight-symbol-mode)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil)
  )
(add-hook 'org-mode-hook 
          (lambda ()
            (local-set-key (kbd "M-<left>") #'tabbar-backward-tab)
            (local-set-key (kbd "M-<right>") #'tabbar-forward-tab)
            ))
(global-set-key (kbd "M-<left>") 'tabbar-backward-tab)
(global-set-key (kbd "M-<right>") 'tabbar-forward-tab)

;; up and down keys to navigate options, left and right to move through history/directories
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(global-set-key (kbd "C-h C-m") 'discover-my-major)


(provide 'keybindings-init)

;;; keybindings-init.el ends here
