;;; ivy-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup ivy mode as a replacement for ido.

;;; Code:

;; "C-o" in the minibuffer shows a Hydra menu.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-n (ivy-next-line) selects the next candidate                 ;;
;; C-p (ivy-previous-line) selects the previous candidate         ;;
;; M-< (ivy-beginning-of-buffer) selects the first candidate      ;;
;; M-> (ivy-end-of-buffer) selects the last candidate             ;;
;; C-v (ivy-scroll-up-command) scrolls up by ivy-height lines     ;;
;; M-v (ivy-scroll-down-command) scrolls down by ivy-height lines ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; // (self-insert-command) Switch to the root directory. ;;
;; ~ (self-insert-command) Switch to the home directory.  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package ivy
  :ensure swiper
  :if (bound-and-true-p dotemacs-prefer-ivy-over-ido-p)
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t ; When non-nil, add recentf-mode and bookmarks to ivy-switch-buffer completion
                                  ; candidates.
        ivy-virtual-abbreviate 'full
        ivy-wrap t ; Specifies wrap around behavior for "C-n" and "C-p"
        ivy-case-fold-search t
        ivy-height 25 ; Number of lines in the minibuffer window
        ivy-display-style 'fancy
        ivy-extra-directories nil ; Hide "." and ".."
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-count-format "(%d/%d) ")
  :bind
  (("<f12>" . ivy-resume)
   ("C-c C-r" . ivy-recentf)
   ("C-'" . ivy-avy))
  :config
  (use-package counsel
    :ensure t
    :bind
    (([remap describe-function] . counsel-describe-function)
     ;;("C-h f" . counsel-describe-function)
     ([remap describe-variable] . counsel-describe-variable)
     ;;("C-h v" . counsel-describe-variable)
     ([remap execute-extended-command] . counsel-M-x)
     ;; ("M-x" . counsel-M-x)
     ([remap find-file] . counsel-find-file)
     ;;("C-x C-f" . counsel-find-file)
     ("<f3>" . counsel-find-file)
     ("M-y" . counsel-yank-pop))
    :config
    ;; (global-set-key [remap describe-function] #'counsel-describe-function)
    ;; (global-set-key [remap describe-variable] #'counsel-describe-variable)
    ;; (global-set-key [remap execute-extended-command] #'counsel-M-x)
    ;; (global-set-key [remap find-file] #'counsel-find-file)
    (setq counsel-find-file-at-point t))
  :diminish ivy-mode)

(provide 'ivy-init)
;;; ivy-init.el ends here
