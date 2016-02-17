;;; ivy-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup ivy mode as a replacement for ido.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-o (hydra-ivy/body) In the minibuffer shows a Hydra menu.      ;;
;; C-n (ivy-next-line) Selects the next candidate                  ;;
;; C-p (ivy-previous-line) Selects the previous candidate          ;;
;; M-< (ivy-beginning-of-buffer) Selects the first candidate       ;;
;; M-> (ivy-end-of-buffer) Selects the last candidate              ;;
;; C-v (ivy-scroll-up-command) Scrolls up by ivy-height lines      ;;
;; M-v (ivy-scroll-down-command) Scrolls down by ivy-height lines  ;;
;; //  (self-insert-command) Switch to the root directory.         ;;
;; ~   (self-insert-command) Switch to the home directory.         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :ensure swiper
  :if (eq dotemacs-selection 'ivy)
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
   ("<f8>" . ivy-recentf)
   ("C-'" . ivy-avy))
  :config
  (use-package counsel
    :ensure t
    :bind
    (([remap describe-function] . counsel-describe-function)
     ([remap describe-variable] . counsel-describe-variable)
     ([remap execute-extended-command] . counsel-M-x)
     ([remap find-file] . counsel-find-file)
     ("<f3>" . counsel-find-file)
     ([remap yank-pop] . counsel-yank-pop))
    :config (setq counsel-find-file-at-point t))

  (when (eq dotemacs-selection 'ivy)
    (progn
      (bind-key "<f1>" #'counsel-M-x)
      (bind-key [remap switch-to-buffer] #'ivy-switch-buffer)
      (bind-key "<f4>" #'ivy-switch-buffer)
      (bind-key "<f8>" #'ivy-recentf)))
  :diminish ivy-mode)

(provide 'ivy-init)

;;; ivy-init.el ends here
