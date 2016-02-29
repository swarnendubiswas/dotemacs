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
  :preface
  ;; https://github.com/abo-abo/oremacs/blob/github/oleh/modes/ora-ivy.el
  (defun ivy-dired ()
    (interactive)
    (if ivy--directory
        (ivy-quit-and-run
         (dired ivy--directory)
         (when (re-search-forward
                (regexp-quote
                 (substring ivy--current 0 -1)) nil t)
           (goto-char (match-beginning 0))))
      (user-error
       "Not completing files currently")))
  :config
  (setq ivy-use-virtual-buffers t ; When non-nil, add recentf-mode and bookmarks to ivy-switch-buffer completion
                                  ; candidates.
        ivy-virtual-abbreviate 'full
        ivy-wrap t ; Specifies wrap around behavior for "C-n" and "C-p"
        ivy-case-fold-search t
        ivy-height 25 ; Number of lines in the minibuffer window
        ivy-fixed-height-minibuffer t
        ivy-display-style 'fancy
        ivy-extra-directories nil ; Hide "." and ".."
        ;; ivy-count-format "(%d/%d) "
        ivy-re-builders-alist '((t . ivy--regex-plus)))
  (ivy-mode 1)
  (use-package counsel
    :ensure t
    :bind
    (([remap describe-function] . counsel-describe-function)
     ([remap describe-variable] . counsel-describe-variable)
     ([remap execute-extended-command] . counsel-M-x)
     ("<f1>" . counsel-M-x)
     ([remap find-file] . counsel-find-file)
     ("<f3>" . counsel-find-file)
     ([remap yank-pop] . counsel-yank-pop))
    :config
    (setq counsel-find-file-at-point nil
          ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-counsel.el
          counsel-find-file-ignore-regexp (concat
                                           "\\(?:\\`[#.]\\)" ; File names beginning with # or .
                                           ;; File names ending with # or ~
                                           "\\|\\(?:\\`.+?[#~]\\'\\)")))
    :bind
  (("<f7>" . ivy-resume)
   ("<f8>" . ivy-recentf)
   ("C-'" . ivy-avy)
   ([remap switch-to-buffer] . ivy-switch-buffer)
   ("<f4>" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("<return>" . ivy-alt-done)
   ("C-:" . ivy-dired)
   ("<left>" . ivy-previous-line)
   ("<right>" . ivy-next-line))
  :diminish ivy-mode)

(provide 'ivy-init)

;;; ivy-init.el ends here
