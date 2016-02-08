;;; keybindings-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Custom keybindings.  Use M-x describe-personal-keybindings to see modifications.

;;; Code:

;; bind-key*, bind* overrides all minor mode bindings. The kbd macro is not required with bind-key variants. with
;; bind-key, you do not need an explicit "(kbd ...)".
;; Other variants: (global-set-key (kbd "RET") 'newline-and-indent)
;; (define-key global-map (kbd "RET") 'newline-and-indent)

(bind-key "RET" #'newline-and-indent)
(bind-key "C-l" #'goto-line)
(bind-key "C-c z" #'repeat)
(bind-key "C-z" #'undo)

(bind-key "C-c n" #'comment-region)
(bind-key "C-c m" #'uncomment-region)
(bind-key "C-c ;" #'dotemacs-comment-line)
(bind-key* "C-c b" #'comment-box) ; Overrides bib-cite keys.

(bind-key "<f9>" #'other-window) ; switch to the other buffer
(bind-key "<f10>" #'delete-other-windows)

(bind-key "<f11>" #'dotemacs-kill-other-buffers) ; kill all non-special buffers
(unbind-key "C-x C-s") ; save-buffer
(bind-key "C-s" #'save-buffer)
(bind-key "C-S-s" #'dotemacs-save-all-buffers)

(bind-key "C-+" #'text-scale-increase)
(bind-key "C--" #'text-scale-decrease)

(bind-key "C-c d b" #'dotemacs-byte-compile-current-file)
(bind-key "C-c d i" #'dotemacs-byte-compile-init-dir)
(bind-key "C-c d n" #'package-list-packages-no-fetch)

;; the command `key-chord-describe' lists currently defined key chords.
(use-package key-chord
  :ensure t
  :disabled t
  :init
  ;; good choices in English: hj
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "uu" 'undo-tree-visualize)
  (key-chord-define-global "xx" 'smex)
  ;; (key-chord-define c++-mode-map ";;"  "\C-e;")
  ;; (key-chord-define c++-mode-map "{}"  "{\n\n}\C-p\t")
  (key-chord-mode 1))

(use-package keyfreq
  :ensure t
  :disabled t
  :init
  (setq keyfreq-file (concat dotemacs-temp-directory "keyfreq"))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(defhydra hydra-apropos (:color blue)
  "Apropos"
  ("a" apropos "apropos")
  ("c" apropos-command "cmd")
  ("d" apropos-documentation "doc")
  ("e" apropos-value "val")
  ("l" apropos-library "lib")
  ("o" apropos-user-option "opt")
  ("v" apropos-variable "var")
  ("i" info-apropos "info")
  ("t" tags-apropos "tags"))
(bind-key "C-h a" #'hydra-apropos/body)

(use-package guide-key
  :ensure t
  :disabled t
  :diminish guide-key-mode
  :init
  (guide-key-mode 1)
  (setq guide-key/guide-key-sequence t
        guide-key/recursive-key-sequence-flag t
        guide-key/popup-window-position 'bottom
        ;; delay before the guide shows up, default is 1 s
        guide-key/idle-delay 1.0)
  (use-package guide-key-tip
    :ensure t
    :disabled t
    :init (setq guide-key-tip/enabled t)))

(use-package which-key ; Show help popups for prefix keys
  :ensure t
  :init
  (which-key-mode)
  (setq which-key-idle-delay 1.0)
  :diminish which-key-mode)

(provide 'keybindings-init)

;;; keybindings-init.el ends here
