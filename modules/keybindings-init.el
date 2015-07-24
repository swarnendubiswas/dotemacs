;;; keybindings-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Custom keybindings. Use M-x describe-personal-keybindings to see modifications.

;;; Code:

;; bind-key*, bind* overrides all minor mode bindings. The kbd macro is not required with bind-key variants.
;; Other variants:
;; (global-set-key (kbd "RET") 'newline-and-indent)
;; (define-key global-map (kbd "RET") 'newline-and-indent)

(bind-key "RET" 'newline-and-indent)
(bind-key "C-l" 'goto-line)
(bind-key "C-c z" 'repeat)
(bind-key "C-z" 'undo)

;; Not useful
;; (global-set-key [f1] 'shell)
;; (global-set-key [f2] 'split-window-vertically)
;; (global-set-key [f3] 'split-window-horizontally)

(global-unset-key (kbd "C-s")) ; isearch-forward-regexp
(bind-key "C-f" 'isearch-forward-regexp)
(bind-key "C-f" 'isearch-repeat-forward isearch-mode-map)

(bind-key "C-c n" #'comment-region)
(bind-key "C-c m" #'uncomment-region)
(bind-key "C-c ;" #'dotemacs-comment-line)

(bind-key "<f9>" 'other-window) ; switch to the other buffer
(bind-key "<f10>" 'delete-other-windows)

(bind-key "<f11>" #'dotemacs-kill-other-buffers) ; kill all non-special buffers
(global-unset-key (kbd "C-x C-s")) ; save-buffer
(bind-key "C-s" 'save-buffer)
(bind-key "C-S-s" #'dotemacs-save-all-buffers)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-<left>") #'tabbar-backward-tab)
            (local-set-key (kbd "M-<right>") #'tabbar-forward-tab)))
(bind-key* "M-<left>" 'tabbar-backward-tab)
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
  :init
  ;; good choices in English: hj
  (key-chord-define-global "jj" 'avy-goto-word-1)
  (key-chord-define-global "uu" 'undo-tree-visualize)
  (key-chord-define-global "xx" 'smex)
  ;; (key-chord-define c++-mode-map ";;"  "\C-e;")
  ;; (key-chord-define c++-mode-map "{}"  "{\n\n}\C-p\t")
  (key-chord-mode 1))

(use-package keyfreq
  :disabled t
  :ensure t
  :init
  (setq keyfreq-file (concat dotemacs-temp-directory "keyfreq"))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(defhydra hydra-mark-lines ()
  "Mark lines"
  ("m" next-line "next line")
  ("n" next-line "next line")
  ("p" previous-line "previous line"))

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
  ("t" tags-apropos "tags")
  ("z" hydra-customize-apropos/body "customize"))
(bind-key "C-h a" #'hydra-apropos/body)

(defhydra hydra-info (:color blue)
  "Info"
  ("e" (funcall (info-display-topic "emacs")) "Emacs")
  ("l" (funcall (info-display-topic "elisp")) "Elisp")
  ("m" (funcall (info-display-topic "magit")) "Magit")
  ("o" (funcall (info-display-topic "org")) "Org Mode")
  ("s" (funcall (info-display-topic "sicp")) "SICP"))
(bind-key "C-h i" #'hydra-info/body)

(defhydra hydra-zoom (global-map "C-c d z")
  "zoom commands"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(provide 'keybindings-init)

;;; keybindings-init.el ends here
