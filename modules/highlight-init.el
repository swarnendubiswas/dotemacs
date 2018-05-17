;;; highlight-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure sentence, line, word highlighting.

;;; Code:

(defvar dotemacs-theme)

;; This is useful, but does not work well with in the terminal mode. Checking for (display-graphics-p) and using hooks
;; do not seem to help. Furthermore, this is a performance bottleneck for large files.
(use-package hl-line
  :ensure t
  :disabled t
  :config
  (setq hl-line-sticky-flag nil)
  (global-hl-line-mode 1)
  (unless (eq dotemacs-theme 'solarized-dark)
    (set-face-attribute 'hl-line nil
                        :background "old lace")))

;; (use-package hl-line+ ; Highlight only when idle
;;   :ensure t
;;   :after hl-line
;;   :config
;;   (global-hl-line-mode -1)
;;   (toggle-hl-line-when-idle 1))

(use-package highlight-numbers
  :ensure t
  :disabled t
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-symbol ; Highlight symbol under point
  :ensure t
  :preface
  ;; http://www.wilfred.me.uk/.emacs.d/init.html
  (defun sb/highlight-symbol-first ()
    "Jump to the first location of symbol at point."
    (interactive)
    (push-mark)
    (eval
     `(progn
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (search-forward-regexp
           (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
           nil t))
        (beginning-of-thing 'symbol))))

  (defun sb/highlight-symbol-last ()
    "Jump to the last location of symbol at point."
    (interactive)
    (push-mark)
    (eval
     `(progn
        (goto-char (point-max))
        (let ((case-fold-search nil))
          (search-backward-regexp
           (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
           nil t)))))
  ;; :init
  ;; (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  ;; ;; Navigate occurrences of the symbol under point with M-n and M-p
  ;; (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
  :config
  (setq highlight-symbol-idle-delay 0.1
        highlight-symbol-on-navigation-p t
        highlight-symbol-highlight-single-occurrence t)
  :diminish highlight-symbol-mode)

(use-package fic-mode ; Highlight certain words
  :ensure t
  :commands fic-mode
  :diminish fic-mode
  :hook ((text-mode prog-mode nxml-mode) . fic-mode)
  :config
  (add-to-list 'fic-highlighted-words '"XXX")
  (add-to-list 'fic-highlighted-words '"LATER")
  (add-to-list 'fic-highlighted-words '"IMP")
  (add-to-list 'fic-highlighted-words '"NOTE")
  (add-to-list 'fic-highlighted-words '"NOTES"))

(use-package beacon ; Highlight cursor position in buffer after scrolling
  :ensure t
  :init (beacon-mode)
  :diminish beacon-mode)

(provide 'highlight-init)

;;; highlight-init.el ends here
