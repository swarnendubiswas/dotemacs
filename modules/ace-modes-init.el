;;; ace-modes-init.el --- Part of Emacs initialization   -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup ace-xxx (jump/buffer/isearch) modes, and avy.

;;; Code:

;; prefer helm-mini if helm is enabled
(use-package ace-jump-buffer
  :ensure t
  :if (not (bound-and-true-p dotemacs-use-helm-p))
  :preface
  ;; leave out certain buffers based on file name patterns
  ;; http://scottfrazersblog.blogspot.com/2010/01/emacs-filtered-buffer-switching.html
  (defvar my-bs-always-show-regexps '("\\*\\(scratch\\)\\*")
    "*Buffer regexps to always show when buffer switching.")
  (defvar my-bs-never-show-regexps '("^\\s-" "^\\*" "TAGS$" "GTAGS$")
    "*Buffer regexps to never show when buffer switching.")
  (defvar my-ido-ignore-dired-buffers nil
    "*If non-nil, buffer switching should ignore dired buffers.")

  (defun my-bs-str-in-regexp-list (str regexp-list)
    "Return non-nil if str matches anything in regexp-list."
    (let ((case-fold-search nil))
      (catch 'done
        (dolist (regexp regexp-list)
          (when (string-match regexp str)
            (throw 'done t))))))

  (defun my-bs-ignore-buffer (name)
    "Return non-nil if the named buffer should be ignored."
    (or (and (not (my-bs-str-in-regexp-list name my-bs-always-show-regexps))
             (my-bs-str-in-regexp-list name my-bs-never-show-regexps))
        (and my-ido-ignore-dired-buffers
             (save-excursion
               (set-buffer name)
               (equal major-mode 'dired-mode)))))

  :init
  (use-package ace-jump-mode
    :ensure t
    :bind*
    (("C-c a f" . ace-jump-mode)
     ("C-c a b" . ace-jump-mode-pop-mark))
    :config (ace-jump-mode-enable-mark-sync))

  :config
  (setq bs-configurations
        '(("all" nil nil nil nil nil)
          ("files" nil nil nil (lambda (buf) (my-bs-ignore-buffer (buffer-name buf))) nil)))
  (setq bs-cycle-configuration-name "files")
  (setq-default ajb-bs-configuration "files")
  (bind-key "M-B" 'ace-jump-buffer-with-configuration)

  :bind ("<f5>" . ace-jump-buffer))

(use-package ace-jump-helm-line ; ace-jump in helm buffers
  :ensure t
  :disabled t
  :config
  (setq ace-jump-helm-line-use-avy-style nil) ;; style: avy-jump and ace-jump-mode-style
  (bind-key "C-;" 'ace-jump-helm-line helm-map))

(use-package ace-window
  :ensure t
  :disabled t
  :config (ace-window-display-mode 1))

(use-package avy
  :ensure t
  :bind (("M-b" . avy-goto-word-1)
         ("C-'" . avy-goto-char))
  :config
  ;; It will bind, for example, avy-isearch to C-' in isearch-mode-map, so that you can select one of the currently
  ;; visible isearch candidates using avy.
  (avy-setup-default)
  (setq avy-background t
        avy-highlight-first t
        ;; options: pre, at, at-full, post, de-bruijn
        ;; pre is a bit distracting because of all the movement while highlighting selection keys. This causes 
        ;; the eyes to lose focus.
        avy-style 'at)

  (defhydra hydra-jump-commands (:color blue)
    "Different avy jump commands."
    ("c" avy-goto-char "avy char")
    ("d" avy-goto-char-2 "avy char 2")
    ("w" avy-goto-word-0 "avy word")
    ("u" avy-goto-word-or-subword-1 "avy word or subword")
    ("l" avy-goto-line "avy line")
    ("s" avy-goto-subword-0 "avy subword"))
  (bind-key "M-g" #'hydra-jump-commands/body))

(provide 'ace-modes-init)

;;; ace-modes-init.el ends here
