(use-package subword
  :straight nil
  :diminish
  :hook (prog-mode-hook . subword-mode))

(use-package outline ; Edit outlines
  :straight t
  :disabled t
  :hook (prog-mode-hook . outline-minor-mode)
  :diminish outline-minor-mode)

;; Hide top-level code blocks. Enable code folding, which is useful for browsing large files. This
;; module is part of Emacs, and is better maintained than other alternatives like `origami'.
(use-package hideshow
  :straight nil
  :disabled t
  :commands (hs-hide-all hs-hide-initial-comment-block hs-show-all hs-show-block)
  :diminish hs-minor-mode
  :hook (prog-mode-hook . hs-minor-mode)
  :custom (hs-isearch-open t))

(use-package dumb-jump
  :straight t
  :after xref
  :demand t
  :commands dumb-jump-xref-activate
  :config
  (setq dumb-jump-quiet t)

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package ivy-xref
  :straight t
  :after (ivy xref)
  :demand t
  :custom
  (xref-show-definitions-function #'ivy-xref-show-defs)
  (xref-show-xrefs-function       #'ivy-xref-show-xrefs))

(use-package counsel-etags
  :straight t
  :defines (counsel-etags-ignore-directories counsel-etags-ignore-filenames)
  :commands counsel-etags-virtual-update-tags
  :if (and (symbol-value 'sb/IS-LINUX) (eq sb/minibuffer-completion 'ivy) (executable-find "ctags"))
  :bind
  (("M-]"     . counsel-etags-find-tag-at-point)
   ("C-c g s" . counsel-etags-find-symbol-at-point)
   ("C-c g f" . counsel-etags-find-tag)
   ("C-c g l" . counsel-etags-list-tag)
   ("C-c g c" . counsel-etags-scan-code))
  :config
  (defalias 'list-tags 'counsel-etags-list-tag-in-current-file)

  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'counsel-etags-virtual-update-tags 'append 'local)))

  (dolist (ignore-dirs '(".vscode" "build" ".metadata" ".recommenders" ".clangd" ".cache"))
    (add-to-list 'counsel-etags-ignore-directories ignore-dirs))

  (dolist (ignore-files '(".clang-format" ".clang-tidy" "*.json" "*.html" "*.xml"))
    (add-to-list 'counsel-etags-ignore-filenames ignore-files)))

;; (use-package highlight-indentation
;;   :straight t
;;   :commands highlight-indentation-mode
;;   :diminish (highlight-indentation-mode highlight-indentation-current-column-mode)
;;   :hook ((yaml-mode-hook python-mode-hook) . highlight-indentation-mode))

;; Claims to be better than `electric-indent-mode'
(use-package aggressive-indent
  :straight t
  :commands aggressive-indent-mode
  :hook (emacs-lisp-mode-hook . aggressive-indent-mode)
  :diminish
  :config
  (setq aggressive-indent-comments-too t
        ;; Never use `electric-indent-mode'
        aggressive-indent-dont-electric-modes t))

;; ;; Highlight symbol under point
;; (use-package symbol-overlay
;;   :straight t
;;   :diminish
;;   :commands (symbol-overlay-mode)
;;   :hook (prog-mode-hook . symbol-overlay-mode)
;;   :bind
;;   (("M-p" . symbol-overlay-jump-prev)
;;    ("M-n" . symbol-overlay-jump-next))
;;   :custom
;;   ;; Delay highlighting to allow for transient cursor placements
;;   (symbol-overlay-idle-time 2))

(provide 'init-languages)
