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

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :straight t
  :commands ws-butler-mode
  :diminish
  :hook (prog-mode-hook . ws-butler-mode))

(use-package highlight-escape-sequences
  :straight t
  :commands hes-mode
  :hook (prog-mode-hook . hes-mode))

(defvar tags-revert-without-query)

(setq large-file-warning-threshold (* 500 1024 1024) ; MB
      tags-add-tables nil
      tags-case-fold-search nil ; t=case-insensitive, nil=case-sensitive
      ;; Do not ask before rereading the `TAGS' files if they have changed
      tags-revert-without-query t)

(use-package xref
  :straight t
  :commands xref-etags-mode
  :bind
  (("M-'"   . xref-find-definitions)
   ("M-?"   . xref-find-references)
   ("C-M-." . xref-find-apropos)
   ("M-,"   . xref-pop-marker-stack)
   :map xref--xref-buffer-mode-map
   ("C-o"   . xref-show-location-at-point)
   ("<tab>" . xref-quit-and-goto-xref)
   ("r"     . xref-query-replace-in-results)))

(use-package ini-mode
  :straight nil
  :commands ini-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-unix-mode))

(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlight and allow to open http links in strings and comments in programming
            ;; buffers.
            (goto-address-prog-mode 1)
            ;; Native from Emacs 27+, disable in TUI since the line characters also get copied.
            (when (display-graphic-p)
              (display-fill-column-indicator-mode 1))))

(use-package elisp-mode
  :straight nil
  :mode
  (("\\.el\\'"  . emacs-lisp-mode)
   ("\\.elc\\'" . elisp-byte-code-mode))
  :hook
  ((lisp-mode emacs-lisp-mode) .
   (lambda ()
     (when buffer-file-name
       (add-hook 'after-save-hook #'check-parens nil t)
       (flycheck-add-next-checker 'emacs-lisp 'emacs-lisp-checkdoc 'append)))))

(use-package yaml-mode
  :straight t
  :defines lsp-ltex-enabled lsp-disabled-clients
  :commands yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'" ".clang-format" ".clang-tidy")
  :hook
  (yaml-mode-hook .
                  (lambda ()
                    ;; `yaml-mode' is derived from `text-mode', so disable grammar and spell
                    ;; checking.
                    (make-local-variable 'lsp-disabled-clients)
                    (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                    (spell-fu-mode -1)
                    (flyspell-mode -1)
                    (lsp-deferred)))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("yaml-language-server" "--stdio"))
    :major-modes '(yaml-mode)
    :remote? t
    :server-id 'yamlls-r)))

(use-package yaml-imenu
  :straight t
  :after yaml-mode
  :demand t
  :config (yaml-imenu-enable))

(use-package css-mode
  :straight t
  :commands css-mode
  :defines sb/flycheck-local-checkers
  :hook (css-mode-hook . lsp-deferred)
  :custom
  (css-indent-offset 2)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("css-languageserver" "--stdio"))
    :major-modes '(css-mode)
    :remote? t
    :server-id 'cssls-r)))

(use-package make-mode
  :straight nil
  :mode
  (("\\Makefile\\'"       . makefile-mode)
   ;; Add "makefile.rules" to `makefile-gmake-mode' for Intel Pin
   ("makefile\\.rules\\'" . makefile-gmake-mode))
  :config
  (add-hook 'makefile-mode-hook (lambda()
                                  (setq-local indent-tabs-mode t)))
  (use-package makefile-executor))

;; Align fields with "C-c C-a"
(use-package csv-mode
  :straight t
  :defines lsp-disabled-clients
  :commands csv-mode
  :hook
  (csv-mode . (lambda ()
                (make-local-variable 'lsp-disabled-clients)
                (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                (spell-fu-mode -1)
                (flyspell-mode -1)))
  :custom
  (csv-separators '("," ";" "|" " ")))

(use-package antlr-mode
  :straight nil
  :mode "\\.g4\\'")

(use-package bison-mode
  :straight t
  :mode ("\\.bison\\'"))

(use-package llvm-mode
  :straight nil
  :load-path "extras"
  :commands llvm-mode
  :mode "\\.ll\\'")

(use-package tablegen-mode
  :straight nil
  :load-path "extras"
  :commands tablegen-mode
  :disabled t
  :mode "\\.td\\'")

;; (use-package autodisass-llvm-bitcode
;;   :straight t
;;   :commands autodisass-llvm-bitcode
;;   :mode "\\.bc\\'")

;; Enable live preview with "C-c C-c l" (`markdown-live-preview-mode'). The following page lists
;; more shortcuts.
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode markdown-insert-bold
                           markdown-insert-italic
                           markdown-insert-blockquote
                           markdown-insert-pre
                           markdown-insert-code markdown-move-up
                           markdown-insert-link
                           markdown-insert-wiki-link
                           markdown-demote
                           markdown-move-down
                           markdown-insert-header-dwim
                           markdown-insert-reference-link-dwim
                           markdown-insert-header-atx-1
                           markdown-insert-header-atx-2
                           markdown-insert-header-atx-3
                           markdown-insert-header-atx-4
                           markdown-promote
                           markdown-insert-list-item
                           markdown-insert-uri
                           markdown-insert-footnote)
  :mode
  ;; The order is important to associate "README.md" with `gfm-mode'
  (("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  ;; :init
  ;; Looks good, but hiding markup makes it difficult to be consistent while editing
  ;; (setq-default markdown-hide-markup t)
  :custom
  (markdown-command
   "pandoc -f markdown -s --mathjax --standalone --quiet --highlight-style=pygments")
  (markdown-enable-math t "Syntax highlight for LaTeX fragments")
  (markdown-enable-wiki-links t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-indent-on-enter 'indent-and-new-item)
  (markdown-list-indent-width 2)
  (markdown-split-window-direction 'horizontal)
  ;; (markdown-make-gfm-checkboxes-buttons nil)
  (markdown-hide-urls t)
  :bind
  (:map markdown-mode-map
        ("C-c C-j" . nil)))

;; ;; Generate TOC with `markdown-toc-generate-toc'
;; (use-package markdown-toc
;;   :straight t
;;   :after markdown-mode
;;   :commands (markdown-toc-refresh-toc markdown-toc-generate-toc
;;                                       markdown-toc-generate-or-refresh-toc))

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf
;; Convert `markdown' to `org': "pandoc -f markdown -t org -o output-file.org input-file.md"
(use-package pandoc-mode
  :straight t
  :commands (pandoc-load-default-settings pandoc-mode)
  :diminish
  :hook (markdown-mode-hook . pandoc-mode)
  :config (pandoc-load-default-settings))

;; Open preview of markdown file in a browser
(use-package markdown-preview-mode
  :straight t
  :disabled t
  :commands markdown-preview-mode)

(provide 'init-languages)
