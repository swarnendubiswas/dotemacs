;;; init-corfu.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/capf)

;; Corfu is not a completion framework, it is a front-end for `completion-at-point'.
(use-package
  corfu
  :straight
  (corfu
    :files (:defaults "extensions/*")
    :includes (corfu-echo corfu-popupinfo corfu-history corfu-info))
  :when (eq sb/capf 'corfu)
  :hook (emacs-startup-hook . global-corfu-mode)
  :bind
  (:map
    corfu-map
    ("ESCAPE" . corfu-quit)
    ([escape] . corfu-quit)
    ("TAB" . corfu-next)
    ([tab] . corfu-next)
    ("S-TAB" . corfu-previous)
    ([backtab] . corfu-previous))
  :custom
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-auto t "Enable auto completion")
  (corfu-auto-delay 0.05 "Recommended to not use zero for performance reasons")
  (corfu-exclude-modes
    '
    (dired-mode
      erc-mode
      message-mode
      comint-mode
      inferior-python-mode
      vterm-mode
      magit-status-mode
      help-mode
      gud-mode
      eshell-mode
      shell-mode
      csv-mode
      minibuffer-inactive-mode))
  :config
  ;; The goal is to use a smaller prefix for programming languages to get faster auto-completion,
  ;; but the popup wraps around with `corfu-terminal-mode' on TUI Emacs. This mostly happens with
  ;; longish completion entries. Hence, a larger prefix can limit to more precise and smaller
  ;; entries.
  (add-hook 'prog-mode-hook (lambda () (setq-local corfu-auto-prefix 2))))

(use-package
  corfu-info
  :straight nil
  :after corfu
  :bind (:map corfu-map ("M-d" . corfu-info-documentation) ("M-l" . corfu-info-location)))

;; (use-package corfu-quick
;;   :straight nil
;;   :after corfu
;;   :bind (:map corfu-map ("C-'" . corfu-quick-insert)))

(use-package
  corfu-quick-access
  :straight (:host codeberg :repo "spike_spiegel/corfu-quick-access.el")
  :when (eq sb/capf 'corfu)
  :hook
  (corfu-mode-hook
    .
    (lambda ()
      (ignore-errors
        (corfu-quick-access-mode)))))

;; We do not need this if we use prescient-based sorting.
(use-package
  corfu-history
  :straight nil
  :when (eq sb/capf 'corfu)
  :hook (corfu-mode-hook . corfu-history-mode)
  :config
  (with-eval-after-load "savehist"
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package
  corfu-echo
  :straight nil
  :when (eq sb/capf 'corfu)
  :hook (corfu-mode-hook . corfu-echo-mode))

(use-package
  corfu-popupinfo
  :straight nil
  :when (eq sb/capf 'corfu)
  :hook (corfu-mode-hook . corfu-popupinfo-mode)
  :bind
  (:map
    corfu-map
    ("M-n" . corfu-popupinfo-scroll-up)
    ("M-p" . corfu-popupinfo-scroll-down)
    ([remap corfu-show-documentation] . corfu-popupinfo-toggle)))

(use-package
  popon
  :straight (:host codeberg :repo "akib/emacs-popon")
  :when (and (eq sb/capf 'corfu) (not (display-graphic-p))))

(use-package
  corfu-terminal
  :straight (:host codeberg :repo "akib/emacs-corfu-terminal")
  :when (and (eq sb/capf 'corfu) (not (display-graphic-p)))
  :hook (corfu-mode-hook . corfu-terminal-mode)
  :custom
  ;; TODO: This is supposedly a bug, report to the maintainer.
  (corfu-terminal-position-right-margin 5 "Prevent wraparound at the right edge"))

(use-package
  kind-icon
  :when (eq sb/corfu-icons 'kind-icon)
  :after corfu
  :demand t
  :commands kind-icon-margin-formatter
  :custom (kind-icon-default-face 'corfu-default "Compute blended backgrounds correctly")
  ;; Prefer smaller icons and a more compact popup
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 0.6))
  ;; (kind-icon-blend-background nil)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

  ;; (when (eq sb/corfu-icons 'nerd-icons)
  ;;   (with-eval-after-load "nerd-icons"
  ;;     (setq kind-icon-use-icons nil)
  ;;     (setq kind-icon-mapping
  ;;       `
  ;;       ((array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
  ;;         (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
  ;;         (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
  ;;         (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
  ;;         (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
  ;;         (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
  ;;         (constructor
  ;;           ,(nerd-icons-codicon "nf-cod-triangle_right")
  ;;           :face font-lock-function-name-face)
  ;;         (enummember
  ;;           ,(nerd-icons-codicon "nf-cod-symbol_enum_member")
  ;;           :face font-lock-builtin-face)
  ;;         (enum-member
  ;;           ,(nerd-icons-codicon "nf-cod-symbol_enum_member")
  ;;           :face font-lock-builtin-face)
  ;;         (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
  ;;         (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
  ;;         (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
  ;;         (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
  ;;         (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
  ;;         (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
  ;;         (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
  ;;         (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
  ;;         (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
  ;;         (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
  ;;         (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
  ;;         (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
  ;;         (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
  ;;         (operator
  ;;           ,(nerd-icons-codicon "nf-cod-symbol_operator")
  ;;           :face font-lock-comment-delimiter-face)
  ;;         (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
  ;;         (property
  ;;           ,(nerd-icons-codicon "nf-cod-symbol_property")
  ;;           :face font-lock-variable-name-face)
  ;;         (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
  ;;         (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
  ;;         (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
  ;;         (struct
  ;;           ,(nerd-icons-codicon "nf-cod-symbol_structure")
  ;;           :face font-lock-variable-name-face)
  ;;         (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
  ;;         (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
  ;;         (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
  ;;         (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
  ;;         (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
  ;;         (variable
  ;;           ,(nerd-icons-codicon "nf-cod-symbol_variable")
  ;;           :face font-lock-variable-name-face)
  ;;         (group ,(nerd-icons-codicon "nf-cod-variable_group") :face font-lock-variable-name-face)
  ;;         (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))))
  )

(use-package
  nerd-icons-corfu
  :straight (:host github :repo "LuigiPiucco/nerd-icons-corfu")
  :when (eq sb/corfu-icons 'nerd-icons)
  :after corfu
  :demand t
  :commands kind-icon-margin-formatter
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; (use-package kind-all-the-icons
;;   :straight (:host github :repo "Hirozy/kind-all-the-icons")
;;   :when (and (eq sb/corfu-icons 'kind-all-the-icons) (display-graphic-p))
;;   :after corfu
;;   :demand t
;;   :config (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter))

;; (use-package
;;   company-auctex
;;   :after tex-mode
;;   :demand t
;;   :commands
;;   (company-auctex-labels
;;     company-auctex-bibs
;;     company-auctex-macros
;;     company-auctex-symbols
;;     company-auctex-environments))

;; ;; Required by `ac-math' and `company-math'
;; (use-package math-symbols :after tex-mode :demand t)

;; (use-package
;;   company-math
;;   :after tex-mode
;;   :demand t
;;   :commands (company-math-symbols-latex company-math-symbols-unicode company-latex-commands))

;; ;; Uses RefTeX to complete label references and citations. When working with multi-file documents,
;; ;; ensure that the variable `TeX-master' is appropriately set in all files, so that RefTeX can find
;; ;; citations across documents.
;; (use-package
;;   company-reftex
;;   :after tex-mode
;;   :demand t
;;   :commands (company-reftex-labels company-reftex-citations)
;;   :custom
;;   ;; https://github.com/TheBB/company-reftex/pull/13
;;   (company-reftex-labels-parse-all nil))

;; (use-package company-bibtex :after tex-mode :demand t :commands company-bibtex)

;; FIXME: Add to capf
(use-package yasnippet-capf :straight (:host github :repo "elken/yasnippet-capf"))

;; Here is a snippet to show how to support `company' backends with `cape'.
;; https://github.com/minad/cape/issues/20
;; (fset #'cape-path (cape-company-to-capf #'company-files))
;; (add-hook 'completion-at-point-functions #'cape-path)

;; `cape-super-capf' works only well for static completion functions like `cape-dabbrev',
;; `cape-keyword', `cape-dict', etc., but not for complex multi-step completions like `cape-file'.
(use-package
  cape
  :after corfu
  :demand t
  :commands
  (cape-history ; Complete from Eshell, Comint, or minibuffer history
    cape-file ; Complete file name at point
    cape-keyword ; Complete programming language keyword
    cape-tex ; Complete unicode char from TeX command, e.g. \hbar.
    cape-abbrev ; Complete abbreviation at point
    cape-dict ; Complete word from dictionary at point
    cape-line ; Complete current line from other lines in buffer
    cape-elisp-symbol ; Elisp symbol
    cape-elisp-block ; Complete Elisp in Org or Markdown code block
    cape-dabbrev ; Complete with Dabbrev at point
    cape-emoji ; Complete emoji in Emacs 29+
    )
  :init
  ;; Initialize for all generic languages that are not specifically handled
  (add-to-list 'completion-at-point-functions #'cape-keyword 'append)
  (add-to-list 'completion-at-point-functions #'cape-file 'append)
  (add-to-list 'completion-at-point-functions (cape-super-capf #'cape-dabbrev #'cape-dict) 'append)
  :custom
  (cape-dabbrev-min-length 3)
  (cape-dict-grep nil "Load the word files in memory for better performance")
  (cape-dict-file
    `
    (,(expand-file-name "wordlist.5" sb/extras-directory)
      ,(expand-file-name "company-dict/text-mode" user-emacs-directory)))
  (cape-dabbrev-check-other-buffers 'some)
  :config
  ;; Override CAPFS for specific major modes
  (dolist (mode '(emacs-lisp-mode-hook lisp-data-mode-hook))
    (add-hook
      mode
      (lambda ()
        (setq-local completion-at-point-functions
          (list
            #'cape-file
            (cape-super-capf
              #'elisp-completion-at-point
              #'citre-completion-at-point
              #'cape-elisp-symbol)
            (cape-super-capf #'cape-dabbrev #'cape-dict))))))

  (add-hook
    'text-mode-hook
    (lambda ()
      (setq-local completion-at-point-functions
        (list #'cape-file (cape-super-capf #'cape-dabbrev #'cape-dict)))))

  ;; TODO: Support latex-mode better.
  (dolist (mode '(latex-mode-hook LaTeX-mode-hook))
    (add-hook
      mode
      (lambda ()
        (when (bound-and-true-p lsp-managed-mode)
          (setq-local completion-at-point-functions
            (list
              #'cape-file
              (cape-super-capf
                (mapcar
                  #'cape-company-to-capf
                  (list
                    #'company-math-symbols-latex
                    #'company-latex-commands
                    #'company-reftex-labels
                    #'company-reftex-citations
                    #'company-auctex-environments
                    #'company-auctex-macros
                    #'company-math-symbols-unicode
                    #'company-auctex-symbols))
                #'cape-tex ; Leads to unwanted completions
                )
              (cape-super-capf #'cape-dabbrev #'cape-dict))))
        (when (bound-and-true-p eglot--managed-mode)
          (setq-local completion-at-point-functions
            (list
              #'cape-file #'eglot-completion-at-point #'cape-tex ; Leads to unwanted completions
              (cape-super-capf #'cape-dabbrev #'cape-dict)))))))

  ;; FIXME: Conditional for both `lsp-mode' and `eglot' is not working.
  (with-eval-after-load "lsp-mode"
    (dolist
      (mode
        '
        (c-mode-hook
          c-ts-mode-hook
          c++-mode-hook
          c++-ts-mode-hook
          java-mode-hook
          java-ts-mode-hook
          python-mode-hook
          python-ts-mode-hook
          sh-mode-hook
          bash-ts-mode-hook
          cmake-mode-hook
          cmake-ts-mode-hook
          json-mode-hook
          json-ts-mode-hook
          jsonc-mode-hook
          yaml-mode-hook
          yaml-ts-mode-hook))
      (add-hook
        mode
        (lambda ()
          (setq-local completion-at-point-functions
            (list
              #'cape-file
              (cape-super-capf #'lsp-completion-at-point #'citre-completion-at-point #'cape-keyword)
              (cape-super-capf #'cape-dabbrev #'cape-dict)))))))

  (with-eval-after-load "eglot"
    (dolist
      (mode
        '
        (c-mode-hook
          c-ts-mode-hook
          c++-mode-hook
          c++-ts-mode-hook
          java-mode-hook
          java-ts-mode-hook
          python-mode-hook
          python-ts-mode-hook
          sh-mode-hook
          bash-ts-mode-hook
          cmake-mode-hook
          cmake-ts-mode-hook
          json-mode-hook
          json-ts-mode-hook
          jsonc-mode-hook
          yaml-mode-hook
          yaml-ts-mode-hook))
      (add-hook
        mode
        (lambda ()
          (setq-local completion-at-point-functions
            (list
              #'cape-file
              (cape-super-capf
                #'eglot-completion-at-point
                #'citre-completion-at-point
                #'cape-keyword)
              (cape-super-capf #'cape-dabbrev #'cape-dict))))))))

(provide 'init-corfu)

;;; init-corfu.el ends here
