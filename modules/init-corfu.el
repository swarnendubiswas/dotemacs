;;; init-corfu.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/capf)

;; Corfu is not a completion framework, it is just a front-end for `completion-at-point'.
(use-package corfu
  :preface
  (defun sb/corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))

  (defun sb/corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))
  :straight
  (corfu
    :files (:defaults "extensions/*")
    :includes (corfu-quick corfu-echo corfu-indexed corfu-popupinfo corfu-history))
  :if (eq sb/capf 'corfu)
  :hook (emacs-startup-hook . global-corfu-mode)
  :bind
  (:map
    corfu-map
    ([remap move-beginning-of-line] . sb/corfu-beginning-of-prompt)
    ([remap move-end-of-line] . sb/corfu-end-of-prompt)
    ([escape] . corfu-quit))
  :custom
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-auto t "Enable auto completion")
  (corfu-auto-delay 0.1 "Recommended to not use zero for performance reasons")
  (corfu-bar-width 0 "See if this helps with corfu-terminal wrap around")
  :config
  ;; The goal is to use a smaller prefix for programming languages to get faster auto-completion,
  ;; but the popup wraps around with `corfu-terminal-mode' on TUI Emacs. This mostly happens with
  ;; longish completion entries. Hence, a larger prefix can limit to more precise and smaller
  ;; entries.
  (add-hook 'prog-mode-hook (lambda () (setq-local corfu-auto-prefix 2))))

(use-package corfu-info
  :straight nil
  :after corfu
  :bind (:map corfu-map ("M-d" . corfu-info-documentation) ("M-l" . corfu-info-location)))

(use-package corfu-quick
  :straight nil
  :after corfu
  :bind (:map corfu-map ("C-'" . corfu-quick-insert)))

;; We do not need this if we use prescient-based sorting.
(use-package corfu-history
  :straight nil
  :after (corfu savehist)
  :commands corfu-history-mode
  :init
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (corfu-history-mode 1))

(use-package corfu-echo
  :straight nil
  :after corfu
  :commands corfu-echo-mode
  :init (corfu-echo-mode 1))

(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :hook (corfu-mode-hook . corfu-popupinfo-mode)
  :bind
  (:map
    corfu-map
    ("M-n" . corfu-popupinfo-scroll-up)
    ("M-p" . corfu-popupinfo-scroll-down)
    ([remap corfu-show-documentation] . corfu-popupinfo-toggle)))

(use-package popon
  :straight (:host codeberg :repo "akib/emacs-popon")
  :if (and (eq sb/capf 'corfu) (not (display-graphic-p))))

(use-package corfu-terminal
  :straight (:host codeberg :repo "akib/emacs-corfu-terminal")
  :if (and (eq sb/capf 'corfu) (not (display-graphic-p)))
  :hook (corfu-mode-hook . corfu-terminal-mode)
  :custom (corfu-terminal-position-right-margin 10))

(use-package kind-icon
  :if (eq sb/corfu-icons 'kind-icon)
  :after corfu
  :demand t
  :commands kind-icon-margin-formatter
  :custom
  (kind-icon-default-face 'corfu-default) ; To compute blended backgrounds correctly
  ;; Prefer smaller icons and a more compact popup
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 0.6))
  (kind-icon-blend-background nil)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

  (when (eq sb/icons-provider 'nerd-icons)
    (setq kind-icon-use-icons nil)
    (setq kind-icon-mapping
      `
      ((array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
        (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
        (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
        (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
        (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
        (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
        (constructor
          ,(nerd-icons-codicon "nf-cod-triangle_right")
          :face font-lock-function-name-face)
        (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
        (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
        (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
        (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
        (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
        (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
        (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
        (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
        (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
        (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
        (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
        (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
        (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
        (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
        (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
        (operator
          ,(nerd-icons-codicon "nf-cod-symbol_operator")
          :face font-lock-comment-delimiter-face)
        (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
        (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
        (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
        (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
        (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
        (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
        (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
        (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
        (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
        (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
        (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
        (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
        (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))))

(use-package kind-all-the-icons
  :straight (:host github :repo "Hirozy/kind-all-the-icons")
  :if (and (eq sb/corfu-icons 'kind-all-the-icons) (display-graphic-p))
  :after corfu
  :demand t
  :config (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter))

(use-package corfu-quick-access
  :straight (:host codeberg :repo "spike_spiegel/corfu-quick-access.el")
  :after corfu
  :commands corfu-quick-access-mode
  :init (corfu-quick-access-mode 1))

;; Here is a snippet to show how to support `company' backends with `cape'.
;; https://github.com/minad/cape/issues/20
;; (fset #'cape-path (cape-company-to-capf #'company-files))
;; (add-hook 'completion-at-point-functions #'cape-path)

(use-package cape
  :after corfu
  :demand t
  :commands
  (cape-history ; Complete from Eshell, Comint or minibuffer history
    cape-file
    cape-keyword ; Complete programming language keyword
    cape-tex ; Complete unicode char from TeX command, e.g. \hbar.
    cape-abbrev ; Complete abbreviation at point
    cape-dict ; Complete word from dictionary at point
    cape-line ; Complete current line from other lines in buffer
    cape-symbol ; Elisp symbol
    cape-elisp-block ; Complete Elisp in Org or Markdown code block
    cape-ispell ; Complete word at point with Ispell
    cape-dabbrev ; Complete with Dabbrev at point
    cape-capf-buster
    cape-company-to-capf
    cape-super-capf
    sh-completion-at-point-function
    comint-completion-at-point
    citre-completion-at-point
    TeX--completion-at-point
    completion-at-point
    complete-tag)
  :init
  ;; Initialize for all generic languages that are not specifically handled
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
  ;; https://github.com/minad/cape/issues/53
  ;; Override CAPFS for specific major modes

  (add-hook
    'emacs-lisp-mode-hook
    (lambda ()
      (setq-local completion-at-point-functions
        (list
          (cape-super-capf #'elisp-completion-at-point #'citre-completion-at-point #'cape-symbol)
          #'cape-file
          #'cape-dabbrev
          #'cape-dict))))

  (dolist (modes '(latex-mode-hook LaTeX-mode-hook))
    (add-hook
      modes
      (lambda ()
        (when (bound-and-true-p lsp-managed-mode)
          (setq-local completion-at-point-functions
            (list
              (cape-super-capf
                #'lsp-completion-at-point #'citre-completion-at-point
                #'cape-tex ; Leads to unwanted completions
                )
              #'cape-file (cape-super-capf #'cape-dabbrev #'cape-dict))))
        (when (bound-and-true-p eglot--managed-mode)
          (setq-local completion-at-point-functions
            (list
              (cape-super-capf
                #'eglot-completion-at-point #'citre-completion-at-point
                #'cape-tex ; Leads to unwanted completions
                )
              #'cape-file (cape-super-capf #'cape-dabbrev #'cape-dict)))))))

  (dolist (lsp-prog-modes '(c-mode-hook c++-mode-hook java-mode-hook python-mode-hook sh-mode-hook))
    (add-hook
      lsp-prog-modes
      (lambda ()
        (setq-local completion-at-point-functions
          (append
            completion-at-point-functions
            '(cape-keyword cape-file (cape-super-capf cape-dabbrev cape-dict))))
        ;; (progn
        ;;   (add-to-list 'completion-at-point-functions #'cape-keyword 'append)
        ;;   (add-to-list 'completion-at-point-functions #'cape-file 'append)
        ;;   (add-to-list 'completion-at-point-functions (cape-super-capf #'cape-dabbrev #'cape-dict)
        ;;     'append))
        ))))

(provide 'init-corfu)

;;; init-corfu.el ends here
