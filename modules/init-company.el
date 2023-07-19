;;; init-company.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/capf)

;; Use "M-x company-diag" or the modeline status (without diminish) to see the backend used for the
;; last completion. Try "M-x company-complete-common" when there are no completions. Use "C-M-i" for
;; `complete-symbol' with regex search.

(use-package company
  :if (eq sb/capf 'company)
  :preface
  ;; https://stackoverflow.com/questions/56382840/is-there-a-way-to-automatically-add-a-whitespace-upon-completion-in-company-mode
  (defun sb/company-after-completion-hook (&rest _ignored)
    ;; This would be called with the completion candidate, so you could modify it to insert spaces
    ;; based on the candidate.
    (just-one-space))
  :defines
  (company-dabbrev-downcase
    company-dabbrev-ignore-case
    company-dabbrev-other-buffers
    company-ispell-available
    company-ispell-dictionary
    company-clang-insert-arguments)
  :commands
  (company-abort
    company-files
    company-yasnippet
    company-ispell
    company-dabbrev
    company-capf
    company-dabbrev-code)
  :hook (emacs-startup-hook . global-company-mode)
  :bind
  (("C-M-/" . company-other-backend) ; Invoke the next backend in `company-backends'
    :map
    company-active-map
    ("C-M-/" . company-other-backend)
    ("C-s" . company-search-candidates)
    ("C-M-s" . company-filter-candidates)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("<tab>" . company-complete-common)
    ("TAB" . company-complete-common)
    ([escape] . company-abort)
    ("M-." . company-show-location)
    ("C-h" . company-show-doc-buffer)
    :map
    company-search-map
    ("C-s" . company-search-repeat-forward)
    ("C-r" . company-search-repeat-backward)
    ("C-g" . company-search-abort)
    ("DEL" . company-search-delete-char))
  :custom
  (company-idle-delay 0.05 "Start autocompletion faster")
  (company-dabbrev-other-buffers t "Search in other buffers with the same major mode")
  ;; (company-dabbrev-ignore-case t "Ignore case when *collecting* completion candidates")
  ;; (company-dabbrev-downcase nil "Do not downcase returned candidates")
  (company-ispell-dictionary (expand-file-name "wordlist.5" sb/extras-directory))
  (company-minimum-prefix-length 3 "Small words can be faster to type")
  (company-require-match nil "Allow typing input characters that do not match candidates")
  (company-show-quick-access t "Speed up completion")
  ;; Align additional metadata, like type signatures, to the right-hand side if non-nil.
  (company-tooltip-align-annotations nil)
  ;; Choices are: "company-pseudo-tooltip-unless-just-one-frontend" shows popup unless there is only
  ;; one candidate, "company-preview-frontend" shows the preview in-place which is too intrusive,
  ;; "company-preview-if-just-one-frontend" shows in-place preview if there is only choice,
  ;; "company-echo-metadata-frontend" shows selected candidate docs in echo area, and
  ;; `company-pseudo-tooltip-frontend' which always shows the candidates in an overlay.
  (company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
  (company-global-modes
    '
    (not dired-mode
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
  (company-format-margin-function nil "Disable icons")
  (company-selection-wrap-around t "Convenient to wrap around completion items at boundaries")
  ;; (company-tooltip-flip-when-above t "Flip the tooltip when it is close to the bottom")
  :config
  ;; Options: `company-sort-prefer-same-case-prefix', `company-sort-by-occurrence',
  ;; `company-sort-by-statistics', `company-sort-by-length', `company-sort-by-backend-importance',
  ;; `delete-dups'.

  ;; Ignore matches that consist solely of numbers from `company-dabbrev'
  ;; https://github.com/company-mode/company-mode/issues/358
  (push
    (apply-partially #'cl-remove-if (lambda (c) (string-match-p "\\`[0-9]+\\'" c)))
    company-transformers)
  (add-to-list 'company-transformers 'delete-dups)
  (add-to-list 'company-transformers 'company-sort-by-backend-importance)
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix))

;; Posframes do not have unaligned rendering issues with variable `:height' unlike an overlay.
;; However, posframes do not work with TUI, and the width of the frame popup is often not enough and
;; the right side gets cut off. https://github.com/company-mode/company-mode/issues/1010

(use-package company-posframe
  :when (display-graphic-p)
  :hook (company-mode . company-posframe-mode)
  :custom
  (company-posframe-show-metadata t "Difficult to distinguish the help text from completions")
  (company-posframe-show-indicator nil "The backend display in the posframe modeline is not great")
  (company-posframe-quickhelp-delay nil "Disable showing the help frame")
  :diminish)

(use-package company-quickhelp
  :after company
  :if (display-graphic-p)
  :hook (prog-mode-hook . company-quickhelp-mode))

(use-package company-quickhelp-terminal
  :after company
  :unless (display-graphic-p)
  :hook (prog-mode-hook . company-quickhelp-terminal-mode))

(use-package company-statistics
  :after company
  :commands company-statistics-mode
  :init (company-statistics-mode 1))

;; We should enable `company-fuzzy-mode' at the very end of configuring `company'. Nice feature but
;; slows completions.

;; (use-package company-fuzzy
;;   :straight flx
;;   :straight t
;;   :after company
;;   :commands (global-company-fuzzy-mode company-fuzzy-mode)
;;   :custom
;;   (company-fuzzy-sorting-backend 'alphabetic) ; Using "flx" slows down completion significantly
;;   ;; (company-fuzzy-passthrough-backends '(company-capf))
;;   (company-fuzzy-show-annotation t "The right-hand side may get cut off")
;;   ;; We should not need this with "flx" sorting because the "flx" sorting accounts for the prefix.
;;   ;; Disabling the requirement may help with performance.
;;   (company-fuzzy-prefix-on-top t))

(use-package company-auctex
  :after (tex-mode company)
  :demand t
  :commands
  (company-auctex-labels
    company-auctex-bibs
    company-auctex-macros
    company-auctex-symbols
    company-auctex-environments))

;; Required by `ac-math' and `company-math'
(use-package math-symbols
  :after (tex-mode company)
  :demand t)

(use-package company-math
  :after (tex-mode company)
  :demand t
  :commands (company-math-symbols-latex company-math-symbols-unicode company-latex-commands))

;; Uses RefTeX to complete label references and citations. When working with multi-file documents,
;; ensure that the variable `TeX-master' is appropriately set in all files, so that RefTeX can find
;; citations across documents.
(use-package company-reftex
  :after (tex-mode company)
  :demand t
  :commands (company-reftex-labels company-reftex-citations)
  :custom
  ;; https://github.com/TheBB/company-reftex/pull/13
  (company-reftex-labels-parse-all nil))

(use-package company-bibtex
  :after (tex-mode company)
  :demand t
  :commands company-bibtex)

(use-package company-anywhere ; Complete in the middle of words
  :straight (:host github :repo "zk-phi/company-anywhere")
  :after company
  :demand t)

(use-package company-dict
  :after company
  :demand t
  :custom
  (company-dict-dir (expand-file-name "company-dict" user-emacs-directory))
  (company-dict-enable-fuzzy t)
  (company-dict-enable-yasnippet nil))

(use-package company-dirfiles ; Better replacement for `company-files'
  :straight (:host codeberg :repo "cwfoo/company-dirfiles")
  :after company
  :demand t)

(use-package company-org-block
  :after company
  :demand t)

(use-package company-c-headers
  :after company
  :demand t
  :config (setq company-c-headers-path-system '("/usr/include/c++/11" "/usr/include" "/usr/local/include")))

;; Try completion backends in order untill there is a non-empty completion list:
;; (setq company-backends '(company-xxx company-yyy company-zzz))

;; Merge completions of all the backends:
;; (setq company-backends '((company-xxx company-yyy company-zzz)))

;; Merge completions of all the backends but keep the candidates organized in accordance with the
;; grouped backends order.
;; (setq company-backends '((company-xxx company-yyy company-zzz :separate)))

;; Another keyword :with helps to make sure the results from major/minor mode agnostic backends
;; (such as company-yasnippet, company-dabbrev-code) are returned without preventing results from
;; context-aware backends (such as company-capf or company-clang). For this feature to work, put
;; backends dependent on a mode at the beginning of the grouped backends list, then put a keyword
;; :with, and only then put context agnostic backend(s).
;; (setq company-backends '((company-capf :with company-yasnippet)))

;; Most backends will not pass control to the following backends (e.g., `company-yasnippet' and
;; `company-tempo'). Only a few backends are specialized on certain major modes or certain contexts
;; (e.g. outside of strings and comments), and pass on control to later backends when outside of
;; that major mode or context.

;; A few backends are applicable to all modes: `company-yasnippet', `company-ispell',
;; `company-dabbrev-code', and `company-dabbrev'. `company-yasnippet' is blocking. `company-dabbrev'
;; returns a non-nil prefix in almost any context (major mode, inside strings or comments). That is
;; why it is better to put `company-dabbrev' at the end. The ‘prefix’ bool command always returns
;; non-nil for following backends even when their ‘candidates’ list command is empty:
;; `company-abbrev', `company-dabbrev', `company-dabbrev-code'.

;; Company does not support grouping of entirely arbitrary backends, they need to be compatible in
;; what `prefix' returns. If the group contains keyword `:with', the backends listed after this
;; keyword are ignored for the purpose of the `prefix' command. If the group contains keyword
;; `:separate', the candidates that come from different backends are sorted separately in the
;; combined list. That is, with `:separate', the multi-backend-adapter will stop sorting and keep
;; the order of completions just like the backends returned them.

(with-eval-after-load "company"
  ;; Override `company-backends' for unhandled major modes.
  (setq
    company-backends
    '
    (company-dirfiles
      (company-capf :with company-dabbrev-code company-yasnippet)
      ;; If we have `company-dabbrev' first, then other matches from `company-ispell' will be
      ;; ignored.
      (company-ispell company-dabbrev company-dict :separate))
    company-transformers
    '
    (delete-dups ; company-sort-by-backend-importance
      ; company-sort-by-occurrence
      company-sort-by-statistics
      company-sort-prefer-same-case-prefix))

  (progn
    (declare-function sb/company-latex-mode "init-company")

    (defun sb/company-latex-mode ()
      "Add backends for `latex-mode' completion in company mode."
      (make-local-variable 'company-backends)

      ;; Example: company-backends: https://github.com/TheBB/company-reftex/issues/10

      ;; `company-reftex' should be considerably more powerful than `company-auctex' backends for
      ;; labels and citations. `company-reftex-labels' is expected to be better than
      ;; `company-auctex-labels'. `company-reftex-citations' is better than `company-bibtex' and
      ;; `company-auctex-bibs'

      (setq company-backends
        '
        (company-dirfiles
          (company-math-symbols-latex ; Math latex tags
            company-latex-commands
            company-reftex-labels
            company-reftex-citations
            company-auctex-environments
            company-auctex-macros
            ;; Math unicode symbols and sub(super)scripts
            company-math-symbols-unicode
            company-auctex-symbols
            company-bibtex
            :separate)
          company-dict company-ispell company-dabbrev company-capf)))

    (dolist (hook '(latex-mode-hook LaTeX-mode-hook))
      (add-hook
        hook
        (lambda ()
          (sb/company-latex-mode)
          ;; `company-capf' does not pass to later backends with Texlab, so we use
          ;; `company-fuzzy-mode' to merge results from all backends.
          ;; (company-fuzzy-mode 1)
          ;; (diminish 'company-fuzzy-mode)
          ))))

  (progn
    (defun sb/company-org-mode ()
      "Add backends for org completion in company mode."
      (set
        (make-local-variable 'company-backends)
        '
        (company-dirfiles
          company-org-block
          (company-ispell company-dabbrev company-dict :separate))))

    (add-hook 'org-mode-hook (lambda () (sb/company-org-mode))))

  (progn
    (declare-function sb/company-text-mode "init-company")

    (defun sb/company-text-mode ()
      "Add backends for `text-mode' completion in company mode."
      (defvar company-backends)
      (set
        (make-local-variable 'company-backends)
        '(company-dirfiles (company-dabbrev company-ispell company-dict :separate))))

    ;; Extends to derived modes like `markdown-mode' and `org-mode'
    (add-hook
      'text-mode-hook
      (lambda ()
        (unless (or (derived-mode-p 'latex-mode) (derived-mode-p 'LaTeX-mode))
          (sb/company-text-mode)

          ;; (setq-local company-after-completion-hook #'sb/company-after-completion-hook)
          ))))

  (progn
    (declare-function sb/company-yaml-mode "init-company")

    (defun sb/company-yaml-mode ()
      "Add backends for `yaml-mode' completion in company mode."
      (defvar company-backends)
      (make-local-variable 'company-backends)
      (setq company-backends
        '
        (company-dirfiles
          (company-capf
            :with
            company-dabbrev-code ; Useful for variable names
            company-yasnippet
            :separate)
          (company-dabbrev company-dict company-ispell :separate))))

    (add-hook 'yaml-mode-hook (lambda () (sb/company-yaml-mode))))

  (progn
    (defun sb/company-prog-mode ()
      "Add backends for `prog-mode' completion in company mode."
      (defvar company-minimum-prefix-length)
      ;; Typing short prefixes help with faster completion and a more responsive UI
      (setq-local company-minimum-prefix-length 2)

      (defvar company-backends)
      (make-local-variable 'company-backends)

      ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names

      (cond
        ((eq sb/lsp-provider 'eglot)
          (setq company-backends
            '
            (company-dirfiles
              (company-capf
                company-c-headers
                :with company-keywords
                company-dabbrev-code ; Useful for variable names
                company-yasnippet
                :separate)
              (company-dabbrev company-dict company-ispell :separate))))
        ((eq sb/lsp-provider 'lsp-mode)
          (setq company-backends
            '
            (company-dirfiles
              (company-capf
                company-citre-tags company-c-headers
                :with company-keywords
                company-dabbrev-code ; Useful for variable names
                company-yasnippet
                :separate)
              (company-dabbrev company-dict company-ispell :separate))))))

    (add-hook
      'prog-mode-hook
      (lambda ()
        (unless
          (or (derived-mode-p 'emacs-lisp-mode)
            (derived-mode-p 'flex-mode)
            (derived-mode-p 'bison-mode))
          (sb/company-prog-mode)))))

  (progn
    (defun sb/company-elisp-mode ()
      "Add backends for `emacs-lisp-mode' completion in company mode."
      (defvar company-minimum-prefix-length)
      ;; Typing short prefixes help with faster completion and a more responsive UI
      (setq-local company-minimum-prefix-length 2)

      (defvar company-backends)
      (make-local-variable 'company-backends)

      (setq company-backends
        '
        (company-dirfiles
          (company-capf
            company-elisp company-citre-tags
            :with company-keywords
            company-dabbrev-code ; Useful for variable names
            company-yasnippet
            :separate)
          (company-dabbrev company-dict company-ispell :separate))))

    (dolist (hook '(emacs-lisp-mode-hook lisp-data-mode-hook))
      (add-hook hook (lambda () (sb/company-elisp-mode))))))

(provide 'init-company)

;;; init-company.el ends here
