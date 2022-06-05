;;; init-company.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

;; The module does not specify an `autoload'. So we get the following error without the following
;; declaration.
;; "Company backend ’company-capf’ could not be initialized: Autoloading file failed to define
;; function company-capf"
(use-package company-capf
  :straight company
  :commands company-capf)

;; Use "M-x company-diag" or the modeline status to see the backend used. Try "M-x
;; company-complete-common" when there are no completions. Use "C-M-i" for `complete-symbol' with
;; regex search.
(use-package company
  :if (eq sb/capf 'company)
  :commands (company-abort company-files company-yasnippet
                           company-ispell company-dabbrev
                           company-capf company-dabbrev-code
                           company-clang-set-prefix
                           global-company-mode)
  :defines (company-dabbrev-downcase company-dabbrev-ignore-case
                                     company-dabbrev-other-buffers
                                     company-ispell-available
                                     company-ispell-dictionary
                                     company-clang-insert-arguments)
  :hook
  ((after-init-hook . (lambda ()
                        (when (string= (buffer-name) "*scratch*")
                          (company-mode 1))))
   (after-init-hook . global-company-mode))
  :custom
  (company-dabbrev-downcase nil "Do not downcase returned candidates")
  ;; Do not ignore case when collecting completion candidates. It is recommended to change the
  ;; default value of "keep-prefix" if we modify `company-dabbrev-downcase'.
  (company-dabbrev-ignore-case nil)
  ;; Search in other buffers with the same major mode. This can cause performance overhead if
  ;; there are lots of open buffers.
  (company-dabbrev-other-buffers t)
  (company-ispell-available t)
  (company-ispell-dictionary (expand-file-name "wordlist.5" sb/extras-directory))
  (company-minimum-prefix-length 3 "Small words can be faster to type")
  (company-require-match nil "Allow input string that do not match candidates")
  (company-selection-wrap-around t)
  (company-show-quick-access t "Speed up completion")
  ;; Align additional metadata, like type signatures, to the right-hand side
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 15)
  (company-clang-insert-arguments nil "Disable insertion of arguments")
  ;; Start a search using `company-filter-candidates' (bound to "C-s") to narrow out-of-order
  ;; strings
  ;; https://github.com/company-mode/company-mode/discussions/1211
  (company-search-regexp-function 'company-search-words-in-any-order-regexp)
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       ;; Always show candidates in overlay tooltip
                       ;; company-pseudo-tooltip-frontend
                       ;; company-preview-frontend ; Too instrusive
                       ;; Show in-place preview if there is only choice
                       company-preview-if-just-one-frontend
                       ;; Show selected candidate docs in echo area
                       company-echo-metadata-frontend))
  :config
  ;; We set `company-backends' as a local variable, so it is not important to delete backends
  ;; (dolist (backends '(company-semantic company-bbdb company-oddmuse company-cmake company-clang))
  ;;   (delq backends company-backends))

  ;; Ignore matches that consist solely of numbers from `company-dabbrev'
  ;; https://github.com/company-mode/company-mode/issues/358
  (push (apply-partially #'cl-remove-if
                         (lambda (c)
                           (string-match-p "\\`[0-9]+\\'" c)))
        company-transformers)

  ;; The `company-posframe' completion kind indicator is not great, but we are now using
  ;; `company-fuzzy'.
  (when (display-graphic-p)
    (diminish 'company-mode))
  :bind
  (:map company-active-map
        ("C-s"      . nil) ; Was bound to `company-search-candidates'
        ("C-M-s"    . nil) ; Was bound to `company-filter-candidates'
        ("C-n"      . company-select-next)
        ("C-p"      . company-select-previous)
        ;; Insert the common part of all candidates, or select the next one
        ("<tab>"    . company-complete-common-or-cycle)
        ("C-M-/"    . company-other-backend)
        ("<escape>" . company-abort)))

;; Posframes do not have unaligned rendering issues with variable `:height' unlike an overlay.
;; However, the width of the frame popup is often not enough and the right side gets cut off.
;; https://github.com/company-mode/company-mode/issues/1010
(use-package company-posframe
  :if (display-graphic-p)
  :after company
  :commands company-posframe-mode
  :diminish
  :custom
  (company-posframe-show-metadata nil "Difficult to distinguish the help text from completions")
  (company-posframe-show-indicator t "The display is not great")
  (company-posframe-quickhelp-delay nil "Disable showing the help frame")
  :init
  (company-posframe-mode 1))

(use-package company-quickhelp
  :after company
  :hook (prog-mode-hook . company-quickhelp-mode))

(use-package company-statistics
  :after company
  :commands company-statistics-mode
  :init (company-statistics-mode 1))

;; Nice but slows completions. We should invoke this only at the very end of configuring `company'.
(use-package company-fuzzy
  :after company
  :commands (global-company-fuzzy-mode company-fuzzy-mode)
  :demand t
  :disabled t
  :custom
  (company-fuzzy-sorting-backend 'flx) ; Using "flx" slows down completion significantly
  (company-fuzzy-show-annotation t "The right-hand side may get cut off")
  ;; We should not need this with "flx" sorting because the "flx" sorting accounts for the prefix.
  ;; Disabling the requirement may help with performance.
  (company-fuzzy-prefix-on-top t)
  (company-fuzzy-passthrough-backends '(company-capf)))

(use-package company-shell
  :disabled t
  :after (:any sh-mode fish-mode)
  :demand t
  :defines company-shell-delete-duplictes
  :commands (company-shell company-shell-env company-fish-shell)
  :custom (company-shell-delete-duplictes t))

(use-package company-auctex
  :after tex-mode
  :demand t
  :commands (company-auctex-init company-auctex-labels
                                 company-auctex-bibs company-auctex-macros
                                 company-auctex-symbols company-auctex-environments))

(use-package math-symbols
  :after tex-mode
  :demand t) ; Required by `ac-math' and `company-math'

(use-package company-math
  :after tex-mode
  :demand t
  :commands (company-math-symbols-latex company-math-symbols-unicode company-latex-commands))

;; Uses RefTeX to complete label references and citations
(use-package company-reftex
  :after tex-mode
  :demand t
  :commands (company-reftex-labels company-reftex-citations))

;; (use-package company-bibtex
;;   :after tex-mode
;;   :demand t
;;   :commands company-bibtex)

(use-package company-anywhere
  :straight (company-anywhere :type git :host github :repo "zk-phi/company-anywhere")
  :after company
  :demand t)

;; A few backends are applicable to all modes and can be blocking: `company-yasnippet',
;; `company-ispell', and `company-dabbrev'. `company-dabbrev' returns a non-nil prefix in almost any
;; context (major mode, inside strings or comments). That is why it is better to put it at the end.

;; https://tychoish.com/post/better-company/
;; https://www.reddit.com/r/emacs/comments/l03dy1/priority_for_companymode/
;; https://emacs.stackexchange.com/questions/64038/how-to-use-multiple-backends-in-priority-for-company-mode

;; Try completion backends in order till there is a non-empty completion list
;; `(setq company-backends '(company-xxx company-yyy company-zzz))'
;; Merge completions of all the backends
;; `(setq company-backends '((company-xxx company-yyy company-zzz)))'
;; Merge completions of all the backends, give priority to `company-xxx'
;; `(setq company-backends '((company-xxx :separate company-yyy company-zzz)))'
;; Company does not support grouping of entirely arbitrary backends, they need to be compatible in
;; what `prefix' returns.

;; If the group contains keyword `:with', the backends listed after this keyword are ignored for
;; the purpose of the `prefix' command. If the group contains keyword `:separate', the candidates
;; that come from different backends are sorted separately in the combined list.

;; The first merges completions from `company-capf' and `company-dabbrev'. In the second case,
;; `company' will use only the backends before `:with' for determining the prefix (the text to be
;; completed). This implies that the candidates from backends after `:with' will be ignored by
;; `company', irrespective of whether the backends return a prefix or no, if none of the backends
;; before `:with' return a prefix.

;; `(add-to-list 'company-backends '(company-capf company-dabbrev))'
;; `(add-to-list 'company-backends '(company-capf :with company-dabbrev))'

;; https://github.com/sboosali/.emacs.d/sboo/sboo-company.el
;; The ‘prefix’ bool command always returns non-nil for following backends even when their
;; ‘candidates’ list command is empty: `company-abbrev', `company-dabbrev', `company-dabbrev-code'.

;; (with-eval-after-load "company"
;;   (progn
;;     (declare-function sb/company-xml-mode "init-completion")

;;     (defun sb/company-xml-mode ()
;;       "Add backends for completion with company."
;;       (defvar company-minimum-prefix-length)
;;       (defvar company-backends)

;;       (setq-local company-minimum-prefix-length 3)
;;       (make-local-variable 'company-backends)

;;       (setq company-backends '(company-capf
;;                                company-files
;;                                company-dabbrev-code
;;                                company-dabbrev)))

;;     (dolist (hook '(nxml-mode-hook))
;;       (add-hook hook (lambda ()
;;                        (sb/company-xml-mode)
;;                        (company-fuzzy-mode 1)
;;                        (diminish 'company-fuzzy-mode))))))

(with-eval-after-load "company"
  (progn
    (declare-function sb/company-latex-mode "init-completion")

    (defun sb/company-latex-mode ()
      "Add backends for latex completion in company mode."

      (setq-local company-minimum-prefix-length 3
                  company-transformers '(company-sort-by-backend-importance))
      (make-local-variable 'company-backends)

      ;; `company-reftex' should be considerably more powerful than `company-auctex' backends for
      ;; labels and citations.

      ;; https://github.com/TheBB/company-reftex/issues/10
      ;; FIXME: Cannot autocomplete "\includegraphics" without texlab
      (setq company-backends '(;; company-capf ; Necessary if we are using a language server
                               company-files
                               company-math-symbols-latex
                               company-latex-commands
                               company-reftex-labels
                               company-reftex-citations
                               company-auctex-environments
                               company-auctex-macros
                               company-math-symbols-unicode
                               company-auctex-symbols
                               ;; company-reftex is expected to be better than `company-auctex-bibs' and `company-auctex-labels'
                               ;; company-auctex-bibs
                               ;; company-auctex-labels
                               ;; `company-reftex-citations' is better than `company-bibtex'
                               ;; company-bibtex
                               ;; company-yasnippet ; FIXME: Untested
                               (company-ispell :with
                                               company-dabbrev))))

    (dolist (hook '(latex-mode-hook LaTeX-mode-hook))
      (add-hook hook (lambda ()
                       (sb/company-latex-mode)
                       ;; (company-fuzzy-mode 1)
                       ;; (diminish 'company-fuzzy-mode)
                       )))))

(with-eval-after-load "company"
  (progn
    (declare-function sb/company-text-mode "init-completion")

    (defun sb/company-text-mode ()
      "Add backends for text completion in company mode."
      (defvar company-minimum-prefix-length)
      (defvar company-backends)

      ;; Slightly larger value to have more precise matches and so that the popup does not block
      (setq-local company-minimum-prefix-length 3
                  company-transformers '(company-sort-by-backend-importance
                                         delete-dups))

      (set (make-local-variable 'company-backends)
           '(company-files
             (company-ispell :with
                             company-dabbrev))))

    (dolist (hook '(text-mode-hook)) ; Extends to derived modes like `markdown-mode' and `org-mode'
      (add-hook hook (lambda ()
                       (unless (or (derived-mode-p 'latex-mode) (derived-mode-p 'LaTeX-mode))
                         (sb/company-text-mode)
                         ;; (company-fuzzy-mode 1)
                         ;; (diminish 'company-fuzzy-mode)
                         ))))))

;; (progn
;;   (defun sb/company-java-mode ()
;;     "Add backends for Java completion in company mode."
;;     (defvar company-minimum-prefix-length)
;;     (defvar company-backends)

;;     (setq-local company-minimum-prefix-length 3)
;;     (make-local-variable 'company-backends)
;;     (setq company-backends '((company-capf :with company-yasnippet)
;;                              (company-files : with company-yasnippet)
;;                              (company-dabbrev-code :with company-yasnippet)
;;                              company-dabbrev)))

;;   (add-hook 'java-mode-hook #'sb/company-java-mode))

;; (with-eval-after-load "company"
;;   (progn
;;     (declare-function sb/company-sh-mode "init-completion")

;;     (defun sb/company-sh-mode ()
;;       "Add backends for shell script completion in company mode."
;;       (defvar company-minimum-prefix-length)
;;       (defvar company-backends)

;;       (setq-local company-minimum-prefix-length 3)
;;       (make-local-variable 'company-backends)

;;       (setq company-backends '(company-capf
;;                                company-shell
;;                                company-shell-env
;;                                company-dabbrev-code
;;                                company-files
;;                                company-dabbrev)))

;;     (add-hook 'sh-mode-hook (lambda ()
;;                               (sb/company-sh-mode)
;;                               ;; (company-fuzzy-mode 1)
;;                               ;; (diminish 'company-fuzzy-mode)
;;                               ))))

;; (with-eval-after-load "company"
;;   (progn
;;     (declare-function sb/company-fish-mode "init-completion")

;;     (defun sb/company-fish-mode ()
;;       "Add backends for fish shell script completion in company mode."
;;       (defvar company-minimum-prefix-length)
;;       (defvar company-backends)

;;       (setq-local company-minimum-prefix-length 3)
;;       (make-local-variable 'company-backends)

;;       (setq company-backends '(company-capf
;;                                company-shell
;;                                company-shell-env
;;                                company-fish-shell
;;                                company-dabbrev-code
;;                                company-files
;;                                company-dabbrev)))

;;     (add-hook 'fish-mode-hook (lambda ()
;;                                 (sb/company-fish-mode)
;;                                 ;; (company-fuzzy-mode 1)
;;                                 ;; (diminish 'company-fuzzy-mode)
;;                                 ))))

;; https://emacs.stackexchange.com/questions/19072/company-completion-very-slow
;; `company-clang' is slow
(with-eval-after-load "company"
  (progn
    (declare-function sb/company-prog-mode "init-completion")

    (defun sb/company-prog-mode ()
      "Add backends for program completion in company mode."
      (defvar company-minimum-prefix-length)
      (defvar company-backends)

      (setq-local company-minimum-prefix-length 2)

      ;; Other choices: `company-sort-by-length', `company-sort-by-occurrence',
      ;; `company-sort-by-backend-importance', `company-sort-prefer-same-case-prefix'

      (make-local-variable 'company-backends)

      ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
      (setq company-backends '(company-files
                               company-capf
                               (company-dabbrev-code ; Useful for variable names
                                company-etags)
                               (company-ispell :with
                                               company-dabbrev))))

    (add-hook 'prog-mode-hook
              (lambda ()
                (unless (or (derived-mode-p 'sh-mode) (derived-mode-p 'fish-mode))
                  (sb/company-prog-mode)
                  ;; (company-fuzzy-mode 1)
                  ;; (diminish 'company-fuzzy-mode)
                  )))))

(provide 'init-company)

;;; init-company.el ends here
