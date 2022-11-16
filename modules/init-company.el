;;; init-company.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/capf)

;; Do not diminish company to see the backend used on the modeline with GUI Emacs. Otherwise, use
;; "M-x company-diag" or the modeline status (without diminish) to see the backend used. Try "M-x
;; company-complete-common" when there are no completions. Use "C-M-i" for `complete-symbol' with
;; regex search.

(use-package company
  :if (eq sb/capf 'company)
  :preface
  ;; https://stackoverflow.com/questions/56382840/is-there-a-way-to-automatically-add-a-whitespace-upon-completion-in-company-mode
  (defun sb/company-after-completion-hook (&rest _ignored)
    ;; This would be called with the completion candidate, so you could modify it to insert spaces
    ;; based on the candidate
    (just-one-space))
  :defines
  (company-dabbrev-downcase company-dabbrev-ignore-case
                            company-dabbrev-other-buffers
                            company-ispell-available
                            company-ispell-dictionary
                            company-clang-insert-arguments)
  :commands
  (company-abort company-files company-yasnippet
                 company-ispell company-dabbrev
                 company-capf company-dabbrev-code
                 company-clang-set-prefix)
  :hook
  (after-init-hook . global-company-mode)
  :bind
  ;; `company-search-candidates' is bound to "C-s" and `company-filter-candidates' is bound to
  ;; "C-M-s"
  (("C-M-/"    . company-other-backend)
   :map company-active-map
   ("C-n"      . company-select-next)
   ("C-p"      . company-select-previous)
   ;; Insert the common part of all candidates, or select the next one
   ("<tab>"    . company-complete-common-or-cycle)
   ("<escape>" . company-abort))
  :custom
  (company-dabbrev-downcase nil "Do not downcase returned candidates")
  ;; Do not ignore case when collecting completion candidates. It is recommended to change the
  ;; default value of "keep-prefix" if we modify `company-dabbrev-downcase'.
  (company-dabbrev-ignore-case nil)
  ;; Search in other buffers with the same major mode. This can cause performance overhead if
  ;; there are lots of open buffers.
  (company-dabbrev-other-buffers t)
  (company-ispell-dictionary (expand-file-name "wordlist.5" sb/extras-directory))
  (company-minimum-prefix-length 3 "Small words can be faster to type")
  (company-require-match nil "Allow input string that do not match candidates")
  (company-show-quick-access t "Speed up completion")
  ;; Align additional metadata, like type signatures, to the right-hand side
  (company-tooltip-align-annotations t)
  ;; Other choices are: "company-pseudo-tooltip-unless-just-one-frontend" which shows popup unless
  ;; there is only one candidate, "company-preview-frontend" which shows the preview in-place which
  ;; is too instrusive, "company-preview-if-just-one-frontend" shows in-place preview if there is
  ;; only choice, and "company-echo-metadata-frontend" which shows selected candidate docs in echo
  ;; area.
  (company-frontends '(company-pseudo-tooltip-frontend)) ; Always show candidates in overlay tooltip
  (company-global-modes '(not dired-mode erc-mode message-mode
                              comint-mode inferior-python-mode
                              magit-status-mode help-mode
                              gud-mode eshell-mode shell-mode
                              csv-mode))
  :config
  (add-to-list 'company-transformers 'delete-dups)
  ;; Ignore matches that consist solely of numbers from `company-dabbrev'
  ;; https://github.com/company-mode/company-mode/issues/358
  (push (apply-partially #'cl-remove-if
                         (lambda (c)
                           (string-match-p "\\`[0-9]+\\'" c)))
        company-transformers)
  (add-to-list 'company-transformers 'company-sort-by-backend-importance)
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix))

;; Posframes do not have unaligned rendering issues with variable `:height' unlike an overlay.
;; However, the width of the frame popup is often not enough and the right side gets cut off.
;; https://github.com/company-mode/company-mode/issues/1010

(use-package company-posframe
  :if (display-graphic-p)
  :after company
  :commands company-posframe-mode
  :diminish
  :custom
  (company-posframe-show-metadata t "Difficult to distinguish the help text from completions")
  (company-posframe-show-indicator nil "The display is not great")
  (company-posframe-quickhelp-delay nil "Disable showing the help frame")
  :init
  (company-posframe-mode 1))

(use-package company-quickhelp
  :after company
  :if (display-graphic-p)
  :hook
  (prog-mode-hook . company-quickhelp-mode))

(use-package company-quickhelp-terminal
  :after company
  :unless (display-graphic-p)
  :hook
  (prog-mode-hook . company-quickhelp-terminal-mode))

(use-package company-statistics
  :after company
  :commands company-statistics-mode
  :init (company-statistics-mode 1))

;; We want `capf' sort for programming modes, not with recency. `company-prescient' seems to break
;; support for the `:separate' keyword in `company'.

;; (use-package company-prescient
;;   :after (company prescient)
;;   :demand t
;;   :commands company-prescient-mode
;;   ;; :custom
;;   ;; (company-prescient-sort-length-enable nil)
;;   :config
;;   (company-prescient-mode 1))

;; Nice feature but slows completions. We should invoke this only at the very end of configuring
;; `company'.

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

;; ;; FIXME: Do we need this with the bash language sever?
;; (use-package company-shell
;;   :disabled t
;;   :after company
;;   :after (:any sh-mode fish-mode)
;;   :demand t
;;   :defines company-shell-delete-duplictes
;;   :commands (company-shell company-shell-env company-fish-shell)
;;   :custom (company-shell-delete-duplictes t))

(use-package company-auctex
  :after (tex-mode company)
  :demand t
  :commands
  (company-auctex-init company-auctex-labels
                       company-auctex-bibs company-auctex-macros
                       company-auctex-symbols company-auctex-environments))

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
  :after tex-mode
  :demand t
  :commands company-bibtex)

(use-package company-anywhere ; Complete in the middle of words
  :straight
  (company-anywhere :type git :host github :repo "zk-phi/company-anywhere")
  :after company
  :demand t)

(use-package company-dict
  :after company
  :demand t
  :custom
  (company-dict-dir (expand-file-name "company-dict" user-emacs-directory))
  (company-dict-enable-fuzzy t)
  (company-dict-enable-yasnippet nil))

;; TODO: `company-citre' possibly makes this package obsolete.

;; You can set `company-ctags-extra-tags-files' to load extra tags files. Check "../README.org" to
;; see example of invocation.

;; (use-package company-ctags
;;   :straight (:type git :host github :repo "redguardtoo/company-ctags")
;;   :after company
;;   :demand t
;;   :custom
;;   (company-ctags-quiet t)
;;   (company-ctags-fuzzy-match-p nil)
;;   (company-ctags-everywhere t "Offer completions in comments and strings"))

(use-package company-dirfiles ; Better replacement for `company-files'
  :straight (:type git :host github :repo "cwfoo/company-dirfiles")
  :after company
  :demand t)

(use-package consult-company
  :if (eq sb/minibuffer-completion 'vertico)
  :bind
  (:map company-mode-map
        ([remap completion-at-point] . consult-company)))

(use-package company-wordfreq
  :straight (:type git :host github :repo "johannes-mueller/company-wordfreq.el")
  :commands company-wordfreq)

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

;; Try completion backends in order untill there is a non-empty completion list.
;; (setq company-backends '(company-xxx company-yyy company-zzz))

;; Merge completions of all the backends
;; (setq company-backends '((company-xxx company-yyy company-zzz)))

;; Merge completions of all the backends, give priority to `company-xxx'
;; (setq company-backends '((company-xxx :separate company-yyy company-zzz)))
;; (setq company-backends '((company-tabnine :separate company-capf company-yasnippet)))

;; The first merges completions from `company-capf' and `company-dabbrev'. In the second case,
;; `company' will use only the backends before `:with' for determining the prefix (the text to be
;; completed). This implies that the candidates from backends after `:with' will be ignored by
;; `company', irrespective of whether the backends return a prefix or no, if none of the backends
;; before `:with' return a prefix.

;;     (setq company-backends '((company-capf :with company-yasnippet)
;;                              (company-files : with company-yasnippet)
;;                              (company-dabbrev-code :with company-yasnippet)
;;                              company-dabbrev)))

;; (add-to-list 'company-backends '(company-capf company-dabbrev))
;; (add-to-list 'company-backends '(company-capf :with company-dabbrev))

;; Options: company-sort-prefer-same-case-prefix, company-sort-by-occurrence,
;; company-sort-by-statistics, company-sort-by-length, company-sort-by-backend-importance,
;; delete-dups

;; (setq-local company-transformers '(company-sort-by-backend-importance
;;                                    delete-dups))

(with-eval-after-load "company"
  ;; We override `company-backends', so it is not important to delete individual backends. This is
  ;; the default for unhandled major modes.
  (setq company-backends '(company-dirfiles
                           company-capf
                           company-dabbrev-code
                           ;; If we have `company-dabbrev' first, then other matches from
                           ;; `company-ispell' will be ignored.
                           (company-ispell :with
                                           company-dabbrev
                                           company-dict))))

(with-eval-after-load "company"
  (progn
    (declare-function sb/company-latex-mode "init-company")

    (defun sb/company-latex-mode ()
      "Add backends for latex completion in company mode."

      (setq-local company-minimum-prefix-length 3)

      (make-local-variable 'company-backends)

      ;; https://github.com/TheBB/company-reftex/issues/10

      ;; `company-reftex' should be considerably more powerful than `company-auctex' backends for
      ;; labels and citations. `company-reftex-labels' is expected to be better than
      ;; `company-auctex-labels'. `company-reftex-citations' is better than `company-bibtex' and
      ;; `company-auctex-bibs'

      ;; Example: company-backends: https://github.com/TheBB/company-reftex/issues/10
      ;; ((:separate company-reftex-labels company-reftex-citations)
      ;; (:separate company-auctex-symbols company-auctex-environments company-capf company-auctex-macros)
      ;; (company-semantic company-dabbrev-code company-gtags company-etags company-keywords)
      ;; company-files company-dabbrev)

      (setq company-backends '(company-dirfiles
                               (:separate
                                company-math-symbols-latex ; math latex tags
                                company-latex-commands
                                company-reftex-labels
                                company-reftex-citations
                                company-auctex-environments
                                company-auctex-macros
                                ;; Math unicode symbols and sub(super)scripts
                                company-math-symbols-unicode
                                company-auctex-symbols
                                company-bibtex)

                               ;; FIXME: Untested
                               ;; company-yasnippet

                               (company-dabbrev :with
                                                company-wordfreq
                                                company-ispell
                                                company-dict)
                               company-capf)))

    ;; (declare-function company-fuzzy-mode "company-fuzzy")

    (dolist (hook '(latex-mode-hook LaTeX-mode-hook))
      (add-hook hook (lambda ()
                       (sb/company-latex-mode)
                       ;; `company-capf' does not pass to later backends with Texlab, so we use
                       ;; `company-fuzzy-mode' to merge results from all backends.
                       ;; (company-fuzzy-mode 1)
                       ;; (diminish 'company-fuzzy-mode)
                       )))))

;; https://emacs.stackexchange.com/questions/21171/company-mode-completion-for-org-keywords
(with-eval-after-load "company"
  (progn
    (declare-function sb/company-org-mode "init-company")

    (defun sb/company-org-mode ()
      "Add backends for org-mode completion with company mode."

      (setq-local company-minimum-prefix-length 3)
      (make-local-variable 'company-backends)
      (setq company-backends '(company-dirfiles
                               company-capf
                               (company-ispell :with
                                               company-dabbrev
                                               company-dict))))

    (add-hook 'org-mode-hook
              (lambda ()
                (require 'pcomplete)
                (add-hook 'completion-at-point-functions
                          'pcomplete-completions-at-point nil t)
                (sb/company-org-mode)
                ;; (company-fuzzy-mode 1)
                ;; (diminish 'company-fuzzy-mode)
                ))))

(with-eval-after-load "company"
  (progn
    (declare-function sb/company-text-mode "init-company")

    (defun sb/company-text-mode ()
      "Add backends for text completion in company mode."
      (defvar company-minimum-prefix-length)
      (defvar company-backends)

      ;; Slightly larger value to have more precise matches and so that the popup does not block
      (setq-local company-minimum-prefix-length 3)

      (set (make-local-variable 'company-backends)
           '(company-dirfiles
             (company-wordfreq :with
                               company-dabbrev
                               company-ispell
                               company-dict))))

    ;; Extends to derived modes like `markdown-mode' and `org-mode'
    (add-hook 'text-mode-hook
              (lambda ()
                (unless (or (derived-mode-p 'latex-mode)
                            (derived-mode-p 'LaTeX-mode)
                            (derived-mode-p 'org-mode))
                  (sb/company-text-mode)
                  ;; (setq-local company-after-completion-hook #'sb/company-after-completion-hook)
                  ;; (company-fuzzy-mode 1)
                  ;; (diminish 'company-fuzzy-mode)
                  )))))

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

;; `company-clang' is slow:
;; https://emacs.stackexchange.com/questions/19072/company-completion-very-slow
(with-eval-after-load "company"
  (progn

    ;; (defvar citre-completion-case-sensitive)

    ;; (declare-function citre-capf--get-annotation "citre")
    ;; (declare-function citre-capf--get-collection "citre")
    ;; (declare-function citre-get-property "citre")
    ;; (declare-function citre-get-symbol "citre")

    ;; (declare-function company-begin-backend "company")

    (defun company-citre (-command &optional -arg &rest _ignored)
      "Completion backend of Citre.  Execute COMMAND with ARG and IGNORED."
      (interactive (list 'interactive))
      (cl-case -command
        (interactive (company-begin-backend 'company-citre))
        (prefix (and (bound-and-true-p citre-mode)
                     (or (citre-get-symbol) 'stop)))
        (meta (citre-get-property 'signature -arg))
        (annotation (citre-capf--get-annotation -arg))
        (candidates (all-completions -arg (citre-capf--get-collection -arg)))
        (ignore-case (not citre-completion-case-sensitive))))

    (declare-function sb/company-prog-mode "init-company")

    (defun sb/company-prog-mode ()
      "Add backends for `prog-mode' completion in company mode."
      (defvar company-minimum-prefix-length)
      (defvar company-backends)

      ;; Typing short prefixes help with faster completion and a more responsive UI
      (setq-local company-minimum-prefix-length 2)

      (make-local-variable 'company-backends)

      ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
      (setq company-backends '(company-dirfiles
                               (:separate
                                company-capf
                                company-citre
                                company-dabbrev-code ; Useful for variable names
                                :with company-yasnippet)
                               ;; (company-capf :with
                               ;;               company-dabbrev-code ; Useful for variable names
                               ;;               company-ctags
                               ;;               company-yasnippet)
                               (company-wordfreq :with
                                                 company-dabbrev
                                                 company-dict
                                                 company-ispell))))

    (add-hook 'prog-mode-hook
              (lambda ()
                ;; (unless (or (derived-mode-p 'sh-mode) (derived-mode-p 'fish-mode))
                (sb/company-prog-mode)
                ;; (company-fuzzy-mode 1)
                ;; (diminish 'company-fuzzy-mode)
                ))))

(provide 'init-company)

;;; init-company.el ends here
