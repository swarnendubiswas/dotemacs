;;; init-spell.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/extras-directory)
(defvar sb/minibuffer-completion)

(declare-function sb/inhibit-message-call-orig-fun "init-core.el")

(use-package ispell
  :straight (:type built-in)
  :bind ("M-$" . ispell-word)
  :custom
  (ispell-dictionary "en_US")
  (ispell-local-dictionary "en_US")
  (ispell-personal-dictionary (expand-file-name "spell" sb/extras-directory))
  (ispell-alternate-dictionary (expand-file-name "wordlist.5" sb/extras-directory))
  (ispell-silently-savep t "Save a new word to personal dictionary without asking")
  :config
  (when (and (symbol-value 'sb/IS-WINDOWS) (executable-find "hunspell"))
    (setenv "LANG" "en_US")
    (setenv "DICTIONARY" "en_US")
    (setenv "DICPATH" `,(concat user-emacs-directory "hunspell"))

    (setq
      ispell-local-dictionary "en_US"
      ispell-program-name "hunspell"
      ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))
      ispell-hunspell-dictionary-alist ispell-local-dictionary-alist
      ispell-hunspell-dict-paths-alist `(("en_US" ,(concat user-emacs-directory "hunspell/en_US.aff")))))

  (when (and (symbol-value 'sb/IS-LINUX) (executable-find "aspell"))
    (setq
      ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--size=90")))


  (when (and (symbol-value 'sb/IS-LINUX) (executable-find "hunspell"))
    (setenv "LANG" "en_US")
    (setenv "DICTIONARY" "en_US")
    (setenv "DICPATH" `,(concat user-emacs-directory "hunspell"))

    (setq
      ispell-local-dictionary "en_US"
      ispell-program-name "hunspell"
      ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))
      ispell-hunspell-dictionary-alist ispell-local-dictionary-alist
      ispell-hunspell-dict-paths-alist `(("en_US" ,(concat user-emacs-directory "hunspell/en_US.aff")))))

  ;; Skip regions in `org-mode'
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_example" . "#\\+end_example"))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  ;; Verbatim regions in org mode should not be ispelled
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  ;; Properties block in org mode do not need to be ispelled
  (add-to-list 'ispell-skip-region-alist '("\:PROPERTIES\:$" . "\:END\:$"))
  ;; Footnotes in org that have http links that are line breaked should not be ispelled
  (add-to-list 'ispell-skip-region-alist '("^http" . "\\]"))
  (add-to-list 'ispell-skip-region-alist '("`" "`"))
  ;; Skip some math environments
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{multline}" . "\\\\end{multline}"))
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{equation}" . "\\\\end{equation}"))
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{align}" . "\\\\end{align}"))

  ;; Hide the "Starting new Ispell process" message
  (advice-add 'ispell-init-process :around #'sb/inhibit-message-call-orig-fun)
  (advice-add 'ispell-lookup-words :around #'sb/inhibit-message-call-orig-fun))

(use-package flyspell
  :preface
  ;; Move point to previous error
  ;; http://emacs.stackexchange.com/a/14912/2017
  ;; http://pragmaticemacs.com/emacs/jump-back-to-previous-typo/
  (defun sb/flyspell-goto-previous-error (arg)
    "Go to arg previous spelling error."
    (interactive "p")
    (while (not (= 0 arg))
      (let
        (
          (pos (point))
          (min (point-min)))
        (if (and (eq (current-buffer) flyspell-old-buffer-error) (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
              ;; Goto beginning of buffer
              (progn
                (message "Restarting from end of buffer")
                (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
        ;; Seek the next error
        (while
          (and (> pos min)
            (let
              (
                (ovs (overlays-at pos))
                (r '()))
              (while (and (not r) (consp ovs))
                (if (flyspell-overlay-p (car ovs))
                  (setq r t)
                  (setq ovs (cdr ovs))))
              (not r)))
          (backward-word 1)
          (setq pos (point)))
        ;; Save the current location for the next invocation
        (setq arg (1- arg))
        (setq flyspell-old-pos-error pos)
        (setq flyspell-old-buffer-error (current-buffer))
        (goto-char pos)
        (if (= pos min)
          (progn
            (message "No more misspelled words!")
            (setq arg 0))
          (forward-word)))))
  :straight (:type built-in)
  :commands (flyspell-overlay-p flyspell-correct-previous flyspell-correct-next flyspell-buffer)
  :hook
  ( ;; (before-save-hook . flyspell-buffer) ; Saving files will be slow
    ;; Enabling `flyspell-prog-mode' does not seem to be very useful and highlights links and
    ;; language-specific words. Furthermore, it is supposedly slow.
    (prog-mode-hook . flyspell-prog-mode)
    ;; `find-file-hook' will not work for buffers with no associated files
    (emacs-startup-hook
      .
      (lambda ()
        (when (string= (buffer-name) "*scratch*")
          (flyspell-mode 1))))
    (text-mode-hook . flyspell-mode))
  :bind
  (("C-c f f" . flyspell-mode)
    ("C-c f b" . flyspell-buffer)
    :map
    flyspell-mode-map
    ("C-," . sb/flyspell-goto-previous-error))
  :custom (flyspell-abbrev-p t "Add corrections to abbreviation table")
  ;; Do not print messages for every word (when checking the entire buffer). This is a major
  ;; performance gain.
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  :diminish)

;; Silence "Starting 'look' process..." message
(advice-add 'lookup-words :around #'sb/inhibit-message-call-orig-fun)

;; (use-package flyspell-popup
;;   :after flyspell
;;   :bind
;;   (:map flyspell-mode-map
;;         ("C-;" . flyspell-popup-correct))
;;   :custom (flyspell-popup-correct-delay 0.1))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-at-point)))

;; As of Emacs 28, `flyspell' does not provide a way to automatically check only the on-screen text.
;; Running `flyspell-buffer' on an entire buffer can be slow.
(use-package spell-fu
  :if (or (executable-find "aspell") (executable-find "hunspell"))
  :disabled t
  :defines spell-fu-directory
  :commands spell-fu-mode
  :init
  (add-hook
    'text-mode-hook
    (lambda ()
      (setq spell-fu-faces-exclude
        '
        (hl-line
          ;; `nxml-mode' is derived from `text-mode'
          nxml-attribute-local-name))
      (spell-fu-mode)))

  (add-hook
    'org-mode-hook
    (lambda ()
      (setq spell-fu-faces-exclude
        '
        (org-block
          org-block-begin-line
          org-block-end-line
          org-cite
          org-cite-key
          org-code
          org-date
          org-footnote
          org-formula
          org-latex-and-related
          org-link
          org-meta-line
          org-property-value
          org-ref-cite-face
          org-special-keyword
          org-tag
          org-todo
          org-todo-keyword-done
          org-todo-keyword-habt
          org-todo-keyword-kill
          org-todo-keyword-outd
          org-todo-keyword-todo
          org-todo-keyword-wait
          org-verbatim
          org-modern-tag
          hl-line))
      (spell-fu-mode)))

  (add-hook
    'markdown-mode-hook
    (lambda ()
      (setq spell-fu-faces-exclude
        '
        (markdown-blockquote-face
          markdown-code-face
          markdown-html-attr-name-face
          markdown-html-attr-value-face
          markdown-html-tag-name-face
          markdown-inline-code-face
          markdown-link-face
          markdown-markup-face
          markdown-plain-url-face
          markdown-reference-face
          markdown-url-face
          pandoc-citation-key-face
          hl-line))
      (spell-fu-mode)))

  (dolist (hook '(LaTeX-mode-hook latex-mode-hook))
    (add-hook
      hook
      (lambda ()
        (setq spell-fu-faces-exclude
          '
          (font-latex-math-face
            font-latex-sedate-face
            font-lock-function-name-face
            font-lock-keyword-face
            font-lock-variable-name-face
            hl-line))
        (spell-fu-mode))))
  :bind
  (("C-c f n" . spell-fu-goto-next-error)
    ("C-c f p" . spell-fu-goto-previous-error)
    ("C-c f a" . spell-fu-word-add))
  :custom (spell-fu-directory (expand-file-name "spell-fu" no-littering-var-directory))
  :config
  ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/64
  (add-to-list 'spell-fu-faces-include 'font-lock-string-face)
  (add-to-list 'spell-fu-faces-include 'font-lock-doc-face)
  (add-to-list 'spell-fu-faces-include 'font-lock-comment-face)
  (add-to-list 'spell-fu-faces-include 'tree-sitter-hl-face:comment)
  (add-to-list 'spell-fu-faces-include 'tree-sitter-hl-face:doc)
  (add-to-list 'spell-fu-faces-include 'tree-sitter-hl-face:string)

  ;; Ignore read-only buffers
  (setq global-spell-fu-ignore-buffer (lambda (buf) (buffer-local-value 'buffer-read-only buf))))

;; "M-$" triggers correction for the misspelled word before point, "C-u M-$" triggers correction for
;; the entire buffer.
(use-package jinx
  :hook (text-mode-hook . jinx-mode)
  :custom (jinx-languages "en_US")
  :bind ([remap ispell-word] . jinx-correct)
  :config
  (with-eval-after-load "vertico-multiform"
    (add-to-list 'vertico-multiform-categories '(jinx grid (vertico-grid-annotate . 20))))
  :diminish)

(provide 'init-spell)

;;; init-spell.el ends here
