;;; init-spell.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(use-package ispell
  :straight nil
  :if (symbol-value 'sb/IS-LINUX)
  :custom
  (ispell-dictionary "en_US")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--size=90"))
  (ispell-local-dictionary "en_US")
  (ispell-personal-dictionary (expand-file-name "spell" sb/extras-directory))
  (ispell-silently-savep t "Save a new word to personal dictionary without asking")
  :config
  ;; Skip regions in Org-mode
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC"     . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src"     . "#\\+end_src"))
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
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{align}"    . "\\\\end{align}")))

(use-package flyspell
  :straight nil
  :if (symbol-value 'sb/IS-LINUX)
  :commands (flyspell-overlay-p flyspell-correct-previous flyspell-correct-next flyspell-buffer)
  :diminish
  :preface
  ;; Move point to previous error
  ;; http://emacs.stackexchange.com/a/14912/2017
  ;; http://pragmaticemacs.com/emacs/jump-back-to-previous-typo/
  (defun sb/flyspell-goto-previous-error (arg)
    "Go to arg previous spelling error."
    (interactive "p")
    (while (not (= 0 arg))
      (let ((pos (point))
            (min (point-min)))
        (if (and (eq (current-buffer) flyspell-old-buffer-error)
                 (eq pos flyspell-old-pos-error))
            (progn
              (if (= flyspell-old-pos-error min)
                  ;; Goto beginning of buffer
                  (progn
                    (message "Restarting from end of buffer")
                    (goto-char (point-max)))
                (backward-word 1))
              (setq pos (point))))
        ;; Seek the next error
        (while (and (> pos min)
                    (let ((ovs (overlays-at pos))
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
  :config
  (setq flyspell-abbrev-p           t ; Add corrections to abbreviation table
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  :hook
  (;; (before-save-hook . flyspell-buffer) ; Saving files will be slow
   ;; Enabling `flyspell-prog-mode' does not seem to be very useful and highlights links and
   ;; language-specific words
   (prog-mode-hook . flyspell-prog-mode)
   ;; `find-file-hook' will not work for buffers with no associated files
   (after-init-hook . (lambda ()
                        (when (string= (buffer-name) "*scratch*")
                          (flyspell-mode 1))))
   (text-mode-hook . flyspell-mode))
  :bind
  (("C-c f f" . flyspell-mode)
   ("C-c f b" . flyspell-buffer)
   :map flyspell-mode-map
   ("C-;"     . nil)
   ("C-,"     . sb/flyspell-goto-previous-error)))

;; Flyspell popup is more efficient. Ivy-completion does not show the "Save" option in a few cases.
(use-package flyspell-popup
  :straight t
  :after flyspell
  :disabled t
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-popup-correct))
  :config (setq flyspell-popup-correct-delay 0.1))

(use-package flyspell-correct
  :straight t
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-wrapper)))

;; As of Emacs 28, `flyspell' does not provide a way to automatically check only the on-screen text.
;; Running `flyspell-buffer' on an entire buffer can be slow.
(use-package spell-fu
  :straight t
  :defines spell-fu-directory
  :commands spell-fu-mode
  :custom
  (spell-fu-directory (expand-file-name "spell-fu" no-littering-var-directory))
  :init
  (add-hook 'text-mode-hook
            (lambda ()
              (setq spell-fu-faces-exclude '(hl-line
                                             ;; `nxml-mode' is derived from `text-mode'
                                             nxml-attribute-local-name))
              (spell-fu-mode)))

  (add-hook 'org-mode-hook
            (lambda ()
              (setq spell-fu-faces-exclude '(org-block
                                             org-block-begin-line
                                             org-block-end-line
                                             org-code
                                             org-date
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
                                             hl-line))
              (spell-fu-mode)))

  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq spell-fu-faces-exclude '(markdown-blockquote-face
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
                                             hl-line
                                             pandoc-citation-key-face))
              (spell-fu-mode)))

  (dolist (hook '(LaTeX-mode-hook latex-mode-hook))
    (add-hook hook (lambda ()
                     (setq spell-fu-faces-exclude '(font-latex-math-face
                                                    font-latex-sedate-face
                                                    hl-line))
                     (spell-fu-mode))))
  :bind
  (("C-c f n" . spell-fu-goto-next-error)
   ("C-c f p" . spell-fu-goto-previous-error)
   ("C-c f a" . spell-fu-word-add)))

(use-package consult-flyspell
  :straight t
  :after (consult flyspell)
  :commands consult-flyspell)

(provide 'init-spell)

;;; init-spell.el ends here
