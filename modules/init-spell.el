;;; init-spell.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(when (symbol-value 'sb/IS-LINUX)
  (with-eval-after-load "ispell"
    (defvar ispell-dictionary)
    (defvar ispell-extra-args)
    (defvar ispell-local-dictionary)
    (defvar ispell-silently-savep)

    (setq ispell-dictionary "en_US"
          ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--size=90")
          ispell-local-dictionary "en_US"
          ispell-personal-dictionary (expand-file-name "spell" sb/extras-directory)
          ;; Save a new word to personal dictionary without asking
          ispell-silently-savep t)

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
    (add-to-list 'ispell-skip-region-alist '("\\\\begin{align}"    . "\\\\end{align}"))))

(declare-function flyspell-overlay-p "flyspell")

(unless (fboundp 'flyspell-prog-mode)
  (autoload #'flyspell-prog-mode "flyspell" nil t))
(unless (fboundp 'flyspell-mode)
  (autoload #'flyspell-mode "flyspell" nil t))
(unless (fboundp 'flyspell-buffer)
  (autoload #'flyspell-buffer "flyspell" nil t))
(unless (fboundp 'sb/flyspell-goto-previous-error)
  (autoload #'sb/flyspell-goto-previous-error "init-autoload" nil t))
(unless (fboundp 'flyspell-overlay-p)
  (autoload #'flyspell-overlay-p "flyspell" nil t))
(unless (fboundp 'flyspell-correct-previous)
  (autoload #'flyspell-correct-previous "flyspell" nil t))
(unless (fboundp 'flyspell-correct-next)
  (autoload #'flyspell-correct-next "flyspell" nil t))

;; Enabling `flyspell-prog-mode' does not seem to be very useful and highlights links and
;; language-specific words
(add-hook 'text-mode-hook #'flyspell-mode)
;; Enabling `flyspell-prog-mode' does not seem to be very useful and highlights links and
;; language-specific words
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; (add-hook 'before-save-hook #'flyspell-buffer) ; Saving files will be slow

;; `find-file-hook' will not work for buffers with no associated files
(add-hook 'after-init-hook
          (lambda nil
            (when (string= (buffer-name) "*scratch*")
              (flyspell-mode 1))))

(eval-and-compile
  ;; Move point to previous error
  ;; http://emacs.stackexchange.com/a/14912/2017
  ;; http://pragmaticemacs.com/emacs/jump-back-to-previous-typo/
  (defun sb/flyspell-goto-previous-error (arg)
    "Go to arg previous spelling error."
    (interactive "p")
    (defvar flyspell-old-pos-error)
    (defvar flyspell-old-buffer-error)

    (while (not (= 0 arg))
      (let ((pos (point))
            (min (point-min)))
        (if (and (eq (current-buffer) flyspell-old-buffer-error)
                 (eq pos flyspell-old-pos-error))
            (progn
              (if (= flyspell-old-pos-error min)
                  (progn
                    (message "Restarting from end of buffer")
                    (goto-char (point-max)))
                (backward-word 1))
              (setq pos (point))))
        (while (and (> pos min)
                    (let ((ovs (overlays-at pos))
                          (r 'nil))
                      (while (and (not r)
                                  (consp ovs))
                        (if (flyspell-overlay-p
                             (car ovs))
                            (setq r t)
                          (setq ovs (cdr ovs))))
                      (not r)))
          (backward-word 1)
          (setq pos (point)))
        (setq arg (1- arg))
        (setq flyspell-old-pos-error pos)
        (setq flyspell-old-buffer-error
              (current-buffer))
        (goto-char pos)
        (if
            (= pos min)
            (progn
              (message "No more misspelled word!")
              (setq arg 0))
          (forward-word))))))

(with-eval-after-load "flyspell"
  (defvar flyspell-abbrev-p)
  (defvar flyspell-issue-message-flag)
  (defvar flyspell-issue-welcome-flag)

  (setq flyspell-abbrev-p t ; Add corrections to abbreviation table
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)

  (diminish 'flyspell-mode)

  ;; Flyspell popup is more efficient. Ivy completion does not show the "Save" option in a few
  ;; cases.
  (unless (fboundp 'flyspell-popup-correct)
    (autoload #'flyspell-popup-correct "flyspell-popup" nil t))

  (defvar flyspell-popup-correct-delay)

  (setq flyspell-popup-correct-delay 0.2)

  (bind-keys :package flyspell-popup
             ("C-;" . flyspell-popup-correct)))

(defvar flyspell-mode-map)
(bind-keys :package flyspell
           ("C-c f f" . flyspell-mode)
           ("C-c f b" . flyspell-buffer)
           :map flyspell-mode-map
           ("C-;")
           ("C-,"     . sb/flyspell-goto-previous-error))

;; Flyspell popup is more efficient. Ivy-completion does not show the "Save" option in a few cases.
(use-package flyspell-popup
  :after flyspell
  :disabled t
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-popup-correct))
  :config (setq flyspell-popup-correct-delay 0.1))

(use-package flyspell-correct
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-wrapper)))

;; As of Emacs 28, `flyspell' does not provide a way to automatically check only the on-screen text.
;; Running `flyspell-buffer' on an entire buffer can be slow.
(use-package spell-fu
  :defines spell-fu-directory
  :commands spell-fu-mode
  :if (executable-find "aspell")
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
                                                    font-lock-function-name-face
                                                    font-lock-keyword-face
                                                    font-lock-variable-name-face
                                                    hl-line))
                     (spell-fu-mode))))
  :bind
  (("C-c f n" . spell-fu-goto-next-error)
   ("C-c f p" . spell-fu-goto-previous-error)
   ("C-c f a" . spell-fu-word-add)))

(use-package consult-flyspell
  :after (consult flyspell)
  :commands consult-flyspell)

(provide 'init-spell)

;;; init-spell.el ends here
