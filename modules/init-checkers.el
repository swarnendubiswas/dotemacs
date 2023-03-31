;;; init-checkers.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/modeline-theme)
(defvar sb/user-home-directory)
(defvar sb/minibuffer-completion)

(declare-function f-dirname "f")

;; Identify weasel words, passive voice, and duplicate words. The module does not check grammar and
;; checks only the writing style. `textlint' includes `writegood'.

(use-package writegood-mode
  :commands
  (writegood-passive-voice-turn-off
   writegood-passive-voice-turn-on
   writegood-weasels-turn-on
   writegood-weasels-turn-off
   writegood-duplicates-turn-on
   writegood-duplicates-turn-off)
  :hook
  (text-mode-hook . writegood-duplicates-turn-on)
  :config
  ;; https://emacs.stackexchange.com/questions/32644/how-to-concatenate-two-lists
  ;; https://emacs.stackexchange.com/questions/20465/append-lists-smartly
  (let ((sb/weasel-words '("actionable" "actually" "basically" "clearly" "easily" "easy"
                           "it turns out that" "In this regard" "In this sense" "With this in mind"
                           "With the above in mind" "may have" "often" "simple" "probably" "simply"
                           "specifically")))
    (cl-union writegood-weasel-words sb/weasel-words))
  :diminish)

(use-package flycheck
  :commands
  (flycheck-add-next-checker flycheck-next-checker
                             flycheck-mode
                             flycheck-previous-error
                             flycheck-describe-checker
                             flycheck-buffer
                             flycheck-list-errors
                             flycheck-select-checker
                             flycheck-verify-setup
                             flycheck-next-error
                             flycheck-disable-checker
                             flycheck-add-mode
                             flycheck-manual
                             flycheck-display-error-messages-unless-error-list
                             flycheck-sexp-to-string)
  :hook
  (after-init-hook . global-flycheck-mode)
  :custom
  ;; Remove newline checks, since they would trigger an immediate check when we want the
  ;; `flycheck-idle-change-delay' to be in effect while editing.
  (flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change))
  (flycheck-checker-error-threshold 1500)
  (flycheck-idle-buffer-switch-delay 2 "Increase the time (s) to allow for quick transitions")
  (flycheck-idle-change-delay 2 "Increase the time (s) to allow for transient edits")
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; There are no checkers for `csv-mode', and many program modes use lsp. `yaml-mode' is
  ;; derived from `text-mode'.
  (flycheck-global-modes '(not csv-mode conf-mode))
  :config
  ;; Terminal Emacs does not support fringes
  (if (display-graphic-p)
      (setq flycheck-indication-mode 'left-fringe)
    (setq flycheck-indication-mode 'left-margin))

  ;; We prefer not to use `textlint' and `proselint'. `chktex' errors are often not very helpful.
  (dolist (checkers '(proselint textlint tex-chktex))
    (delq checkers flycheck-checkers))

  ;; These themes have their own styles for displaying flycheck info.
  (when (or (eq sb/modeline-theme 'doom-modeline) (eq sb/modeline-theme 'spaceline))
    (setq flycheck-mode-line nil))

  (setq-default flycheck-markdown-markdownlint-cli-config (expand-file-name ".markdownlint.json"
                                                                            sb/user-home-directory)
                flycheck-chktexrc "chktexrc"
                flycheck-pylintrc '("setup.cfg" "pylintrc")
                flycheck-python-pylint-executable "python3"
                flycheck-shellcheck-follow-sources nil)

  ;; Add support for textidote
  (flycheck-define-checker tex-textidote
    "A LaTeX grammar/spelling checker using textidote.
  See https://github.com/sylvainhalle/textidote."
    :modes (latex-mode plain-tex-mode)
    :command
    ("java" "-jar" (eval (expand-file-name (no-littering-expand-etc-file-name "textidote.jar")))
     "--read-all"
     "--output" "singleline"
     "--no-color"
     "--check" (eval (if ispell-current-dictionary (substring ispell-current-dictionary 0 2) "en"))
     ;; Try to honor local aspell dictionary and replacements if they exist
     "--dict" (eval (expand-file-name ispell-personal-dictionary))
     "--replace" (eval (expand-file-name "~/.aspell.en.prepl"))
     "--ignore" "lt:en:MORFOLOGIK_RULE_EN_US,lt:en:WORD_CONTAINS_UNDERSCORE"
     ;; Using source ensures that a single temporary file in a different dir is created
     ;; such that textidote won't process other files. This serves as a hacky workaround for
     ;; https://github.com/sylvainhalle/textidote/issues/200.
     source)
    :error-patterns
    ((warning line-start (file-name)
              "(L" line "C" column "-" (or (seq "L" end-line "C" end-column) "?") "): "
              (message (one-or-more (not "\""))) (one-or-more not-newline) line-end)))

  (add-to-list 'flycheck-checkers 'tex-textidote)

  ;; https://github.com/flycheck/flycheck/issues/1833
  (add-to-list 'flycheck-hooks-alist '(after-revert-hook . flycheck-buffer))

  ;; Exclude directories and files from being checked
  ;; https://github.com/flycheck/flycheck/issues/1745

  (defvar sb/excluded-directory-regexps
    '(".git" "elpa" ".cache" ".clangd"))

  (defun sb/flycheck-may-check-automatically (&rest _conditions)
    (or (null buffer-file-name)
        (let ((bufname (file-truename buffer-file-name)))
          (not (seq-some (lambda (re) (string-match-p re bufname))
                         sb/excluded-directory-regexps)))))

  (advice-add 'flycheck-may-check-automatically :after-while #'sb/flycheck-may-check-automatically)

  ;; Chain flycheck checkers with lsp. We prefer to use per-project directory local variables
  ;; instead of defining here.
  ;; https://github.com/flycheck/flycheck/issues/1762

  (defvar-local sb/flycheck-local-checkers nil)

  (defun sb/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker sb/flycheck-local-checkers))
        (funcall fn checker property)))

  (advice-add 'flycheck-checker-get :around 'sb/flycheck-checker-get)

  ;; (add-hook 'lsp-managed-mode-hook
  ;;           (lambda ()
  ;;             (when (derived-mode-p 'python-mode)
  ;;               (setq sb/flycheck-local-checkers
  ;;                     '((lsp . ((next-checkers . (python-pylint)))))))))

  (with-eval-after-load "counsel"
    (bind-key "C-c ! !" #'counsel-flycheck flycheck-mode-map)))

;; Showing error messages in the echo area is less intrusive, so the following packages are
;; disabled.

;; (use-package flycheck-popup-tip ; Show error messages in popups
;;   :disabled t
;;   :unless (display-graphic-p)
;;   :hook
;;   (flycheck-mode-hook . flycheck-popup-tip-mode))

;; Does not display popup under TTY, check possible workarounds at
;; https://github.com/flycheck/flycheck-popup-tip
;; (use-package flycheck-pos-tip
;;   :disabled t
;;   :if (display-graphic-p)
;;   :hook
;;   (flycheck-mode-hook . flycheck-pos-tip-mode))

;; Showing errors/warnings in a posframe seems more intrusive than showing errors in the minibuffer
;; (use-package flycheck-posframe
;;   :if (display-graphic-p)
;;   :disabled t
;;   :commands (flycheck-posframe-mode flycheck-posframe-configure-pretty-defaults)
;;   :hook
;;   (flycheck-mode-hook . flycheck-posframe-mode)
;;   :custom
;;   (flycheck-posframe-position 'point-bottom-left-corner)
;;   (flycheck-posframe-border-width 1)
;;   :config
;;   (flycheck-posframe-configure-pretty-defaults)
;;   ;; Do not display popups if company is open
;;   (with-eval-after-load "company"
;;     (declare-function company--active-p "company")

;;     (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p)))

;; `aphelia' allows for formatting via a background process, supports Tramp, and does not move the
;; point after formatting.

;; Use for major modes which do not provide a formatter.
(use-package format-all
  :commands
  (format-all-buffer)
  :hook
  ((format-all-mode-hook . format-all-ensure-formatter)
   ;; Formatting LaTeX files with latexindent is very slow
   ((web-mode-hook markdown-mode-hook) . format-all-mode))
  :custom
  (format-all-formatters '(("BibTeX" Emacs)
                           ("C" clang-format)
                           ("C++" clang-format)
                           ("Cuda" clang-format)
                           ("Emacs Lisp" emacs-lisp)
                           ("HTML" tidy)
                           ("LaTeX" latexindent)
                           ("Markdown" prettier)
                           ("Perl" perltidy)
                           ("Python" (yapf "--style" "file") isort)
                           ("Shell script" shfmt)
                           ("YAML" prettier)))
  :diminish)

;; ;; Enable using ".dir-locals.el" file
;; (use-package editorconfig
;;   :if (executable-find "editorconfig")
;;   :init
;;   (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)
;;   ;; :hook
;;   ;; (prog-mode-hook . editorconfig-mode)
;;   :config
;;   (add-to-list 'editorconfig-indentation-alist
;;                '(json-mode js-indent-level json-reformat:indent-width))
;;   (add-to-list 'editorconfig-indentation-alist
;;                '(nxml-mode nxml-child-indent nxml-attribute-indent)))

;; The advantage with `flycheck-grammarly' over `lsp-grammarly' is that you need not set up lsp
;; support, so you can use it anywhere. But `flycheck-grammarly' does not support a PRO Grammarly
;; account. We only need this package for checking text in "*scratch*" buffer.
(use-package flycheck-grammarly
  :after flycheck
  :defines flycheck-grammarly-check-time
  :demand t
  :config
  (setq flycheck-grammarly-check-time 3
        ;; Remove from the beginning of the list `flycheck-checkers' and append to the end
        flycheck-checkers (delete 'grammarly flycheck-checkers))

  (add-to-list 'flycheck-checkers 'grammarly t))

;; https://languagetool.org/download/LanguageTool-stable.zip
;; The "languagetool" folder should include all files in addition to the ".jar" files.
(use-package langtool
  :defines
  (languagetool-java-arguments languagetool-console-command languagetool-server-command)
  :commands
  (langtool-check langtool-check-done
                  langtool-show-message-at-point langtool-correct-buffer)
  :init
  (setq langtool-default-language "en-US"
        languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command (no-littering-expand-etc-file-name
                                      "languagetool/languagetool-commandline.jar")
        languagetool-server-command (no-littering-expand-etc-file-name
                                     "languagetool/languagetool-server.jar")
        langtool-language-tool-jar (no-littering-expand-etc-file-name
                                    "languagetool/languagetool-commandline.jar")
        langtool-disabled-rules '("MORFOLOGIK_RULE_EN_US"
                                  ;; "WHITESPACE_RULE"
                                  ;; "EN_QUOTES"
                                  ;; "DASH_RULE"
                                  ;; "OXFORD_SPELLING_ISE_VERBS"
                                  ;; "OXFORD_SPELLING_NOUNS")
                                  )))

;; https://languagetool.org/download/LanguageTool-stable.zip
;; The "languagetool" folder should include all files in addition to the ".jar" files.
(use-package flycheck-languagetool
  :after flycheck
  :defines (flycheck-languagetool-commandline-jar flycheck-languagetool-check-time)
  :demand t
  :init
  (setq flycheck-languagetool-server-jar (no-littering-expand-etc-file-name
                                          "languagetool/languagetool-server.jar")
        flycheck-checkers (delete 'languagetool flycheck-checkers))

  (add-to-list 'flycheck-checkers 'languagetool t))

(use-package consult-flycheck
  :after (flycheck consult)
  :bind
  (:map flycheck-command-map
        ("!" . consult-flycheck)))

;; Most likely, `text', `org', `markdown', and `latex' files will be in directories that can use LSP
;; support via `lsp-grammarly' and `lsp-ltex'. We need to enable `flycheck' support for the
;; "*scratch*" buffer which is in `text-mode'.
(add-hook 'text-mode-hook
          (lambda ()
            (when (string= (buffer-name) "*scratch*")
              (if (featurep 'flycheck-grammarly)
                  (progn
                    (flycheck-select-checker 'grammarly)
                    (when (featurep 'flycheck-languagetool)
                      (flycheck-add-next-checker 'grammarly 'languagetool)))
                (when (featurep 'flycheck-languagetool)
                  (flycheck-select-checker 'languagetool))))))

;; (use-package clang-format
;;   :if (executable-find "clang-format")
;;   :after (mlir-mode)
;;   :commands
;;   (clang-format clang-format-buffer clang-format-region)
;;   :custom
;;   (clang-format-style "file")
;;   (clang-format-style-option "{BasedOnStyle: LLVM, IndentWidth: 2, ColumnLimit: 100}"))

;; (use-package clang-format+
;;   :if (executable-find "clang-format")
;;   :defines clang-format+-always-enable
;;   :hook
;;   (mlir-mode-hook . clang-format+-mode)
;;   :custom
;;   (clang-format+-always-enable t))

(use-package highlight-indentation
  :hook
  ((yaml-mode-hook python-mode-hook) . highlight-indentation-mode)
  :diminish (highlight-indentation-current-column-mode highlight-indentation-mode))

;; Claims to be better than `electric-indent-mode'. But we are now using `format-all'.

;; Only indents and does not reformat the whole file.
;; (use-package aggressive-indent
;;   :hook
;;   (emacs-lisp-mode-hook . aggressive-indent-mode)
;;   :custom
;;   (aggressive-indent-comments-too t)
;;   ;; Never use `electric-indent-mode'
;;   (aggressive-indent-dont-electric-modes t)
;;   :diminish)

;; format-all-the-code just runs Emacs' built-in `indent-region' for `emacs-lisp'.

;; (use-package elisp-autofmt
;;   :commands (elisp-autofmt-mode elisp-autofmt-buffer)
;;   :hook (emacs-lisp-mode-hook . elisp-autofmt-mode))

(provide 'init-checkers)

;;; init-checkers.el ends here
