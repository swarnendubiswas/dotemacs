;;; init-checkers.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/modeline-theme)
(defvar sb/user-home-directory)
(defvar sb/textlint-directory)
(defvar sb/minibuffer-completion)

(declare-function lsp-register-client "lsp-mode")
(declare-function make-lsp-client "lsp-mode")
(declare-function f-dirname "f")

;; Identify weasel words, passive voice, and duplicate words. The module does not check grammar and
;; checks only the writing style. `textlint' includes `writegood'.
(use-package writegood-mode
  :commands (writegood-passive-voice-turn-off)
  :diminish
  :hook (text-mode-hook . writegood-mode))

(use-package flycheck
  :commands (flycheck-add-next-checker flycheck-next-checker
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
  :hook (after-init-hook . global-flycheck-mode)
  :custom
  ;; Remove newline checks, since they would trigger an immediate check when we want the
  ;; `flycheck-idle-change-delay' to be in effect while editing.
  (flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change))
  (flycheck-checker-error-threshold 1500)
  (flycheck-idle-buffer-switch-delay 2 "Increase the time (s) to allow for quick transitions")
  (flycheck-idle-change-delay 2 "Increase the time (s) to allow for transient edits")
  ;; Show error messages only if the error list is not already visible
  ;; (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; There are no checkers for `csv-mode', and many program modes use lsp. `yaml-mode' is
  ;; derived from `text-mode'.
  (flycheck-global-modes '(not csv-mode))
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
                flycheck-shellcheck-follow-sources nil
                flycheck-textlint-config (expand-file-name "textlintrc.json" sb/textlint-directory)
                flycheck-textlint-executable (expand-file-name "node_modules/.bin/textlint"
                                                               sb/textlint-directory))

  (add-to-list 'flycheck-textlint-plugin-alist '(tex-mode . "latex"))
  (add-to-list 'flycheck-textlint-plugin-alist '(rst-mode . "rst"))

  ;; Add support for `org-lint' as a checker
  (defconst flycheck-org-lint-form
    (flycheck-prepare-emacs-lisp-form
      (require 'org)
      (require 'org-attach)
      (let ((source (car command-line-args-left))
            (process-default-directory default-directory))
        (with-temp-buffer
          (insert-file-contents source 'visit)
          (setq buffer-file-name source)
          (setq default-directory process-default-directory)
          (delay-mode-hooks (org-mode))
          (setq delayed-mode-hooks nil)
          (dolist (err (org-lint))
            (let ((inf (cl-second err)))
              (princ (elt inf 0))
              (princ ": ")
              (princ (elt inf 2))
              (terpri)))))))

  (defconst flycheck-org-lint-variables
    '(org-directory
      org-id-locations
      org-id-locations-file
      org-attach-id-dir
      org-attach-use-inheritance
      org-attach-id-to-path-function-list)
    "Variables inherited by the `org-lint' subprocess.")

  (defun flycheck-org-lint-variables-form ()
    (require 'org-attach)  ; Needed to make variables available
    `(progn
       ,@(seq-map (lambda (opt) `(setq-default ,opt ',(symbol-value opt)))
                  (seq-filter #'boundp flycheck-org-lint-variables))))

  (flycheck-define-checker org-lint
    "Org buffer checker using `org-lint'."
    :command ("emacs" (eval flycheck-emacs-args)
              "--eval" (eval (concat "(add-to-list 'load-path \""
                                     (file-name-directory (locate-library "org"))
                                     "\")"))
              "--eval" (eval (flycheck-sexp-to-string
                              (flycheck-org-lint-variables-form)))
              "--eval" (eval flycheck-org-lint-form)
              "--" source)
    :error-patterns
    ((error line-start line ": " (message) line-end))
    :modes (org-mode))

  (add-to-list 'flycheck-checkers 'org-lint t)

  ;; https://github.com/flycheck/flycheck/issues/1833
  (add-to-list 'flycheck-hooks-alist '(after-revert-hook . flycheck-buffer))

  ;; Exclude directories and files from being checked
  ;; https://github.com/flycheck/flycheck/issues/1745

  (defvar sb/excluded-directory-regexps
    '(".git/" "elpa/" ".cache" ".clangd"))

  (defun sb/flycheck-may-check-automatically (&rest _conditions)
    (or (null buffer-file-name)
        (let ((bufname (file-truename buffer-file-name)))
          (not (seq-some (lambda (re) (string-match-p re bufname))
                         sb/excluded-directory-regexps)))))

  (advice-add 'flycheck-may-check-automatically
              :after-while #'sb/flycheck-may-check-automatically)

  ;; Chain flycheck checkers with lsp-mode.
  ;; https://github.com/flycheck/flycheck/issues/1762

  (defvar-local sb/flycheck-local-checkers nil)

  (defun sb/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker sb/flycheck-local-checkers))
        (funcall fn checker property)))

  (advice-add 'flycheck-checker-get :around 'sb/flycheck-checker-get)

  ;; We prefer to use per-project directory local variables.

  ;; (add-hook 'lsp-managed-mode-hook
  ;;           (lambda ()
  ;;             (when (derived-mode-p 'python-mode)
  ;;               (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (python-pylint)))))))
  ;;             ))
  )

;; Showing error messages in the echo area is less intrusive.
(use-package flycheck-popup-tip ; Show error messages in popups
  :disabled t
  :unless (display-graphic-p)
  :hook (flycheck-mode-hook . flycheck-popup-tip-mode))

;; Does not display popup under TTY, check possible workarounds at
;; https://github.com/flycheck/flycheck-popup-tip
(use-package flycheck-pos-tip
  :disabled t
  :if (display-graphic-p)
  :hook (flycheck-mode-hook . flycheck-pos-tip-mode))

;; Showing errors/warnings in a posframe seems more intrusive than showing errors in the minibuffer
(use-package flycheck-posframe
  :if (display-graphic-p)
  :disabled t
  :commands (flycheck-posframe-mode flycheck-posframe-configure-pretty-defaults)
  :hook (flycheck-mode-hook . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-position 'point-bottom-left-corner)
  (flycheck-posframe-border-width 1)
  :config
  (flycheck-posframe-configure-pretty-defaults)
  ;; Do not display popups if company is open
  (with-eval-after-load "company"
    (declare-function company--active-p "company")

    (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p)))

;; Use for major modes which do not provide a formatter. `aphelia' allows for formatting via a
;; background process but does not support Tramp and supports fewer formatters.
(use-package format-all
  :commands (format-all-buffer)
  :diminish
  :preface
  (defun sb/enable-format-all ()
    "Delay enabling format-all to avoid slowing down Emacs startup."
    (dolist (hook '(bazel-mode-hook LaTeX-mode-hook web-mode-hook
                                    markdown-mode-hook emacs-lisp-mode-hook))
      (add-hook hook #'format-all-mode))
    (add-hook 'format-all-mode-hook #'format-all-ensure-formatter))
  :diminish
  :hook
  ((format-all-mode-hook . format-all-ensure-formatter)
   ((bazel-mode-hook LaTeX-mode-hook web-mode-hook lisp-data-mode-hook
                     markdown-mode-hook emacs-lisp-mode-hook) . format-all-mode)))

(use-package editorconfig
  :if (executable-find "editorconfig")
  :commands editorconfig-mode)

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
  :defines (languagetool-java-arguments languagetool-console-command languagetool-server-command)
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :init
  (setq langtool-default-language "en-US"
        languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command (no-littering-expand-etc-file-name
                                      "languagetool/languagetool-commandline.jar")
        languagetool-server-command (no-littering-expand-etc-file-name
                                     "languagetool/languagetool-server.jar")
        langtool-language-tool-jar (no-littering-expand-etc-file-name
                                    "languagetool/languagetool-commandline.jar")))

;; https://languagetool.org/download/LanguageTool-stable.zip
;; The "languagetool" folder should include all files in addition to the ".jar" files.
(use-package flycheck-languagetool
  :after flycheck
  :defines (flycheck-languagetool-commandline-jar flycheck-languagetool-check-time)
  :demand t
  :init
  (setq flycheck-languagetool-server-jar (no-littering-expand-etc-file-name
                                          "languagetool/languagetool-server.jar")
        flycheck-checkers (delete 'languagetool flycheck-checkers)
        flycheck-languagetool-check-time 3)

  (add-to-list 'flycheck-checkers 'languagetool t))

;; We need to enable lsp workspace to allow `lsp-grammarly' to work, which makes it ineffective for
;; temporary text files. However, `lsp-grammarly' supports PRO Grammarly accounts. If there are
;; failures, then try logging out of Grammarly and logging in again. Make sure to run "M-x
;; keytar-install".
(use-package lsp-grammarly
  :defines (lsp-grammarly-active-modes lsp-grammarly-user-words)
  :commands (lsp-grammarly--server-command lsp-grammarly--init
                                           lsp-grammarly--get-credentials lsp-grammarly--get-token
                                           lsp-grammarly--store-token lsp-grammarly--show-error
                                           lsp-grammarly--update-document-state)
  :hook
  ((text-mode-hook markdown-mode-hook org-mode-hook LaTeX-mode-hook) .
   (lambda ()
     (require 'lsp-grammarly)
     (lsp-deferred)))
  :custom
  (lsp-grammarly-suggestions-oxford-comma t)
  (lsp-grammarly-suggestions-passive-voice t)
  :config
  (defvar lsp-grammarly-active-modes)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection #'lsp-grammarly--server-command)
    :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-grammarly-active-modes))
    :priority -1
    :remote? t
    :add-on? t
    :server-id 'grammarly-r
    :download-server-fn (lambda (_client callback error-callback _update?)
                          (lsp-package-ensure 'grammarly-ls callback error-callback))
    :after-open-fn #'lsp-grammarly--init
    :async-request-handlers
    (ht ("$/getCredentials" #'lsp-grammarly--get-credentials)
        ("$/getToken" #'lsp-grammarly--get-token)
        ("$/storeToken" #'lsp-grammarly--store-token)
        ("$/showError" #'lsp-grammarly--show-error)
        ("$/updateDocumentState" #'lsp-grammarly--update-document-state)))))

(use-package lsp-ltex
  :defines (lsp-ltex-enabled lsp-ltex-check-frequency lsp-ltex-dictionary lsp-ltex-java-path)
  :commands (lsp-ltex--downloaded-extension-path lsp-ltex--execute)
  :hook
  ((text-mode-hook markdown-mode-hook org-mode-hook LaTeX-mode-hook) .
   (lambda ()
     (require 'lsp-ltex)
     (lsp-deferred)))
  :init
  (setq lsp-ltex-check-frequency "save"
        ;; lsp-ltex-dictionary ("microbenchmarks")
        lsp-ltex-java-path "/usr/lib/jvm/java-11-openjdk-amd64"
        lsp-ltex-version "15.2.0")
  :config
  ;; https://github.com/ggbaker/doom-emacs-config/blob/f977ee6f33ef2d19b577e38a81b32af43ced6df5/config.el
  ;; Disable spell checking since we cannot get `lsp-ltex' to work with custom dict words
  (setq lsp-ltex-disabled-rules
        #s(hash-table size 30 data
                      ("en-US" ["MORFOLOGIK_RULE_EN_US"])
                      ("en-US" ["WHITESPACE_RULE"])))

  (defvar lsp-ltex-active-modes)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     "/home/swarnendu/.emacs.d/var/lsp/server/ltex-ls/latest/bin/ltex-ls")
    :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-ltex-active-modes))
    :priority -2
    :add-on? t
    :remote? t
    :server-id 'ltex-r
    :download-server-fn
    (lambda (_client _callback error-callback _update?)
      (lsp-package-ensure
       'ltex-ls
       (lambda ()
         (let ((dest (f-dirname (lsp-ltex--downloaded-extension-path))))
           (unless (lsp-ltex--execute "tar" "-xvzf" (lsp-ltex--downloaded-extension-path)
                                      "-C" dest)
             (error "Error during the unzip process: tar"))))
       error-callback)))))

(use-package consult-flycheck
  :if (eq sb/minibuffer-completion 'vertico)
  :after (flycheck)
  :bind
  (:map flycheck-command-map
        ("!" . consult-flycheck)))

(when (eq sb/minibuffer-completion 'ivy)
  (with-eval-after-load "counsel"
    (with-eval-after-load "flycheck"
      (bind-key "C-c ! !" #'counsel-flycheck flycheck-mode-map))))

;; Most likely, `org', `markdown', and `latex' files will be in directories that can use LSP
;; support. We need to enable `flycheck' support for the "*scratch*" buffer which is in `text-mode'.

;; org -> grammarly -> languagetool
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (flycheck-select-checker 'org-lint)
;;             (when (featurep 'flycheck-grammarly)
;;               (flycheck-add-next-checker 'org-lint 'grammarly))
;;             (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
;;               (flycheck-add-next-checker 'grammarly 'languagetool))
;;             (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
;;               (flycheck-add-next-checker 'org-lint 'languagetool))))

;; We only limit to "*scratch*" buffer since we can use `grammarly' and `ltex' for directories.
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

;; `markdown-mode' is derived from `text-mode'
;; markdown-markdownlint-cli -> grammarly -> languagetool
;; (add-hook 'markdown-mode-hook
;;           (lambda()
;;             (flycheck-select-checker 'markdown-markdownlint-cli)
;;             (when (featurep 'flycheck-grammarly)
;;               ;; (make-local-variable 'flycheck-error-list-minimum-level)
;;               ;; (setq flycheck-error-list-minimum-level 'warning
;;               ;;       flycheck-navigation-minimum-level 'warning)
;;               ;; (flycheck-add-next-checker 'markdown-markdownlint-cli '(warning . grammarly) 'append)
;;               (flycheck-add-next-checker 'markdown-markdownlint-cli 'grammarly))
;;             ;; (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
;;             ;;   (flycheck-add-next-checker 'grammarly 'languagetool))
;;             ;; (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
;;             ;;   (flycheck-add-next-checker 'markdown-markdownlint-cli 'languagetool))
;;             ))

;; (dolist (hook '(LaTex-mode-hook latex-mode-hook))
;;   (add-hook hook (lambda ()
;;                    (flycheck-select-checker 'tex-chktex)
;;                    (when (featurep 'flycheck-grammarly)
;;                      (flycheck-add-next-checker 'tex-chktex 'grammarly))
;;                    (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
;;                      (flycheck-add-next-checker 'grammarly 'languagetool))
;;                    (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
;;                      (flycheck-add-next-checker 'tex-chktex 'languagetool)))))

(provide 'init-checkers)

;;; init-checkers.el ends here
