;;; init-completion.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

;; Replace `dabbrev-exp' with `hippie-expand'. Use "C-M-/" for `dabbrev-completion' which finds all
;; expansions in the current buffer and presents suggestions for completion.
(use-package hippie-exp
  :straight (:type built-in)
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol))
  (hippie-expand-verbose nil)
  :bind
  (("M-/"   . hippie-expand)
   ("C-M-/" . dabbrev-completion)))

(use-package ivy
  :straight t
  :functions ivy-format-function-line
  :commands (ivy-read ivy-mode)
  :if (eq sb/minibuffer-completion 'ivy)
  :preface
  ;; https://github.com/abo-abo/swiper/wiki/Hiding-dired-buffers
  (defun sb/ignore-dired-buffers (str)
    "Return non-nil if STR names a Dired buffer.
  This function is intended for use with `ivy-ignore-buffers'."
    (let ((buf (get-buffer str)))
      (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))
  :hook (after-init-hook . ivy-mode)
  :config
  (setq ivy-count-format "(%d/%d) " ; Helps identify wrap around
        ivy-extra-directories nil ; Hide . and ..
        ivy-fixed-height-minibuffer t ; Distracting if the height keeps changing
        ivy-height 12
        ;; Make the height of the minibuffer proportionate to the screen
        ;; ivy-height-alist '((t
        ;;                      lambda (_caller)
        ;;                      (/ (frame-height) 2)))
        ;; We update `ivy-re-builders-alist' after loading `orderless'
        ;; ivy-re-builders-alist '((counsel-M-x       . ivy--regex-fuzzy)
        ;;                         (counsel-find-file . ivy--regex-fuzzy)
        ;;                         (t                 . ivy--regex-ignore-order))
        ivy-truncate-lines nil ; `counsel-flycheck' output gets truncated
        ivy-wrap t
        ;; Do not start searches with ^
        ivy-initial-inputs-alist nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; The default sorter is much to slow and the default for `ivy-sort-max-size'
        ;; is way too big (30,000). Turn it down so big repos affect project
        ;; navigation less.
        ivy-sort-max-size 7500)

  (dolist (buffer
           '("TAGS" "magit-process" "*emacs*" "*xref*"
             ;; "*eldoc for use-package*" "^\\*Help\\*$" "^\\*Ibuffer\\*$" "*Warnings*"
             ;; "^\\*Compile-Log\\*$" "^\\*.+Completions\\*$" "^\\*Backtrace\\*$"
             ;; "*flycheck-posframe-buffer*" "^\\*prettier" "^\\*json*" "^\\*texlab*"
             ;; "^\\*clangd*" "^\\*shfmt*" "*company-documentation*"
             ))
    (add-to-list 'ivy-ignore-buffers buffer))

  ;; Ignore `dired' buffers from `ivy-switch-buffer'
  ;; (add-to-list 'ivy-ignore-buffers #'sb/ignore-dired-buffers)
  :diminish
  :bind
  (("C-c r"    . ivy-resume)
   ("<f3>"     . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("<return>" . ivy-alt-done) ; Continue completion
   ("<left>"   . ivy-previous-line)
   ("<right>"  . ivy-next-line)))

(use-package counsel
  :straight amx ; `counsel' makes use of `amx' if installed
  :straight t
  :if (eq sb/minibuffer-completion 'ivy)
  :commands counsel-mode
  :preface
  ;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
  (defun sb/counsel-goto-recent-directory ()
    "Open recent directories with `dired'."
    (interactive)
    (unless recentf-mode (recentf-mode 1))
    (let ((collection
           (delete-dups
            (append (mapcar 'file-name-directory recentf-list)
                    (if (executable-find "fasd")
                        (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
      (ivy-read "Directories:" collection :action 'dired)))
  :bind
  (;; Counsel can use the sorting from `amx' or `smex' for `counsel-M-x'.
   ([remap execute-extended-command] . counsel-M-x)
   ([remap completion-at-point]      . counsel-company)
   ("C-M-i"                          . counsel-company)
   ([remap find-file]                . counsel-find-file)
   ;; `counsel-flycheck' shows less information than `flycheck-list-errors'
   ;; ([remap flycheck-list-errors]  . counsel-flycheck)
   ("<f1>"                           . counsel-M-x)
   ("<f2>"                           . counsel-find-file)
   ("C-c s g"                        . counsel-git-grep)
   ("C-<f9>"                         . sb/counsel-goto-recent-directory)
   ("C-c d m"                        . counsel-minor)
   ("<f9>"                           . counsel-recentf)
   ("C-c s r"                        . counsel-rg)
   ("C-c C-m"                        . counsel-mark-ring)
   ;; Enabling preview can make switching over remote buffers slow
   ("S-<f3>"                         . counsel-switch-buffer)
   ("<f4>"                           . counsel-grep-or-swiper))
  :bind* ("C-c C-j"                  . counsel-imenu)
  :diminish
  :hook (ivy-mode-hook . counsel-mode)
  :config
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable
        counsel-find-file-at-point t
        counsel-find-file-ignore-regexp (concat
                                         "\\(?:\\`[#.]\\)"
                                         "\\|\\(?:\\`.+?[#~]\\'\\)"
                                         "\\|.cb$"
                                         "\\|.cb2$"
                                         "\\|.class$"
                                         "\\|.djvu$"
                                         "\\|.doc$"
                                         "\\|.docx$"
                                         "\\|.elc$"
                                         "\\|.fdb_latexmk$"
                                         "\\|.fls$"
                                         "\\|.lof$"
                                         "\\|.lot$"
                                         "\\|.o$"
                                         "\\|.ppt$"
                                         "\\|.pptx$"
                                         "\\|.pyc$"
                                         "\\|.rel$"
                                         "\\|.rip$"
                                         "\\|.so$"
                                         "\\|.synctex$"
                                         "\\|.synctex.gz$"
                                         "\\|.toc$"
                                         "\\|.xls$"
                                         "\\|.xlsx$"
                                         "\\|tags"
                                         "\\|TAGS"
                                         "\\|GPATH"
                                         "\\|GRTAGS"
                                         "\\|GTAGS"
                                         "\\|tramp"
                                         "\\|.clangd"
                                         "\\|.cache"
                                         "\\|.metadata"
                                         "\\|.recommenders"
                                         "\\|typings"
                                         "\\|__pycache__")
        counsel-mode-override-describe-bindings t
        counsel-preselect-current-file t
        counsel-switch-buffer-preview-virtual-buffers nil ; Removes recent files and bookmarks
        counsel-yank-pop-preselect-last t
        counsel-yank-pop-separator "\n------------------------------------------\n"))

;; Enable before `ivy-rich-mode' for better performance. The new transformers (file permissions)
;; seem an overkill, and it hides long file names.
(use-package all-the-icons-ivy-rich
  :straight ivy-rich
  :straight t
  :commands all-the-icons-ivy-rich-mode
  :if (display-graphic-p)
  :hook (ivy-mode-hook . all-the-icons-ivy-rich-mode)
  :custom (all-the-icons-ivy-rich-icon-size 0.9)
  :config
  (plist-put all-the-icons-ivy-rich-display-transformers-list
             'counsel-recentf
             '(:columns
               ((all-the-icons-ivy-rich-file-icon)
                (all-the-icons-ivy-rich-file-name (:width 0.5))
                (all-the-icons-ivy-rich-file-id (:width 15
                                                        :face all-the-icons-ivy-rich-file-owner-face
                                                        :align right)))
               :delimiter "\t"))

  (plist-put all-the-icons-ivy-rich-display-transformers-list
             'counsel-find-file
             '(:columns
               ((all-the-icons-ivy-rich-file-icon)
                (all-the-icons-ivy-rich-file-name (:width 0.4))
                (all-the-icons-ivy-rich-file-id (:width 15
                                                        :face all-the-icons-ivy-rich-file-owner-face
                                                        :align right)))
               :delimiter "\t"))

  (plist-put
   all-the-icons-ivy-rich-display-transformers-list
   'ivy-switch-buffer
   '(:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-rich-candidate (:width 30))
      (ivy-rich-switch-buffer-indicators (:width 4
                                                 :face all-the-icons-ivy-rich-indicator-face
                                                 :align right))
      (all-the-icons-ivy-rich-switch-buffer-major-mode
       (:width 18 :face all-the-icons-ivy-rich-major-mode-face))
      (ivy-rich-switch-buffer-project (:width 0.12 :face all-the-icons-ivy-rich-project-face))
      (ivy-rich-switch-buffer-path (:width
                                    (lambda (x)
                                      (ivy-rich-switch-buffer-shorten-path
                                       x
                                       (ivy-rich-minibuffer-width 0.3)))
                                    :face all-the-icons-ivy-rich-path-face)))
     :predicate (lambda (cand) (get-buffer cand))
     :delimiter "\t")))

(use-package ivy-rich
  :straight t
  :commands (ivy-rich-modify-column ivy-rich-set-columns ivy-rich-modify-columns)
  :after (ivy counsel) ; We do not enable `all-the-icons-ivy-rich' in TUI mode
  :preface
  ;; Adapted from
  ;; https://github.com/tshu-w/.emacs.d/blob/master/lisp/editor-completion.el
  (defun sb/ivy-rich-file-size (candidate)
    "Displays the file size of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let ((size (file-attribute-size (file-attributes candidate))))
          (cond
           ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
           ((> size 1000) (format "%.1fk " (/ size 1000.0)))
           (t (format "%d " size)))))))

  (defun sb/ivy-rich-file-user (candidate)
    "Displays the file user of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((user-id (file-attribute-user-id (file-attributes candidate)))
               (user-name (user-login-name user-id)))
          (format "%s" user-name)))))
  :init (ivy-rich-mode 1)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (ivy-rich-project-root-cache-mode 1)

  ;; (if (display-graphic-p)
  ;;     (ivy-rich-set-columns 'counsel-find-file
  ;;                           '((all-the-icons-ivy-rich-file-icon)
  ;;                             (ivy-rich-candidate    (:width 0.70))
  ;;                             (sb/ivy-rich-file-size (:width 10 :align right
  ;;                                                            :face font-lock-doc-face))))
  ;;   (ivy-rich-set-columns 'counsel-find-file
  ;;                         '((ivy-rich-candidate    (:width 0.70))
  ;;                           (sb/ivy-rich-file-size (:width 10 :align right
  ;;                                                          :face font-lock-doc-face)))))

  ;; ;; Increase the width to see the major mode clearly
  ;; (ivy-rich-modify-columns 'ivy-switch-buffer
  ;;                          '((ivy-rich-switch-buffer-size (:align right))
  ;;                            (ivy-rich-switch-buffer-major-mode (:width 16 :face error))
  ;;                            (ivy-rich-switch-buffer-project (:width 0.24 :face success))))

  ;; (ivy-rich-set-columns 'counsel-recentf
  ;;                       '((file-name-nondirectory (:width 0.24))
  ;;                         (ivy-rich-candidate (:width 0.75))))
  )

;; https://kristofferbalintona.me/posts/vertico-marginalia-all-the-icons-completion-and-orderless/
(use-package vertico
  :straight t
  :if (eq sb/minibuffer-completion 'vertico)
  :defines read-extended-command-predicate
  :commands command-completion-default-include-p
  :hook (after-init-hook . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 12)
  (vertico-scroll-margin 4)
  :config
  ;; Hide commands in "M-x" in Emacs 28 which do not work in the current mode. Vertico commands are
  ;; hidden in normal buffers.
  (when sb/EMACS28+
    (setq read-extended-command-predicate #'command-completion-default-include-p))
  :bind
  (("<f2>" .  find-file)
   :map vertico-map
   ("<escape>" . minibuffer-keyboard-quit)
   ("?" . minibuffer-completion-help)
   ("M-RET" . minibuffer-force-complete-and-exit)
   ("M-TAB" . minibuffer-complete)))

;; More convenient directory navigation commands
(use-package vertico-directory
  :after vertico
  :straight nil
  :load-path "extras"
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))

(use-package vertico-repeat
  :after vertico
  :straight nil
  :load-path "extras"
  :hook (minibuffer-setup-hook . vertico-repeat-save)
  :bind
  (("C-c r" . vertico-repeat-last)
   ("M-r" . vertico-repeat-select)))

(use-package vertico-indexed
  :after vertico
  :straight nil
  :load-path "extras"
  :commands vertico-indexed-mode
  :init (vertico-indexed-mode 1))

(use-package vertico-quick
  :after vertico
  :straight nil
  :bind
  (:map vertico-map
        ("C-c q" . vertico-quick-insert)
        ("C-'" . vertico-quick-exit)))

(use-package consult
  :straight t
  :if (eq sb/minibuffer-completion 'vertico)
  :commands consult--customize-put
  :custom
  (consult-line-start-from-top t "Start search from the beginning")
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-project-function #'projectile-project-root)
  (consult-line-numbers-widen t)
  :bind
  (("C-x M-:" . consult-complex-command)
   ([remap repeat-complex-command] . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("<f3>" . consult-buffer)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap bookmark-jump] . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ("M-y" . consult-yank-pop)
   ([remap yank-pop] . consult-yank-pop)
   ([remap apropos] . consult-apropos)
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ([remap goto-line] . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("C-c C-j" . consult-imenu)
   ([remap imenu] . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ([remap load-theme] . consult-theme)
   ;; M-s bindings (search-map)
   ("M-s f" . consult-find)
   ([remap locate] . consult-locate)
   ("M-s l" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("<f4>" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ("<f9>" . consult-recent-file)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap multi-occur] . consult-multi-occur)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   )
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :config
  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; TODO: Is this what is causing issues with latex?
  (unless (display-graphic-p)
    (setq completion-in-region-function #'consult-completion-in-region))

  ;; Disable live preview
  (consult-customize
   consult-recent-file consult-buffer
   :preview-key nil))

;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :after vertico
  :straight t
  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command
        which-key-use-C-h-commands nil)
  :bind
  (([remap describe-bindings] . embark-bindings)
   :map vertico-map
   ("C-l" . embark-act)
   ("C-c C-l" . embark-export)))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

;; https://kristofferbalintona.me/posts/corfu-kind-icon-and-corfu-doc/
(use-package corfu
  :straight t
  :if (and (display-graphic-p) (eq sb/capf 'corfu))
  :preface
  (defun sb/corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :hook (after-init-hook . corfu-global-mode)
  :custom
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-auto t "Enable auto completion")
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  (corfu-min-width 60)
  (corfu-max-width corfu-min-width)
  (corfu-count 15)
  (corfu-preselect-first t)
  :bind
  (:map corfu-map
        ([tab] . corfu-next)
        ([backtab] . corfu-previous)
        ("M-m" . sb/corfu-move-to-minibuffer)))

(use-package corfu-doc
  :straight t
  :if (and (display-graphic-p) (eq sb/capf 'corfu))
  :hook (corfu-mode-hook . corfu-doc-mode))

;; https://kristofferbalintona.me/posts/cape/
(use-package cape
  :straight t
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; Complete programming language keyword
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; Complete unicode char from TeX command, e.g. \hbar.
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; Complete abbreviation at point.
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; Complete word from dictionary at point.
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; Complete current line from other lines in buffer.
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol) ; Elisp symbol
  ;; Complete word at point with Ispell.
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; Complete with Dabbrev at point.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :custom
  (cape-dict-file "/home/swarnendu/.config/Code/User/spellright.dict"))

;; We prefer to use "kind-icon" package for icons since it has more active commits but I do not know
;; which is better.
(use-package all-the-icons-completion
  :straight t
  :straight all-the-icons
  :disabled t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

(use-package kind-icon
  :straight t
  :after corfu
  :demand t
  :commands kind-icon-margin-formatter
  :custom
  (kind-icon-face 'corfu-default)
  (kind-icon-default-face 'corfu-default) ; To compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package marginalia
  :straight t
  :after vertico
  :init (marginalia-mode 1))

;; The module does not specify an `autoload'. So we get the following error without the following
;; declaration.
;; "Company backend ’company-capf’ could not be initialized: Autoloading file failed to define
;; function company-capf"
(use-package company-capf
  :straight company
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :commands company-capf)

;; Use "M-x company-diag" or the modeline status to see the backend used. Try "M-x
;; company-complete-common" when there are no completions. Use "C-M-i" for `complete-symbol' with
;; regex search.
(use-package company
  :straight t
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
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
  :hook (after-init-hook . global-company-mode)
  ;; The `company-posframe' completion kind indicator is not great, but we are now using
  ;; `company-fuzzy'.
  :diminish
  :config
  (setq company-dabbrev-downcase nil ; Do not downcase returned candidates
        company-dabbrev-ignore-case nil ; Do not ignore case when collecting completion candidates
        ;; Search in other buffers with the same major mode. This can cause
        ;; performance overhead if there are lots of open buffers.
        company-dabbrev-other-buffers t
        company-ispell-available t
        company-ispell-dictionary (expand-file-name "wordlist.5" sb/extras-directory)
        company-minimum-prefix-length 3 ; Small words can be faster to type
        company-require-match nil ; Allow input string that do not match candidates
        company-selection-wrap-around t
        company-show-quick-access t ; Speed up completion
        ;; Align additional metadata, like type signatures, to the right-hand side
        company-tooltip-align-annotations t
        ;; Disable insertion of arguments
        company-clang-insert-arguments nil
        ;; Start a search using `company-filter-candidates' (bound to "C-s") to narrow out-of-order
        ;; strings
        ;; https://github.com/company-mode/company-mode/discussions/1211
        company-search-regexp-function 'company-search-words-in-any-order-regexp
        company-frontends '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
                            ;; show selected candidate docs in echo area
                            company-echo-metadata-frontend)
        company-backends '(company-capf))

  ;; We set `company-backends' as a local variable, so it is not important to delete backends
  ;; (dolist (backends '(company-semantic company-bbdb company-oddmuse company-cmake company-clang))
  ;;   (delq backends company-backends))

  ;; Ignore matches that consist solely of numbers from `company-dabbrev'
  ;; https://github.com/company-mode/company-mode/issues/358
  (push (apply-partially #'cl-remove-if
                         (lambda (c)
                           (string-match-p "\\`[0-9]+\\'" c)))
        company-transformers)
  :bind
  (:map company-active-map
        ("C-n"      . company-select-next)
        ("C-p"      . company-select-previous)
        ;; Insert the common part of all candidates, or select the next one
        ("<tab>"    . company-complete-common-or-cycle)
        ("C-M-/"    . company-other-backend) ; Was bound to `dabbrev-completion'
        ("<escape>" . company-abort)))

;; Silence "Starting 'look' process..." message
(advice-add 'lookup-words :around #'sb/inhibit-message-call-orig-fun)
;; Hide the "Starting new Ispell process" message
(advice-add 'ispell-init-process :around #'sb/inhibit-message-call-orig-fun)
(advice-add 'ispell-lookup-words :around #'sb/inhibit-message-call-orig-fun)

;; Posframes do not have unaligned rendering issues with variable `:height' unlike an overlay.
;; However, the width of the frame popup is often not enough and the right side gets cut off.
;; https://github.com/company-mode/company-mode/issues/1010
(use-package company-posframe
  :straight t
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :after company
  :demand t
  :commands company-posframe-mode
  :diminish
  :config
  (setq company-posframe-show-metadata nil ; Difficult to distinguish the help text from completions
        company-posframe-show-indicator nil ; Hide the backends, the display is not great
        ;; Disable showing the help frame
        company-posframe-quickhelp-delay nil)
  (company-posframe-mode 1))

(use-package company-quickhelp
  :straight t
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :after company
  :commands company-quickhelp-mode
  ;; :init (run-with-idle-timer 3 nil #'company-quickhelp-mode)
  :hook (after-init-hook . company-quickhelp-mode))

(use-package company-statistics
  :straight t
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :after company
  :demand t
  :commands company-statistics-mode
  :config (company-statistics-mode 1))

;; Nice but slows completions. We should invoke this only at the very end of configuring `company'.
(use-package company-fuzzy
  :straight t
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :straight flx
  :after company
  :diminish (company-fuzzy-mode global-company-fuzzy-mode)
  :commands (global-company-fuzzy-mode company-fuzzy-mode)
  :demand t
  :custom
  (company-fuzzy-sorting-backend 'flx)
  (company-fuzzy-show-annotation nil "The right-hand side gets cut off")
  ;; We should not need this because the `flx' sorting accounts for the prefix
  (company-fuzzy-prefix-on-top t))

(use-package company-shell
  :straight t
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :after (:any sh-mode fish-mode)
  :demand t
  :defines company-shell-delete-duplictes
  :commands (company-shell company-shell-env company-fish-shell)
  :custom (company-shell-delete-duplictes t))

(use-package company-auctex
  :straight t
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :after tex-mode
  :demand t
  :commands (company-auctex-init company-auctex-labels
                                 company-auctex-bibs company-auctex-macros
                                 company-auctex-symbols company-auctex-environments))

(use-package math-symbols
  :straight t
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :after tex-mode
  :demand t) ; Required by `ac-math' and `company-math'

(use-package company-math
  :straight t
  :after tex-mode
  :demand t
  :commands (company-math-symbols-latex company-math-symbols-unicode company-latex-commands))

(use-package company-reftex ; Reftex must be enabled to work
  :straight t
  :after tex-mode
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :demand t
  :commands (company-reftex-labels company-reftex-citations))

(use-package company-bibtex
  :straight t
  :after tex-mode
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :demand t
  :commands company-bibtex)

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

;; LATER: I do not understand the difference between the following two, and the explanation.
;; `(add-to-list 'company-backends '(company-capf company-dabbrev))'
;; `(add-to-list 'company-backends '(company-capf :with company-dabbrev))'

(progn
  (defun sb/company-xml-mode ()
    "Add backends for completion with company."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 3)
    (make-local-variable 'company-backends)

    (setq company-backends '(company-capf
                             company-files
                             company-yasnippet
                             company-dabbrev-code
                             company-dabbrev)))

  (dolist (hook '(nxml-mode-hook))
    (add-hook hook (lambda ()
                     (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                       (sb/company-xml-mode)
                       (company-fuzzy-mode 1)
                       (diminish 'company-fuzzy-mode))))))

(progn
  (defun sb/company-latex-mode ()
    "Add backends for latex completion in company mode."

    (setq-local company-minimum-prefix-length 3)
    (make-local-variable 'company-backends)

    ;; `company-reftex' should be considerably more powerful than `company-auctex' backends for
    ;; labels and citations.

    (setq company-backends '(company-capf
                             company-files
                             company-reftex-citations
                             company-reftex-labels
                             company-auctex-environments
                             company-auctex-macros
                             company-latex-commands
                             company-math-symbols-latex
                             company-math-symbols-unicode
                             company-auctex-symbols
                             company-auctex-bibs
                             company-auctex-labels
                             company-bibtex
                             company-dabbrev
                             company-ispell)))

  (dolist (hook '(latex-mode-hook))
    (add-hook hook (lambda ()
                     (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                       (sb/company-latex-mode))))))

(progn
  (defun sb/company-web-mode ()
    "Add backends for web completion in company mode."

    (make-local-variable 'company-backends)

    (setq company-backends '(company-capf
                             company-files
                             company-yasnippet
                             company-dabbrev
                             company-ispell)))

  (dolist (hook '(web-mode-hook))
    (add-hook hook (lambda ()
                     (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                       (sb/company-web-mode)
                       (company-fuzzy-mode 1)
                       (diminish 'company-fuzzy-mode))))))

(progn
  (defun sb/company-text-mode ()
    "Add backends for text completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    ;; Slightly larger value to have more precise matches and so that the popup does not block
    (setq-local company-minimum-prefix-length 3
                company-transformers '(delete-dups))

    (set (make-local-variable 'company-backends)
         '(company-files
           company-dabbrev
           company-ispell
           company-abbrev)))

  (dolist (hook '(text-mode-hook)) ; Extends to derived modes like `markdown-mode' and `org-mode'
    (add-hook hook (lambda ()
                     (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                       (unless (derived-mode-p 'latex-mode)
                         (sb/company-text-mode)
                         (company-fuzzy-mode 1)
                         (diminish 'company-fuzzy-mode)))))))

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

;; https://emacs.stackexchange.com/questions/19072/company-completion-very-slow
;; `company-clang' is slow
;; (progn
;;   (defun sb/company-c-mode ()
;;     "Add backends for C/C++ completion in company mode."
;;     (defvar company-minimum-prefix-length)
;;     (defvar company-backends)

;;     (setq-local company-minimum-prefix-length 2)
;;     (make-local-variable 'company-backends)

;;     (setq company-backends '(company-capf
;;                              company-dabbrev-code
;;                              company-files
;;                              company-yasnippet
;;                              company-dabbrev)))

;;   (add-hook 'c-mode-common-hook (lambda ()
;;                                   (sb/company-c-mode)
;;                                   (company-fuzzy-mode 1)
;;                                   (diminish 'company-fuzzy-mode))))

(progn
  (defun sb/company-sh-mode ()
    "Add backends for shell script completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)

    (setq company-backends '(company-capf
                             company-shell
                             company-shell-env
                             company-dabbrev-code
                             company-yasnippet
                             company-files
                             company-dabbrev)))

  (add-hook 'sh-mode-hook (lambda ()
                            (unless (display-graphic-p)
                              (sb/company-sh-mode)
                              (company-fuzzy-mode 1)
                              (diminish 'company-fuzzy-mode))))

  (defun sb/company-fish-mode ()
    "Add backends for fish shell script completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)

    (setq company-backends '(company-capf
                             company-shell
                             company-shell-env
                             company-fish-shell
                             company-dabbrev-code
                             company-yasnippet
                             company-files
                             company-dabbrev)))

  (add-hook 'fish-mode-hook (lambda ()
                              (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                                (sb/company-fish-mode)
                                (company-fuzzy-mode 1)
                                (diminish 'company-fuzzy-mode)))))

(progn
  (defun sb/company-elisp-mode ()
    "Set up company for elisp mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)

    (setq company-backends '(company-capf
                             company-yasnippet
                             company-files
                             company-dabbrev-code
                             company-dabbrev)))

  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                                      (sb/company-elisp-mode)
                                      (company-fuzzy-mode 1)
                                      (diminish 'company-fuzzy-mode)))))

(progn
  (defun sb/company-python-mode ()
    "Add backends for Python completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 3)
    (make-local-variable 'company-backends)

    ;; `company-dabbrev-code' is useful for variable names
    (setq company-backends '(company-capf
                             company-yasnippet
                             company-dabbrev-code
                             company-files
                             company-dabbrev)))

  (add-hook 'python-mode-hook (lambda ()
                                (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                                  (sb/company-python-mode)
                                  (company-fuzzy-mode 1)
                                  (diminish 'company-fuzzy-mode)))))

;; (progn
;;   (defun sb/company-prog-mode ()
;;     "Add backends for program completion in company mode."
;;     (defvar company-minimum-prefix-length)
;;     (defvar company-backends)

;;     (setq-local company-minimum-prefix-length 2)
;;     (make-local-variable 'company-backends)

;;     ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
;;     (setq company-backends '(company-capf
;;                              company-yasnippet
;;                              company-files
;;                              company-dabbrev-code
;;                              company-dabbrev)))

;;   (add-hook 'prog-mode-hook (lambda ()
;;                               (sb/company-prog-mode)
;;                               (company-fuzzy-mode 1)
;;                               (diminish 'company-fuzzy-mode))))

(use-package orderless
  :straight t
  :after (:any ivy vertico)
  :demand t
  :defines orderless-component-separator
  :config
  (with-eval-after-load "ivy"
    ;; (defvar ivy-re-builders-alist)
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder))))

  (setq completion-styles '(orderless partial-completion) ; initials, basic, emacs22
        orderless-matching-styles '(orderless-regexp)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic-remote orderless partial-completion))
                                        ;; (minibuffer (initials))))
                                        )))

(use-package yasnippet
  :straight t
  :commands (yas-global-mode snippet-mode yas-hippie-try-expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook ((text-mode-hook prog-mode-hook) . yas-global-mode)
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-verbosity 0)
  :config
  (with-eval-after-load "hippie-expand"
    (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))
  (unbind-key "<tab>" yas-minor-mode-map))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet
  :demand t
  :commands yasnippet-snippets-initialize
  :config (yasnippet-snippets-initialize))

(use-package ivy-yasnippet
  :straight t
  :after ivy
  :bind ("C-M-y" . ivy-yasnippet))

(use-package consult-yasnippet
  :straight t
  :bind ("C-M-y" . consult-yasnippet))

;; Ivy is not well supported, and we are using `company-fuzzy' for sorting completion frameworks
(use-package prescient
  :straight t
  :commands prescient-persist-mode
  :hook (after-init-hook . prescient-persist-mode)
  :custom (prescient-sort-full-matches-first t))

;; We are using `company-fuzzy' for sorting completion candidates
(use-package company-prescient
  :straight t
  :after company
  :demand t
  :commands company-prescient-mode
  :disabled t
  :config
  ;; We want `capf' sort for programming modes, not with recency. This breaks support for the
  ;; `:separate' keyword in `company'.
  ;; (setq company-prescient-sort-length-enable nil)
  (company-prescient-mode 1))

(provide 'init-completion)

;;; init-completion.el ends here