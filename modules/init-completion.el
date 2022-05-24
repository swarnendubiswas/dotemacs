;;; init-completion.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)
(defvar sb/extras-directory)
(defvar sb/EMACS28+)
(defvar sb/capf)

(defvar hippie-expand-verbose)
(defvar savehist-additional-variables)
(defvar recentf-list)
(defvar dabbrev-ignored-buffer-regexps)
(defvar which-key-use-C-h-commands)
(defvar dabbrev-completion-ignored-buffer-regexps)

(declare-function sb/inhibit-message-call-orig-fun "init-core.el")

;; Use "C-M-/" for `dabbrev-completion' which finds all expansions in the current buffer and
;; presents suggestions for completion.
(use-package dabbrev
  :straight nil
  :custom
  (dabbrev-completion-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  :bind ("C-M-/" . dabbrev-completion))

;; Replace `dabbrev-exp' with `hippie-expand'.
(use-package hippie-exp
  :straight nil
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
   ([remap dabbrev-expand] . hippie-expand)))

(use-package ivy
  :functions ivy-format-function-line
  :commands (ivy-read)
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
        ivy-initial-inputs-alist nil ; Do not start searches with ^
        ivy-use-virtual-buffers nil ; Do not show recent files in `switch-buffer'
        ;; The default sorter is much to slow and the default for `ivy-sort-max-size' is way too big
        ;; (30,000). Turn it down so big repos affect project navigation less.
        ivy-sort-max-size 10000)

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
  :if (eq sb/minibuffer-completion 'ivy)
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
   ("<f1>"                           . counsel-M-x)
   ("<f2>"                           . counsel-find-file)
   ("C-<f9>"                         . sb/counsel-goto-recent-directory)
   ("<f9>"                           . counsel-recentf)
   ("C-c d m"                        . counsel-minor)
   ("C-c s g"                        . counsel-git-grep)
   ("C-c s r"                        . counsel-rg)
   ("<f4>"                           . counsel-grep-or-swiper)
   ("C-c C-m"                        . counsel-mark-ring)
   ;; Enabling preview can make switching over remote buffers slow
   ("S-<f3>"                         . counsel-switch-buffer)
   ([remap imenu]                    . counsel-imenu)
   ("C-c C-j"                        . counsel-imenu))
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
     :delimiter "\t"))

  (plist-put
   all-the-icons-ivy-rich-display-transformers-list
   'projectile-completing-read
   '(:columns
     ((all-the-icons-ivy-rich-file-icon)
      (all-the-icons-ivy-rich-project-find-file-transformer (:width 0.4))
      (all-the-icons-ivy-rich-project-file-id (:width 15 :face
                                                      all-the-icons-ivy-rich-file-owner-face
                                                      :align right)))
     :delimiter "\t")))

(use-package ivy-rich
  :commands (ivy-rich-mode ivy-rich-modify-column
                           ivy-rich-set-columns ivy-rich-modify-columns
                           ivy-format-function-line)
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
  :if (eq sb/minibuffer-completion 'vertico)
  :straight (vertico :files (:defaults "extensions/*")
                     :includes
                     (vertico-directory
                      vertico-grid
                      vertico-indexed
                      vertico-quick
                      vertico-repeat))
  :if (eq sb/minibuffer-completion 'vertico)
  :defines read-extended-command-predicate
  :commands (command-completion-default-include-p minibuffer-keyboard-quit)
  :hook (after-init-hook . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 12)
  (vertico-scroll-margin 2)
  :config
  ;; Hide commands in "M-x" in Emacs 28 which do not work in the current mode. Vertico commands are
  ;; hidden in normal buffers.
  (when sb/EMACS28+
    (setq read-extended-command-predicate #'command-completion-default-include-p))
  (when (display-graphic-p)
    (bind-key "<escape>" #'minibuffer-keyboard-quit vertico-map))

  :bind
  (("<f2>" .  find-file)
   :map vertico-map
   ("C-M-j" . vertico-exit-input)
   ("<tab>" . vertico-insert)))

;; More convenient directory navigation commands
(use-package vertico-directory
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-directory))
  :if  (eq sb/minibuffer-completion 'vertico)
  :after vertico
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy) ; Tidy shadowed file names
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-repeat
  :if (eq sb/minibuffer-completion 'vertico)
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-repeat))
  :after vertico
  :hook (minibuffer-setup-hook . vertico-repeat-save)
  :bind
  (("C-c r" . vertico-repeat-last)
   ("M-r" . vertico-repeat-select)))

(use-package vertico-indexed
  :if (eq sb/minibuffer-completion 'vertico)
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed))
  :after vertico
  :commands vertico-indexed-mode
  :init (vertico-indexed-mode 1))

;; Scanning a grid takes time. Furthermore, it hides marginalia annotations.
(use-package vertico-grid
  :if  (eq sb/minibuffer-completion 'vertico)
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-grid))
  :after vertico
  :disabled t
  :commands vertico-grid-mode
  :init (vertico-grid-mode 1)
  :custom
  (vertico-grid-max-columns 4))

(use-package vertico-quick
  :if (eq sb/minibuffer-completion 'vertico)
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-quick))
  :after vertico
  :bind
  (:map vertico-map
        ;; ("C-c q" . vertico-quick-insert)
        ;; ("C-'" . vertico-quick-exit)
        ("C-'" . vertico-quick-jump)))

(use-package consult
  :after vertico
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
   ;; Press SPC to show ephemeral buffers, "b SPC" to filter by buffers, "f SPC" to filter by files,
   ;; "p SPC" to filter by projects. If you press "DEL" afterwards, the full candidate list will be
   ;; shown again.
   ("C-x b" . consult-buffer)
   ("<f3>" . consult-buffer)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap bookmark-jump] . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ("M-y" . consult-yank-pop)
   ([remap yank-pop] . consult-yank-pop)
   ([remap apropos] . consult-apropos)
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ([remap goto-line] . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("C-c C-j" . consult-imenu)
   ([remap imenu] . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ([remap load-theme] . consult-theme)
   ;; M-s bindings (search-map)
   ("C-c s f" . consult-find)
   ([remap locate] . consult-locate)
   ("C-c s l" . consult-locate)
   ;; Prefix argument "C-u" allows to specify the directory
   ("C-c s g" . consult-grep)
   ("C-c s G" . consult-git-grep)
   ("C-c s r" . consult-ripgrep)
   ("C-c s h" . consult-isearch-history)
   ("<f4>" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("<f9>" . consult-recent-file)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap multi-occur] . consult-multi-occur)
   ;; Isearch integration
   :map isearch-mode-map
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi))
  :config
  ;; Disable live preview
  (consult-customize consult-recent-file consult-buffer consult-theme
                     consult-ripgrep consult-git-grep consult-grep consult-bookmark
                     consult-xref consult-projectile
                     :preview-key nil))

;; https://kristofferbalintona.me/posts/corfu-kind-icon-and-corfu-doc/
;; https://github.com/minad/corfu/wiki
(use-package corfu
  :if (eq sb/capf 'corfu)
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-indexed
                              corfu-quick
                              corfu-info
                              corfu-history))
  :commands corfu--goto
  :preface
  (defun sb/corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

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
  :hook (after-init-hook . global-corfu-mode)
  :custom
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-auto t "Enable auto completion")
  (corfu-auto-delay 0.1 "Recommended to not use zero for performance reasons")
  (corfu-auto-prefix 3)
  (corfu-min-width 60)
  (corfu-max-width corfu-min-width "Always have the same width")
  (corfu-count 15)
  (corfu-preselect-first t)
  (corfu-separator ?\s) ; Use space
  (corfu-quit-no-match 'separator)
  :config
  (unless (featurep 'corfu-doc)
    (setq corfu-echo-documentation t))
  :bind
  (:map corfu-map
        ("[tab]" . corfu-next)
        ("C-n" . corfu-next)
        ("[backtab]" . corfu-previous)
        ("C-p" . corfu-previous)
        ("<escape>" . corfu-quit)
        ([remap move-beginning-of-line] . sb/corfu-beginning-of-prompt)
        ([remap move-end-of-line] . sb/corfu-end-of-prompt)
        ("M-m" . sb/corfu-move-to-minibuffer)))

(use-package corfu-info
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-info))
  :if (eq sb/capf 'corfu)
  :after corfu
  :bind
  (:map corfu-map
        ("M-d" . corfu-info-documentation)
        ("M-l" . corfu-info-location)))

(use-package corfu-indexed
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-indexed))
  :if (eq sb/capf 'corfu)
  :after corfu
  :commands corfu-indexed-mode
  :init (corfu-indexed-mode 1))

(use-package corfu-quick
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-quick))
  :if (eq sb/capf 'corfu)
  :after corfu
  :bind
  (:map corfu-map
        ("C-'" . corfu-quick-insert)))

(use-package corfu-history
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-history))
  :if (eq sb/capf 'corfu)
  :commands corfu-history-mode
  :after (corfu savehist)
  :init
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (corfu-history-mode 1))

(use-package corfu-doc
  :if (eq sb/capf 'corfu)
  :hook (corfu-mode-hook . corfu-doc-mode)
  :bind
  (:map corfu-map
        ("M-p" . corfu-doc-scroll-down)
        ("M-n" . corfu-doc-scroll-up)
        ([remap corfu-info-documentation] . corfu-doc-toggle))
  :custom
  ;; Do not show documentation shown in both the echo area and in the `corfu-doc' popup
  (corfu-echo-documentation nil))

(use-package popon
  :straight (popon :type git
                   :repo "https://codeberg.org/akib/emacs-popon.git")
  :if (and (not (display-graphic-p)) (eq sb/capf 'corfu)))

(use-package corfu-terminal
  :straight (corfu-terminal :type git
                            :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :if (and (not (display-graphic-p)) (eq sb/capf 'corfu))
  :hook (corfu-mode-hook . corfu-terminal-mode))

;; Here is a snippet to show how to support `company' backends with `cape'.
;; https://github.com/minad/cape/issues/20
;; (fset #'cape-path (cape-company-to-capf #'company-files))
;; (add-hook 'completion-at-point-functions #'cape-path)

;; https://kristofferbalintona.me/posts/cape/
(use-package cape
  :if (eq sb/capf 'corfu)
  :after corfu
  :demand t
  :commands (cape-history ; Complete from Eshell, Comint or minibuffer history
             cape-file
             cape-keyword ; Complete programming language keyword
             cape-tex ; Complete unicode char from TeX command, e.g. \hbar.
             cape-abbrev ; Complete abbreviation at point
             cape-dict ; Complete word from dictionary at point
             cape-line ; Complete current line from other lines in buffer
             cape-symbol ; Elisp symbol
             cape-ispell ; Complete word at point with Ispell
             ;; Complete with Dabbrev at point
             cape-dabbrev)
  :init
  ;; (dolist (backends '(cape-symbol cape-keyword cape-file cape-tex
  ;;                                 cape-dabbrev cape-ispell cape-dict cape-history))
  ;;   (add-to-list 'completion-at-point-functions backends))
  :custom
  (cape-dabbrev-min-length 3)
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              ;; (setq-local completion-at-point-functions
              ;;             (list (cape-super-capf #'cape-symbol #'cape-keyword #'cape-dabbrev #'cape-file #'cape-history #'cape-ispell #'cape-dict)))
              (add-to-list 'completion-at-point-functions #'cape-file)
              (add-to-list 'completion-at-point-functions #'cape-symbol)
              (add-to-list 'completion-at-point-functions #'cape-keyword)
              (add-to-list 'completion-at-point-functions #'cape-history)
              (add-to-list 'completion-at-point-functions #'cape-dict)
              (add-to-list 'completion-at-point-functions #'cape-ispell)
              (add-to-list 'completion-at-point-functions #'cape-dabbrev 'append)))
  (add-hook 'prog-mode-hook
            (lambda ()
              ;; (setq-local completion-at-point-functions
              ;;             (list (cape-super-capf #'cape-keyword #'cape-dabbrev #'cape-file #'cape-history #'cape-ispell #'cape-dict)))
              (add-to-list 'completion-at-point-functions #'cape-file)
              (add-to-list 'completion-at-point-functions #'cape-keyword)
              (add-to-list 'completion-at-point-functions #'cape-history)
              (add-to-list 'completion-at-point-functions #'cape-dict)
              (add-to-list 'completion-at-point-functions #'cape-ispell)
              (add-to-list 'completion-at-point-functions #'cape-dabbrev 'append)))
  (add-hook 'text-mode-hook
            (lambda ()
              ;; (setq-local completion-at-point-functions
              ;;             (list (cape-super-capf #'cape-dabbrev #'cape-file #'cape-history #'cape-ispell #'cape-dict)))
              (add-to-list 'completion-at-point-functions #'cape-file)
              (add-to-list 'completion-at-point-functions #'cape-history)
              (add-to-list 'completion-at-point-functions #'cape-dict)
              (add-to-list 'completion-at-point-functions #'cape-ispell)
              (add-to-list 'completion-at-point-functions #'cape-dabbrev 'append))))

;; Provide icons for Corfu
(use-package kind-icon
  :after corfu
  :demand t
  :commands kind-icon-margin-formatter
  :if (display-graphic-p)
  :custom
  (kind-icon-face 'corfu-default)
  (kind-icon-default-face 'corfu-default) ; To compute blended backgrounds correctly
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 1.0 :scale 0.8))
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
  :hook (after-init-hook . global-company-mode)
  ;; The `company-posframe' completion kind indicator is not great, but we are now using
  ;; `company-fuzzy'.
  ;; :diminish
  :config
  (setq company-dabbrev-downcase nil ; Do not downcase returned candidates
        ;; Do not ignore case when collecting completion candidates. It is recommended to change the
        ;; default value of "keep-prefix" if we modify `company-dabbrev-downcase'.
        company-dabbrev-ignore-case nil
        ;; Search in other buffers with the same major mode. This can cause performance overhead if
        ;; there are lots of open buffers.
        company-dabbrev-other-buffers t
        company-ispell-available t
        company-ispell-dictionary (expand-file-name "wordlist.5" sb/extras-directory)
        company-minimum-prefix-length 3 ; Small words can be faster to type
        company-require-match nil ; Allow input string that do not match candidates
        company-selection-wrap-around t
        company-show-quick-access t ; Speed up completion
        ;; Align additional metadata, like type signatures, to the right-hand side
        company-tooltip-align-annotations t
        company-clang-insert-arguments nil ; Disable insertion of arguments
        ;; Start a search using `company-filter-candidates' (bound to "C-s") to narrow out-of-order
        ;; strings
        ;; https://github.com/company-mode/company-mode/discussions/1211
        company-search-regexp-function 'company-search-words-in-any-order-regexp
        company-frontends '(;; company-pseudo-tooltip-unless-just-one-frontend
                            company-pseudo-tooltip-frontend ; Always show candidates in overlay tooltip
                            company-preview-frontend ; Too instrusive
                            company-preview-if-just-one-frontend ; Show in-place preview if there is only choice
                            ;; Show selected candidate docs in echo area
                            company-echo-metadata-frontend))

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
  :if (and (display-graphic-p) (eq sb/capf 'company))
  :after company
  :commands company-posframe-mode
  :diminish
  :custom
  (company-posframe-show-metadata nil "Difficult to distinguish the help text from completions")
  (company-posframe-show-indicator t "Hide the backends, the display is not great")
  (company-posframe-quickhelp-delay nil "Disable showing the help frame")
  :init
  (company-posframe-mode 1))

(use-package company-quickhelp
  :if (eq sb/capf 'company)
  :hook (company-mode-hook . company-quickhelp-mode))

(use-package company-statistics
  :if (eq sb/capf 'company)
  :after company
  :demand t
  :commands company-statistics-mode
  :config (company-statistics-mode 1))

(use-package flx)

;; Nice but slows completions. We should invoke this only at the very end of configuring `company'.
(use-package company-fuzzy
  :if (eq sb/capf 'company)
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
  :if (eq sb/capf 'company)
  :after (:any sh-mode fish-mode)
  :demand t
  :defines company-shell-delete-duplictes
  :commands (company-shell company-shell-env company-fish-shell)
  :custom (company-shell-delete-duplictes t))

(use-package company-auctex
  :if (eq sb/capf 'company)
  :after tex-mode
  :demand t
  :commands (company-auctex-init company-auctex-labels
                                 company-auctex-bibs company-auctex-macros
                                 company-auctex-symbols company-auctex-environments))

(use-package math-symbols
  :if (eq sb/capf 'company)
  :after tex-mode
  :demand t) ; Required by `ac-math' and `company-math'

(use-package company-math
  :after tex-mode
  :if (eq sb/capf 'company)
  :demand t
  :commands (company-math-symbols-latex company-math-symbols-unicode company-latex-commands))

;; Uses RefTeX to complete label references and citations
(use-package company-reftex
  :after tex-mode
  :if (eq sb/capf 'company)
  :demand t
  :commands (company-reftex-labels company-reftex-citations))

;; (use-package company-bibtex
;;   :after tex-mode
;;   :if (eq sb/capf 'company)
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

;; LATER: I do not understand the difference between the following two, and the explanation.
;; `(add-to-list 'company-backends '(company-capf company-dabbrev))'
;; `(add-to-list 'company-backends '(company-capf :with company-dabbrev))'

;; https://github.com/sboosali/.emacs.d/sboo/sboo-company.el
;; The ‘prefix’ bool command always returns non-nil for following backends even when their
;; ‘candidates’ list command is empty: `company-abbrev', `company-dabbrev', `company-dabbrev-code'.

(with-eval-after-load "company"
  (progn
    (declare-function sb/company-xml-mode "init-completion")

    (defun sb/company-xml-mode ()
      "Add backends for completion with company."
      (defvar company-minimum-prefix-length)
      (defvar company-backends)

      (setq-local company-minimum-prefix-length 3)
      (make-local-variable 'company-backends)

      (setq company-backends '(company-capf
                               company-files
                               company-dabbrev-code
                               company-dabbrev)))

    (dolist (hook '(nxml-mode-hook))
      (add-hook hook (lambda ()
                       (sb/company-xml-mode)
                       (company-fuzzy-mode 1)
                       (diminish 'company-fuzzy-mode))))))

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
                               company-reftex-citations
                               company-reftex-labels
                               company-auctex-environments
                               company-auctex-macros
                               company-latex-commands
                               company-math-symbols-latex
                               company-math-symbols-unicode
                               company-auctex-symbols
                               ;; company-reftex is expected to be better than `company-auctex-bibs' and `company-auctex-labels'
                               ;; company-auctex-bibs
                               ;; company-auctex-labels
                               ;; `company-reftex-citations' is better than `company-bibtex'
                               ;; company-bibtex
                               ;; company-yasnippet ; FIXME: Untested
                               company-dabbrev
                               company-ispell)))

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
             company-ispell
             company-dabbrev)))

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

(with-eval-after-load "company"
  (progn
    (declare-function sb/company-sh-mode "init-completion")

    (defun sb/company-sh-mode ()
      "Add backends for shell script completion in company mode."
      (defvar company-minimum-prefix-length)
      (defvar company-backends)

      (setq-local company-minimum-prefix-length 3)
      (make-local-variable 'company-backends)

      (setq company-backends '(company-capf
                               company-shell
                               company-shell-env
                               company-dabbrev-code
                               company-files
                               company-dabbrev)))

    (add-hook 'sh-mode-hook (lambda ()
                              (sb/company-sh-mode)
                              ;; (company-fuzzy-mode 1)
                              ;; (diminish 'company-fuzzy-mode)
                              ))))

(with-eval-after-load "company"
  (progn
    (declare-function sb/company-fish-mode "init-completion")

    (defun sb/company-fish-mode ()
      "Add backends for fish shell script completion in company mode."
      (defvar company-minimum-prefix-length)
      (defvar company-backends)

      (setq-local company-minimum-prefix-length 3)
      (make-local-variable 'company-backends)

      (setq company-backends '(company-capf
                               company-shell
                               company-shell-env
                               company-fish-shell
                               company-dabbrev-code
                               company-files
                               company-dabbrev)))

    (add-hook 'fish-mode-hook (lambda ()
                                (sb/company-fish-mode)
                                ;; (company-fuzzy-mode 1)
                                ;; (diminish 'company-fuzzy-mode)
                                ))))

;; https://emacs.stackexchange.com/questions/19072/company-completion-very-slow
;; `company-clang' is slow
(with-eval-after-load "company"
  (progn
    (declare-function sb/company-prog-mode "init-completion")

    (defun sb/company-prog-mode ()
      "Add backends for program completion in company mode."
      (defvar company-minimum-prefix-length)
      (defvar company-backends)

      (setq-local company-minimum-prefix-length 3
                  ;; Other choices: `company-sort-by-length', `company-sort-by-occurrence',
                  ;; `company-sort-by-backend-importance', `company-sort-prefer-same-case-prefix'
                  company-transformers '(delete-dups
                                         company-sort-by-statistics))
      (make-local-variable 'company-backends)

      ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
      (setq company-backends '(company-files
                               (company-capf :with company-yasnippet)
                               company-etags
                               (company-dabbrev-code ; Useful for variable names
                                company-dabbrev))))

    (add-hook 'prog-mode-hook
              (lambda ()
                (unless (or (derived-mode-p 'sh-mode) (derived-mode-p 'fish-mode))
                  (sb/company-prog-mode)
                  ;; (company-fuzzy-mode 1)
                  ;; (diminish 'company-fuzzy-mode)
                  )))))

(use-package orderless
  :after (:any ivy vertico)
  :demand t
  :defines orderless-component-separator
  :config
  (with-eval-after-load "ivy"
    (defvar ivy-re-builders-alist)

    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder))))

  (setq orderless-matching-styles '(orderless-regexp)
        orderless-component-separator #'orderless-escapable-split-on-space
        completion-styles '(orderless
                            ;;basic partial-completion initials emacs22
                            )
        completion-category-defaults nil
        ;; LATER: I do not understand this.
        ;; completion-category-overrides '((file (styles basic substring remote orderless partial-completion))
        ;;                                 ;; (minibuffer (initials))))
        ;;                                 )
        ))

;; To use YASnippet as a non-global minor mode, do not call `yas-global-mode'; instead call
;; `yas-reload-all' to load the snippet tables and then call `yas-minor-mode' from the hooks of
;; major-modes where you want YASnippet enabled.
;; https://github.com/joaotavora/yasnippet/blob/master/README.mdown
(use-package yasnippet
  :commands (snippet-mode yas-hippie-try-expand yas-reload-all)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook (prog-mode-hook . yas-global-mode)
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-verbosity 0)
  :config
  (with-eval-after-load "hippie-expand"
    (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))
  (unbind-key "<tab>" yas-minor-mode-map))

;; YASnippet no longer bundles snippets directly
(use-package yasnippet-snippets
  :after yasnippet
  :demand t
  :commands yasnippet-snippets-initialize
  :config (yasnippet-snippets-initialize))

(use-package ivy-yasnippet
  :if (eq sb/minibuffer-completion 'ivy)
  :after ivy
  :bind ("C-M-y" . ivy-yasnippet))

(use-package consult-yasnippet
  :if (eq sb/minibuffer-completion 'vertico)
  :after consult
  :bind ("C-M-y" . consult-yasnippet))

;; ;; Ivy is not well supported, and we are using `company-fuzzy' for sorting completion frameworks
;; (use-package prescient
;;   :commands prescient-persist-mode
;;   :hook (after-init-hook . prescient-persist-mode)
;;   :custom (prescient-sort-full-matches-first t))

;; ;; We want `capf' sort for programming modes, not with recency. This breaks support for the
;; ;; `:separate' keyword in `company'. We are using `company-fuzzy' for sorting completion candidates.
;; (use-package company-prescient
;;   :if (eq sb/capf 'company)
;;   :after (company prescient)
;;   :demand t
;;   :commands company-prescient-mode
;;   :config
;;   ;; (setq company-prescient-sort-length-enable nil)
;;   (company-prescient-mode 1))

(use-package consult-company
  :bind
  (:map company-mode-map
        ([remap completion-at-point] . consult-company)))

(provide 'init-completion)

;;; init-completion.el ends here
