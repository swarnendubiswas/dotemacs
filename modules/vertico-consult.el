;;; init-vertico.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*") :includes (vertico-directory vertico-repeat))
  :if (eq sb/minibuffer-completion 'vertico)
  :defines read-extended-command-predicate
  :hook (emacs-startup-hook . vertico-mode)
  :bind
  (:map
    vertico-map
    ("M-<" . vertico-first)
    ("M->" . vertico-last)
    ("C-M-j" . vertico-exit-input)
    ;; https://emacs.stackexchange.com/questions/77036/how-can-i-bind-vertico-insert-to-toggle-in-the-list
    ("TAB" . vertico-insert))
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-preselect 'first)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Hide commands in "M-x" in Emacs 28 which do not work in the current mode. Vertico commands are
  ;; hidden in normal buffers.
  (when sb/EMACS28+
    (setq read-extended-command-predicate #'command-completion-default-include-p))

  (when (display-graphic-p)
    (bind-key "[escape]" #'minibuffer-keyboard-quit vertico-map))

  ;; (cond
  ;;  ;;  ((eq sb/theme 'modus-vivendi)
  ;;  ;;   (set-face-attribute 'vertico-current nil :background "#384551" :inherit t))
  ;;  ((eq sb/theme 'modus-operandi)
  ;;   (set-face-attribute 'vertico-current nil :background "#E6F2FF" :inherit t))
  ;;  ;;  ((eq sb/theme 'standard-light)
  ;;  ;;   (set-face-attribute 'vertico-current nil :background "#9AB8C4" :inherit t))
  ;;  (t (set-face-attribute 'vertico-current nil :background "#3A3F5A" :inherit t))
  ;;  )
  )

;; More convenient directory navigation commands
(use-package vertico-directory
  :straight nil
  :after vertico
  :hook
  ;; Tidy shadowed file names. That is, when using a command for selecting a file in the minibuffer,
  ;; the following fixes the path so the selected path does not have prepended junk left behind.
  ;; This works with `file-name-shadow-mode' enabled. When you are in a sub-directory and use, say,
  ;; `find-file' to go to your home '~/' or root '/' directory, Vertico will clear the old path to
  ;; keep only your current input.
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :bind
  (:map
    vertico-map
    ("RET" . vertico-directory-enter)
    ("DEL" . vertico-directory-delete-char)
    ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook (minibuffer-setup-hook . vertico-repeat-save)
  :bind (("C-c r" . vertico-repeat-last) ("M-r" . vertico-repeat-select))
  :config
  (with-eval-after-load "savehist"
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history)))

;; (use-package vertico-indexed ; Select candidates by number with "C-u number RET"
;;   :straight nil
;;   :after vertico
;;   :commands vertico-indexed-mode
;;   :init (vertico-indexed-mode 1))

(use-package vertico-quick
  :straight nil
  :after vertico
  :bind (:map vertico-map ("C-c q" . vertico-quick-insert) ("C-'" . vertico-quick-jump)))

;; (use-package vertico-multiform
;;   :straight nil
;;   :after vertico
;;   :commands vertico-multiform-mode
;;   :init (vertico-multiform-mode 1)
;;   :custom
;;   (vertico-multiform-categories '((embark-keybinding grid)))
;;   (vertico-multiform-commands
;;     '
;;     ( ;; (execute-extended-command indexed)
;;       ;; (completion-at-point vertical)
;;       ;; (consult-imenu buffer indexed)
;;       ;; (ffap flat (vertico-cycle . t))
;;       ;; (consult-projectile-switch-project grid)
;;       ;; (consult-yank-pop indexed)
;;       ;; (embark-bindings buffer)
;;       ;; (xref-find-references buffer)
;;       ;; (find-file-at-point (vertico-sort-function . sort-directories-first))
;;       ;; (consult-line buffer)
;;       ;; (consult-grep buffer)
;;       ;; (consult-git-grep buffer)
;;       ;; (consult-ripgrep buffer)
;;       )))

(use-package consult
  :after vertico
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :bind
  (("<f1>" . execute-extended-command)
    ("C-x M-:" . consult-complex-command)
    ([remap repeat-complex-command] . consult-complex-command)
    ;; Press "SPC" to show ephemeral buffers, "b SPC" to filter by buffers, "f SPC" to filter by
    ;; files, "p SPC" to filter by projects. If you press "DEL" afterwards, the full candidate list
    ;; will be shown again.
    ([remap switch-to-buffer] . consult-buffer)
    ("C-x b" . consult-buffer)
    ("<f3>" . consult-buffer)
    ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
    ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
    ([remap bookmark-jump] . consult-bookmark)
    ([remap project-switch-to-buffer] . consult-project-buffer)
    ([remap yank-pop] . consult-yank-pop)
    ("M-y" . consult-yank-pop)
    ([remap apropos] . consult-apropos)
    ("M-g e" . consult-compile-error)
    ([remap goto-line] . consult-goto-line)
    ("M-g o" . consult-outline)
    ("C-c C-m" . consult-mark)
    ("M-g k" . consult-global-mark)
    ([remap imenu] . consult-imenu)
    ("C-c C-j" . consult-imenu)
    ("M-g I" . consult-imenu-multi)
    ([remap flymake-show-diagnostic] . consult-flymake)
    ([remap flymake-show-buffer-diagnostics] . consult-flymake)
    ([remap flymake-show-diagnostics-buffer] . consult-flymake)
    ([remap customize] . consult-customize)
    ([remap load-theme] . consult-theme)
    ("C-c h" . consult-history)
    ("C-c s f" . consult-find)
    ([remap locate] . consult-locate)
    ("C-c s l" . consult-locate)
    ;; Prefix argument "C-u" allows to specify the directory
    ([remap rgrep] . consult-grep)
    ("C-c s g" . consult-grep)
    ([remap vc-git-grep] . consult-git-grep)
    ("C-c s G" . consult-git-grep)
    ("C-c s r" . consult-ripgrep)
    ("C-c s h" . consult-isearch-history)
    ([remap isearch-forward] . consult-line)
    ("<f4>" . consult-line)
    ("M-s m" . consult-multi-occur)
    ([remap recentf-open-files] . consult-recent-file)
    ("<f9>" . consult-recent-file)
    ([remap multi-occur] . consult-multi-occur)
    :map
    isearch-mode-map
    ("M-s e" . consult-isearch-history)
    :map
    minibuffer-local-map
    ("M-s" . consult-history))
  :custom
  (consult-line-start-from-top t "Start search from the beginning")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-line-numbers-widen t)
  (consult-preview-key nil "Disable preview by default, enable for selected commands")
  (completion-in-region-function #'consult-completion-in-region "Complete M-:")
  ;; Having multiple other sources like recentf makes it difficult to identify and switch quickly
  ;; between only buffers, especially while wrapping around.
  (consult-buffer-sources '(consult--source-buffer))
  :config
  (consult-customize
    consult-line
    consult-ripgrep
    consult-git-grep
    consult-grep
    consult-recent-file
    consult-bookmark
    consult-xref
    consult-yank-from-kill-ring
    :preview-key
    '(:debounce 1.5 any)
    consult-theme
    consult-buffer
    :preview-key
    "M-."
    consult-find
    :sort
    t
    consult-line
    consult-ripgrep
    consult-grep
    ;; Initialize search string with the highlighted region
    :initial
    (when (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))))

  (with-eval-after-load "projectile"
    (setq consult-project-function (lambda (_) (projectile-project-root)))
    (bind-key [remap projectile-ripgrep] #'consult-ripgrep)
    (bind-key [remap projectile-grep] #'consult-grep)))

(use-package embark-consult
  :after consult
  :demand t)

;; Provide context-dependent actions similar to a content menu
;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :after vertico
  :defines (vertico-map which-key-use-C-h-commands)
  :commands embark-prefix-help-command
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode)
  :bind
  (([remap describe-bindings] . embark-bindings)
    ("C-`" . embark-dwim)
    :map
    vertico-map
    ("C-l" . embark-act)
    ("C-c C-l" . embark-export))
  :custom
  ;; Replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command))

;; Enriches the completion display with annotations, e.g., documentation strings or file
;; information.
(use-package marginalia
  :after vertico
  :init (marginalia-mode 1)
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :config
  (setq marginalia-annotator-registry (assq-delete-all 'file marginalia-annotator-registry))
  (add-to-list 'marginalia-annotator-registry '(symbol-help marginalia-annotate-variable))
  (add-to-list 'marginalia-annotator-registry '(project-buffer marginalia-annotate-project-buffer)))

(use-package consult-tramp
  :straight (:host github :repo "Ladicle/consult-tramp")
  :after consult
  :bind ("C-c d t" . consult-tramp))

(use-package consult-eglot
  :if (eq sb/lsp-provider 'eglot)
  :after (consult eglot)
  :commands consult-eglot-symbols)

(use-package consult-project-extra
  :if (and (eq sb/minibuffer-completion 'vertico) (eq sb/project-handler 'project))
  :demand t
  :commands consult-project-extra-find-other-window
  :bind (:map project-prefix-map ("z" . consult-project-extra-find))
  :config
  ;; (add-to-list 'project-switch-commands '(consult-project-extra-find "Find file" ?f))
  ;; (add-to-list 'project-switch-commands '(consult-project-buffer "Buffer"))
  (setq project-switch-commands 'consult-project-extra-find))

(use-package consult-dir
  :if (eq sb/minibuffer-completion 'vertico)
  :bind
  (("C-x C-d" . consult-dir)
    :map
    minibuffer-local-completion-map
    ("C-x C-d" . consult-dir)
    ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-flyspell
  :after (consult flyspell)
  :defines consult-flyspell-select-function
  :bind ("C-c f l" . consult-flyspell)
  :config
  (setq consult-flyspell-select-function
    (lambda ()
      (flyspell-correct-at-point)
      (consult-flyspell))))

(use-package consult-flycheck
  :after (flycheck consult)
  :bind (:map flycheck-command-map ("!" . consult-flycheck)))

(use-package consult-lsp
  :after (consult lsp)
  :commands (consult-lsp-diagnostics consult-lsp-symbols consult-lsp-file-symbols)
  :bind (:map lsp-mode-map ([remap xref-find-apropos] . consult-lsp-symbols)))

(use-package consult-yasnippet
  :after consult
  :bind ("C-M-y" . consult-yasnippet))

(use-package consult-projectile
  :if (eq sb/project-handler 'projectile)
  :after projectile
  :commands consult-projectile-recentf
  :bind
  (("<f5>" . consult-projectile-switch-project)
    ("<f6>" . consult-projectile)
    ([remap projectile-recentf] . consult-projectile-recentf)
    ([remap projectile-switch-to-buffer] . consult-projectile-switch-to-buffer)
    ([remap projectile-find-file] . consult-projectile-find-file)
    ([remap projectile-find-dir] . consult-projectile-find-dir)
    ([remap projectile-switch-project] . consult-projectile-switch-project))
  :config (consult-customize consult-projectile :preview-key nil))

(provide 'vertico-consult)

;;; init-vertico.el ends here
