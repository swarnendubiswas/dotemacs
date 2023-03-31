;;; init-vertico.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)

(use-package vertico
  :straight
  (vertico :files (:defaults "extensions/*") :includes (vertico-directory
                                                        vertico-indexed
                                                        vertico-quick
                                                        vertico-repeat))
  :if (eq sb/minibuffer-completion 'vertico)
  :defines read-extended-command-predicate
  :commands
  (command-completion-default-include-p minibuffer-keyboard-quit)
  :hook
  (emacs-startup-hook . vertico-mode)
  :bind
  (:map vertico-map
        ("M-<"   . vertico-first)
        ("M->"   . vertico-last)
        ("C-M-j" . vertico-exit-input)
        ("<tab>" . vertico-insert))
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-preselect 'first)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties '(read-only t
                                                 cursor-intangible t face minibuffer-prompt))
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
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :bind
  (:map vertico-map
        ("RET"   . vertico-directory-enter)
        ("DEL"   . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook
  (minibuffer-setup-hook . vertico-repeat-save)
  :bind
  (("C-c r" . vertico-repeat-last)
   ("M-r"   . vertico-repeat-select)))

(use-package vertico-indexed
  :straight nil
  :after vertico
  :commands vertico-indexed-mode
  :init (vertico-indexed-mode 1))

(use-package vertico-quick
  :straight nil
  :after vertico
  :bind
  (:map vertico-map
        ("C-c q" . vertico-quick-insert)
        ("C-'" . vertico-quick-jump)))

(use-package consult
  :after vertico
  :commands
  (consult--customize-put projectile-project-root)
  :bind
  (("C-x M-:" . consult-complex-command)
   ([remap repeat-complex-command] . consult-complex-command)
   ;; Press "SPC" to show ephemeral buffers, "b SPC" to filter by buffers, "f SPC" to filter by
   ;; files, "p SPC" to filter by projects. If you press "DEL" afterwards, the full candidate list
   ;; will be shown again.
   ("C-x b" . consult-buffer)
   ("<f3>" . consult-buffer)
   ([remap switch-to-buffer] . consult-buffer)
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
   ("C-c C-j" . consult-imenu)
   ([remap imenu] . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ([remap load-theme] . consult-theme)
   ("C-c h" . consult-history)
   ("C-c s f" . consult-find)
   ([remap locate] . consult-locate)
   ("C-c s l" . consult-locate)
   ;; Prefix argument "C-u" allows to specify the directory
   ("C-c s g" . consult-grep)
   ("C-c s G" . consult-git-grep)
   ("C-c s r" . consult-ripgrep)
   ("C-c s h" . consult-isearch-history)
   ("<f4>" . consult-line)
   ("M-s m" . consult-multi-occur)
   ("<f9>" . consult-recent-file)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap multi-occur] . consult-multi-occur)
   :map isearch-mode-map
   ("M-s e" . consult-isearch-history)
   :map minibuffer-local-map
   ("M-s" . consult-history))
  :custom
  (consult-line-start-from-top t "Start search from the beginning")
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-line-numbers-widen t)
  (consult-preview-key nil "Disable preview by default, enable for selected commands")
  (completion-in-region-function #'consult-completion-in-region)
  ;; Having multiple other sources like recentf makes it difficult to identify and switch quickly
  ;; between only buffers
  (consult-buffer-sources '(consult--source-buffer))
  :config
  (consult-customize
   consult-theme consult-line consult-ripgrep consult-git-grep consult-grep
   consult-recent-file consult-buffer consult-bookmark consult-xref consult-yank-from-kill-ring
   :preview-key '(:debounce 1.5 any)
   consult-find
   :sort t
   consult-line consult-ripgrep consult-grep
   ;; Initialize search string with the highlighted region
   :initial (when (use-region-p)
              (buffer-substring-no-properties
               (region-beginning) (region-end))))

  (with-eval-after-load "projectile"
    (setq consult-project-function #'projectile-project-root)
    (bind-key [remap projectile-ripgrep] #'consult-ripgrep)
    (bind-key [remap projectile-grep] #'consult-grep)))

;; Provide context-dependent actions similar to a content menu
;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :after vertico
  :defines (vertico-map which-key-use-C-h-commands)
  :commands embark-prefix-help-command
  :bind
  (([remap describe-bindings] . embark-bindings)
   ("C-`"     . embark-dwim)
   :map vertico-map
   ("C-l"     . embark-act)
   ("C-c C-l" . embark-export))
  :custom
  ;; Replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))

;; Enriches the completion display with annotations, e.g., documentation strings or file
;; information.
(use-package marginalia
  :after vertico
  :commands marginalia-mode
  :init (marginalia-mode 1)
  :config
  ;; Add project-buffer annotator.
  (add-to-list 'marginalia-annotator-registry
               '(project-buffer marginalia-annotate-project-buffer)))

(provide 'init-vertico)

;;; init-vertico.el ends here
