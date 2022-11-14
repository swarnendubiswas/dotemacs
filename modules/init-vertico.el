;;; init-vertico.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)

;; https://kristofferbalintona.me/posts/vertico-marginalia-all-the-icons-completion-and-orderless/
(use-package vertico
  :straight
  (vertico :files (:defaults "extensions/*") :includes (vertico-directory
                                                        vertico-grid
                                                        vertico-indexed
                                                        vertico-quick
                                                        vertico-repeat))
  :if (eq sb/minibuffer-completion 'vertico)
  :defines read-extended-command-predicate
  :commands
  (command-completion-default-include-p minibuffer-keyboard-quit)
  :hook
  (after-init-hook . vertico-mode)
  :bind
  (:map vertico-map
        ("C-M-j" . vertico-exit-input)
        ("<tab>" . vertico-insert))
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-scroll-margin 2)
  :config
  ;; Hide commands in "M-x" in Emacs 28 which do not work in the current mode. Vertico commands are
  ;; hidden in normal buffers.
  (when sb/EMACS28+
    (setq read-extended-command-predicate #'command-completion-default-include-p))

  (when (display-graphic-p)
    (bind-key "<escape>" #'minibuffer-keyboard-quit vertico-map))

  (unless (display-graphic-p)
    (cond
     ((eq sb/tui-theme 'modus-vivendi)
      (set-face-attribute 'vertico-current nil :background "#384551" :inherit t))
     ((eq sb/tui-theme 'modus-operandi)
      (set-face-attribute 'vertico-current nil :background "#E6F2FF" :inherit t))
     (t (set-face-attribute 'vertico-current nil :background "#3A3F5A" :inherit t))))

  (when (display-graphic-p)
    (cond
     ((eq sb/gui-theme 'modus-vivendi)
      (set-face-attribute 'vertico-current nil :background "#384551" :inherit t))
     ((eq sb/gui-theme 'modus-operandi)
      (set-face-attribute 'vertico-current nil :background "#E6F2FF" :inherit t))
     (t (set-face-attribute 'vertico-current nil :background "#3A3F5A" :inherit t)))))

;; More convenient directory navigation commands
(use-package vertico-directory
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-directory))
  :if (eq sb/minibuffer-completion 'vertico)
  :after vertico
  ;; Tidy shadowed file names. That is, when using a command for selecting a file in the minibuffer,
  ;; the following fixes the path so the selected path does not have prepended junk left behind.
  :hook
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-repeat
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-repeat))
  :if (eq sb/minibuffer-completion 'vertico)
  :after vertico
  :hook
  (minibuffer-setup-hook . vertico-repeat-save)
  :bind
  (("C-c r" . vertico-repeat-last)
   ("M-r" . vertico-repeat-select)))

(use-package vertico-indexed
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed))
  :if (eq sb/minibuffer-completion 'vertico)
  :after vertico
  :commands vertico-indexed-mode
  :init (vertico-indexed-mode 1))

;; ;; Scanning a grid takes time. Furthermore, it hides marginalia annotations.
;; (use-package vertico-grid
;;   :straight (vertico :files (:defaults "extensions/*")
;;                      :includes (vertico-grid))
;;   :if  (eq sb/minibuffer-completion 'vertico)
;;   :disabled t
;;   :after vertico
;;   :commands vertico-grid-mode
;;   :init (vertico-grid-mode 1)
;;   :custom
;;   (vertico-grid-max-columns 4))

(use-package vertico-quick
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-quick))
  :if (eq sb/minibuffer-completion 'vertico)
  :after vertico
  :bind
  (:map vertico-map
        ;; ("C-c q" . vertico-quick-insert)
        ;; ("C-'" . vertico-quick-exit)
        ("C-'" . vertico-quick-jump)))

(use-package consult
  :if (eq sb/minibuffer-completion 'vertico)
  :after vertico
  :commands
  (consult--customize-put projectile-project-root)
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
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ("M-y" . consult-yank-pop)
   ([remap apropos] . consult-apropos)
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ([remap goto-line] . consult-goto-line)
   ("M-g o" . consult-outline)
   ("C-c C-m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("C-c C-j" . consult-imenu)
   ([remap imenu] . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ([remap load-theme] . consult-theme)
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
   ;; Isearch integration
   :map isearch-mode-map
   ("M-s e" . consult-isearch-history))
  :custom
  (consult-line-start-from-top t "Start search from the beginning")
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-line-numbers-widen t)
  (consult-preview-key nil "Disable preview by default, enable for selected commands")
  (completion-in-region-function #'consult-completion-in-region)
  ;; Having multiple other sources like recentf makes it difficult to identify and switch quickly between only buffers
  (consult-buffer-sources '(consult--source-buffer
                            ;; consult--source-hidden-buffer
                            ;; consult--source-recent-file
                            ))
  :config
  (consult-customize
   consult-theme consult-line consult-ripgrep consult-git-grep consult-grep
   :preview-key '(:debounce 0.2 any)
   consult-recent-file consult-buffer consult-bookmark consult-xref consult-yank-from-kill-ring
   :preview-key (kbd "M-.")
   consult-find
   :sort t)

  (when (featurep 'projectile)
    (setq consult-project-function #'projectile-project-root)))

;; Provide context-dependent actions similar to a content menu
;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :after vertico
  :defines (vertico-map which-key-use-C-h-commands)
  :commands embark-prefix-help-command
  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command
        which-key-use-C-h-commands nil)
  :bind
  (([remap describe-bindings] . embark-bindings)
   :map vertico-map
   ("C-l" . embark-act)
   ("C-," . embark-dwim)
   ("C-c C-l" . embark-export)))

(use-package embark-consult
  :after (embark consult))

;; Enriches the completion display with annotations, e.g., documentation strings or file information.
;; FIXME: Align marginalia columns correctly.

(use-package marginalia
  :after vertico
  :commands marginalia-mode
  :init (marginalia-mode 1)
  :bind
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle))
  :custom
  (marginalia-align 'left)
  :config
  ;; Add project-buffer annotator.
  (add-to-list 'marginalia-annotator-registry
               '(project-buffer marginalia-annotate-project-buffer))
  (with-eval-after-load "projectile"
    (add-to-list 'marginalia-command-categories
                 '(projectile-switch-project . file))
    (add-to-list 'marginalia-command-categories
                 '(projectile-switch-open-project . file))
    (add-to-list 'marginalia-command-categories
                 '(projectile-find-file . project-file))
    (add-to-list 'marginalia-command-categories
                 '(projectile-recentf . project-file))
    (add-to-list 'marginalia-command-categories
                 '(projectile-display-buffer . project-buffer))
    (add-to-list 'marginalia-command-categories
                 '(projectile-switch-to-buffer . project-buffer))))

(provide 'init-vertico)

;;; init-vertico.el ends here
