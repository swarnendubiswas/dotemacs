;;; init-vertico.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

;; https://kristofferbalintona.me/posts/vertico-marginalia-all-the-icons-completion-and-orderless/
(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-directory
                                vertico-grid
                                vertico-indexed
                                vertico-quick
                                vertico-repeat))
  :if (eq sb/minibuffer-completion 'vertico)
  :defines read-extended-command-predicate
  :commands (command-completion-default-include-p minibuffer-keyboard-quit)
  :hook (after-init-hook . vertico-mode)
  :custom-face
  ;; (vertico-current ((t (:background "#3a3f5a"))))
  (vertico-current ((t (:background "#384551"))))
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
  (("<f2>"  .  find-file)
   :map vertico-map
   ("C-M-j" . vertico-exit-input)
   ("<tab>" . vertico-insert)))

;; More convenient directory navigation commands

(use-package vertico-directory
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-directory))
  :if (eq sb/minibuffer-completion 'vertico)
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

;; ;; Scanning a grid takes time. Furthermore, it hides marginalia annotations.
;; (use-package vertico-grid
;;   :if  (eq sb/minibuffer-completion 'vertico)
;;   :straight (vertico :files (:defaults "extensions/*")
;;                      :includes (vertico-grid))
;;   :after vertico
;;   :disabled t
;;   :commands vertico-grid-mode
;;   :init (vertico-grid-mode 1)
;;   :custom
;;   (vertico-grid-max-columns 4))

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
                     consult-bookmark consult-xref
                     :preview-key nil)

  (when (featurep 'projectile)
    (setq consult-project-function #'projectile-project-root)))

;; Provide context-dependent actions similar to a content menu
;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :after vertico
  :defines vertico-map
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

;; Enriches the completion display with annotations, e.g., documentation strings or file information
(use-package marginalia
  :after vertico
  :init (marginalia-mode 1)
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
