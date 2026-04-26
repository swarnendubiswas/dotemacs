;;; dank-emacs-theme.el --- Enhanced theme using Matugen SCSS variables with dank16 colors

;; Copyright (C) 2025

;; Author: Generated (Enhanced)
;; Version: 1.3
;; Package-Requires: ((emacs "24.1"))
;; Keywords: faces

;;; Commentary:

;; An enhanced theme using Matugen SCSS variables integrated with dank16 colors:
;; - Rich color palette from dank16 for vibrant syntax highlighting
;; - Improved contrast and readability
;; - Better source block distinction with refined backgrounds
;; - Enhanced org-mode styling with hidden asterisks
;; - Superior visual hierarchy and modern aesthetics

;;; Code:

(deftheme dank-emacs "Enhanced theme using Matugen variables with dank16 color integration.")

;; Define all the color variables (replaced by template processor)
(let* ((bg "#181115")
      (err "#ff727b")  ; Red from dank16
      (err-container "#93000a")
      (on-background "#eddfe4")
      (on-err "#690005")
      (on-err-container "#ffdad6")
      (on-primary "#511d3f")
      (on-primary-container "#ffd8eb")
      (on-secondary "#402a36")
      (on-secondary-container "#fcd9e9")
      (on-surface "#eddfe4")
      (on-surface-variant "#d3c2c9")
      (on-tertiary "#4a2713")
      (on-tertiary-container "#ffdbca")
      (outline-color "#9c8d93")
      (outline-variant "#504349")
      (primary "#fab1d9")
      (primary-container "#6b3456")
      (secondary "#debecd")
      (secondary-container "#58404d")
      (shadow "#000000")
      (surface "#181115")
      (surface-container "#30282b")
      (surface-container-high "#3b3236")
      (surface-container-highest "#30282b")
      (surface-container-low "#211a1d")
      (surface-container-lowest "#130c10")
      (surface-variant "#504349")
      (tertiary "#f4ba9d")
      (tertiary-container "#653d27")

      ;; Enhanced dank16 colors for better syntax highlighting
      (dank-red "#ff727b")          ; Bright red
      (dank-red-alt "#ff9fa5")      ; Alternative red
      (dank-green "#9ffa7d")        ; Vibrant green
      (dank-green-bright "#beffa5") ; Bright green
      (dank-yellow "#ffe072")       ; Warm yellow
      (dank-yellow-bright "#ffeba5") ; Bright yellow
      (dank-blue "#ed9ac7")         ; Blue-green
      (dank-magenta "#733557")      ; Teal-magenta
      (dank-cyan "#fab1d9")         ; Bright cyan
      (dank-cyan-bright "#ffbfe2") ; Brightest cyan
      (dank-cyan-dark "#ffcae7")   ; Dark cyan
      (dank-teal "#ffddef")        ; Dark teal
      (dank-fg "#ffeff8")           ; Light foreground
      (dank-gray "#a2969c")         ; Gray
      (dank-white "#fff8fc")       ; White

      ;; Map success colors to green
      (success "#9ffa7d")
      (on-success "#4a2713")
      (success-container "#653d27")
      (on-success-container "#ffdbca")

      ;; Map fixed colors
      (primary-fixed "#ffd8eb")
      (primary-fixed-dim "#fab1d9")
      (secondary-fixed "#fcd9e9")
      (secondary-fixed-dim "#debecd")
      (tertiary-fixed "#ffdbca")
      (tertiary-fixed-dim "#f4ba9d")
      (on-primary-fixed "#370729")
      (on-primary-fixed-variant "#6b3456")
      (on-secondary-fixed "#291521")
      (on-secondary-fixed-variant "#58404d")
      (on-tertiary-fixed "#311303")
      (on-tertiary-fixed-variant "#653d27")

      ;; Map inverse colors
      (inverse-on-surface "#362e32")
      (inverse-primary "#864b6e")
      (inverse-surface "#eddfe4")

      ;; Terminal colors from dank16
      (term0 "#181115")
      (term1 "#ff727b")
      (term2 "#9ffa7d")
      (term3 "#ffe072")
      (term4 "#ed9ac7")
      (term5 "#733557")
      (term6 "#fab1d9")
      (term7 "#ffeff8")
      (term8 "#a2969c")
      (term9 "#ff9fa5")
      (term10 "#beffa5")
      (term11 "#ffeba5")
      (term12 "#ffbfe2")
      (term13 "#ffcae7")
      (term14 "#ffddef")
      (term15 "#fff8fc"))

  (custom-theme-set-faces
   'dank-emacs
   ;; Basic faces
   `(default ((t (:background ,bg :foreground ,on-background))))
   `(cursor ((t (:background ,dank-cyan-bright))))
   `(highlight ((t (:background ,primary-container :foreground ,on-primary-container))))
   `(region ((t (:background ,primary-container :foreground ,dank-cyan-bright :extend t))))
   `(secondary-selection ((t (:background ,secondary-container :foreground ,on-secondary-container :extend t))))
   `(isearch ((t (:background ,dank-yellow :foreground ,bg :weight bold))))
   `(lazy-highlight ((t (:background ,secondary-container :foreground ,dank-yellow-bright))))
   `(vertical-border ((t (:foreground ,surface-variant))))
   `(border ((t (:background ,surface-variant :foreground ,surface-variant))))
   `(fringe ((t (:background ,surface :foreground ,dank-gray))))
   `(shadow ((t (:foreground ,dank-gray))))
   `(link ((t (:foreground ,dank-cyan-bright :underline t))))
   `(link-visited ((t (:foreground ,dank-magenta :underline t))))
   `(success ((t (:foreground ,success))))
   `(warning ((t (:foreground ,dank-yellow))))
   `(error ((t (:foreground ,err))))
   `(match ((t (:background ,dank-yellow :foreground ,bg :weight bold))))

   ;; Font-lock - enhanced with dank16 colors for vibrant syntax highlighting
   `(font-lock-builtin-face ((t (:foreground ,dank-cyan-bright))))
   `(font-lock-comment-face ((t (:foreground ,dank-gray :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,outline-variant))))
   `(font-lock-constant-face ((t (:foreground ,dank-yellow-bright :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,dank-fg :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,dank-cyan :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,dank-red-alt :weight bold))))
   `(font-lock-string-face ((t (:foreground ,dank-green))))
   `(font-lock-type-face ((t (:foreground ,dank-yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,dank-fg))))
   `(font-lock-warning-face ((t (:foreground ,err :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,dank-teal))))
   `(font-lock-negation-char-face ((t (:foreground ,dank-red))))

   ;; Show paren
   `(show-paren-match ((t (:background ,primary-container :foreground ,dank-cyan-bright :weight bold))))
   `(show-paren-mismatch ((t (:background ,err-container :foreground ,on-err-container :weight bold))))

   ;; Mode line - improved status bar styling
   `(mode-line ((t (:background ,surface-container :foreground ,on-surface :box nil))))
   `(mode-line-inactive ((t (:background ,surface :foreground ,dank-gray :box nil))))
   `(mode-line-buffer-id ((t (:foreground ,dank-cyan :weight bold))))
   `(mode-line-emphasis ((t (:foreground ,dank-cyan :weight bold))))
   `(mode-line-highlight ((t (:foreground ,dank-cyan-bright :box nil))))

   ;; Improved Source blocks - seamless integration
   `(org-block ((t (:background ,surface-container-low :extend t :inherit fixed-pitch))))
   `(org-block-begin-line ((t (:background ,surface-container-low :foreground ,dank-teal :extend t :slant italic :inherit fixed-pitch))))
   `(org-block-end-line ((t (:background ,surface-container-low :foreground ,dank-teal :extend t :slant italic :inherit fixed-pitch))))
   `(org-code ((t (:background ,surface-container-low :foreground ,dank-yellow-bright :inherit fixed-pitch))))
   `(org-verbatim ((t (:background ,surface-container-low :foreground ,dank-cyan :inherit fixed-pitch))))
   `(org-meta-line ((t (:foreground ,dank-gray :slant italic))))

   ;; Org mode with enhanced colors and hidden asterisks
   `(org-level-1 ((t (:foreground ,dank-cyan :weight bold :height 1.2))))
   `(org-level-2 ((t (:foreground ,dank-blue :weight bold :height 1.1))))
   `(org-level-3 ((t (:foreground ,dank-magenta :weight bold))))
   `(org-level-4 ((t (:foreground ,dank-green :weight bold))))
   `(org-level-5 ((t (:foreground ,dank-yellow :weight bold))))
   `(org-level-6 ((t (:foreground ,dank-cyan-bright :weight bold))))
   `(org-level-7 ((t (:foreground ,dank-red-alt :weight bold))))
   `(org-level-8 ((t (:foreground ,dank-teal :weight bold))))
   `(org-document-title ((t (:foreground ,dank-cyan :weight bold :height 1.3))))
   `(org-document-info ((t (:foreground ,dank-blue))))
   `(org-todo ((t (:foreground ,dank-red :weight bold))))
   `(org-done ((t (:foreground ,success :weight bold))))
   `(org-headline-done ((t (:foreground ,dank-gray))))
   `(org-hide ((t (:foreground ,bg))))
   `(org-ellipsis ((t (:foreground ,dank-blue :underline nil))))
   `(org-table ((t (:foreground ,dank-magenta :inherit fixed-pitch))))
   `(org-formula ((t (:foreground ,dank-yellow-bright :inherit fixed-pitch))))
   `(org-checkbox ((t (:foreground ,dank-cyan :weight bold :inherit fixed-pitch))))
   `(org-date ((t (:foreground ,dank-teal :underline t))))
   `(org-special-keyword ((t (:foreground ,dank-gray :slant italic))))
   `(org-tag ((t (:foreground ,dank-gray :weight normal))))

   ;; Magit with enhanced diff colors
   `(magit-section-highlight ((t (:background ,surface-container-low))))
   `(magit-diff-hunk-heading ((t (:background ,surface-container :foreground ,dank-gray))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,surface-container-high :foreground ,on-surface))))
   `(magit-diff-context ((t (:foreground ,dank-gray))))
   `(magit-diff-context-highlight ((t (:background ,surface-container-low :foreground ,on-surface))))
   `(magit-diff-added ((t (:background ,success-container :foreground ,dank-green-bright))))
   `(magit-diff-added-highlight ((t (:background ,success-container :foreground ,dank-green-bright :weight bold))))
   `(magit-diff-removed ((t (:background ,err-container :foreground ,dank-red-alt))))
   `(magit-diff-removed-highlight ((t (:background ,err-container :foreground ,dank-red-alt :weight bold))))
   `(magit-hash ((t (:foreground ,dank-gray))))
   `(magit-branch-local ((t (:foreground ,dank-blue :weight bold))))
   `(magit-branch-remote ((t (:foreground ,dank-cyan :weight bold))))

   ;; Company
   `(company-tooltip ((t (:background ,surface-container :foreground ,on-surface))))
   `(company-tooltip-selection ((t (:background ,primary-container :foreground ,dank-cyan-bright))))
   `(company-tooltip-common ((t (:foreground ,dank-cyan))))
   `(company-tooltip-common-selection ((t (:foreground ,dank-cyan-bright :weight bold))))
   `(company-tooltip-annotation ((t (:foreground ,dank-yellow))))
   `(company-scrollbar-fg ((t (:background ,dank-cyan))))
   `(company-scrollbar-bg ((t (:background ,surface-variant))))
   `(company-preview ((t (:foreground ,dank-gray :slant italic))))
   `(company-preview-common ((t (:foreground ,dank-cyan :slant italic))))

   ;; Ido
   `(ido-first-match ((t (:foreground ,dank-cyan :weight bold))))
   `(ido-only-match ((t (:foreground ,dank-green :weight bold))))
   `(ido-subdir ((t (:foreground ,dank-blue))))
   `(ido-indicator ((t (:foreground ,dank-red))))
   `(ido-virtual ((t (:foreground ,dank-gray))))

   ;; Helm
   `(helm-selection ((t (:background ,primary-container :foreground ,dank-cyan-bright))))
   `(helm-match ((t (:foreground ,dank-cyan :weight bold))))
   `(helm-source-header ((t (:background ,surface-container-high :foreground ,dank-cyan :weight bold :height 1.1))))
   `(helm-candidate-number ((t (:foreground ,dank-yellow :weight bold))))
   `(helm-ff-directory ((t (:foreground ,dank-cyan :weight bold))))
   `(helm-ff-file ((t (:foreground ,on-surface))))
   `(helm-ff-executable ((t (:foreground ,dank-green))))

   ;; corfu
   `(corfu-default ((t (:background ,surface-container :foreground ,on-surface))))
   `(corfu-current ((t (:background ,primary-container :foreground ,dank-cyan-bright))))

   ;; Which-key
   `(which-key-key-face ((t (:foreground ,dank-cyan :weight bold))))
   `(which-key-separator-face ((t (:foreground ,outline-variant))))
   `(which-key-command-description-face ((t (:foreground ,on-surface))))
   `(which-key-group-description-face ((t (:foreground ,dank-blue))))
   `(which-key-special-key-face ((t (:foreground ,dank-yellow :weight bold))))

   ;; Line numbers
   `(line-number ((t (:foreground ,dank-gray :inherit fixed-pitch))))
   `(line-number-current-line ((t (:foreground ,dank-cyan :weight bold :inherit fixed-pitch))))

   ;; Parenthesis matching
   `(sp-show-pair-match-face ((t (:background ,primary-container :foreground ,dank-cyan-bright))))
   `(sp-show-pair-mismatch-face ((t (:background ,err-container :foreground ,on-err-container))))

   ;; Rainbow delimiters - vibrant colors
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,dank-cyan))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,dank-yellow))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,dank-green))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,dank-blue))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,dank-magenta))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,dank-cyan-bright))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,dank-yellow-bright))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,dank-green-bright))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,dank-red-alt))))
   `(rainbow-delimiters-mismatched-face ((t (:foreground ,err :weight bold))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,err :weight bold))))

   ;; Dired
   `(dired-directory ((t (:foreground ,dank-cyan :weight bold))))
   `(dired-ignored ((t (:foreground ,dank-gray))))
   `(dired-flagged ((t (:foreground ,dank-red))))
   `(dired-marked ((t (:foreground ,dank-yellow :weight bold))))
   `(dired-symlink ((t (:foreground ,dank-magenta :slant italic))))
   `(dired-header ((t (:foreground ,dank-cyan :weight bold :height 1.1))))

   ;; Terminal colors
   `(term-color-black ((t (:foreground ,term0 :background ,term0))))
   `(term-color-red ((t (:foreground ,term1 :background ,term1))))
   `(term-color-green ((t (:foreground ,term2 :background ,term2))))
   `(term-color-yellow ((t (:foreground ,term3 :background ,term3))))
   `(term-color-blue ((t (:foreground ,term4 :background ,term4))))
   `(term-color-magenta ((t (:foreground ,term5 :background ,term5))))
   `(term-color-cyan ((t (:foreground ,term6 :background ,term6))))
   `(term-color-white ((t (:foreground ,term7 :background ,term7))))

   ;; EShell
   `(eshell-prompt ((t (:foreground ,dank-cyan :weight bold))))
   `(eshell-ls-directory ((t (:foreground ,dank-cyan :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,dank-magenta :slant italic))))
   `(eshell-ls-executable ((t (:foreground ,dank-green))))
   `(eshell-ls-archive ((t (:foreground ,dank-yellow))))
   `(eshell-ls-backup ((t (:foreground ,dank-gray))))
   `(eshell-ls-clutter ((t (:foreground ,dank-red))))
   `(eshell-ls-missing ((t (:foreground ,dank-red))))
   `(eshell-ls-product ((t (:foreground ,on-surface-variant))))
   `(eshell-ls-readonly ((t (:foreground ,dank-gray))))
   `(eshell-ls-special ((t (:foreground ,dank-blue))))
   `(eshell-ls-unreadable ((t (:foreground ,dank-gray))))

   ;; Improved markdown mode
   `(markdown-header-face ((t (:foreground ,dank-cyan :weight bold))))
   `(markdown-header-face-1 ((t (:foreground ,dank-cyan :weight bold :height 1.2))))
   `(markdown-header-face-2 ((t (:foreground ,dank-blue :weight bold :height 1.1))))
   `(markdown-header-face-3 ((t (:foreground ,dank-magenta :weight bold))))
   `(markdown-header-face-4 ((t (:foreground ,dank-green :weight bold))))
   `(markdown-inline-code-face ((t (:foreground ,dank-yellow-bright :background ,surface-container-low :inherit fixed-pitch))))
   `(markdown-code-face ((t (:background ,surface-container-low :extend t :inherit fixed-pitch))))
   `(markdown-pre-face ((t (:background ,surface-container-low :inherit fixed-pitch))))
   `(markdown-table-face ((t (:foreground ,dank-magenta :inherit fixed-pitch))))

   ;; Web mode
   `(web-mode-html-tag-face ((t (:foreground ,dank-cyan))))
   `(web-mode-html-tag-bracket-face ((t (:foreground ,dank-gray))))
   `(web-mode-html-attr-name-face ((t (:foreground ,dank-yellow))))
   `(web-mode-html-attr-value-face ((t (:foreground ,dank-green))))
   `(web-mode-css-selector-face ((t (:foreground ,dank-cyan))))
   `(web-mode-css-property-name-face ((t (:foreground ,dank-blue))))
   `(web-mode-css-string-face ((t (:foreground ,dank-green))))

   ;; Flycheck
   `(flycheck-error ((t (:underline (:style wave :color ,err)))))
   `(flycheck-warning ((t (:underline (:style wave :color ,dank-yellow)))))
   `(flycheck-info ((t (:underline (:style wave :color ,dank-blue)))))
   `(flycheck-fringe-error ((t (:foreground ,err))))
   `(flycheck-fringe-warning ((t (:foreground ,dank-yellow))))
   `(flycheck-fringe-info ((t (:foreground ,dank-blue))))

   ;; Mini-buffer customization
   `(minibuffer-prompt ((t (:foreground ,dank-cyan :weight bold))))

   ;; Improved search highlighting
   `(lsp-face-highlight-textual ((t (:background ,primary-container :foreground ,dank-cyan-bright :weight bold))))
   `(lsp-face-highlight-read ((t (:background ,secondary-container :foreground ,dank-yellow-bright :weight bold))))
   `(lsp-face-highlight-write ((t (:background ,tertiary-container :foreground ,dank-green-bright :weight bold))))

   ;; Info and help modes
   `(info-title-1 ((t (:foreground ,dank-cyan :weight bold :height 1.3))))
   `(info-title-2 ((t (:foreground ,dank-blue :weight bold :height 1.2))))
   `(info-title-3 ((t (:foreground ,dank-magenta :weight bold :height 1.1))))
   `(info-title-4 ((t (:foreground ,dank-green :weight bold))))
   `(Info-quoted ((t (:foreground ,dank-yellow))))
   `(info-menu-header ((t (:foreground ,dank-cyan :weight bold))))
   `(info-menu-star ((t (:foreground ,dank-cyan))))
   `(info-node ((t (:foreground ,dank-blue :weight bold))))

   ;; Tabs
   `(tab-bar ((t (:background ,surface-container :foreground ,on-surface :box nil))))
   `(tab-bar-tab ((t (:background ,surface-container-high :foreground ,dank-cyan :weight bold :box nil))))
   `(tab-bar-tab-inactive ((t (:background ,surface :foreground ,dank-gray :box nil))))

   `(tab-line ((t (:background ,surface-container :foreground ,on-surface :box nil))))
   `(tab-line-tab ((t (:background ,surface :foreground ,dank-gray :box nil))))
   `(tab-line-tab-current ((t (:background ,surface-container-high :foreground ,dank-cyan :weight bold :box nil))))
   `(tab-line-tab-inactive ((t (:background ,surface :foreground ,dank-gray :box nil))))
   `(tab-line-highlight ((t (:background ,surface-container-highest :foreground ,dank-cyan-bright))))

   `(centaur-tabs-default ((t (:background ,surface-container :foreground ,on-surface))))
   `(centaur-tabs-selected ((t (:background ,surface-container-high :foreground ,dank-cyan :weight bold))))
   `(centaur-tabs-unselected ((t (:background ,surface :foreground ,dank-gray))))
   `(centaur-tabs-selected-modified ((t (:background ,surface-container-high :foreground ,dank-yellow :weight bold))))
   `(centaur-tabs-unselected-modified ((t (:background ,surface :foreground ,dank-yellow))))
   `(centaur-tabs-active-bar-face ((t (:background ,dank-cyan))))

   ;; Fixed-pitch faces
   `(fixed-pitch ((t (:family "monospace"))))
   `(fixed-pitch-serif ((t (:family "monospace serif"))))

   ;; Variable-pitch face
   `(variable-pitch ((t (:family "sans serif"))))
   ))

;; Add org-mode hooks for hiding leading stars
(with-eval-after-load 'org
  (setq org-hide-leading-stars t)
  (setq org-startup-indented t))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'dank-emacs)
;;; dank-emacs-theme.el ends here
