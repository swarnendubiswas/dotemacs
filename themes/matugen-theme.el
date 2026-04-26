(deftheme matugen "Pure Matugen theme")

(let* (
       ;; ===== Core =====
       (bg "#181115")
       (fg "#eddfe4")

       (primary "#fab1d9")
       (primary-container "#6b3456")
       (on-primary-container "#ffd8eb")

       (secondary "#debecd")
       (secondary-container "#58404d")

       (tertiary "#f4ba9d")
       (tertiary-container "#653d27")

       (surface "#181115")
       (surface-variant "#504349")

       (surface-container-low "#211a1d")
       (surface-container "#251e21")
       (surface-container-high "#30282b")

       (outline "#9c8d93")
       (outline-variant "#504349")

       (error "#ffb4ab")
       (error-container "#93000a")
       (on-error-container "#ffdad6")
       )

  (custom-theme-set-faces
   'matugen

   ;; ===== Basics =====
   `(default ((t (:background ,bg :foreground ,fg))))
   `(cursor ((t (:background ,primary))))
   `(region ((t (:background ,primary-container :extend t))))
   `(highlight ((t (:background ,surface-container))))
   `(fringe ((t (:background ,surface :foreground ,outline))))
   `(vertical-border ((t (:foreground ,surface-variant))))

   ;; ===== Syntax =====
   `(font-lock-builtin-face ((t (:foreground ,primary))))
   `(font-lock-comment-face ((t (:foreground ,outline :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,outline-variant))))
   `(font-lock-constant-face ((t (:foreground ,secondary-container :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,fg :slant italic))))
   `(font-lock-function-name-face ((t (:foreground ,primary :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,primary :weight bold))))
   `(font-lock-string-face ((t (:foreground ,tertiary))))
   `(font-lock-type-face ((t (:foreground ,secondary))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-warning-face ((t (:foreground ,error :weight bold))))

   ;; ===== Parens =====
   `(show-paren-match ((t (:background ,primary-container :foreground ,on-primary-container :weight bold))))
   `(show-paren-mismatch ((t (:background ,error-container :foreground ,on-error-container :weight bold))))

   ;; ===== Mode line =====
   `(mode-line ((t (:background ,surface-container :foreground ,fg :box nil))))
   `(mode-line-inactive ((t (:background ,surface :foreground ,outline :box nil))))

   ;; ===== Org =====
   `(org-block ((t (:background ,surface-container-low :extend t))))
   `(org-code ((t (:background ,surface-container-low :foreground ,secondary))))
   `(org-level-1 ((t (:foreground ,primary :weight bold :height 1.2))))
   `(org-level-2 ((t (:foreground ,secondary :weight bold :height 1.1))))
   `(org-level-3 ((t (:foreground ,tertiary :weight bold))))
   `(org-level-4 ((t (:foreground ,primary-container :weight bold))))

   ;; ===== Magit =====
   `(magit-diff-added ((t (:background ,tertiary-container :foreground ,tertiary))))
   `(magit-diff-removed ((t (:background ,error-container :foreground ,error))))

   ;; ===== Company =====
   `(company-tooltip ((t (:background ,surface-container :foreground ,fg))))
   `(company-tooltip-selection ((t (:background ,primary-container))))

   ;; ===== Line numbers =====
   `(line-number ((t (:foreground ,outline))))
   `(line-number-current-line ((t (:foreground ,primary :weight bold))))

   ;; ===== Rainbow =====
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,primary))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,secondary))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,tertiary))))
   `(rainbow-delimiters-mismatched-face ((t (:foreground ,error :weight bold))))

   ;; ===== Terminal =====
   `(term-color-black ((t (:foreground ,bg :background ,bg))))
   `(term-color-red ((t (:foreground ,error :background ,error))))
   `(term-color-green ((t (:foreground ,tertiary :background ,tertiary))))
   `(term-color-yellow ((t (:foreground ,secondary :background ,secondary))))
   `(term-color-blue ((t (:foreground ,primary :background ,primary))))
   `(term-color-magenta ((t (:foreground ,tertiary-container :background ,tertiary-container))))
   `(term-color-cyan ((t (:foreground ,primary-container :background ,primary-container))))
   `(term-color-white ((t (:foreground ,fg :background ,fg))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'matugen)
(provide 'matugen-theme)
