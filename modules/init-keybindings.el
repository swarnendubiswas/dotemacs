;;; init-keybindings.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp;
;;; coding:utf-8; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)
(defvar popup-menu-keymap)
(defvar sb/tab-bar-handler)

(declare-function sb/comment-line "init-functions")
(declare-function sb/save-all-buffers "init-functions")
(declare-function sb/next-buffer "init-functions")
(declare-function sb/previous-buffer "init-functions")
(declare-function sb/switch-to-scratch "init-functions")
(declare-function sb/counsel-all-files-recursively "init-functions")

;; "<f10" and "<f11>" conflict with Gnome window manager keybindings
(bind-keys
 ("RET"       . newline-and-indent)
 ("C-l"       . goto-line)
 ("C-c z"     . repeat)
 ("C-z"       . undo)
 ("C-<f11>"   . delete-other-windows)
 ("C-x k"     . kill-this-buffer)
 ("M-<left>"  . previous-buffer)
 ("C-S-<iso-lefttab>" . previous-buffer)
 ("M-<right>" . next-buffer)
 ("C-<tab>"   . next-buffer)
 ("C-c d f"   . auto-fill-mode)
 ("M-c"       . capitalize-dwim)
 ("M-u"       . upcase-dwim)
 ("M-l"       . downcase-dwim)
 ("<f7>"      . previous-error)
 ("<f8>"      . next-error)
 ;; The default keybinding "C-S-backspace" does not work with the TUI without Alacritty customizations.
 ("M-k"       . kill-whole-line))

;; In a line with comments, "C-u M-;" removes the comments altogether. That means deleting the
;; comment, NOT UNCOMMENTING but removing all commented text and the comment marker itself.
(bind-keys
 ("C-c n" . comment-region)
 ("C-c m" . uncomment-region)
 ("C-c ;" . sb/comment-line)
 ("C-c b" . comment-box))

(bind-keys
 ("C-s"   . save-buffer)
 ("C-S-s" . sb/save-all-buffers))

(bind-keys
 ("M-\\" . delete-horizontal-space)
 ("M-#" . cycle-spacing))

(unbind-key "C-]") ; Bound to `abort-recursive-edit'

(unbind-key "C-x s") ; Bound to `save-some-buffers'
(bind-key   "C-x s" #'sb/switch-to-scratch)

(with-eval-after-load "popup"
  (bind-key "[escape]" #'keyboard-quit popup-menu-keymap))

(when (eq sb/minibuffer-completion 'ivy)
  (bind-key "C-x j" #'sb/counsel-all-files-recursively))

(unless sb/tab-bar-handler
  (global-set-key [remap next-buffer]     #'sb/next-buffer)
  (global-set-key [remap previous-buffer] #'sb/previous-buffer))

(use-package default-text-scale
  :if (display-graphic-p)
  :bind
  (("C-M-+" . default-text-scale-increase)
   ("C-M--" . default-text-scale-decrease)))

(use-package free-keys
  :commands free-keys)

(use-package keyfreq
  :commands (keyfreq-mode keyfreq-autosave-mode)
  :hook
  (after-init-hook . (lambda ()
                       (keyfreq-mode 1)
                       (keyfreq-autosave-mode 1))))

(use-package which-key ; Show help popups for prefix keys
  :commands which-key-setup-side-window-right-bottom
  :hook
  (after-init-hook . which-key-mode)
  :custom
  ;; Allow "C-h" to trigger `which-key' before it is done automatically
  (which-key-show-early-on-C-h nil)
  (which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-setup-side-window-right-bottom)
  :diminish)

(use-package which-key-posframe
  :if (display-graphic-p)
  :hook
  (which-key-mode-hook . which-key-posframe-mode)
  :custom
  ;; Thicker border makes the posframe easier to distinguish
  (which-key-posframe-border-width 4)
  ;; Positioning the frame at the top obstructs the view to a lesser degree
  (which-key-posframe-poshandler 'posframe-poshandler-frame-top-center)
  ;; :config
  ;; Modify the posframe background if it has a low contrast
  ;; (set-face-attribute 'which-key-posframe nil :background "floralwhite" :foreground "black")
  )

;; Hydras, https://github.com/abo-abo/hydra

;; ":exit nil" means the hydra state will continue, ":exit t" will quit the hydra. ":color red"
;; means continue the hydra on a valid key but stop when a foreign key has been pressed. ":color
;; blue" means exit.

(use-package hydra
  :commands (hydra-default-pre hydra-keyboard-quit defhydra
                               hydra-show-hint hydra-set-transient-map
                               hydra--call-interactively-remap-maybe))

(use-package hydra-posframe
  :straight (hydra-posframe :type git :host github :repo "Ladicle/hydra-posframe")
  :if (display-graphic-p)
  :after hydra
  :commands hydra-posframe-mode
  :init (hydra-posframe-mode 1))

(use-package ivy-hydra ; Additional keybindings for `ivy'
  :after (ivy hydra)
  :demand t
  :commands (ivy-dispatching-done-hydra ivy--matcher-desc ivy-hydra/body))

(use-package pretty-hydra
  :after hydra
  :commands pretty-hydra-define)

(declare-function s-concat "s")
(declare-function all-the-icons-octicon "all-the-icons")
(declare-function all-the-icons-alltheicon "all-the-icons")
(declare-function all-the-icons-faicon "all-the-icons")
(declare-function all-the-icons-fileicon "all-the-icons")

;; https://github.com/WalkerGriggs/dot-emacs/blob/master/configs/hydra.el
(defun sb/with-alltheicon (icon str &optional height v-adjust)
  "Displays ICON from all-the-icon in STR."
  (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun sb/with-faicon (icon str &optional height v-adjust)
  "Displays icon from Font Awesome icon in STR."
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun sb/with-fileicon (icon str &optional height v-adjust)
  "Displays an icon from the Atom File Icons package."
  (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun sb/with-octicon (icon str &optional height v-adjust)
  "Displays an icon from the GitHub Octicons."
  (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(declare-function spell-fu-goto-next-error "spell-fu")
(declare-function spell-fu-goto-previous-error "spell-fu")
(declare-function spell-fu-word-add "spell-fu")
(declare-function flyspell-correct-previous "flyspell")
(declare-function flyspell-correct-next "flyspell")

(pretty-hydra-define sb/hydra-spelling
  (:color amaranth :quit-key "q" :title (sb/with-faicon "magic" "Spell check" 1 -0.05)
          :foreign-keys warn)
  ("Action"
   (("c" ispell "ispell")
    ("f" flyspell-buffer "flyspell buffer"))
   "Correct"
   (("<" flyspell-correct-previous "correct previous error")
    (">" flyspell-correct-next "correct next error"))
   "Spell fu"
   (("n" spell-fu-goto-next-error "next error")
    ("p" spell-fu-goto-previous-error "previous error")
    ("a" spell-fu-word-add "add word"))))

(declare-function default-text-scale-decrease "default-text-scale")
(declare-function default-text-scale-increase "default-text-scale")

(pretty-hydra-define sb/hydra-text-scale-zoom
  (:color amaranth :quit-key "q" :title "Zoom action" :foreign-keys warn)
  (""
   (("i" default-text-scale-increase "zoom in")
    ("o" default-text-scale-decrease "zoom out")
    ("r" default-text-scale-reset "reset"))))

(pretty-hydra-define sb/hydra-error
  (:color amaranth :quit-key "q" :title "Navigate errors" :foreign-keys warn)
  (""
   (("h" first-error "first error")
    ("j" next-error "next error")
    ("k" previous-error "previous error")
    ("v" recenter-top-bottom "recenter"))))

;; https://github.com/abo-abo/hydra/wiki/avy
(pretty-hydra-define sb/hydra-avy
  (:color red :title "Actions" :quit-key "q" :foreign-keys warn)
  ("Line"
   (("y" avy-copy-line "yank line")
    ("m" avy-move-line "move line")
    ("l" avy-goto-line "go to line")
    ("L" avy-goto-end-of-line "go to end of line")
    ("k" avy-kill-whole-line "kill whole line"))
   "Region"
   (("Y" avy-copy-region "copy region ")
    ("M" avy-move-region "move region")
    ("K" avy-kill-region "kill region"))
   "Word"
   (("w" avy-goto-word-1 "go to word")
    ("W" avy-goto-word-0 "go to word"))
   "Character"
   (("c" avy-goto-char-timer "go to char with timer")
    ("C" avy-goto-char "go to char"))))

(declare-function projectile-add-known-project "projectile")
(declare-function projectile-remove-known-project "projectile")
(declare-function projectile-find-dir "projectile")
(declare-function projectile-invalidate-cache "projectile")
(declare-function projectile-compile "projectile")
(declare-function projectile-add-known-project "projectile")
(declare-function projectile-remove-known-project "projectile")

(pretty-hydra-define sb/hydra-projectile
  (:color teal :hint nil global-map "C-c p" :quit-key "q" :foreign-keys warn)
  ("PROJECTILE: %(projectile-project-root)"
   (("p"   projectile-switch-project "switch project")
    ("s"   projectile-switch-project "switch project")
    ("a"   projectile-add-known-project "add known project")
    ("x"   projectile-remove-known-project "remove known project")
    ("X"   projectile-cleanup-known-projects "clean up known projects"))
   "Files"
   (("f"   projectile-find-file "find file")
    ("F"   projectile-find-file-dwim "find file dwim")
    ("d"   projectile-find-dir "find directory")
    ("D"   projectile-find-file-in-directory "file file in directory"))
   "Buffers"
   (("i"   projectile-ibuffer "ibuffer")
    ("b"   projectile-switch-to-buffer "switch to buffer")
    ("r"   projectile-recentf "recent files")
    ("k"   projectile-kill-buffers "kill all buffers"))
   "Misc"
   (("c"   projectile-invalidate-cache "invalidate cache")
    ("z"   projectile-cache-current-file "cache current file")
    ("g"   projectile-find-tag "find tag")
    ("o"   projectile-multi-occur "multi occur")
    ("m"   projectile-compile "compile"))))

(pretty-hydra-define sb/hydra-move-text
  (:quit-key "q" :title "Move text" :foreign-keys warn)
  (""
   (("u" move-text-up "up")
    ("d" move-text-down "down"))))

(declare-function flycheck-verify-setup "flycheck")
(declare-function flycheck-previous-error "flycheck")
(declare-function flycheck-next-error "flycheck")
(declare-function flycheck-list-errors "flycheck")
(declare-function flycheck-select-checker "flycheck")
(declare-function flycheck-describe-checker "flycheck")
(declare-function flycheck-disable-checker "flycheck")
(declare-function flycheck-buffer "flycheck")

(pretty-hydra-define sb/hydra-flycheck
  (:color blue :quit-key "q" :title (sb/with-faicon "plane" "Flycheck actions" 1 -0.05)
          :foreign-keys warn)
  ("Setup"
   (("M" flycheck-manual "Manual")
    ("m" flycheck-mode "enable mode")
    ("v" flycheck-verify-setup "Verify setup"))
   "Errors"
   (("<" flycheck-previous-error :color pink "previous error")
    (">" flycheck-next-error :color pink "next error")
    ("f" flycheck-buffer "check buffer")
    ("l" flycheck-list-errors "list"))
   "Checker"
   (("d" flycheck-disable-checker "disable")
    ("s" flycheck-select-checker "select checker")
    ("?" flycheck-describe-checker "describe checker"))))

(declare-function python-indent-shift-left "python")
(declare-function python-indent-shift-right "python")

(pretty-hydra-define sb/hydra-python-indent
  (:quit-key "q" :title "Adjust Python indentation." :foreign-keys warn)
  (""
   ((">" python-indent-shift-right "right")
    ("<" python-indent-shift-left "left"))))

(with-eval-after-load "python-mode"
  (defvar python-mode-map)

  (bind-key "C-c" #'sb/hydra-python-indent/body python-mode-map))

(declare-function smerge-keep-all "smerge-mode")
(declare-function smerge-keep-base "smerge-mode")
(declare-function smerge-keep-upper "smerge-mode")
(declare-function smerge-keep-lower "smerge-mode")
(declare-function smerge-keep-current "smerge-mode")
(declare-function smerge-next "smerge-mode")
(declare-function smerge-prev "smerge-mode")
(declare-function smerge-auto-leave "smerge-mode")
(declare-function smerge-diff-base-upper "smerge-mode")
(declare-function smerge-diff-upper-lower "smerge-mode")
(declare-function smerge-diff-base-lower "smerge-mode")
(declare-function smerge-refine "smerge-mode")
(declare-function smerge-resolve "smerge-mode")
(declare-function smerge-combine-with-next "smerge-mode")
(declare-function smerge-kill-current "smerge-mode")

(pretty-hydra-define sb/hydra-smerge
  (:color pink :hint nil :post (smerge-auto-leave) :quit-key "q"
          :title (with-alltheicon "git" "Merge actions" 1 -0.05) :foreign-keys warn)
  ("Conflict actions"
   (("n" smerge-next "Next conflict")
    ("p" smerge-prev "Previous conflict"))
   "Keep actions"
   (("b" smerge-keep-base "Keep base")
    ("u" smerge-keep-upper "Keep upper")
    ("l" smerge-keep-lower "Keep lower")
    ("a" smerge-keep-all "Keep all")
    ("RET" smerge-keep-current "Keep current")
    ("\C-m" smerge-keep-current "Keep current"))
   "Diff actions"
   (("<" smerge-diff-base-upper "Diff base upper")
    ("=" smerge-diff-upper-lower "Diff base lower")
    (">" smerge-diff-base-lower "Diff base lower")
    ("R" smerge-refine "Refine")
    ("E" smerge-ediff "Ediff"))
   "Others"
   (("C" smerge-combine-with-next "Combine")
    ("r" smerge-resolve "Resolve")
    ("k" smerge-kill-current "Kill current"))))

(declare-function mc/mark-previous-like-this "multiple-cursors")
(declare-function mc/mark-next-like-this "multiple-cursors")
(declare-function mc/unmark-previous-like-this "multiple-cursors")
(declare-function mc/skip-to-previous-like-this "multiple-cursors")
(declare-function mc/edit-lines "multiple-cursors")

(pretty-hydra-define sb/hydra-multiple-cursors
  (:hint nil :quit-key "q" :title "Multiple cursors" :foreign-keys warn)
  ("Up"
   (("p" mc/mark-previous-like-this "Up next")
    ("P" mc/skip-to-previous-like-this "Up skip")
    ("M-p" mc/unmark-previous-like-this "Unmark"))
   "Down"
   (("n" mc/mark-next-like-this "Down next")
    ("N" mc/skip-to-next-like-this "Down skip")
    ("M-n" mc/unmark-next-like-this "Down unmark"))
   "Others"
   (("l" mc/edit-lines :exit t "Edit lines")
    ("a" mc/mark-all-like-this :exit t "Mark all")
    ("r" mc/mark-all-in-region-regexp :exit t "Mark by regexp"))))

(declare-function sp-beginning-of-sexp "smartparens")
(declare-function sp-end-of-sexp "smartparens")
(declare-function sp-forward-sexp "smartparens")
(declare-function sp-backward-sexp "smartparens")
(declare-function sp-backward-down-sexp "smartparens")
(declare-function sp-backward-up-sexp "smartparens")
(declare-function sp-up-sexp "smartparens")
(declare-function sp-down-sexp "smartparens")
(declare-function sp-backward-slurp-sexp "smartparens")
(declare-function sp-backward-barf-sexp "smartparens")
(declare-function sp-forward-slurp-sexp "smartparens")
(declare-function sp-forward-barf-sexp "smartparens")
(declare-function sp-rewrap-sexp "smartparens")
(declare-function sp-unwrap-sexp "smartparens")
(declare-function sp-backward-unwrap-sexp "smartparens")
(declare-function sp-wrap-round "smartparens")
(declare-function sp-wrap-curly "smartparens")
(declare-function sp-wrap-square "smartparens")
(declare-function sp-split-sexp "smartparens")
(declare-function sp-splice-sexp "smartparens")
(declare-function sp-raise-sexp "smartparens")
(declare-function sp-join-sexp "smartparens")
(declare-function sp-transpose-sexp "smartparens")
(declare-function sp-absorb-sexp "smartparens")
(declare-function sp-emit-sexp "smartparens")
(declare-function sp-convolute-sexp "smartparens")
(declare-function sp-change-inner "smartparens")
(declare-function sp-change-enclosing "smartparens")
(declare-function sp-kill-sexp "smartparens")
(declare-function sp-backward-kill-sexp "smartparens")
(declare-function sp-copy-sexp "smartparens")

(pretty-hydra-define sb/hydra-smartparens
  (:hint nil :quit-key "q" :title "Smartparens" :foreign-keys warn)
  ("Moving"
   (("a" sp-beginning-of-sexp "Beginning")
    ("e" sp-end-of-sexp "End")
    ("f" sp-forward-sexp "Forward")
    ("b" sp-backward-sexp "Backward")
    ("n" sp-down-sexp "Down")
    ("N" sp-backward-down-sexp "Backward down")
    ("p" sp-up-sexp "Up")
    ("P" sp-backward-up-sexp "Backward up"))
   "Slurping & barfing"
   (("h" sp-backward-slurp-sexp "Backward slurp")
    ("H" sp-backward-barf-sexp "Backward barf")
    ("l" sp-forward-slurp-sexp "Slurp")
    ("L" sp-forward-barf-sexp "Forward barf"))
   "Wrapping"
   (("R" sp-rewrap-sexp "Rewrap")
    ("u" sp-unwrap-sexp "Unwrap")
    ("U" sp-backward-unwrap-sexp "Backward unwrap")
    ("(" sp-wrap-round "Wrap parenthesis")
    ("{" sp-wrap-curly "Wrap curly")
    ("[" sp-wrap-square "Wrap square"))
   "Sexp juggling"
   (("S" sp-split-sexp "Split")
    ("s" sp-splice-sexp "Splice")
    ("r" sp-raise-sexp "Raise")
    ("j" sp-join-sexp "Join")
    ("t" sp-transpose-sexp "Transpose")
    ("A" sp-absorb-sexp "Absorb")
    ("E" sp-emit-sexp "Emit")
    ("o" sp-convolute-sexp "Convolute"))
   "Destructive editing"
   (("c" sp-change-inner :exit t "Change inner")
    ("C" sp-change-enclosing :exit t "Change outer")
    ("k" sp-kill-sexp "Kill sexp")
    ("K" sp-backward-kill-sexp "Backward kill sexp")
    ("w" sp-copy-sexp "Copy sexp"))))

(declare-function lsp-format-buffer "lsp-mode")
(declare-function lsp-ui-imenu "lsp-mode")
(declare-function lsp-execute-code-action "lsp-mode")
(declare-function lsp-describe-session "lsp-mode")
(declare-function lsp-workspace-restart "lsp-mode")
(declare-function lsp-workspace-shutdown "lsp-mode")
(declare-function lsp-find-declaration "lsp-mode")
(declare-function lsp-ui-peek-find-definitions "lsp-mode")
(declare-function lsp-ui-peek-find-references "lsp-mode")
(declare-function lsp-ui-peek-find-implementation "lsp-mode")
(declare-function lsp-find-type-definition "lsp-mode")
(declare-function lsp-signature-help "lsp-mode")
(declare-function lsp-describe-thing-at-point "lsp-mode")
(declare-function lsp-rename "lsp-mode")

(pretty-hydra-define sb/hydra-lsp
  (:exit t :hint nil :quit-key "q" :title "LSP Mode" :foreign-keys warn)
  ("Buffer"
   (("f" lsp-format-buffer "Format buffer")
    ("m" lsp-ui-imenu "Imenu")
    ("x" lsp-execute-code-action "Execute code action"))
   "Server"
   (("M-s" lsp-describe-session "Describe session")
    ("M-r" lsp-workspace-restart "Restart language server")
    ("S" lsp-workspace-shutdown "Shutdown"))
   "Symbol"
   (("d" lsp-find-declaration "Find declaration")
    ("D" lsp-ui-peek-find-definitions "Find definitions")
    ("R" lsp-ui-peek-find-references "Find references")
    ("i" lsp-ui-peek-find-implementation "Find implementation")
    ("t" lsp-find-type-definition "Type definition")
    ("s" lsp-signature-help "Signature")
    ("o" lsp-describe-thing-at-point "Describe thing at point")
    ("r" lsp-rename "Rename"))))

(declare-function markdown-insert-bold "markdown-mode")
(declare-function markdown-insert-italic "markdown-mode")
(declare-function markdown-insert-blockquote "markdown-mode")
(declare-function markdown-insert-pre "markdown-mode")
(declare-function markdown-insert-code "markdown-mode")
(declare-function markdown-insert-header-dwim "markdown-mode")
(declare-function markdown-insert-header-atx-1 "markdown-mode")
(declare-function markdown-insert-header-atx-2 "markdown-mode")
(declare-function markdown-insert-header-atx-3 "markdown-mode")
(declare-function markdown-insert-header-atx-4 "markdown-mode")
(declare-function markdown-insert-list-item "markdown-mode")
(declare-function markdown-promote "markdown-mode")
(declare-function markdown-demote "markdown-mode")
(declare-function markdown-move-up "markdown-mode")
(declare-function markdown-move-down "markdown-mode")
(declare-function markdown-insert-link "markdown-mode")
(declare-function markdown-insert-uri "markdown-mode")
(declare-function markdown-insert-footnote "markdown-mode")
(declare-function markdown-insert-wiki-link "markdown-mode")
(declare-function markdown-insert-reference-link-dwim "markdown-mode")

(pretty-hydra-define sb/hydra-markdown-mode
  (:hint nil :title "Markdown mode" :quit-key "q" :foreign-keys warn)
  ("Formatting"
   (("s" markdown-insert-bold "Bold")
    ("e" markdown-insert-italic "Italic")
    ("b" markdown-insert-blockquote :color blue "Blockquote")
    ("p" markdown-insert-pre :color blue "Pre-formatted")
    ("c" markdown-insert-code "Code"))
   "Headings"
   (("h" markdown-insert-header-dwim "DWIM")
    ("1" markdown-insert-header-atx-1 "H1")
    ("2" markdown-insert-header-atx-2 "H2")
    ("3" markdown-insert-header-atx-3 "H3")
    ("4" markdown-insert-header-atx-4 "H4"))
   "Lists"
   (("m" markdown-insert-list-item "Insert list item"))
   "Promote/Demote"
   (("l" markdown-promote "Promote")
    ("r" markdown-demote "Demote")
    ("d" markdown-move-down "Move down")
    ("u" markdown-move-up "Move up"))
   "Links"
   (("L" markdown-insert-link :color blue "Insert link")
    ("U" markdown-insert-uri :color blue "Insert uri")
    ("F" markdown-insert-footnote :color blue "Insert footnote")
    ("W" markdown-insert-wiki-link :color blue "Insert wiki link")
    ("R" markdown-insert-reference-link-dwim :color blue "Insert reference link"))))

(declare-function straight-check-all "straight")
(declare-function straight-check-package "straight")
(declare-function straight-rebuild-all "straight")
(declare-function straight-rebuild-package "straight")
(declare-function straight-fetch-all "straight")
(declare-function straight-fetch-package "straight")
(declare-function straight-pull-all "straight")
(declare-function straight-pull-package "straight")
(declare-function straight-merge-all "straight")
(declare-function straight-merge-package "straight")
(declare-function straight-normalize-all "straight")
(declare-function straight-normalize-package "straight")
(declare-function straight-freeze-versions "straight")
(declare-function straight-thaw-versions "straight")
(declare-function straight-get-recipe "straight")

(pretty-hydra-define sb/hydra-straight
  (:hint nil :quit-key "q" :title "Straight actions" :foreign-keys warn)
  (""
   (("c" straight-check-all "check all")
    ("C" straight-check-package "check package")
    ("r" straight-rebuild-all "rebuild all")
    ("R" straight-rebuild-package "rebuild package")
    ("f" straight-fetch-all "fetch all")
    ("F" straight-fetch-package "fetch package")
    ("p" straight-pull-all "pull all")
    ("P" straight-pull-package "pull package")
    ("m" straight-merge-all "merge all")
    ("M" straight-merge-package "merge package")
    ("n" straight-normalize-all "normalize all")
    ("N" straight-normalize-package "normalize package")
    ("v" straight-freeze-versions "freeze versions")
    ("V" straight-thaw-versions "thaw versions")
    ("g" straight-get-recipe "get recipe"))))

(pretty-hydra-define sb/hydra-comments
  (:hint nil :color teal :exit t :title "Commentary Actions" :foreign-keys warn)
  (""
   (("b" comment-box)
    ("c" comment-dwim)
    ("l" comment-line)
    ("r" comment-region))))

(pretty-hydra-define sb/hydra-magit
  (:hint nil :color teal :quit-key "q" :title (sb/with-faicon "code-fork" "Git" 1 -0.05)
         :foreign-keys warn)
  ("Action"
   (("b" magit-blame "blame")
    ("c" magit-commit "commit")
    ("i" magit-init "init")
    ("l" magit-log-buffer-file "commit log (current file)")
    ("L" magit-log-current "commit log (project)")
    ("s" magit-status "status"))))

(bind-key "C-c h a" #'sb/hydra-avy/body)
(bind-key "C-c h d" #'sb/hydra-markdown-mode/body)
(bind-key "C-c h e" #'sb/hydra-error/body)
(bind-key "C-c h f" #'sb/hydra-flycheck/body)
(bind-key "C-c h g" #'sb/hydra-smerge/body)
(bind-key "C-c h j" #'sb/hydra-projectile/body)
(bind-key "C-c h l" #'sb/hydra-lsp/body)
(bind-key "C-c h m" #'sb/hydra-multiple-cursors/body)
(bind-key "C-c h p" #'sb/hydra-smartparens/body)
(bind-key "C-c h s" #'sb/hydra-spelling/body)
(bind-key "C-c h t" #'sb/hydra-move-text/body)
(bind-key "C-c h z" #'sb/hydra-text-scale-zoom/body)
(bind-key "C-c h i" #'sb/hydra-straight/body)
(bind-key "C-c h v" #'sb/hydra-magit/body)
(bind-key "C-c h y" #'sb/hydra-python-indent/body)

(pretty-hydra-define sb/hydra-help
  (:color teal :title "Hydra Overview" :foreign-keys warn)
  ("Groups"
   (("a" sb/hydra-avy/body "+ avy")
    ("d" sb/hydra-markdown-mode/body "+ markdown")
    ("e" sb/hydra-error/body "+ error")
    ("f" sb/hydra-flycheck/body "+ flycheck")
    ("g" sb/hydra-smerge/body "+ smerge")
    ("j" sb/hydra-projectile/body "+ projectile")
    ("l" sb/hydra-lsp/body "+ lsp")
    ("m" sb/hydra-multiple-cursors/body "+ multiple cursors")
    ("p" sb/hydra-smartparens/body "+ smartparens")
    ("s" sb/hydra-spelling/body "+ spelling")
    ("t" sb/hydra-move-text/body "+ move text")
    ("z" sb/hydra-text-scale-zoom/body "+ text scale")
    ("i" sb/hydra-straight/body "+ straight")
    ("v" sb/hydra-magit/body "+ magit")
    ("y" sb/hydra-python-indent/body "+ python indent"))))

(bind-key "C-c h h" #'sb/hydra-help/body)

(use-package term-keys
  :straight (term-keys :type git :host github :repo "CyberShadow/term-keys")
  :hook
  (after-init-hook . term-keys-mode)
  :config
  (require 'term-keys-alacritty))

(transient-define-prefix sb/help-transient ()
  ["Help Commands"
   ["Mode & Bindings"
    ("m" "Mode" describe-mode)
    ("b" "Major Bindings" which-key-show-full-major-mode)
    ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
    ("d" "Descbinds" describe-bindings)
    ]
   ["Describe"
    ("c" "Command" helpful-command)
    ("f" "Function" helpful-callable)
    ("o" "Symbol"  helpful-symbol)
    ("v" "Variable" helpful-variable)
    ("k" "Key" helpful-key)
    ]
   ["Info on"
    ("C-c" "Emacs Command" Info-goto-emacs-command-node)
    ("C-f" "Function" info-lookup-symbol)
    ("C-v" "Variable" info-lookup-symbol)
    ("C-k" "Emacs Key" Info-goto-emacs-key-command-node)
    ]
   ["Goto Source"
    ("L" "Library" find-library)
    ("F" "Function" find-function)
    ("V" "Variable" find-variable)
    ("K" "Key" find-function-on-key)
    ]
   ]
  [
   ["Internals"
    ("e" "Echo Messages" view-echo-area-messages)
    ("l" "Lossage" view-lossage)
    ]
   ["Describe"
    ("s" "Symbol" helpful-symbol)
    ("." "At Point   " helpful-at-point)
    ("C-d" "Face" describe-face)
    ("w" "Where Is" where-is)
    ("=" "Position" what-cursor-position)
    ]
   ["Info Manuals"
    ("C-i" "Info" info)
    ("C-4" "Other Window " info-other-window)
    ]
   ["Exit"
    ("q" "Quit" transient-quit-one)
    ("<escape>" "Quit" transient-quit-one)
    ]
   ]
  [
   ["External"
    ("W" "Dictionary" dictionary-lookup-definition)
    ]
   ]
  )
(bind-key "M-H" #'sb/help-transient)

(provide 'init-keybindings)

;;; init-keybindings.el ends here
