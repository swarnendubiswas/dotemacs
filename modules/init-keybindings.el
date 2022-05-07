;;; init-keybindings.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp;
;;; coding:utf-8; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)
(defvar popup-menu-keymap)

(declare-function sb/comment-line "init-functions")
(declare-function sb/save-all-buffers "init-functions")
(declare-function sb/next-buffer "init-functions")
(declare-function sb/previous-buffer "init-functions")
(declare-function sb/switch-to-scratch "init-functions")
(declare-function sb/counsel-all-files-recursively "init-functions")

(bind-keys
 ("RET"       . newline-and-indent)
 ("C-l"       . goto-line)
 ("C-c z"     . repeat)
 ("C-z"       . undo)
 ;; ("<f11>"     . delete-other-windows) ; Conflicts with Gnome window manager keybindings
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
 ;; The default keybinding "C-S-backspace" does not work with the TUI.
 ("M-k"       . kill-whole-line))

;; In a line with comments, "C-u M-;" removes the comments altogether. That means deleting the
;; comment, NOT UNCOMMENTING but removing all commented text and the comment marker itself.
(bind-keys*
 ("C-c n" . comment-region)
 ("C-c m" . uncomment-region)
 ("C-c ;" . sb/comment-line)
 ("C-c b" . comment-box))

(bind-keys*
 ("C-s"   . save-buffer)
 ("C-S-s" . sb/save-all-buffers))

(unbind-key "C-]") ; Bound to `abort-recursive-edit'

(unbind-key "C-x s") ; Bound to `save-some-buffers'
(bind-key   "C-x s" #'sb/switch-to-scratch)

;; Both `counsel-imenu' and `consult-imenu' show lot of unnecessary information
(bind-key "C-c C-j" #'imenu)

(with-eval-after-load "popup"
  (bind-key "[escape]" #'keyboard-quit popup-menu-keymap))

(when (eq sb/minibuffer-completion 'ivy)
  (bind-key   "C-x j" #'sb/counsel-all-files-recursively))

(unless (featurep 'centaur-tabs)
  (global-set-key [remap next-buffer]     #'sb/next-buffer)
  (global-set-key [remap previous-buffer] #'sb/previous-buffer))

(use-package default-text-scale
  :bind
  (("C-M-+" . default-text-scale-increase)
   ("C-M--" . default-text-scale-decrease)))

(use-package free-keys
  :commands free-keys)

(use-package keyfreq
  :hook
  (after-init-hook . (lambda ()
                       (keyfreq-mode 1)
                       (keyfreq-autosave-mode 1))))

(use-package which-key ; Show help popups for prefix keys
  :diminish
  :commands (which-key-mode which-key-setup-side-window-right-bottom)
  ;; :init (run-with-idle-timer 3 nil #'which-key-mode)
  :hook (after-init-hook . which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  :custom
  ;; Allow "C-h" to trigger `which-key' before it is done automatically
  (which-key-show-early-on-C-h nil)
  (which-key-sort-order 'which-key-key-order-alpha))

(use-package which-key-posframe
  :disabled t
  :commands which-key-posframe-mode
  :hook (which-key-mode-hook . which-key-posframe-mode)
  :config
  ;; Modify the posframe background if it has a low contrast
  ;; (set-face-attribute 'which-key-posframe nil :background "floralwhite" :foreground "black")
  :custom
  ;; Thicker border makes the posframe easier to distinguish
  (which-key-posframe-border-width 4)
  ;; Positioning the frame at the top obstructs the view to a lesser degree
  (which-key-posframe-poshandler 'posframe-poshandler-frame-top-center))

;; Hydras, https://github.com/abo-abo/hydra

;; ":exit nil" means the hydra state will continue, ":exit t" will quit the hydra. ":color red"
;; means continue the hydra on a valid key but stop when a foreign key has been pressed. ":color
;; blue" means exit.

(setq lv-use-separator t)

(use-package hydra
  :commands (hydra-default-pre hydra-keyboard-quit defhydra
                               hydra-show-hint hydra-set-transient-map
                               hydra--call-interactively-remap-maybe))

(progn
  (eval-when-compile
    (if (bound-and-true-p sb/disable-package.el)
        (use-package hydra-posframe
          :straight (hydra-posframe :type git :host github :repo "Ladicle/hydra-posframe"))
      (use-package hydra-posframe
        :ensure nil
        :load-path "extras")))

  (declare-function hydra-posframe-mode "hydra-posframe")

  (unless (fboundp 'hydra-posframe-mode)
    (autoload #'hydra-posframe-mode "hydra-posframe" nil t))

  (with-eval-after-load "hydra"
    (hydra-posframe-mode 1)))

(use-package ivy-hydra ; Additional keybindings for `ivy'
  :after (ivy hydra)
  :demand t
  :commands (ivy-dispatching-done-hydra ivy--matcher-desc ivy-hydra/body))

(use-package pretty-hydra
  :after hydra
  :commands pretty-hydra-define)

(declare-function spell-fu-goto-next-error "spell-fu")
(declare-function spell-fu-goto-previous-error "spell-fu")
(declare-function spell-fu-word-add "spell-fu")

(pretty-hydra-define sb/hydra-spelling
  (:color amaranth :quit-key "q")
  ("Action"
   (("c" ispell "ispell")
    ("f" flyspell-buffer "flyspell buffer")

    ("<" flyspell-correct-previous "correct previous error")
    (">" flyspell-correct-next "correct next error")

    ("n" spell-fu-goto-next-error "next error")
    ("p" spell-fu-goto-previous-error "previous error")
    ("a" spell-fu-word-add "add word"))))

(pretty-hydra-define sb/hydra-text-scale-zoom
  (:color amaranth :quit-key "q")
  ("Zoom action"
   (("i" default-text-scale-increase "zoom in")
    ("o" default-text-scale-decrease "zoom out"))))

(pretty-hydra-define sb/hydra-error
  (:color amaranth :quit-key "q")
  ("Navigate errors"
   (("h" first-error "first error")
    ("j" next-error "next error")
    ("k" previous-error "previous error")
    ("v" recenter-top-bottom "recenter"))))

;; https://github.com/abo-abo/hydra/wiki/avy
(pretty-hydra-define sb/hydra-avy
  (:color red)
  ("Actions"
   (("y" avy-copy-line "yank line")
    ("m" avy-move-line "move line")
    ("k" avy-kill-whole-line "kill whole line")

    ("Y" avy-copy-region "copy region ")
    ("M" avy-move-region "move region")
    ("K" avy-kill-region "kill region")

    ("c" avy-goto-char-timer "go to char with timer")
    ("w" avy-goto-word-1 "go to word")
    ("l" avy-goto-line "go to line")

    ("C" avy-goto-char "go to char")
    ("W" avy-goto-word-0 "go to word")
    ("L" avy-goto-end-of-line "go to end of line"))))

(pretty-hydra-define sb/hydra-projectile
  (:color teal :hint nil global-map "C-c p" :quit-key "q")
  ("PROJECTILE: %(projectile-project-root)"
   (("p"   projectile-switch-project "switch project")
    ("s"   projectile-switch-project "switch project")
    ("a"   projectile-add-known-project "add known project")
    ("x"   projectile-remove-known-project "remove known project")
    ("X"   projectile-cleanup-known-projects "clean up known projects")

    ("f"   projectile-find-file "find file")
    ("F"   projectile-find-file-dwim "find file dwim")
    ("d"   projectile-find-dir "find directory")
    ("D"   projectile-find-file-in-directory "file file in directory")

    ("i"   projectile-ibuffer "ibuffer")
    ("b"   projectile-switch-to-buffer "switch to buffer")
    ("r"   projectile-recentf "recent files")
    ("k"   projectile-kill-buffers "kill all buffers")

    ("c"   projectile-invalidate-cache "invalidate cache")
    ("z"   projectile-cache-current-file "cache current file")
    ("g"   projectile-find-tag "find tag")
    ("o"   projectile-multi-occur "multi occur")
    ("m"   projectile-compile "compile"))))

(pretty-hydra-define sb/hydra-move-text
  (:quit-key "q")
  ("Move text"
   (("u" move-text-up "up")
    ("d" move-text-down "down"))))

;; (declare-function flycheck-verify-setup "flycheck")
;; (declare-function flycheck-previous-error "flycheck")
;; (declare-function flycheck-next-error "flycheck")
;; (declare-function flycheck-list-errors "flycheck")
;; (declare-function flycheck-select-checker "flycheck")
;; (declare-function flycheck-describe-checker "flycheck")
;; (declare-function flycheck-disable-checker "flycheck")
;; (declare-function flycheck-buffer "flycheck")

(pretty-hydra-define sb/hydra-flycheck
  (:color blue :quit-key "q")
  ("Flycheck actions"
   (("M" flycheck-manual "Manual")
    ("v" flycheck-verify-setup "Verify setup")

    ("<" flycheck-previous-error :color pink "previous error")
    (">" flycheck-next-error :color pink "next error")
    ("f" flycheck-buffer "check buffer")
    ("l" flycheck-list-errors "list")

    ("d" flycheck-disable-checker "disable")
    ("m" flycheck-mode "enable mode")
    ("s" flycheck-select-checker "select checker")
    ("?" flycheck-describe-checker "describe checker"))))

(defhydra sb/hydra-python-indent ()
  "Adjust Python indentation."
  (">" python-indent-shift-right "right")
  ("<" python-indent-shift-left "left"))

(with-eval-after-load "python-mode"
  (bind-key "C-c" #'sb/hydra-python-indent/body python-mode-map))

(defhydra sb/smerge-hydra
  (:color pink :hint nil :post (smerge-auto-leave))
  "
                                              ^Move^       ^Keep^               ^Diff^                 ^Other^
                                              ^^-----------^^-------------------^^---------------------^^-------
                                              _n_ext       _b_ase               _<_: upper/base        _C_ombine
                                              _p_rev       _u_pper              _=_: upper/lower       _r_esolve
                                              ^^           _l_ower              _>_: base/lower        _k_ill current
                                              ^^           _a_ll                _R_efine
                                              ^^           _RET_: current       _E_diff
                                              "
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("k" smerge-kill-current)
  ("q" nil "cancel" :color blue))

(defhydra sb/hydra-multiple-cursors (:hint nil)
  "
                                              ^Up^            ^Down^        ^Other^
                                              ----------------------------------------------
                                              [_p_]   Next    [_n_]   Next    [_l_] Edit lines
                                              [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
                                              [_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
                                              ^ ^             ^ ^             [_q_] Quit
                                              "
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))

(defhydra sb/hydra-smartparens (:hint nil)
  "
                                              Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
                                              ------------------------------------------------------------------------------------------------------------------------
                                              [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
                                              [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
                                              [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
                                              [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
  ;; Moving
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-down-sexp)
  ("N" sp-backward-down-sexp)
  ("p" sp-up-sexp)
  ("P" sp-backward-up-sexp)

  ;; Slurping & barfing
  ("h" sp-backward-slurp-sexp)
  ("H" sp-backward-barf-sexp)
  ("l" sp-forward-slurp-sexp)
  ("L" sp-forward-barf-sexp)

  ;; Wrapping
  ("R" sp-rewrap-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("(" sp-wrap-round)
  ("{" sp-wrap-curly)
  ("[" sp-wrap-square)

  ;; Sexp juggling
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("j" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("A" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ("o" sp-convolute-sexp)

  ;; Destructive editing
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("w" sp-copy-sexp)

  ("q" nil)
  ("g" nil))

(defhydra sb/hydra-lsp (:exit t :hint nil)
  "
  Buffer^^               Server^^                   Symbol
  -----------------------------------------------------------------------------------------------------------
  [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
  [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
  [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"

  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-workspace-restart)
  ("S" lsp-workspace-shutdown))

(defhydra sb/hydra-markdown-mode (:hint nil)
  "
  Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code

  Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4

  Lists             C-c C-x    _m_: insert item

  Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down

  Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference

  "

  ("s" markdown-insert-bold)
  ("e" markdown-insert-italic)
  ("b" markdown-insert-blockquote :color blue)
  ("p" markdown-insert-pre :color blue)
  ("c" markdown-insert-code)

  ("h" markdown-insert-header-dwim)
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)

  ("m" markdown-insert-list-item)

  ("l" markdown-promote)
  ("r" markdown-demote)
  ("d" markdown-move-down)
  ("u" markdown-move-up)

  ("L" markdown-insert-link :color blue)
  ("U" markdown-insert-uri :color blue)
  ("F" markdown-insert-footnote :color blue)
  ("W" markdown-insert-wiki-link :color blue)
  ("R" markdown-insert-reference-link-dwim :color blue))

(pretty-hydra-define sb/hydra-straight
  (:hint nil :quit-key "q")
  ("Straight actions")
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
   ("u" straight-push-all "push all")
   ("U" straight-push-package "push package")
   ("v" straight-freeze-versions "freeze versions")
   ("V" straight-thaw-versions "thaw versions")
   ("w" straight-watcher-start "watcher start")
   ("W" straight-watcher-quit "watcher quit")
   ("g" straight-get-recipe "get recipe")
   ("e" straight-prune-build "prune build")))

(bind-key "C-c h a" #'sb/hydra-avy/body)
(bind-key "C-c h d" #'sb/hydra-markdown-mode/body)
;; (bind-key "C-c h e" #'sb/hydra-error/body)
(bind-key "C-c h f" #'sb/hydra-flycheck/body)
(bind-key "C-c h g" #'sb/smerge-hydra/body)
(bind-key "C-c h j" #'sb/hydra-projectile/body)
(bind-key "C-c h l" #'sb/hydra-lsp/body)
(bind-key "C-c h m" #'sb/hydra-multiple-cursors/body)
(bind-key "C-c h p" #'sb/hydra-smartparens/body)
(bind-key "C-c h s" #'sb/hydra-spelling/body)
(bind-key "C-c h t" #'sb/hydra-move-text/body)
(bind-key "C-c h z" #'sb/hydra-text-scale-zoom/body)
(bind-key "C-c h i" #'sb/hydra-straight/body)

(provide 'init-keybindings)

;;; init-keybindings.el ends here
