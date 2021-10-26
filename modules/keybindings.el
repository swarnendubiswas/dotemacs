;;; keybindings.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

;; Keybindings

;; Generic keybindings, package-specific are usually in their own modules. The `C-x' keymap is for
;; global bindings that are expected to work regardless of the active modes. The `C-c' binding
;; followed by a control character or a digit are reserved for major modes. The sequence of `C-c'
;; followed by any other ASCII punctuation or symbol character are allocated for minor modes.
;; Sequences consisting of `C-c' and a letter (either upper or lower case) are reserved for users.
;; Check the "Key Binding Conventions" in the Emacs manual (`C-h i'). Use `C-h b' to see available
;; bindings in a buffer. Use `M-x describe-personal-keybindings' to see modifications.

;; `bind-key*', `bind*' overrides all minor mode bindings. The `kbd` macro is not required with
;; `bind-key' variants. With `bind-key', you do not need an explicit `(kbd ...)'.

;; Other variants:
;; `(global-set-key (kbd "RET") 'newline-and-indent)'
;; `(define-key global-map (kbd "RET") 'newline-and-indent)'

;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs

(bind-keys
 ("RET"       . newline-and-indent)
 ("C-l"       . goto-line)
 ("C-c z"     . repeat)
 ("C-z"       . undo)
 ("<f11>"     . delete-other-windows)
 ("C-x k"     . kill-this-buffer)
 ("M-<left>"  . previous-buffer)
 ("M-<right>" . next-buffer)
 ("C-c d f"   . auto-fill-mode)
 ("M-c"       . capitalize-dwim)
 ("M-u"       . upcase-dwim)
 ("M-l"       . downcase-dwim)
 ("<f7>"      . previous-error)
 ("<f8>"      . next-error)
 ;; The default keybinding `C-S-backspace' does not work with TUI
 ("M-k"       . kill-whole-line))

;; In a line with comments, `C-u M-;' removes the comments altogether. That means deleting the
;; comment, NOT UNCOMMENTING but removing all commented text and the comment marker itself.
(bind-keys*
 ("C-c n" . comment-region)
 ("C-c m" . uncomment-region)
 ("C-c ;" . sb/comment-line)
 ("C-c b" . comment-box)
 ("C-s"   . save-buffer)
 ("C-S-s" . sb/save-all-buffers))

(unbind-key "C-]") ; Bound to `abort-recursive-edit'

(unbind-key "C-x s") ; Bound to `save-some-buffers'
(bind-key   "C-x s" #'sb/switch-to-scratch)
(bind-key   "C-x j" #'sb/counsel-all-files-recursively)

(global-set-key [remap next-buffer]     #'sb/next-buffer)
(global-set-key [remap previous-buffer] #'sb/previous-buffer)

(use-package default-text-scale
  :bind
  (("C-M-+" . default-text-scale-increase)
   ("C-M--" . default-text-scale-decrease)))

(use-package free-keys
  :commands free-keys)

(use-package which-key ; Show help popups for prefix keys
  :diminish
  :commands (which-key-mode which-key-setup-side-window-right-bottom)
  :init (run-with-idle-timer 3 nil #'which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t
        which-key-sort-order 'which-key-key-order-alpha))

(use-package which-key-posframe
  :disabled t
  :commands which-key-posframe-mode
  :hook (which-key-mode . which-key-posframe-mode)
  :config
  ;; The posframe has a low contrast
  ;; (set-face-attribute 'which-key-posframe nil :background "floralwhite" :foreground "black")
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-center))

;; Hydras

;; `:exit t' will quit the hydra
(defhydra sb/hydra-spelling (:color blue)
  "
  ^
  ^Spelling^          ^Errors^            ^Checker^             ^Spell fu^
  ^────────^──────────^──────^────────────^───────^─────────────^────────^────────
  _q_ quit            _<_ previous        _c_ correction        _n_ next error
  ^^                  _>_ next            _d_ dictionary        _p_ previous error
  ^^                  _f_ check           _m_ mode              _a_ add word
  ^^                  ^^                  ^^                    ^^
  "
  ("q" nil "quit")
  ("<" flyspell-correct-previous :color pink)
  (">" flyspell-correct-next :color pink)
  ("c" ispell)
  ("d" ispell-change-dictionary)
  ("f" flyspell-buffer)
  ("m" flyspell-mode)
  ("n" spell-fu-goto-next-error)
  ("p" spell-fu-goto-previous-error)
  ("a" spell-fu-word-add))

(defhydra sb/hydra-text-scale-zoom ()
  "Zoom the text"
  ("i" default-text-scale-increase "in")
  ("o" default-text-scale-decrease "out")
  ("q" nil "quit"))

(defhydra sb/hydra-error (global-map "C-c h e")
  "goto-error"
  ("h" first-error "first")
  ("j" next-error "next")
  ("k" previous-error "prev")
  ("v" recenter-top-bottom "recenter")
  ("q" nil "quit"))

;; https://github.com/abo-abo/hydra/wiki/avy
(defhydra sb/hydra-avy (:exit t :hint nil)
  "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
  ("c" avy-goto-char-timer)
  ("C" avy-goto-char)
  ("w" avy-goto-word-1)
  ("W" avy-goto-word-0)
  ("l" avy-goto-line)
  ("L" avy-goto-end-of-line)
  ("m" avy-move-line)
  ("M" avy-move-region)
  ("k" avy-kill-whole-line)
  ("K" avy-kill-region)
  ("y" avy-copy-line)
  ("Y" avy-copy-region))

(defhydra sb/hydra-projectile (:color teal :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: find tags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir
"
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("a"   projectile-ag)
  ("g"   projectile-find-tag)
  ("q"   nil "cancel" :color blue))


(defhydra sb/hydra-flycheck (:color blue)
  "
  ^
  ^Flycheck^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^─────
  _q_ quit            _<_ previous        _?_ describe
  _M_ manual          _>_ next            _d_ disable
  _v_ verify setup    _f_ check           _m_ mode
  ^^                  _l_ list            _s_ select
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("<" flycheck-previous-error :color pink)
  (">" flycheck-next-error :color pink)
  ("?" flycheck-describe-checker)
  ("M" flycheck-manual)
  ("d" flycheck-disable-checker)
  ("f" flycheck-buffer)
  ("l" flycheck-list-errors)
  ("m" flycheck-mode)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup))

(with-eval-after-load "python"
  (defhydra sb/hydra-python-indent (python-mode-map "C-c")
    "Adjust Python indentation."
    (">" python-indent-shift-right "right")
    ("<" python-indent-shift-left "left")))

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

(defhydra multiple-cursors-hydra (:hint nil)
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

(global-set-key (kbd "C-c m") #'multiple-cursors-hydra/body)

(provide 'keybindings)

;;; keybindings.el ends here
