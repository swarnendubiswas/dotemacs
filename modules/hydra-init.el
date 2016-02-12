;;; hydra-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup hydras.

;;; Code:

(use-package hydra
  :ensure t
  :disabled t
  :config
  (defhydra hydra-jump-commands (:color blue)
    "Different avy jump commands."
    ("c" avy-goto-char "avy char")
    ("d" avy-goto-char-2 "avy char 2")
    ("w" avy-goto-word-0 "avy word")
    ("u" avy-goto-word-or-subword-1 "avy word or subword")
    ("l" avy-goto-line "avy line")
    ("s" avy-goto-subword-0 "avy subword"))
  (bind-key "M-g" #'hydra-jump-commands/body)

  (defhydra hydra-ecb (:color blue)
    "ecb commands"
    ("g h" ecb-goto-window-history "history")
    ("g m" ecb-goto-window-methods "methods")
    ("g s" ecb-goto-window-sources "sources")
    ("g d" ecb-goto-window-directories "directories")
    ("g y" ecb-goto-window-symboldef "symbol def")
    ("l u" ecb-redraw-layout "redraw layout"))
  (bind-key "C-c ." 'hydra-ecb/body)

  ;; http://oremacs.com/2015/02/21/hydra-docstring-sexp
  (defhydra hydra-dired-marked (dired-mode-map "" :color pink)
    "Number of marked items: %(length (dired-get-marked-files))"
    ("m"   dired-mark                      "mark")
    ("u"   dired-unmark                    "unmark")
    ("U"   dired-unmark-all-marks          "unmark ALL")
    ("t"   dired-toggle-marks              "toggle marks")
    ("P"   dired-prev-marked-file          "prev marked")
    ("M-{" dired-prev-marked-file          "prev marked")
    ("N"   dired-next-marked-file          "next marked")
    ("M-}" dired-next-marked-file          "next marked")
    ("w"   dired-copy-filename-as-kill     "copy file name(s)")
    ("W"   (dired-copy-filename-as-kill 0) "copy file name(s) - full path")
    ("C-g" nil                             "cancel" :color blue))
  (bind-key "." 'hydra-dired-marked/body dired-mode-map)

  ;; http://ericjmritz.name/2015/04/06/organizing-key-bindings-in-gnu-emacs-using-hydra/
  (defhydra hydra-helm (:color blue)
    "helm commands"
    ("x" helm-M-x "helm-M-x")
    ("b" helm-mini "helm-mini")
    ("i" helm-buffers-list "helm-buffers-list")
    ("f" helm-find-files "helm-find-files")
    ("r" helm-recentf "helm-recentf")
    ("l" helm-locate "helm-locate")
    ("y" helm-show-kill-ring "helm-show-kill-ring")
    ("s" helm-swoop "helm-swoop")
    ("/" helm-multi-swoop "helm-multi-swoop")
    ("a" helm-apropos "helm-apropos")
    ("g" helm-do-grep "helm-do-grep")
    ("u" helm-resume "helm-resume")
    ;; swoop is better than occur
    ("o" helm-occur "helm-occur"))
  (global-unset-key (kbd "C-b"))
  (bind-key "C-b" 'hydra-helm/body)

  (defhydra hydra-buffer-menu (:color pink)
    "Buffer menu commands"
    ("m" Buffer-menu-mark "mark")
    ("u" Buffer-menu-unmark "unmark")
    ("U" Buffer-menu-backup-unmark "backup-unmark")
    ("d" Buffer-menu-delete "delete")
    ("D" Buffer-menu-delete-backwards "delete-backwards")
    ("s" Buffer-menu-save "save")
    ("~" Buffer-menu-not-modified "not modified")
    ("x" Buffer-menu-execute "execute")
    ("b" Buffer-menu-bury "bury")
    ("g" revert-buffer "revert")
    ("T" Buffer-menu-toggle-files-only "toggle files only")
    ("O" Buffer-menu-multi-occur "multi occur" :color blue)
    ("I" Buffer-menu-isearch-buffers "isearch buffers" :color blue)
    ("R" Buffer-menu-isearch-buffers-regexp "isearch buffers regexp" :color blue)
    ("c" nil "cancel")
    ("v" Buffer-menu-select "select" :color blue)
    ("o" Buffer-menu-other-window "other-window" :color blue)
    ("q" quit-window "quit" :color blue))
  (bind-key "." 'hydra-buffer-menu/body ibuffer-mode-map)

  (defhydra hydra-org (:color red :columns 3)
    "Org Mode Movements"
    ("n" outline-next-visible-heading "next heading")
    ("p" outline-previous-visible-heading "prev heading")
    ("N" org-forward-heading-same-level "next heading at same level")
    ("P" org-backward-heading-same-level "prev heading at same level")
    ("u" outline-up-heading "up heading")
    ("g" org-goto "goto" :exit t))
  (bind-key "C-c o" #'hydra-org/body)

  (defhydra hydra-projectile (:color teal)
    "projectile"
    ("h" helm-projectile "helm-projectile")
    ("p" helm-projectile-switch-project "switch project")
    ("f" helm-projectile-find-file-dwim "find file dwim")
    ("d" helm-projectile-find-dir "find dir")
    ("b" helm-projectile-switch-to-buffer "switch to another buffer in the project")
    ("a" helm-projectile-find-other-file "find other file")
    ("c" projectile-invalidate-cache "invalidate cache")
    ("i" projectile-ibuffer "ibuffer")
    ("S" projectile-save-project-buffers "save project buffers")
    ("l" projectile-replace "replace")
    ("r" helm-projectile-recentf "recentf")
    ("K" projectile-kill-buffers "kill buffers")
    ("g" helm-projectile-grep "grep")
    ("o" projectile-multi-occur "multi-occur"))
  (bind-key* "C-c p" 'hydra-projectile/body)

  (defhydra hydra-helm-gtags (:color blue)
    "helm gtags"
    ("h" 'helm-gtags-previous-history "previous history")
    ("c" 'helm-gtags-create-tags "create tags")
    ("u" 'helm-gtags-update-tags "update tags")
    ("s" 'helm-gtags-find-symbol "find symbol")
    ("r" 'helm-gtags-find-rtag "find rtag")
    ("p" 'helm-gtags-parse-file "parse file")
    ("t" 'helm-gtags-find-tag "find tag")
    ("g" 'helm-gtags-find-pattern "find pattern")
    ("f" 'helm-gtags-find-files "find files")
    ("o" 'helm-gtags-find-tag-other-window "find tag other window"))
  (bind-key "C-c g" 'hydra-helm-gtags/body)

  (defhydra hydra-apropos (:color blue)
    "Apropos"
    ("a" apropos "apropos")
    ("c" apropos-command "cmd")
    ("d" apropos-documentation "doc")
    ("e" apropos-value "val")
    ("l" apropos-library "lib")
    ("o" apropos-user-option "opt")
    ("v" apropos-variable "var")
    ("i" info-apropos "info")
    ("t" tags-apropos "tags"))
  (bind-key "C-h a" #'hydra-apropos/body))

(provide 'hydra-init)

;;; hydra-init.el ends here
