;; The built-in `describe-function' includes both functions and macros. `helpful-function' is
;; functions only, so we use `helpful-callable' as a drop-in replacement.
(use-package helpful
  :straight t
  :bind
  (("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h f" . helpful-callable)
   ("C-h c" . helpful-command)
   ("C-h p" . helpful-at-point)
   ("C-h o" . helpful-symbol)
   :map helpful-mode-map
   ("q"     . helpful-kill-buffers)))

;; Erase all consecutive white space characters in a given direction
(use-package hungry-delete
  :straight t
  :commands (hungry-delete-mode global-hungry-delete-mode)
  :diminish
  :hook
  ((minibuffer-setup-hook . (lambda ()
                              (hungry-delete-mode -1)))
   (after-init-hook . global-hungry-delete-mode)))

;; (use-package move-text ; Move lines with "M-<up>" and "M-<down>"
;;   :straight t
;;   :commands (move-text-up move-text-down move-text-default-bindings)
;;   :init (move-text-default-bindings))

(use-package duplicate-thing
  :straight t
  :bind* ("C-c C-d" . duplicate-thing))

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :straight t
  :bind
  (("C-h C-m" . discover-my-major)
   ("C-h M-m" . discover-my-mode)))

;; Manage minor-mode on the dedicated interface buffer
(use-package manage-minor-mode
  :straight t
  :commands manage-minor-mode)

(use-package expand-region ; Expand region by semantic units
  :straight t
  :bind
  (("C-="   . er/expand-region)
   ("C-M-=" . er/contract-region)))

(use-package expand-line
  :straight t
  :diminish
  :bind ("M-i" . turn-on-expand-line-mode))

;; Restore point to the initial location with "C-g" after marking a region
(use-package smart-mark
  :straight t
  ;; :init (run-with-idle-timer 3 nil #'smart-mark-mode)
  :hook (after-init-hook . smart-mark-mode))

;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :straight t
  :commands (whole-line-or-region-local-mode whole-line-or-region-global-mode)
  :diminish (whole-line-or-region-local-mode)
  ;; :init (run-with-idle-timer 3 nil #'whole-line-or-region-global-mode)
  :hook (after-init-hook . whole-line-or-region-global-mode))

(use-package goto-last-change
  :straight t
  :bind ("C-x C-\\" . goto-last-change))

;; The real beginning and end of buffers (i.e., `point-min' and `point-max') are accessible by
;; pressing the keys "M-<" and "M->" keys again.
(use-package beginend
  :straight t
  ;; :init (run-with-idle-timer 3 nil #'beginend-global-mode)
  :hook (after-init-hook . beginend-global-mode)
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode)))

    (use-package undo-tree
  :straight t
  :defines undo-tree-map
  :commands (global-undo-tree-mode undo-tree-redo)
  :diminish
  :config
  (setq undo-tree-auto-save-history              t
        undo-tree-visualizer-diff                t
        undo-tree-visualizer-relative-timestamps t
        undo-tree-visualizer-timestamps          t)
  (unbind-key "C-/" undo-tree-map)
  :hook (find-file-hook . undo-tree-mode)
  :bind
  (([remap undo] . undo-tree-undo)
   ([remap redo] . undo-tree-redo)
   ("C-z"   . undo-tree-undo)
   ("C-x u" . undo-tree-visualize)))

(use-package iedit ; Edit multiple regions in the same way simultaneously
  :straight t
  :bind* ("C-." . iedit-mode))

;; Avoid the "Overwrite old session file (not loaded)?" warning by loading the `session' package
(use-package session
  :straight t
  :disabled t
  :commands (session-initialize)
  :hook (after-init-hook . session-initialize))

(use-package hl-todo
  :straight t
  :commands global-hl-todo-mode
  ;; :init (run-with-idle-timer 3 nil #'global-hl-todo-mode)
  :hook (after-init-hook . global-hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces (append '(("LATER"    . "#d0bf8f")
                                        ("ISSUE"    . "#ff8c00")
                                        ("DEBUG"    . "#ff8c00")
                                        ("TEST"     . "tomato")
                                        ("WARNING"  . "#cc0000")
                                        ("BEWARE"   . "#aa0000")
                                        ("REFACTOR" . "#cc9393"))
                                      hl-todo-keyword-faces)))

(use-package highlight-numbers
  :straight t
  :commands highlight-numbers-mode
  :hook ((prog-mode-hook yaml-mode-hook conf-mode-hook
                         css-mode-hook html-mode-hook) . highlight-numbers-mode))

;; (use-package page-break-lines ; Display ugly "^L" page breaks as tidy horizontal lines
;;   :straight t
;;   :diminish
;;   :commands (global-page-break-lines-mode page-break-lines-mode)
;;   ;; :init (run-with-idle-timer 3 nil #'global-page-break-lines-mode)
;;   :hook (after-init-hook . global-page-break-lines-mode))

(provide 'init-misc)
