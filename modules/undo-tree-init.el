;;; undo-tree-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup undo-tree.

;;; Code:

;; Visualize with C-x u
(use-package undo-tree
  :ensure t
  :defer t
  :config
  (setq undo-tree-mode-lighter ""
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-relative-timestamps t
        undo-tree-auto-save-history nil
        undo-tree-visualizer-diff t)
  (global-undo-tree-mode 1)
  :diminish undo-tree-mode)

(provide 'undo-tree-init)

;;; undo-tree-init.el ends here
