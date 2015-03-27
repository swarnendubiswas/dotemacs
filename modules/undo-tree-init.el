;;; undo-tree-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup undo-tree

;;; Code:

;; visualize with C-x u
(use-package undo-tree
  :ensure t
  :disabled t
  :init (global-undo-tree-mode 1)
  :config
  (setq undo-tree-mode-lighter ""
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :diminish undo-tree-mode)

(provide 'undo-tree-init)

;;; undo-tree-init.el ends here
