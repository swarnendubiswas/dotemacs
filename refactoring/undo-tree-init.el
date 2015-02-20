;;; undo-tree-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup undo-tree

;;; Code:

;; visualize with C-x u

(use-package )

(setq undo-tree-mode-lighter ""
      undo-tree-visualizer-timestamps t
      undo-tree-visualizer-diff t
      )
(global-undo-tree-mode 1)

(provide 'undo-tree-init)

;;; undo-tree-init.el ends here
