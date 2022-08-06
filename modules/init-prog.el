;;; init-languages.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp;
;;; coding:utf-8; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(add-hook 'prog-mode-hook
          (lambda ()
            ;; Native from Emacs 27+, disable in TUI since the line characters also get copied.
            (when (display-graphic-p)
              (display-fill-column-indicator-mode 1))))

(use-package subword
  :straight (:type built-in)
  :hook
  (prog-mode-hook . subword-mode)
  :diminish)

(use-package outline ; Edit outlines
  :hook
  (prog-mode-hook . outline-minor-mode)
  :diminish outline-minor-mode)

;; Hide top-level code blocks. Enable code folding, which is useful for browsing large files. This
;; module is part of Emacs, and is better maintained than other alternatives like `origami'.
(use-package hideshow
  :straight (:type built-in)
  :commands
  (hs-hide-all hs-hide-initial-comment-block hs-show-all hs-show-block)
  :hook
  ;; Hideshow is not defined for `ini-mode'.
  ((python-mode-hook emacs-lisp-mode-hook java-mode-hook sh-mode-hook) . hs-minor-mode)
  :custom
  (hs-isearch-open t "Open all folds while searching")
  :diminish hs-minor-mode)

(use-package symbol-overlay ; Highlight symbol under point
  :commands
  (transient-define-prefix)
  :hook
  (prog-mode-hook . symbol-overlay-mode)
  :bind
  (("M-p" . symbol-overlay-jump-prev)
   ("M-n" . symbol-overlay-jump-next))
  :custom
  ;; Delay highlighting to allow for transient cursor placements
  (symbol-overlay-idle-time 2)
  ;; :config
  ;; (transient-define-prefix sb/symbol-overlay-transient ()
  ;;   "Symbol Overlay transient"
  ;;   ["Symbol Overlay"
  ;;    ["Overlays"
  ;;     ("." "Add/Remove at point" symbol-overlay-put)
  ;;     ("k" "Remove All" symbol-overlay-remove-all)
  ;;     ]
  ;;    ["Move to Symbol"
  ;;     ("n" "Next" symbol-overlay-jump-next)
  ;;     ("p" "Previous" symbol-overlay-jump-prev)
  ;;     ]
  ;;    ["Other"
  ;;     ("m" "Highlight symbol-at-point" symbol-overlay-mode)
  ;;     ]
  ;;    ]
  ;;   )
  ;; (bind-key "M-o" #'sb/symbol-overlay-transient)
  :diminish)

(use-package highlight-escape-sequences
  :hook
  (prog-mode-hook . hes-mode))

(use-package compile
  :straight (:type built-in)
  :bind
  ("<f11>" . recompile)
  :custom
  (compilation-always-kill t "Kill a compilation process before starting a new one")
  (compilation-ask-about-save nil "Save all modified buffers without asking")
  ;; Automatically scroll the *Compilation* buffer as output appears, but stop at the first
  ;; error.
  (compilation-scroll-output 'first-error))

(use-package fancy-compilation
  :straight (:type git :repo "https://codeberg.org/ideasman42/emacs-fancy-compilation")
  :after compile
  :init (fancy-compilation-mode 1))

(use-package rainbow-delimiters
  :hook
  ((prog-mode-hook latex-mode-hook LaTeX-mode-hook
                   org-src-mode-hook) . rainbow-delimiters-mode))

(provide 'init-prog)

;;; init-prog.el ends here
