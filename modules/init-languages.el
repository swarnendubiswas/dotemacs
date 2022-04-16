(use-package subword
  :straight nil
  :diminish
  :hook (prog-mode-hook . subword-mode))

(use-package outline ; Edit outlines
  :straight t
  :disabled t
  :hook (prog-mode-hook . outline-minor-mode)
  :diminish outline-minor-mode)

;; Hide top-level code blocks. Enable code folding, which is useful for browsing large files. This
;; module is part of Emacs, and is better maintained than other alternatives like `origami'.
(use-package hideshow
  :straight nil
  :disabled t
  :commands (hs-hide-all hs-hide-initial-comment-block hs-show-all hs-show-block)
  :diminish hs-minor-mode
  :hook (prog-mode-hook . hs-minor-mode)
  :custom (hs-isearch-open t))


(provide 'init-languages)
