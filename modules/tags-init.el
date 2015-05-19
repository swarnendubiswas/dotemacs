;;; tags-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup tags for different projects.

;;; Code:

(setq tags-revert-without-query t
      ;; t=case-insensitive, nil=case-sensitive
      tags-case-fold-search nil)

(use-package ctags
  :ensure t
  :defer t)

(use-package ctags-update
  :ensure t
  :defer t)

(use-package etags
  :bind ("M-T" . tags-search))

(use-package gtags
  :ensure t
  :defer t)

(use-package ctags-update
  :ensure t
  :diminish ctags-auto-update-mode
  :config
  (setq ctags-update-delay-seconds (* 30 60)) ; every 1/2 hour
  (ctags-auto-update-mode 1))

;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-gtags.el
;; front end to gnu global, use gtags -v
(use-package ggtags
  :ensure t
  :diminish ggtags-mode
  :config
  (setq ggtags-navigation-mode-lighter nil
        ggtags-oversize-limit (* 30 1024 1024)
        ;; use helm for completion
        ggtags-completing-read-function nil)
  (add-hook 'prog-mode-hook #'ggtags-mode))

(defhydra hydra-ggtags (:color blue)
  "ggtags"
  ("s" 'ggtags-find-other-symbol "find other symbol")
  ("h" 'ggtags-view-tag-history "view tag history")
  ("r" 'ggtags-find-reference "find reference")
  ("f" 'ggtags-find-file "find file")
  ("c" 'ggtags-create-tags "create tags")
  ("u" 'ggtags-update-tags "update tags"))
(bind-key "C-c g" 'hydra-ggtags/body)

;; http://wikemacs.org/wiki/C-ide
;; http://tuhdo.github.io/c-ide.html
;; use M-n to move to next candidate and M-p to move back previous candidate. Use M-g s to invoke Isearch on candidate buffer list 
(use-package helm-gtags
  :ensure t
  :diminish helm-gtags-mode
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-c g"
        helm-gtags-suggested-key-mapping t)
  (add-hook 'prog-mode-hook #'helm-gtags-mode)
  (bind-key "M-." 'helm-gtags-dwim helm-gtags-mode-map)
  (bind-key "M-," 'helm-gtags-pop-stack helm-gtags-mode-map)
  (bind-key "M-'" 'helm-gtags-select helm-gtags-mode-map))

;; create tags for a latex project, no need to setup a keybinding
;; http://stackoverflow.com/questions/548414/how-to-programmatically-create-update-a-tags-file-with-emacs
(defun create-latex-etags ()
  "Create etags for the current latex project."
  (interactive)
  (compile "find . -type f -name \"*.tex\" -print | etags -a -"))

(defun create-latex-ctags () ; (dir-name))
  "Create ctags for the current latex project."
  ;;(interactive "DDirectory: ")
  ;; (shell-command
  ;;  (format "ctags -o TAGS -R *.tex %s" (directory-file-name dir-name)))
  (interactive)
  ;;(compile "find . -name \"*.tex\" -print | ctags -a -u -o TAGS -")
  (compile "find . -type f -name \"*.tex\" -print | xargs ctags -o TAGS"))

(provide 'tags-init)

;;; tags-init.el ends here
