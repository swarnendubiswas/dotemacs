;;; tags-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup tags for different projects.

;;; Code:

(setq tags-revert-without-query t
      ;; t=case-insensitive, nil=case-sensitive
      tags-case-fold-search nil)

(use-package ctags
  :ensure t
  :disabled t)

(use-package ctags-update
  :ensure t
  :disabled t)

(use-package ctags-update
  :ensure t
  :disabled t
  :diminish ctags-auto-update-mode
  :config
  (setq ctags-update-delay-seconds (* 30 60)) ; every 1/2 hour
  (ctags-auto-update-mode 1))

(use-package etags
  :bind ("M-T" . tags-search))

(use-package gtags
  :ensure t
  :disabled t)

(use-package jtags
  :ensure t
  :disabled t
  ;;:init (autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
  :config (add-hook 'java-mode-hook #'jtags-mode))

;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-gtags.el
;; front end to gnu global, use gtags -v -c. Languages supported are C, C++, Yacc, Java, PHP4 and assembly.
(use-package ggtags
  :ensure t
  :if (eq system-type 'gnu/linux)
  :diminish ggtags-mode
  :init
  (setq ggtags-navigation-mode-lighter nil
        ggtags-oversize-limit (* 50 1024 1024)
        ;; use helm for completion
        ggtags-completing-read-function nil)
  (add-hook 'c-mode-common-hook #'ggtags-mode)

  :config
  ;; http://wikemacs.org/wiki/C-ide
  ;; http://tuhdo.github.io/c-ide.html
  ;; Use M-n to move to next candidate and M-p to move back previous candidate. Use M-g s to invoke Isearch on candidate
  ;; buffer list.
  (use-package helm-gtags
    :ensure t
    :diminish helm-gtags-mode
    :init
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          ;;helm-gtags-prefix-key "\C-c g"
          helm-gtags-suggested-key-mapping t
          helm-gtags-ignore-case t
          helm-gtags-fuzzy-match t
          helm-gtags-maximum-candidates 1000
          helm-gtags-cache-select-result t
          helm-gtags-display-style 'detail
          helm-gtags-update-interval-second 60)
    (add-hook 'c-mode-common-hook #'helm-gtags-mode)
    :config
    (bind-key "M-." 'helm-gtags-dwim helm-gtags-mode-map)
    (bind-key "M-," 'helm-gtags-pop-stack helm-gtags-mode-map)
    (bind-key "M-'" 'helm-gtags-select helm-gtags-mode-map)
    (bind-key "M-t" 'helm-gtags-find-tag helm-gtags-mode-map)))

(defhydra hydra-ggtags (:color blue)
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
(bind-key "C-c g" 'hydra-ggtags/body)

;; create tags for a latex project, no need to setup a keybinding
;; http://stackoverflow.com/questions/548414/how-to-programmatically-create-update-a-tags-file-with-emacs
(defun dotemacs-create-latex-etags ()
  "Create etags for the current latex project."
  (interactive)
  (compile "find . -type f -name \"*.tex\" -print | etags -a -"))

(defun dotemacs-create-latex-ctags () ; (dir-name))
  "Create ctags for the current latex project."
  ;;(interactive "Directory: ")
  ;; (shell-command
  ;;  (format "ctags -o TAGS -R *.tex %s" (directory-file-name dir-name)))
  (interactive)
  ;;(compile "find . -name \"*.tex\" -print | ctags -a -u -o TAGS -")
  (compile "find . -type f -name \"*.tex\" -print | xargs ctags -o TAGS"))

(provide 'tags-init)

;;; tags-init.el ends here
