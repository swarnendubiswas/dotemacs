;;; tags-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup tags for different projects.

;;; Code:

(setq tags-revert-without-query t
      tags-case-fold-search nil)

(use-package etags
  :bind ("M-T" . tags-search)
  :if (eq system-type 'windows-nt)
  :config
  (when (eq dotemacs-completion-in-buffer 'auto-complete)
    (progn
      ;; FIXME: Disabling these packages seems to disable auto-complete
      (or
       (use-package ac-etags
         :ensure t
         :after auto-complete
         :config
         (setq ac-etags-requires 4)
         (ac-etags-setup)
         (add-hook 'c-mode-common-hook 'ac-etags-ac-setup))
       (use-package auto-complete-etags
         :ensure t
         :disabled t
         :after auto-complete
         :config
         (setq ac-etags-use-document t)
         (add-to-list 'ac-sources 'ac-source-etags))))))

;; Front end to gnu global, use gtags -v -c. Languages supported are C, C++, Yacc, Java, PHP4 and assembly.
;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-gtags.el
;; http://tuhdo.github.io/c-ide.html
(use-package ggtags
  :ensure t
  :if (and (eq system-type 'gnu/linux) (not (eq dotemacs-selection 'helm)))
  :diminish ggtags-mode
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1))))
  (add-hook 'python-mode-hook #'ggtags-mode)
  :config
  (setq ggtags-navigation-mode-lighter nil
        ggtags-oversize-limit (* 50 1024 1024)
        ggtags-completing-read-function nil)
  :bind* (:map ggtags-mode-map
               ("C-c g s" . ggtags-find-other-symbol)
               ("C-c g h" . ggtags-view-tag-history)
               ("C-c g r" . ggtags-find-reference)
               ("C-c g f" . ggtags-find-file)
               ("C-c g c" . ggtags-create-tags)
               ("C-c g u" . ggtags-update-tags)
               ("M-." . ggtags-find-tag-dwim)
               ("M-," . pop-tag-mark)))

;; http://wikemacs.org/wiki/C-ide
;; http://tuhdo.github.io/c-ide.html
;; Use M-n to move to next candidate and M-p to move back previous candidate. Use "M-g s" to invoke Isearch on candidate
;; buffer list.
(use-package helm-gtags
  :ensure t
  :diminish helm-gtags-mode
  :if (eq dotemacs-selection 'helm)
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        ;; helm-gtags-prefix-key "\C-c g"
        helm-gtags-suggested-key-mapping t
        helm-gtags-ignore-case t
        helm-gtags-fuzzy-match t
        helm-gtags-maximum-candidates 1000
        helm-gtags-cache-select-result t
        helm-gtags-display-style 'detail
        helm-gtags-update-interval-second 60)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (helm-gtags-mode 1))))
  :bind (:map helm-gtags-mode-map
              ("M-." . helm-gtags-dwim)
              ("M-," . helm-gtags-pop-stack)
              ("M-'" . helm-gtags-select)
              ("M-t" . helm-gtags-find-tag)))

(use-package counsel-gtags
  :ensure t
  :if (eq dotemacs-selection 'ivy)
  :diminish counsel-gtags-mode
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (counsel-gtags-mode 1))))
  (add-hook 'python-mode-hook #'counsel-gtags-mode))

;; http://stackoverflow.com/questions/548414/how-to-programmatically-create-update-a-tags-file-with-emacs
(defun dotemacs-create-latex-etags ()
  "Create etags for the current latex project."
  (interactive)
  (compile "find . -type f -name \"*.tex\" -print | etags -a -"))

(defun dotemacs-create-latex-ctags () ; (dir-name))
  "Create ctags for the current latex project."
  (interactive)
  (compile "find . -type f -name \"*.tex\" -print | xargs ctags -o TAGS"))

(defun dotemacs-create-python-etags ()
  "Create etags for the current python project."
  (interactive)
  (compile "find . -type f -name '*.py' | xargs etags -a -"))

(provide 'tags-init)

;;; tags-init.el ends here
