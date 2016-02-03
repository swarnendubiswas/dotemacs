;;; auto-complete-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Use auto-complete for completion.

;;; Code:

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (global-auto-complete-mode 1)

  (ac-linum-workaround)
  (ac-flyspell-workaround)

  ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-auto-complete.el
  (setq-default ac-expand-on-auto-complete t
                ac-auto-start 3
                ;; To get pop-ups with docs even if a word is uniquely completed
                ac-dwim t)

  (setq ac-auto-show-menu t
        ac-show-menu-immediately-on-auto-complete t
        ac-disable-inline nil
        ac-ignore-case t ; options: 'smart
        ac-use-comphist t
        ac-use-quickhelp t
        ac-quick-help-delay 1.0
        ac-use-fuzzy t
        ;; ac-trigger-key "TAB" ; Should generally be used if ac-auto-start is nil
        tab-always-indent 'complete ; Use 't when auto-complete is disabled
        ac-comphist-file (concat dotemacs-temp-directory "ac-comphist.dat")
        ac-dictionary-directories (concat dotemacs-temp-directory "ac-dict")
        ac-user-dictionary-files (concat dotemacs-temp-directory ".dict"))

  (add-to-list 'ac-sources
               '(ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
                 ac-source-words-in-all-buffers
                 ac-source-abbrev
                 ac-source-features
                 ac-source-dictionary
                 ac-source-filename
                 ac-source-files-in-curren-dir
                 ac-source-yasnippet))

  (dolist (mode '(log-edit-mode org-mode text-mode haml-mode
                                git-commit-mode
                                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                                lisp-mode textile-mode markdown-mode tuareg-mode
                                js3-mode css-mode less-css-mode sql-mode
                                sql-interactive-mode
                                inferior-emacs-lisp-mode))
    (add-to-list 'ac-modes mode))

  ;; ;; https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el
  ;; (defun ac-turn-off-line-truncation (orig &optional force)
  ;;   (toggle-truncate-lines -1)
  ;;   (funcall orig force)
  ;;   (toggle-truncate-lines 1))
  ;; (advice-add 'ac-quick-help :around #'ac-turn-off-line-truncation)

  (use-package ac-capf
    :ensure t
    :config (ac-capf-setup))

  (use-package ac-helm
    :ensure t
    :if (bound-and-true-p dotemacs-use-helm-p))

  (use-package ac-ispell
    :ensure t
    :disabled ; FIXME: Enabling this package seems to disable inline autocompletes or popups
    :init
    (setq ac-ispell-requires 3
          ac-ispell-fuzzy-limit 2)
    :config (add-hook 'text-mode-hook #'ac-ispell-setup))

  (use-package ac-dabbrev
    :ensure t
    :config (add-to-list 'ac-sources 'ac-source-dabbrev))

  (use-package ac-emoji
    :ensure t
    :disabled t)

  :diminish auto-complete-mode)

(provide 'auto-complete-init)

;;; auto-complete-init.el ends here
