;;; auto-complete-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Use auto-complete for completion.

;;; Code:

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)

  (ac-linum-workaround)
  (with-eval-after-load "flyspell"
    (ac-flyspell-workaround))

  (setq-default ac-expand-on-auto-complete t
                ac-auto-start 4
                ac-dwim t
                ac-dwim-enable t)

  (setq ac-auto-show-menu t
        ac-use-menu-map t
        ac-show-menu-immediately-on-auto-complete t
        ac-disable-inline nil
        ac-ignore-case t
        ac-use-comphist t
        ac-quick-help-delay 1.0
        ac-use-fuzzy t
        ac-comphist-file (concat dotemacs-temp-directory "ac-comphist.dat")
        ac-dictionary-directories (concat dotemacs-temp-directory "ac-dict")
        ac-user-dictionary-files (concat dotemacs-temp-directory "dict")
        popup-use-optimized-column-computation nil)

  (add-to-list 'ac-sources
               '(ac-source-words-in-buffer
                 ac-source-words-in-all-buffers
                 ac-source-features
                 ac-source-filename
                 ac-source-files-in-curren-dir))

  (dolist (mode '(csv-mode
                  git-commit-mode
                  html-mode
                  inferior-emacs-lisp-mode
                  js3-mode
                  less-css-mode
                  log-edit-mode
                  markdown-mode
                  nxml-mode
                  org-mode
                  sass-mode
                  sh-mode
                  text-mode))
    (add-to-list 'ac-modes mode))

  (use-package ac-capf
    :ensure t
    :config (ac-capf-setup))

  (use-package ac-helm
    :ensure t
    :if (eq dotemacs-selection 'helm)
    :bind (:map ac-complete-mode-map
                ("C-:" . ac-complete-with-helm)))

  (use-package ac-ispell ; Ispell/aspell completion source for auto-complete
    :ensure t
    :disabled t ; Seems to add noise
    :config
    (setq ac-ispell-requires 4 ; Minimum input for starting completion
          ac-ispell-fuzzy-limit 2)
    (ac-ispell-setup)
    (add-hook 'text-mode-hook #'ac-ispell-ac-setup))

  (use-package ac-dabbrev
    :ensure t
    :config (add-to-list 'ac-sources 'ac-source-dabbrev))

  :bind (;; Filter candidates by pattern with "C-f"
         :map ac-completing-map
              ("C-n" . ac-next)
              ("C-p" . ac-previous)
              ("C-s" . nil)
              ("C-f" . ac-isearch)
              ("C-g" . ac-stop)
              :map ac-complete-mode-map
              ("C-n" . ac-next)
              ("C-p" . ac-previous)
              ("C-s" . nil)
              ("C-f" . ac-isearch)
              ("C-g" . ac-stop)
              :map ac-menu-map
              ("C-s" . nil)
              ("C-f" . ac-isearch))
  :diminish auto-complete-mode)

(provide 'auto-complete-init)

;;; auto-complete-init.el ends here
