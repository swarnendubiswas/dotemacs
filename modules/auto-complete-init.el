;;; auto-complete-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Use auto-complete for completion.

;;; Code:

(use-package auto-complete
  :ensure t
  :init
  (require 'auto-complete-config)
  (global-auto-complete-mode)
  (ac-linum-workaround)
  (ac-config-default)

  :config
  (setq ac-auto-start 4
        ac-auto-show-menu t
        ac-expand-on-auto-complete t
        ac-show-menu-immediately-on-auto-complete t
        ac-disable-inline nil
        ac-ignore-case 'smart
        ac-use-comphist t
        ac-use-quickhelp t
        ac-use-fuzzy t
        ac-trigger-key "TAB"
        tab-always-indent 'complete
        ac-dwim t
        ac-comphist-file (concat dotemacs-temp-directory "ac-comphist.dat"))
  (add-to-list 'ac-sources 'ac-source-words-in-buffer)
  (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
  (add-to-list 'ac-sources 'ac-source-words-in-all-buffers)
  (add-to-list 'ac-sources 'ac-source-abbrev)
  (add-to-list 'ac-sources 'ac-source-features)
  (add-to-list 'ac-sources 'ac-source-dictionary)
  (add-to-list 'ac-sources 'ac-source-filename)
  (add-to-list 'ac-sources 'ac-source-files-in-curren-dir)
  (add-to-list 'ac-sources 'ac-source-imenu)
  (add-to-list 'ac-sources 'ac-source-semantic)
  (add-to-list 'ac-sources 'ac-source-semantic-raw)
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (ac-flyspell-workaround)

  (use-package auto-complete-c-headers
    :ensure t
    :config
    // FIXME: Should this be with-eval-after-load so that is executed only once? Like c-initialization-hook?
    (add-hook 'c-mode-hook
              (lambda ()
                (add-to-list 'ac-sources #'ac-sources-c-headers)
                (add-to-list 'ac-sources #'ac-sources-c-headers-symbols t))))

  ;; https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el
  (defun ac-turn-off-line-truncation (orig &optional force)
    (toggle-truncate-lines -1)
    (funcall orig force)
    (toggle-truncate-lines 1))
  (advice-add 'ac-quick-help :around #'ac-turn-off-line-truncation)

  (use-package ac-capf
    :ensure t)

  (use-package ac-etags
    :ensure t
    :config (ac-etags-setup))

  (use-package ac-helm
    :ensure t)

  (use-package ac-ispell
    :ensure t)

  (use-package ac-math
    :ensure t)

  (use-package ac-python
    :ensure t)

  (use-package ac-dabbrev
    :ensure t)

  (use-package auto-complete-clang
    :ensure t)

  (use-package auto-complete-etags
    :ensure t)

  (use-package auto-complete-exuberant-ctags
    :ensure t)

  (use-package ac-html
    :ensure t)

  (use-package auto-complete-nxml
    :ensure t)

  (use-package auto-complete-auctex
    :ensure t)

  (use-package org-ac
    :ensure t
    :config (org-ac/config-default))

  (use-package auto-complete-chunk
    :ensure t)

  (use-package ac-tex-ref
    :load-path "packages")

  (use-package ac-html-bootstrap
    :ensure t)

  (use-package ac-html-csswatcher
    :ensure t
    :config
    (if (eq dotemacs-completion 'company)
        (company-web-csswatcher-setup)
      (ac-html-csswatcher-setup)))

  :diminish auto-complete-mode)

(provide 'auto-complete-init)

;;; auto-complete-init.el ends here
