;;; auto-complete-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Use auto-complete for completion.

;;; Code:

(use-package auto-complete
  :ensure t
  :config
  (setq ac-auto-start 4
        ac-auto-show-menu t
        ac-expand-on-auto-complete t
        ac-show-menu-immediately-on-auto-complete t
        ac-ignore-case 'smart
        ac-use-comphist t
        ac-use-quickhelp t
        ac-use-fuzzy t
        tab-always-indent 'complete
        ac-dwim t
        ac-comphist-file (concat emacs-temp-directory "ac-comphist.dat"))
  (require 'auto-complete-config)
  (global-auto-complete-mode)
  (ac-linum-workaround)
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
  (ac-config-default)
  (use-package auto-complete-c-headers
    :ensure t
    :config
    (add-hook 'c-mode-hook
              (lambda ()
                (add-to-list 'ac-sources 'ac-sources-c-headers)
                (add-to-list 'ac-sources 'ac-sources-c-headers-symbols t))))
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
    :load-path "lisp")
  :diminish auto-complete-mode)

(provide 'auto-complete-init)

;;; auto-complete-init.el ends here
