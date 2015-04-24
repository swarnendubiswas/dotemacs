;;; auto-complete-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Use auto-complete for completion.

;;; Code:

(use-package auto-complete
  :ensure t
  :config
  (setq ac-auto-start 3
        ac-ignore-case 'smart
        ac-comphist-file (concat emacs-temp-directory "ac-comphist.dat"))
  (require 'auto-complete-config)
  (global-auto-complete-mode)
  (use-package auto-complete-c-headers
    :ensure t
    :config
    (add-hook 'c-mode-hook
              (lambda ()
                (add-to-list 'ac-sources 'ac-sources-c-headers)
                (add-to-list 'ac-sources 'ac-sources-c-headers-symbols t))))
  (use-package ac-capf
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
  (add-to-list 'ac-sources 'ac-source-abbrev)
  (add-to-list 'ac-sources 'ac-source-dictionary)
  (add-to-list 'ac-sources 'ac-source-filename)
  (add-to-list 'ac-sources 'ac-source-files-in-curren-dir)
  (add-to-list 'ac-sources 'ac-source-imenu)
  (add-to-list 'ac-sources 'ac-source-semantic)
  (add-to-list 'ac-sources 'ac-source-words-in-buffer)
  (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (ac-flyspell-workaround)
  (ac-config-default)
  :diminish auto-complete-mode)

(provide 'auto-complete-init)

;;; auto-complete-init.el ends here
