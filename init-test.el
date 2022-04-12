(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")        t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-defer       nil
      use-package-expand-minimally   nil
      use-package-always-ensure        t
      use-package-hook-name-suffix     nil)

(use-package bind-key)
(use-package diminish)

(use-package no-littering :demand t)

(use-package doom-themes
  :init (load-theme 'doom-one t)
  :config
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure all-the-icons
  :ensure t
  :init
  (all-the-icons-install-fonts t)
  (doom-modeline-mode 1))

(use-package beacon
  :diminish
  :hook (after-init-hook . beacon-mode))

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode)
  :config (setq all-the-icons-ibuffer-icon-size 0.8))

(use-package all-the-icons-dired
  :diminish
  :hook
  (dired-mode-hook . (lambda ()
                       (unless (file-remote-p default-directory)
                         (all-the-icons-dired-mode 1)))))

(use-package anzu
  :diminish anzu-mode
  :init
  (setq anzu-search-threshold     10000
        anzu-minimum-input-length 2)
  (global-anzu-mode 1)
  :bind
  (([remap query-replace]        . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)))

(use-package amx
  :hook (after-init-hook . amx-mode)
  :bind
  (("M-x"  . execute-extended-command)
   ("<f1>" . execute-extended-command-for-buffer)))

(use-package orderless
  :after (vertico)
  :demand t
  :config
  (setq completion-styles '(orderless partial-completion) ; initials, basic, emacs22
        orderless-matching-styles '(orderless-regexp)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic-remote orderless partial-completion))
                                        ;; (minibuffer (initials))))
                                        )))

(use-package format-all
  :diminish
  :preface
  (defun sb/enable-format-all ()
    "Delay enabling format-all to avoid slowing down Emacs startup."
    (dolist (hook '(bazel-mode-hook LaTeX-mode-hook web-mode-hook markdown-mode-hook))
      (add-hook hook #'format-all-mode))
    (add-hook 'format-all-mode-hook #'format-all-ensure-formatter))
  :diminish
  :hook
  ((format-all-mode-hook . format-all-ensure-formatter)
   ((bazel-mode-hook LaTeX-mode-hook web-mode-hook markdown-mode-hook) . format-all-mode)))

(use-package vertico
  :hook (after-init-hook . vertico-mode)
  :custom
  (vertico-resize nil)
  (vertico-count 12)
  (vertico-scroll-margin 4)
  :config
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  :bind
  (("<f2>" .  find-file)
   :map vertico-map
   ("<escape>" . minibuffer-keyboard-quit)
   ("?" . minibuffer-completion-help)
   ("M-RET" . minibuffer-force-complete-and-exit)
   ("M-TAB" . minibuffer-complete)))

(use-package consult
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-line-numbers-widen t)
  :bind
  (("C-x b" . consult-buffer)
   ("<f3>" . consult-buffer)
   ("C-x p b" . consult-project-buffer)
   ("M-y" . consult-yank-pop)
   ([remap yank-pop] . consult-yank-pop)
   ([remap goto-line] . consult-goto-line)
   ("C-c C-j" . consult-imenu)
   ([remap imenu] . consult-imenu)
   ("M-s r" . consult-ripgrep)
   ("<f4>" . consult-line)
   ("<f9>" . consult-recent-file)
   ([remap recentf-open-files] . consult-recent-file))
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :config
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq completion-in-region-function #'consult-completion-in-region)

  ;; Disable live preview
  (consult-customize
   consult-recent-file consult-buffer
   :preview-key nil))

;; https://kristofferbalintona.me/posts/corfu-kind-icon-and-corfu-doc/
(use-package corfu
  :hook (after-init-hook . corfu-global-mode)
  :custom
  (corfu-auto t "Enable auto completion")
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  (corfu-count 15)
  (corfu-preselect-first t)
  :bind
  (:map corfu-map
        ([tab] . corfu-next)
        ([backtab] . corfu-previous)))

(use-package corfu-doc
  :hook (corfu-mode-hook . corfu-doc-mode))

;; https://kristofferbalintona.me/posts/cape/
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package marginalia
  :after vertico
  :init (marginalia-mode 1))

(use-package centaur-tabs
  :commands centaur-tabs-group-by-projectile-project
  :hook (emacs-startup-hook . centaur-tabs-mode)
  :init
  (setq centaur-tabs-set-icons nil ; The icons often do not blend well with the theme
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "*"
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-set-close-button nil
        centaur-tabs-show-new-tab-button nil
        centaur-tabs-enable-ido-completion nil)
  :config
  (centaur-tabs-group-by-projectile-project)
  :bind
  (("M-<right>" . centaur-tabs-forward-tab)
   ("M-<left>" . centaur-tabs-backward-tab)))

;;; init-emacs28.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((dired-omit-files . "\\`[.]?#\\|\\`[.][.]?\\'\\|\\.git\\'|\\.cache\\'")
     (eval add-hook 'lsp-managed-mode-hook
	   (lambda nil
	     (when
		 (derived-mode-p 'markdown-mode)
	       (setq sb/flycheck-local-checkers
		     '((lsp
			(next-checkers markdown-markdownlint-cli)))))
	     (when
		 (derived-mode-p 'gfm-mode)
	       (setq sb/flycheck-local-checkers
		     '((lsp
			(next-checkers markdown-markdownlint-cli)))))
	     (when
		 (derived-mode-p 'sh-mode)
	       (setq sb/flycheck-local-checkers
		     '((lsp
			(next-checkers sh-shellcheck)))))
	     (when
		 (derived-mode-p 'yaml-mode)
	       (setq sb/flycheck-local-checkers
		     '((lsp
			(next-checkers yaml-yamllint)))))
	     (when
		 (derived-mode-p 'json-mode)
	       (setq sb/flycheck-local-checkers
		     '((lsp
			(next-checkers json-jsonlint)))))
	     (when
		 (derived-mode-p 'python-mode)
	       (setq sb/flycheck-local-checkers
		     '((lsp
			(next-checkers python-pylint)))))
	     (when
		 (derived-mode-p 'c++-mode)
	       (setq sb/flycheck-local-checkers
		     '((lsp
			(next-checkers c/c++-cppcheck)))))
	     (when
		 (derived-mode-p 'html-mode)
	       (setq sb/flycheck-local-checkers
		     '((lsp
			(next-checkers html-tidy)))))
	     (when
		 (derived-mode-p 'xml-mode)
	       (setq sb/flycheck-local-checkers
		     '((lsp
			(next-checkers xml-xmllint))))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
