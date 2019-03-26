;;; company-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Initialize company mode for auto completion.

;;; Code:

(defvar dotemacs-temp-directory)

(use-package company
  :ensure t
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :preface
  (defun sb/quit-company-save-buffer ()
    "Quit company popup and save the buffer."
    (company-abort)
    (save-buffer))
  ;; :init (global-company-mode 1)
  :hook (after-init . global-company-mode)
  :config
  (setq company-global-modes t ; Turn on company-mode for all major modes
        company-show-numbers t ; Quick-access numbers for the first ten candidates
        company-minimum-prefix-length 2
        company-idle-delay 0.1
        ;; Invert the navigation direction if the completion popup is displayed on top
        company-tooltip-flip-when-above nil
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-selection-wrap-around t
        company-dabbrev-downcase nil ; Do not downcase the returned candidates
        company-dabbrev-ignore-case nil
        company-dabbrev-code-everywhere t ; Offer completions in comments and strings
        company-dabbrev-other-buffers t
        company-dabbrev-code-modes t ; Use company-dabbrev-code in all modes
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-frontend
                            company-echo-metadata-frontend)
        ;; Allow typing keys that do not match any candidates
        company-require-match nil)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-s" . sb/quit-company-save-buffer)))

(use-package company-flx
  :ensure t
  :after company
  :config
  (setq company-flx-limit 20)
  (company-flx-mode 1))

(use-package company-statistics
  :ensure t
  :after company
  :config
  (setq company-statistics-file (concat dotemacs-temp-directory "company-statistics-cache.el"))
  (company-statistics-mode 1))

(use-package company-quickhelp
  :ensure t
  :after company
  :hook (global-company-mode . company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 0.5
        company-quickhelp-max-lines 60))

(use-package company-dict
  :ensure t
  :after company
  :config
  (setq company-dict-dir (concat user-emacs-directory "dict/")
        company-dict-enable-fuzzy t
        company-dict-enable-yasnippet nil)
  (add-to-list 'company-backends 'company-dict))

(with-eval-after-load "counsel"
  (bind-key [remap complete-symbol] #'counsel-company company-mode-map)
  (bind-key [remap completion-at-point] #'counsel-company company-mode-map)
  (bind-key "C-:" #'counsel-company company-mode-map)
  (bind-key "C-:" #'counsel-company company-active-map))

(use-package company-elisp
  :after company
  :config (push 'company-elisp company-backends))

(use-package company-prescient
  :ensure t
  :after (company prescient)
  :config (company-prescient-mode 1))

(provide 'company-init)

;;; company-init.el ends here
