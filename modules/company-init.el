;;; company-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Initialize company mode for auto completion.

;;; Code:

(defvar dotemacs-selection)
(defvar dotemacs-temp-directory)
(defvar dotemacs-use-fci-p)
(defvar dotemacs-completion-in-buffer)

(use-package company
  :ensure t
  :diminish company-mode
  :preface
  (defun dotemacs--quit-company-save-buffer ()
    "Quit company popup and save the buffer."
    (interactive)
    (company-abort)
    (save-buffer))
  :init (global-company-mode 1)
  :config
  (setq company-global-modes t ; Turn on company-mode for all major modes
        company-show-numbers t ; Quick-access numbers for the first ten candidates
        company-minimum-prefix-length 3
        company-tooltip-flip-when-above nil ; Invert the navigation direction if the completion popup is displayed on top
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-selection-wrap-around t
        ;; company-dabbrev-downcase nil ; Do not downcase the returned candidates
        ;; company-dabbrev-ignore-case t
        ;; company-dabbrev-code-everywhere t ; Offer completions in comments and strings
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-frontend
                            company-echo-metadata-frontend)
        company-require-match 'never)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-s" . dotemacs--quit-company-save-buffer)))

(use-package company-flx
  :ensure t
  :after company
  :config
  (setq company-flx-limit 50)
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
  :config
  (setq company-quickhelp-delay 0.2
        company-quickhelp-max-lines 60)
  (company-quickhelp-mode 1))

(use-package company-dict
  :ensure t
  :after company
  :config
  (setq company-dict-dir (concat dotemacs-temp-directory "dict/")
        company-dict-enable-fuzzy t
        company-dict-enable-yasnippet nil)
  (add-to-list 'company-backends 'company-dict))

(use-package helm-company
  :ensure t
  :if (and (eq dotemacs-selection 'helm) (eq dotemacs-completion-in-buffer 'company))
  :after company
  :bind (:map company-mode-map
              ([remap complete-symbol] . helm-company)
              ([remap completion-at-point] . helm-company)
              ("C-:" . helm-company)
              :map company-active-map
              ("C-:" . helm-company)))

(when (and (eq dotemacs-selection 'ivy) (eq dotemacs-completion-in-buffer 'company))
  (with-eval-after-load "counsel"
    (bind-key [remap complete-symbol] #'counsel-company company-mode-map)
    (bind-key [remap completion-at-point] #'counsel-company company-mode-map)
    (bind-key "C-:" #'counsel-company company-mode-map)
    (bind-key "C-:" #'counsel-company company-active-map)))

(provide 'company-init)

;;; company-init.el ends here
