;;; flyspell-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup spell check.

;;; Code:

(use-package flyspell
  :if (eq system-type 'gnu/linux)
  :preface
  (defun dotemacs--activate-flyspell ()
    "Turn on flyspell-mode and call flyspell-buffer."
    (interactive)
    ;; This next line REALLY slows buffer switching.
    (flyspell-mode)
    (flyspell-buffer))

  (defvar customised-hooks-alist
    '(text-mode-hook)
    "An alist of hooks that require customizations.")

  :init
  (use-package flyspell-lazy
    :ensure t
    :init (flyspell-lazy-mode 1))

  (setq-default ispell-program-name "/usr/bin/aspell")
  ;; speed up aspell: ultra | fast | normal | bad-spellers
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
  (setq flyspell-sort-corrections t
        flyspell-check-region-doublons t
        flyspell-issue-message-flag nil)
  (add-hook 'find-file-hooks #'turn-on-flyspell)

  ;; this is useful but slow
  ;;(add-hook 'before-save-hook 'flyspell-buffer)

  ;; ;; Activate flyspell for various major modes.
  ;; (unless noninteractive
  ;;   (add-hook-list customised-hooks-alist 'activate-flyspell))

  :diminish flyspell-mode
  :bind
  (("C-c i f" . flyspell-mode)
   ("C-c i b" . flyspell-buffer)
   ;; another alternative is M-$
   ("C-c i w" . ispell-word))

  :config
  (use-package helm-flyspell
    :ensure t
    :config (define-key flyspell-mode-map (kbd "M-$") 'helm-flyspell-correct))

  (use-package ace-flyspell
    :defer t
    :ensure t)

  (use-package flyspell-popup
    :ensure t
    :config
    (bind-key "C-;" #'flyspell-popup-correct flyspell-mode-map)))

(defhydra hydra-flyspell (:color blue)
  "flyspell mode"
  ("f" flyspell-mode "flyspell-mode")
  ("b" flyspell-buffer "flyspell-buffer")
  ("w" ispell-word "ispell-word"))
(bind-key "C-c i" 'hydra-flyspell/body)

(provide 'flyspell-init)

;;; flyspell-init.el ends here
