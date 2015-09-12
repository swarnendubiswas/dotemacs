;;; flyspell-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup spell check.

;;; Code:

(use-package flyspell
  :if (eq system-type 'gnu/linux)
  :preface
  (defun dotemacs-activate-flyspell ()
    "Turn on flyspell-mode and call flyspell-buffer."
    (interactive)
    ;; This next line REALLY slows buffer switching.
    (flyspell-mode)
    (flyspell-buffer))

  (defvar customised-hooks-alist
    '(text-mode-hook)
    "An alist of hooks that require customizations.")

  :init
  ;; use this package if there are performance issues with flyspell, note that this package disables spell checks for
  ;; certain special buffers, including *scratch*
  (use-package flyspell-lazy
    :ensure t
    :disabled t
    :init (flyspell-lazy-mode 1))

  (setq-default ispell-program-name "/usr/bin/aspell")
  ;; speed up aspell: ultra | fast | normal | bad-spellers
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
  (setq flyspell-sort-corrections t
        flyspell-check-region-doublons t
        flyspell-issue-message-flag nil)

  (add-hook 'find-file-hooks #'turn-on-flyspell)
  ;; this is to turn on spell check in *scratch* buffer, which is in text-mode.
  (add-hook 'text-mode-hook #'turn-on-flyspell)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)

  ;; this is useful but slow
  ;; (add-hook 'before-save-hook 'flyspell-buffer)

  ;; ;; Activate flyspell for various major modes.
  ;; (unless noninteractive
  ;;   (add-hook-list customised-hooks-alist 'activate-flyspell))

  (use-package helm-flyspell
    :ensure t
    :config (bind-key "M-$" #'helm-flyspell-correct flyspell-mode-map))

  (use-package ace-flyspell
    :ensure t
    :disabled t)

  (use-package flyspell-popup
    :ensure t
    :config (bind-key "C-;" #'flyspell-popup-correct flyspell-mode-map))

  :diminish flyspell-mode
  :bind
  (("C-c i f" . flyspell-mode)
   ("C-c i b" . flyspell-buffer)
   ;; another alternative is M-$
   ("C-c i w" . ispell-word)))

(defhydra hydra-flyspell (:color blue)
  "flyspell mode"
  ("f" flyspell-mode "flyspell-mode")
  ("b" flyspell-buffer "flyspell-buffer")
  ("w" ispell-word "ispell-word"))
(bind-key "C-c i" 'hydra-flyspell/body)

(provide 'flyspell-init)

;;; flyspell-init.el ends here
