;;; flyspell-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup spell check.  Assume aspell is available.

;;; Code:

(use-package ispell
  :init
  (setq-default ispell-program-name "/usr/bin/aspell")
  (setq ispell-dictionary "english"
        ;; aspell speed: ultra | fast | normal | bad-spellers
        ispell-extra-args '("--sug-mode=ultra"
                            "--lang=en_US")
        ;; Save a new word to personal dictionary without asking
        ispell-silently-savep t))

(use-package flyspell
  :if (eq system-type 'gnu/linux)
  :after ispell
  :preface
  (defun dotemacs--activate-flyspell ()
    "Turn on flyspell-mode and call flyspell-buffer."
    (interactive)
    (flyspell-mode) ; This REALLY slows buffer switching
    (flyspell-buffer))

  ;; Move point to previous error, based on code by hatschipuh at http://emacs.stackexchange.com/a/14912/2017
  ;; http://pragmaticemacs.com/emacs/jump-back-to-previous-typo/
  (defun dotemacs-flyspell-goto-previous-error (arg)
    "Go to arg previous spelling error."
    (interactive "p")
    (while (not (= 0 arg))
      (let ((pos (point))
            (min (point-min)))
        (if (and (eq (current-buffer) flyspell-old-buffer-error)
                 (eq pos flyspell-old-pos-error))
            (progn
              (if (= flyspell-old-pos-error min)
                  ;; goto beginning of buffer
                  (progn
                    (message "Restarting from end of buffer")
                    (goto-char (point-max)))
                (backward-word 1))
              (setq pos (point))))
        ;; seek the next error
        (while (and (> pos min)
                    (let ((ovs (overlays-at pos))
                          (r '()))
                      (while (and (not r) (consp ovs))
                        (if (flyspell-overlay-p (car ovs))
                            (setq r t)
                          (setq ovs (cdr ovs))))
                      (not r)))
          (backward-word 1)
          (setq pos (point)))
        ;; save the current location for next invocation
        (setq arg (1- arg))
        (setq flyspell-old-pos-error pos)
        (setq flyspell-old-buffer-error (current-buffer))
        (goto-char pos)
        (if (= pos min)
            (progn
              (message "No more miss-spelled word!")
              (setq arg 0))
          (forward-word)))))
  :init
  (setq flyspell-sort-corrections t
        flyspell-issue-message-flag nil)

  ;; This is to turn on spell check in *scratch* buffer, which is in text-mode.
  (dolist (hook '(text-mode-hook
                  find-file-hooks))
    (add-hook hook #'turn-on-flyspell))
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)

  ;; This is useful but SLOW
  ;; (add-hook 'before-save-hook #'flyspell-buffer)

  :config (bind-key* "C-," #'dotemacs-flyspell-goto-previous-error)
  :diminish flyspell-mode
  :bind
  (("C-c f f" . flyspell-mode)
   ("C-c f b" . flyspell-buffer)
   ("C-c f w" . ispell-word)))

(use-package helm-flyspell
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :after flyspell
  :config (bind-key "M-$" #'helm-flyspell-correct flyspell-mode-map))

(use-package flyspell-popup
  :ensure t
  :after flyspell
  :config (bind-key "C-;" #'flyspell-popup-correct flyspell-mode-map))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :config
  (cond ((eq dotemacs-selection 'helm) (use-package flyspell-correct-helm
                                         :ensure t
                                         :config (setq flyspell-correct-interface 'flyspell-correct-helm)))
        ;; Use ivy-read-action (C-M-a) to invoke "correct", "save", "accept" options
        ((eq dotemacs-selection 'ivy) (use-package flyspell-correct-ivy
                                        :ensure t
                                        :config (setq flyspell-correct-interface 'flyspell-correct-ivy)))
        (t (use-package flyspell-correct-popup
             :ensure t
             :config (setq flyspell-correct-interface 'flyspell-correct-popup))))
  (bind-key "M-$" #'flyspell-correct-word-generic flyspell-mode-map))

(provide 'flyspell-init)

;;; flyspell-init.el ends here
