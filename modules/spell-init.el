;;; spell-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup spell check.  Assume aspell is available.

;;; Code:

(defvar dotemacs-extras-directory)

(use-package ispell
  :init
  (setq-default ispell-program-name (executable-find "aspell"))
  (setq ispell-dictionary "english"
        ispell-personal-dictionary (concat dotemacs-extras-directory "spell")
        ;; Aspell speed: ultra | fast | normal | bad-spellers
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")
        ;; Save a new word to personal dictionary without asking
        ispell-silently-savep t))

(use-package flyspell
  :if (eq system-type 'gnu/linux)
  :preface
  ;; Move point to previous error, based on code by hatschipuh at http://emacs.stackexchange.com/a/14912/2017
  ;; http://pragmaticemacs.com/emacs/jump-back-to-previous-typo/
  (defun sb/flyspell-goto-previous-error (arg)
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
  (setq flyspell-sort-corrections nil
        flyspell-issue-message-flag nil)

  ;; This is to turn on spell check in *scratch* buffer, which is in text-mode.
  (dolist (hook '(text-mode-hook find-file-hooks))
    (add-hook hook #'turn-on-flyspell))
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)

  :diminish flyspell-mode
  :bind
  (("C-c f f" . flyspell-mode)
   ("C-c f b" . flyspell-buffer)
   ("C-c f w" . ispell-word)
   :map flyspell-mode-map
   ("C-;" . nil)
   ("C-," . sb/flyspell-goto-previous-error)))

(or
 ;; (use-package flyspell-popup
 ;;   :ensure t
 ;;   :after flyspell
 ;;   :bind ("C-;" . flyspell-popup-correct))

 (use-package flyspell-correct
   :ensure t
   :ensure flyspell-correct-ivy
   ;; :after flyspell
   :bind ("C-;" . flyspell-correct-wrapper)))

(provide 'spell-init)

;;; spell-init.el ends here
