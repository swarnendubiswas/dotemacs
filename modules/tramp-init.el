;;; tramp-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup tramp.

;;; Code:

(defvar tramp-persistency-file-name)
(defvar dotemacs-temp-directory)

;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
(use-package tramp ; /method:user@host#port:filename. Shortcut /ssh:: will connect to default user@host#port.
  :defer t
  :config
  (setq tramp-default-method "ssh" ; ssh is faster than the default scp
        tramp-default-user "sbiswas"
        ;; tramp-default-host "stdlinux.cse.ohio-state.edu"
        tramp-default-host "consensus.ices.utexas.edu"
        ;; Auto-save to a local directory for better performance
        tramp-auto-save-directory (concat dotemacs-temp-directory "tramp-auto-save")
        tramp-persistency-file-name (concat dotemacs-temp-directory "tramp")
        tramp-verbose 8)

  (add-to-list 'tramp-default-method-alist '("" "biswass" "ssh"))
  (add-to-list 'tramp-default-method-alist '("" "sbiswas" "ssh"))
  (add-to-list 'tramp-default-method-alist
               '("\\`localhost\\'" "\\`root\\'" "su"))

  ;; Disable backup
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))
  ;; Disable version control. If you access remote files which are not under version control, a lot of check operations
  ;; can be avoided by disabling VC.
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp
                                     tramp-file-name-regexp))
  (use-package password-cache
    :config (setq password-cache-expiry nil)))

(provide 'tramp-init)
;;; tramp-init.el ends here
