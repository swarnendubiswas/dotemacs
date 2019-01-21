;;; tramp-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup tramp.

;;; Code:

(defvar tramp-persistency-file-name)
(defvar dotemacs-temp-directory)

;; A few hacks are from https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
;; /method:user@host#port:filename. Shortcut /ssh:: will connect to default user@host#port.
;; Open a file with ssh + sudo: C-x C-f /ssh:host|sudo:root:/etc/passwd
(use-package tramp
  :config
  (setq tramp-default-method "ssh" ; ssh is faster than the default scp
        tramp-default-user "sbiswas"
        tramp-default-host "consensus.ices.utexas.edu"
        ;; Auto-save to a local directory for better performance
        tramp-auto-save-directory (concat dotemacs-temp-directory "tramp-auto-save")
        tramp-persistency-file-name (concat dotemacs-temp-directory "tramp")
        tramp-verbose 1 ; Default is 3
        remote-file-name-inhibit-cache nil ; Remote files are not updated outside of Tramp
        tramp-completion-reread-directory-timeout nil)
  (defalias 'exit-tramp 'tramp-cleanup-all-buffers)
  (add-to-list 'tramp-default-method-alist '("" "biswas.38" "ssh"))
  (add-to-list 'tramp-default-method-alist '("" "sbiswas" "ssh"))
  (add-to-list 'tramp-default-method-alist
               '("\\`localhost\\'" "\\`root\\'" "su"))

  ;; Disable backup
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))

  ;; Disable version control. If you access remote files which are not under version control, a lot of check operations
  ;; can be avoided by disabling VC.
  (setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp
                                     tramp-file-name-regexp)))
(use-package counsel-tramp
  :ensure t
  :after tramp)

(provide 'tramp-init)

;;; tramp-init.el ends here
