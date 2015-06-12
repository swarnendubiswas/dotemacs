;;; server-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup emacs server.

;;; Code:

(use-package server
  :disabled t
  :init
  ;; start server if not root user
  (unless (string-equal "root" (getenv "USER"))
    ;; http://stackoverflow.com/questions/9999320/how-to-check-if-a-function-e-g-server-running-p-is-available-under-emacs
    (if (and (fboundp 'server-running-p)
             (not (server-running-p)))
        ;;(server-force-delete)
        (server-start))))

(provide 'server-init)

;;; server-init.el ends here
