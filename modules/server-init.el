;;; server-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup emacs server.

;;; Code:

(use-package server
  :config
  (progn
    ;; start server if not root user
    (unless (string-equal "root" (getenv "USER"))
      (server-force-delete)
      (server-start))))

(provide 'server-init)

;;; server-init.el ends here
