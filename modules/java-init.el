;;; java-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure Java programming mode.

;;; Code:

(defvar dotemacs-completion-in-buffer)
(defvar dotemacs-extras-directory)

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2
                  c-set-style "java")))

(use-package ant
  :ensure t)

(use-package autodisass-java-bytecode ; Can disassemble .class files from within jars as well
  :ensure t
  :defer t)

(use-package jdee
  :ensure t
  :config
  (setq jdee-server-dir dotemacs-extras-directory
        jdee-complete-function 'jdee-complete-minibuf)
  (if (string-equal (system-name) "consensus.ices.utexas.edu")
      (setq jdee-global-classpath '("/usr/lib/jvm/java-1.8.0-openjdk/jre/lib/rt.jar"
                                    "/usr/lib/jvm/java-1.8.0-openjdk/jre/lib/jce.jar"
                                    "/usr/lib/jvm/java-1.8.0-openjdk/jre/lib/jsse.jar"
                                    "/usr/lib/jvm/java-1.8.0-openjdk/jre/lib/charsets.jar"
                                    "/usr/lib/jvm/java-1.8.0-openjdk/jre/lib/resources.jar"))
    (setq jdee-global-classpath '("/usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/rt.jar"
                                  "/usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/jce.jar"
                                  "/usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/jsse.jar"
                                  "/usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/charsets.jar"
                                  "/usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/resources.jar")))
  :defer t)

(use-package eclim
  :ensure t
  :defer t
  :init
  (use-package eclimd
    :config (setq eclimd-autostart t))
  (setq eclim-eclipse-dirs "/home/sbiswas/software/eclipse-neon-java/"
        eclim-executable "/home/sbiswas/software/eclipse-neon-java/eclim"
        eclim-default-workspace "/home/sbiswas/plass-workspace"
        eclim-auto-save t)
  (add-hook 'java-mode-hook #'eclim-mode)
  :config 
  (use-package company-emacs-eclim
    :ensure t
    :if (eq dotemacs-completion-in-buffer 'company)
    :config (company-emacs-eclim-setup))
  (use-package ac-emacs-eclim
    :ensure t
    :if (eq dotemacs-completion-in-buffer 'auto-complete)
    :config (ac-emacs-eclim-config)))

(provide 'java-init)

;;; java-init.el ends here
