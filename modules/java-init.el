;;; java-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure Java programming mode.

;;; Code:

(defvar dotemacs-completion-in-buffer)
(defvar dotemacs-extras-directory)

(add-hook 'java-mode-hook
          (lambda ()
            (setq-default c-basic-offset 2
                          c-set-style "java")))

(use-package ant
  :ensure t
  :defer t)

(use-package autodisass-java-bytecode ; Can disassemble .class files from within jars as well
  :ensure t
  :defer t)

(use-package jdee
  :ensure t
  :disabled t
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
                                  "/usr/lib/jvm/java-1.8.0-openjdk-amd64/jre/lib/resources.jar"))))

(use-package eclim
  :ensure t
  :init
  (use-package eclimd
    :config (setq eclimd-autostart t))
  (if (string-equal (system-name) "consensus.ices.utexas.edu")
      (setq eclim-eclipse-dirs "/h2/sbiswas/software/Eclipse/eclipse-oxygen-java/"
            eclim-executable "/h2/sbiswas/software/Eclipse/eclipse-oxygen-java/eclim")
    (setq eclim-eclipse-dirs "/home/sbiswas/software/Eclipse/eclipse-oxygen-java/"
          eclim-executable "/home/sbiswas/software/Eclipse/eclipse-oxygen-java/eclim"))
  (setq eclim-auto-save t)
  (add-hook 'java-mode-hook #'eclim-mode)
  :config
  (use-package company-emacs-eclim
    :ensure t
    :if (bound-and-true-p dotemacs-completion-in-buffer)
    :config (company-emacs-eclim-setup)))

(provide 'java-init)

;;; java-init.el ends here
