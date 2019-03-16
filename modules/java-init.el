;;; java-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure Java programming mode.

;;; Code:

(defvar dotemacs-extras-directory)

(add-hook 'java-mode-hook
          (lambda ()
            (setq-default c-basic-offset 2
                          c-set-style "java")))

(use-package ant
  :ensure t)

(use-package autodisass-java-bytecode ; Can disassemble .class files from within jars as well
  :ensure t)

(use-package jdee
  :ensure t
  :config
  (setq jdee-server-dir dotemacs-extras-directory
        jdee-complete-function 'jdee-complete-minibuf)
  (setq jdee-global-classpath '("/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/rt.jar"
                                "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/jce.jar"
                                "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/jsse.jar"
                                "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/charsets.jar"
                                "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/resources.jar"
                                "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/management.jar")))

(use-package eclim
  :ensure t
  :disabled t
  :init
  (use-package eclimd
    :config (setq eclimd-autostart t))
  (setq eclim-eclipse-dirs "/home/swarnendu/software/eclipse-java-2018-09-linux-gtk-x86_64/eclipse"
        eclim-executable "/home/swarnendu/software/Eclipse/eclipse-oxygen-java/eclim")
  (setq eclim-auto-save t)
  (add-hook 'java-mode-hook #'eclim-mode)
  :config
  (use-package company-emacs-eclim
    :ensure t
    :config (company-emacs-eclim-setup)))

(provide 'java-init)

;;; java-init.el ends here
