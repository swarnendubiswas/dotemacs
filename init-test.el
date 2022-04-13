(require 'package)

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")        t)
  (add-to-list 'package-archives '("celpa" . "https://celpa.conao3.com/packages/") t)
  (add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/")           t))

(package-initialize)

(setq package-native-compile nil)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-defer       nil
      use-package-expand-minimally   nil
      use-package-always-ensure        t
      use-package-hook-name-suffix   nil)

(use-package bind-key)
(use-package diminish)
(use-package no-littering :demand t)

(setq package-quickstart t ; Populate one big autoloads file
      package-quickstart-file (no-littering-expand-var-file-name "package-quickstart.el"))

(when (boundp 'native-comp-deferred-compilation)
  (setq native-comp-deferred-compilation nil))

(defcustom sb/custom-file
  (no-littering-expand-etc-file-name "custom.el")
  "File to write Emacs customizations."
  :type  'string)

(setq custom-file sb/custom-file)

(let ((gc-cons-threshold most-positive-fixnum))
  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage)))

(use-package all-the-icons
  :preface
  ;; https://github.com/domtronn/all-the-icons.el/issues/120
  (defun sb/font-installed-p (font-name)
    (if (find-font (font-spec :name font-name))
        t
      nil))
  :init
  (unless (sb/font-installed-p "all-the-icons")
    (all-the-icons-install-fonts t)))

(use-package doom-themes
  :init (load-theme 'doom-one t)
  :config
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure all-the-icons
  :ensure t
  :init
  (when (not (sb/font-installed-p "all-the-icons"))
    (all-the-icons-install-fonts t))
  (doom-modeline-mode 1))

(set-face-attribute 'default nil :font "JetBrains Mono" :height 130)
