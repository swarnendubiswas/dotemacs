;;; config-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup configuration variables.

;;; Code:

(defgroup dotemacs nil
  "Custom configuration for dotemacs."
  :group 'local)

(defcustom dotemacs-temp-directory (concat user-emacs-directory "tmp/")
  "Storage location for various configuration files."
  :group 'dotemacs)

(unless (file-exists-p dotemacs-temp-directory)
  (make-directory dotemacs-temp-directory))

(defcustom dotemacs-completion-in-buffer
  'auto-complete
  "Choose company or auto-complete for in-buffer completion."
  :type '(radio
          (const :tag "company" company)
          (const :tag "auto-complete" auto-complete))
  :group 'dotemacs)

(defcustom dotemacs-selection
  'ivy
  "Choose the framework to use for narrowing and selection."
  :type '(radio
          (const :tag "helm" helm)
          (const :tag "ido" ido)
          (const :tag "ivy" ivy)
          (const :tag "none" none)))

(defcustom dotemacs-ido-view-mode
  'grid
  "Specify which view to use for ido."
  :type '(radio
          (const :tag "vertical" vertical)
          (const :tag "grid" grid)
          (const :tag "default" default))
  :group 'dotemacs)

(defcustom dotemacs-theme
  'default
  "Specify which Emacs theme to use."
  :type '(radio
          (const :tag "leuven" leuven)
          (const :tag "professional" professional)
          (const :tag "eclipse" eclipse)
          (const :tag "default" default))
  :group 'dotemacs)

(defcustom dotemacs-mode-line-theme
  'spaceline
  "Specify the mode-line theme to use."
  :type '(radio
          (const :tag "powerline" powerline)
          (const :tag "smart-mode-line" sml)
          (const :tag "telephone-line" telephone-line)
          (const :tag "spaceline" spaceline)
          (const :tag "default" default))
  :group 'dotemacs)

(defcustom dotemacs-window-split
  'horizontal
  "Specify the direction in which the windows should be split.
This depends on the orientation of the display."
  :type '(radio
          (const :tag "vertical" vertical)
          (const :tag "horizontal" horizontal))
  :group 'dotemacs)

(defconst dotemacs-fill-column 120
  "Column beyond which lines should not extend.")

(defcustom dotemacs-fci-p
  nil
  "Control display of fci."
  :type    'boolean
  :group   'dotemacs)

(defcustom dotemacs-enable-whitespace-module-p
  t
  "Control whitespace module.
Control whether the whitespace module should be enabled or
disabled.  Sometimes we do not want to unnecessarily add
differences due to whitespaces."
  :type 'boolean
  :group 'dotemacs)

(provide 'config-init)

;;; config-init.el ends here
