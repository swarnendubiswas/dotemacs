;;; config-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Set configuration variables.

;;; Code:

(defgroup dotemacs nil
  "Custom configuration for dotemacs."
  :group 'local)

(defcustom dotemacs-temp-directory (concat user-emacs-directory "tmp/")
  "Storage location for various configuration files."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-extras-directory (concat user-emacs-directory "extras/")
  "Directory listing third-party packages and files."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-modules-directory (concat user-emacs-directory "modules/")
  "Directory containing setup files for customized configuration."
  :type 'string
  :group 'dotemacs)

(unless (file-exists-p dotemacs-temp-directory)
  (make-directory dotemacs-temp-directory))

(defcustom dotemacs-emacs-custom-file (concat dotemacs-temp-directory "custom.el")
  "File to write Emacs customizations."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-completion-in-buffer
  t
  "Use company for in-buffer completion."
  :type 'boolean
  :group 'dotemacs)

(defcustom dotemacs-theme
  'default
  "Specify which Emacs theme to use."
  :type '(radio
          (const :tag "leuven" leuven)
          (const :tag "professional" professional)
          (const :tag "eclipse" eclipse)
          (const :tag "spacemacs-light" spacemacs-light)
          (const :tag "zenburn" zenburn)
          (const :tag "solarized-light" solarized-light)
          (const :tag "solarized-dark" solarized-dark)
          (const :tag "tangotango" tangotango)
          (const :tag "default" default))
  :group 'dotemacs)

(defcustom dotemacs-mode-line-theme
  'default
  "Specify the mode-line theme to use."
  :type '(radio
          (const :tag "powerline" powerline)
          (const :tag "smart-mode-line" sml)
          (const :tag "spaceline" spaceline)
          (const :tag "default" default))
  :group 'dotemacs)

(defcustom dotemacs-window-split
  'vertical
  "Specify the direction in which the windows should be split.
This depends on the orientation of the display."
  :type '(radio
          (const :tag "vertical" vertical)
          (const :tag "horizontal" horizontal))
  :group 'dotemacs)

(defconst dotemacs-fill-column 120
  "Column beyond which lines should not extend.")

(defcustom dotemacs-use-ignoramus-p
  nil
  "Should the ignoramus package be used?
The package controls ignoring boring file expressions."
  :type 'boolean
  :group 'dotemacs)

(defcustom dotemacs-use-marmalade-repo-p
  nil
  "Should the marmalade repo be used?
The repo often does not work and Emacs fails to connect to it."
  :type 'boolean
  :group 'dotemacs)

(defcustom dotemacs-use-ecb
  nil
  "Should the ECB package be activated?
If yes, then we disable some other packages, like popwin and which-key."
  :type 'boolean
  :group 'dotemacs)

(defcustom dotemacs-cc-tags
  'gtags
  "Choose whether to use gtags or rtags for C/C++ programming."
  :type '(radio
          (const :tag "gtags" gtags)
          (const :tag "rtags" rtags)
          (const :tag "none" none))
  :group 'dotemacs)

(provide 'config-init)

;;; config-init.el ends here
