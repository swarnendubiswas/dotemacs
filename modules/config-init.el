;;; config-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup configuration variables.

;;; Code:

(defgroup dotemacs nil
  "Custom configuration for dotemacs."
  :group 'local)

(defcustom dotemacs--temp-directory (concat user-emacs-directory "tmp/")
  "Storage location for various configuration files."
  :group 'dotemacs)

(unless (file-exists-p dotemacs--temp-directory)
  (make-directory dotemacs--temp-directory))

;; use either auto-complete or company for auto completion support
(defcustom dotemacs--completion
  'company
  "Choose company over auto-complete for completion."
  :type '(radio
          (const :tag "company" company)
          (const :tag "auto-complete" auto-complete))
  :group 'dotemacs)

;; use either helm or other packages that provide similar functionality
(defcustom dotemacs--helm-or-ido
  'both
  "Prefer helm instead of ido and smex, or use both."
  :type '(radio
          (const :tag "helm" helm)
          (const :tag "ido" ido)
          (const :tag "both" both))
  :group 'dotemacs)

;; choices: leuven, professional, eclipse, default.
(defcustom dotemacs--theme
  'default
  "Specify which Emacs theme to use."
  :type '(radio
          (const :tag "leuven" leuven)
          (const :tag "professional" professional)
          (const :tag "eclipse" eclipse)
          (const :tag "default" default))
  :group 'dotemacs)

(defcustom dotemacs--window-split
  'vertical
  "Specify the direction in which the windows should be split."
  :type '(radio
          (const :tag "vertical" vertical)
          (const :tag "horizontal" horizontal))
  :group 'dotemacs)

(defcustom dotemacs--fci-p
  nil
  "Control display of fci."
  :type    'boolean
  :group   'dotemacs)

(defcustom dotemacs--enable-whitespace-module
  t
  "Control whether the whitespace module should be enabled or
disabled. Sometimes we do not want to unnecessarily add
differences due to whitespaces."
  :type 'boolean
  :group 'dotemacs)

(provide 'config-init)

;;; config-init.el ends here
