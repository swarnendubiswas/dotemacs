;;; config-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

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

;; use either auto-complete or company for auto completion support
;; (defconst use-company t
;;   "Choose company over auto-complete for completion.")
(defcustom dotemacs-completion
  'company
  "Choose company over auto-complete for completion."
  :type '(radio
          (const :tag "company" company)
          (const :tag "auto-complete" auto-complete))
  :group 'dotemacs)

;; use either helm or other packages that provide similar functionality
;; (defconst use-helm t
;;   "Prefer helm instead of ido and smex.  Helm provides similar functionalities.")
(defcustom dotemacs-helm-or-ido
  'ido
  "Prefer helm instead of ido and smex.  Helm provides similar functionalities."
  :type '(radio
          (const :tag "helm" helm)
          (const :tag "ido" ido))
  :group 'dotemacs)

;; choices: "leuven", "professional", "eclipse", otherwise default.
;; (defcustom use-theme "eclipse"
;;   "Specify which Emacs theme to use.")
(defcustom dotemacs-theme
  'eclipse
  "Specify which Emacs theme to use."
  :type '(radio
          (const :tag "leuven" leuven)
          (const :tag "professional" professional)
          (const :tag "eclipse" eclipse)
          (const :tag "default" default))
  :group 'dotemacs)

(defcustom dotemacs-window-split
  'vertical
  "Specify the direction in which the windows should be split."
  :type '(radio
          (const :tag "vertical" vertical)
          (const :tag "horizontal" horizontal))
  :group 'dotemacs)

(provide 'config-init)

;;; config-init.el ends here
