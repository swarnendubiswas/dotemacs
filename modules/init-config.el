;;; init-config.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defcustom sb/extras-directory
  (expand-file-name "extras" user-emacs-directory)
  "Path for third-party packages and files."
  :type  'string
  :group 'sb/emacs)

(defcustom sb/gui-theme
  'modus-operandi
  "Specify which Emacs theme to use."
  :type  '(radio
           (const :tag "leuven"            leuven)
           (const :tag "zenburn"           zenburn)
           (const :tag "doom-one"          doom-one)
           (const :tag "doom-nord"         doom-nord)
           (const :tag "doom-molokai"      doom-molokai)
           (const :tag "doom-gruvbox"      doom-gruvbox)
           (const :tag "monokai"           monokai)
           (const :tag "modus-operandi"    modus-operandi)
           (const :tag "modus-vivendi"     modus-vivendi)
           ;; Does not pair brackets
           (const :tag "nano-dark"         nano-dark)
           ;; Company popups cannot be distinguished
           (const :tag "lambda-dark-faded" lambda-dark-faded)
           (const :tag "customized"        sb/customized) ; Customizations over the default theme
           ;; No customization
           (const :tag "none"              none))
  :group 'sb/emacs)

;; A dark theme looks good on the TUI but I now feel `modus-operandi' has better contrast than
;; `modus-vivendi'.
(defcustom sb/tui-theme
  'modus-vivendi
  "Specify which Emacs theme to use."
  :type  '(radio
           (const :tag "leuven"            leuven)
           (const :tag "zenburn"           zenburn)
           (const :tag "doom-one"          doom-one)
           (const :tag "doom-nord"         doom-nord)
           (const :tag "doom-molokai"      doom-molokai)
           (const :tag "doom-gruvbox"      doom-gruvbox)
           (const :tag "monokai"           monokai)
           (const :tag "modus-operandi"    modus-operandi)
           (const :tag "modus-vivendi"     modus-vivendi)
           (const :tag "nano-dark"         nano-dark)
           (const :tag "lambda-dark"       lambda-dark)
           (const :tag "lambda-dark-faded" lambda-dark-faded)
           (const :tag "customized"        sb/customized) ; Customizations over the default theme
           ;; No customization
           (const :tag "none"              none))
  :group 'sb/emacs)

(defcustom sb/modeline-theme
  'powerline
  "Specify the mode-line theme to use."
  :type  '(radio
           (const :tag "powerline"       powerline)
           (const :tag "doom-modeline"   doom-modeline)
           (const :tag "awesome-tray"    awesome-tray)
           (const :tag "spaceline"       spaceline)
           (const :tag "moody"           moody)
           (const :tag "mini-modeline"   mini)
           (const :tag "airline"         airline)
           (const :tag "telephone-line"  telephone)
           (const :tag "nano"            nano)
           (const :tag "lambda-line"     lambda-line)
           ;; No customization
           (const :tag "none"            none))
  :group 'sb/emacs)

(defcustom sb/window-split
  'vertical
  "Specify the direction in which the windows should be split.
This depends on the orientation of the display."
  :type  '(radio
           ;; Split into two windows one above the other (`split-window-below')
           (const :tag "vertical"   vertical)
           ;; Split into two side-by-side windows (`split-window-right')
           (const :tag "horizontal" horizontal))
  :group 'sb/emacs)

;; Large values make reading difficult when the window is split side-by-side, 100 is also actually a
;; stretch for smaller screens.
(defcustom sb/fill-column
  100
  "Column beyond which lines should not extend."
  :type  'number
  :group 'sb/emacs)

(defcustom sb/delete-trailing-whitespace-p
  nil
  "Delete trailing whitespace.
Control whether the trailing whitespace should be deleted or not.
Sometimes we do not want to unnecessarily add differences due to
  whitespaces."
  :type  'boolean
  :group 'sb/emacs)

;; We can use the snap installation of "universal-ctags", but snap packages have poor performance. A
;; better alternative is to build and install "ctags" locally. Check "setup-emacs.sh" for
;; installation instructions.
(defcustom sb/ctags-path
  "/usr/local/bin/ctags"
  "Absolute path to Universal Ctags executable."
  :type  'string
  :group 'sb/emacs)

(defcustom sb/debug-init-file
  nil
  "Enable features to debug errors and performance bottlenecks."
  :type  'boolean
  :group 'sb/emacs)

(defconst sb/user-home-directory
  (getenv "HOME")
  "User HOME directory.")

(defconst sb/user-config-directory
  (or (getenv "XDG_CONFIG_HOME")
      (expand-file-name ".config" sb/user-home-directory))
  "Path to user's local config store.")

(defconst sb/user-cache-directory
  (or (getenv "XDG_CACHE_HOME")
      (expand-file-name ".cache" sb/user-home-directory))
  "Path to user's local cache store.")

(defconst sb/user-tmp-directory
  (expand-file-name "tmp" sb/user-home-directory)
  "User temp directory.
This location is used for temporary installations and files.")

(defcustom sb/textlint-directory
  (expand-file-name "textlint-workspace" sb/user-tmp-directory)
  "Absolute path to textlint workspace."
  :type  'string
  :group 'sb/emacs)

;; `pyls' and `mspyls' are not actively maintained, and improvements to `pylsp' is slow.
;; Furthermore, candidate `pylsp' versions depend on the OS version, and capf sometimes does not
;; work consistently.
(defcustom sb/python-langserver
  'pylsp
  "Choose the Python Language Server implementation."
  :type  '(radio
           (const :tag "pylsp"   pylsp)
           (const :tag "pyright" pyright)
           (const :tag "none"    none))
  :group 'sb/emacs)

(defcustom sb/minibuffer-completion
  'vertico
  "Choose the framework to use for narrowing and selection."
  :type '(radio
          (const :tag "vertico" vertico)
          (const :tag "ivy" ivy))
  :group 'dotemacs)

;; Corfu seems to again not work with LSP with langservers
(defcustom sb/capf
  'corfu
  "Choose the framework to use for completion at point."
  :type '(radio
          (const :tag "corfu" corfu)
          (const :tag "company" company))
  :group 'dotemacs)

(defcustom sb/disable-package.el
  t
  "Disable package.el.
Prefer the straight.el package manager instead."
  :type 'boolean
  :group 'dotemacs)

(defcustom sb/project-handler
  'project
  "Choose the handler for projects."
  :type '(radio
          (const :tag "project.el" project)
          (const :tag "projectile" projectile))
  :group 'dotemacs)

(defcustom sb/tab-bar-handler
  'awesome-tab
  "Choose the handler for tabs."
  :type '(radio
          (const :tag "awesome-tab" awesome-tab)
          (const :tag "centaur-tabs" centaur-tabs)
          (const :tag "none" nil)))

(defconst sb/EMACS27    (= emacs-major-version 27)
  "Non-nil if Emacs version is 27.")
(defconst sb/EMACS27+   (> emacs-major-version 26)
  "Non-nil if Emacs version is 27 and above.")
(defconst sb/EMACS28+   (> emacs-major-version 27)
  "Non-nil if Emacs version is 28 and above.")

(defconst sb/IS-LINUX   (eq system-type 'gnu/linux)
  "Non-nil if the OS is GNU/Linux.")
(defconst sb/IS-WINDOWS (eq system-type 'windows-nt)
  "Non-nil if the OS is Windows.")

(provide 'init-config)

;;; init-config.el ends here
