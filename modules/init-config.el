(defgroup sb/emacs
  nil
  "Personal configuration for dotemacs."
  :group 'local)

(defcustom sb/extras-directory
  (expand-file-name "extras" user-emacs-directory)
  "Path for third-party packages and files."
  :type  'string
  :group 'sb/emacs)

(defcustom sb/gui-theme
  'doom-one
  "Specify which Emacs theme to use."
  :type  '(radio
           (const :tag "leuven"          leuven)
           (const :tag "zenburn"         zenburn)
           (const :tag "doom-one-light"  doom-one-light)
           (const :tag "doom-one"        doom-one)
           (const :tag "doom-nord"       doom-nord)
           (const :tag "doom-molokai"    doom-molokai)
           (const :tag "doom-gruvbox"    doom-gruvbox)
           (const :tag "monokai"         monokai)
           (const :tag "modus-operandi"  modus-operandi)
           (const :tag "modus-vivendi"   modus-vivendi)
           (const :tag "customized"      sb/customized) ; Customizations over the default theme
           (const :tag "none"            none))
  :group 'sb/emacs)

;; A dark theme looks good on the TUI
(defcustom sb/tui-theme
  'modus-vivendi
  "Specify which Emacs theme to use."
  :type  '(radio
           (const :tag "leuven"          leuven)
           (const :tag "zenburn"         zenburn)
           (const :tag "doom-one"        doom-one)
           (const :tag "doom-molokai"    doom-molokai)
           (const :tag "doom-gruvbox"    doom-gruvbox)
           (const :tag "doom-nord"       doom-nord)
           (const :tag "monokai"         monokai)
           (const :tag "modus-operandi"  modus-operandi)
           (const :tag "modus-vivendi"   modus-vivendi)
           (const :tag "customized"      sb/customized) ; Customizations over the default theme
           (const :tag "none"            none))
  :group 'sb/emacs)

(defcustom sb/modeline-theme
  'doom-modeline
  "Specify the mode-line theme to use."
  :type  '(radio
           (const :tag "powerline"       powerline)
           (const :tag "doom-modeline"   doom-modeline)
           (const :tag "awesome-tray"    awesome-tray)
           (const :tag "spaceline"       spaceline)
           (const :tag "moody"           moody)
           (const :tag "mini-modeline"   mini)
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

;; Large values make reading difficult when the window is split side-by-side
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

;; Use the snap installation of universal-ctags
(defcustom sb/ctags-path
  "/snap/bin/ctags"
  "Absolute path to Universal Ctags executable."
  :type  'string
  :group 'sb/emacs)

(defcustom sb/debug-init-file
  nil
  "Enable features to debug errors and performance bottlenecks."
  :type  'boolean
  :group 'sb/emacs)

(defconst sb/user-home
  (getenv "HOME")
  "User HOME directory.")

(defconst sb/user-tmp
  (expand-file-name "tmp" sb/user-home)
  "User temp directory.
This location is used for temporary installations and files.")

(defcustom sb/textlint-home
  (expand-file-name "textlint-workspace" sb/user-tmp)
  "Absolute path to textlint workspace."
  :type  'string
  :group 'sb/emacs)

;; `pyls' and `mspyls' are not actively maintained, and improvements to `py-lsp' is slow
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

(defcustom sb/capf
  'corfu
  "Choose the framework to use for completion at point.
Corfu does not support TUI, so we have to fallback on company."
  :type '(radio
          (const :tag "corfu" corfu)
          (const :tag "company" company))
  :group 'dotemacs)

(defconst sb/EMACS27    (= emacs-major-version 27))
(defconst sb/EMACS27+   (> emacs-major-version 26))
(defconst sb/EMACS28+   (> emacs-major-version 27))
(defconst sb/IS-LINUX   (eq system-type 'gnu/linux))
(defconst sb/IS-WINDOWS (eq system-type 'windows-nt))

(provide 'init-config)
