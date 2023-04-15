;;; init-config.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary: Helper file to "configure" my GNU Emacs configuration.

;;; Code:

(defcustom sb/extras-directory (expand-file-name "extras" user-emacs-directory)
  "Path for third-party packages and files."
  :type 'string
  :group 'sb/emacs)

;; A dark theme looks good on the TUI.
(defcustom sb/theme 'none
  "Specify which Emacs theme to use, unless we are using `circadian'."
  :type
  '
  (radio
    (const :tag "doom-one" doom-one)
    (const :tag "doom-nord" doom-nord)
    (const :tag "doom-molokai" doom-molokai)
    (const :tag "modus-operandi" modus-operandi)
    (const :tag "modus-vivendi" modus-vivendi)
    (const :tag "ef-trio-dark" ef-trio-dark)
    ;; Greenish tinge
    (const :tag "ef-bio" ef-bio)
    ;; Tries to mirror the default Emacs colors
    (const :tag "standard-light" standard-light)
    (const :tag "standard-dark" standard-dark)
    (const :tag "customized" sb/customized) ; Customizations over the default theme
    ;; No customization
    (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/modeline-theme 'powerline
  "Specify the mode-line theme to use."
  :type
  '
  (radio
    ;; Powerline theme for Nano looks great, and takes less space on the modeline. But it
    ;; does not show lsp status and flycheck information.
    (const :tag "powerline" powerline)
    (const :tag "doom-modeline" doom-modeline)
    (const :tag "awesome-tray" awesome-tray)
    (const :tag "spaceline" spaceline)
    ;; No customization
    (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/window-split 'vertical
  "Specify the direction in which the windows should be split.
This depends on the orientation of the display."
  :type
  '
  (radio
    ;; Split into two windows one above the other (`split-window-below')
    (const :tag "vertical" vertical)
    ;; Split into two side-by-side windows (`split-window-right')
    (const :tag "horizontal" horizontal))
  :group 'sb/emacs)

;; Large values make reading difficult when the window is split side-by-side, 100 is also actually a
;; stretch for smaller screens.
(defcustom sb/fill-column 100
  "Column beyond which lines should not extend."
  :type 'number
  :group 'sb/emacs)

(defcustom sb/delete-trailing-whitespace-p nil
  "Delete trailing whitespace.
Control whether the trailing whitespace should be deleted or not.
Sometimes we do not want to unnecessarily add differences due to
  whitespaces."
  :type 'boolean
  :group 'sb/emacs)

(defcustom sb/debug-init-file t
  "Enable features to debug errors and performance bottlenecks."
  :type 'boolean
  :group 'sb/emacs)

(defconst sb/user-home-directory (getenv "HOME")
  "User HOME directory.")

(defconst sb/user-config-directory
  (or (getenv "XDG_CONFIG_HOME") (expand-file-name ".config" sb/user-home-directory))
  "Path to user's local config store.")

(defconst sb/user-tmp-directory (expand-file-name "tmp" sb/user-home-directory)
  "User temp directory.
This location is used for temporary installations and files.")

;; `pyls' and `mspyls' are not actively maintained, and improvements to `pylsp' is slow.
;; Furthermore, candidate `pylsp' versions depend on the OS version, and capf sometimes does not
;; work consistently.

(defcustom sb/python-langserver 'pylsp
  "Choose the Python Language Server implementation."
  :type '(radio (const :tag "pylsp" pylsp) (const :tag "pyright" pyright) (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/minibuffer-completion 'vertico
  "Choose the framework to use for narrowing and selection."
  :type '(radio (const :tag "vertico" vertico) (const :tag "ivy" ivy) (const :tag "none" none))
  :group 'sb/emacs)

;; Corfu is easy to configure, integrates nicely with `orderless' and provides better completion for
;; elisp symbols. However, I prefer to use TUI Emacs, and `corfu-terminal-mode' has a rendering
;; problem for the completion popup at the right edges. The completion entries wrap around, and
;; sometimes messes up the completion. Corfu does not work well with LaTeX for me.

;; Company works better with TUI Emacs and has more extensive LaTeX support. `company-ispell' is
;; configurable, and we can set up a custom file containing completions with `company-dict'.

(defcustom sb/capf 'corfu
  "Choose the framework to use for completion at point."
  :type '(radio (const :tag "corfu" corfu) (const :tag "company" company) (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/corfu-icons 'kind-icons
  "Choose the provider for Corfu icons."
  :type
  '
  (radio
    (const :tag "kind-icon" kind-icon)
    (const :tag "kind-all-the-icons" kind-all-the-icons)
    (const :tag "none" none))
  :group 'sb/emacs)

;; Prefer "straight.el" over "package.el". A big advantage with "straight.el" is it is easy to
;; install packages from arbitrary sources like GitHub.
(defcustom sb/disable-package.el t
  "Disable package.el.
Prefer the straight.el package manager instead."
  :type 'boolean
  :group 'sb/emacs)

;; I do not find any difference in terms of features. However, packages like `centaur-tabs' only
;; support `projectile'.
(defcustom sb/project-handler 'projectile
  "Choose the handler for projects."
  :type '(radio (const :tag "project.el" project) (const :tag "projectile" projectile))
  :group 'sb/emacs)

;; `centaur-tabs' works more reliably for me.
(defcustom sb/tab-bar-handler 'centaur-tabs
  "Choose the handler for tabs."
  :type
  '
  (radio
    (const :tag "awesome-tab" awesome-tab)
    (const :tag "centaur-tabs" centaur-tabs)
    (const :tag "none" nil))
  :group 'sb/emacs)

;; Eglot does not allow multiple servers to connect to a major mode. which is a useful feature. For
;; example, I can use `texlab', `grammarly', and `lsp-ltex' together with LaTeX files. However,
;; Eglot has simpler setup and I expect it to receive significant improvements now that it is in
;; Emacs core.
(defcustom sb/lsp-provider 'lsp-mode
  "Choose between Lsp-mode and Eglot."
  :type '(radio (const :tag "lsp-mode" lsp-mode) (const :tag "eglot" eglot) (const :tag "none" none))
  :group 'sb/emacs)

;; Helper const variables

(defconst sb/EMACS27 (= emacs-major-version 27)
  "Non-nil if Emacs version is 27.")

(defconst sb/EMACS27+ (> emacs-major-version 26)
  "Non-nil if Emacs version is 27 and above.")

(defconst sb/EMACS28 (= emacs-major-version 28)
  "Non-nil if Emacs version is 28.")

(defconst sb/EMACS28+ (> emacs-major-version 27)
  "Non-nil if Emacs version is 28 and above.")

(defconst sb/IS-LINUX (eq system-type 'gnu/linux)
  "Non-nil if the OS is GNU/Linux.")

(defconst sb/IS-WINDOWS (eq system-type 'windows-nt)
  "Non-nil if the OS is Windows.")

(provide 'init-config)

;;; init-config.el ends here
