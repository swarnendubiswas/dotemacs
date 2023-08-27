;;; init.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defgroup sb/emacs nil
  "Personal configuration for GNU Emacs."
  :group 'local)

(defcustom sb/extras-directory (expand-file-name "extras" user-emacs-directory)
  "Path for third-party packages and files."
  :type 'string
  :group 'sb/emacs)

(defcustom sb/op-mode 'standalone
  "Specify the way you expect Emacs to be used."
  :type
  '
  (radio
    (const :tag "server" server)
    (const :tag "daemon" daemon)
    (const :tag "standalone" standalone))
  :group 'sb/emacs)

(defcustom sb/debug-init-file nil
  "Enable features to debug errors and performance bottlenecks."
  :type 'boolean
  :group 'sb/emacs)

;; Prefer "straight.el" over "package.el". "straight.el" makes it easy to install packages from
;; arbitrary sources like GitHub.
(defcustom sb/disable-package.el t
  "Disable package.el.
Prefer the straight.el package manager instead."
  :type 'boolean
  :group 'sb/emacs)

;; A dark theme looks good on the TUI.
(defcustom sb/theme 'modus-vivendi
  "Specify which Emacs theme to use, unless we are using `circadian'."
  :type
  '
  (radio
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
(defcustom sb/python-langserver 'pyright
  "Choose the Python Language Server implementation."
  :type '(radio (const :tag "pylsp" pylsp) (const :tag "pyright" pyright) (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/minibuffer-completion 'vertico
  "Choose the framework to use for narrowing and selection."
  :type '(radio (const :tag "vertico" vertico) (const :tag "ivy" ivy) (const :tag "none" none))
  :group 'sb/emacs)

;; Corfu is easy to configure, integrates nicely with `orderless', and provides better completion
;; for elisp symbols. However, I prefer to use TUI Emacs, and `corfu-terminal-mode' has a rendering
;; problem for the completion popup at the right edges. The completion entries wrap around, and
;; sometimes messes up the completion. Corfu does not work well with LaTeX for me. Company works
;; better with Windows and TUI Emacs, and has more extensive LaTeX support. `company-ispell' is
;; configurable, and we can set up a custom file containing completions with `company-dict'.
;; However, `company-ispell' does not keep prefix case when used as a grouped backend.
(defcustom sb/capf 'company
  "Choose the framework to use for completion at point."
  :type '(radio (const :tag "corfu" corfu) (const :tag "company" company) (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/corfu-icons 'nerd-icons
  "Choose the provider for Corfu icons."
  :type
  '
  (radio
    (const :tag "kind-icon" kind-icon)
    (const :tag "kind-all-the-icons" kind-all-the-icons)
    (const :tag "nerd-icons" nerd-icons)
    (const :tag "none" none))
  :group 'sb/emacs)

;; I do not find any difference in terms of the features I require. However, packages like
;; `centaur-tabs' only support `projectile'.
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

;; `all-the-icons' only supports GUI, while `nerd-icons' supports both GUI and TUI. Using icons
;; sometimes lead to visual misalignment in lists.
(defcustom sb/icons-provider 'nerd-icons
  "Choose the provider for icons."
  :type
  '
  (radio
    (const :tag "all-the-icons" all-the-icons)
    (const :tag "nerd-icons" nerd-icons)
    (const :tag "none" none))
  :group 'sb/emacs)

;; Eglot does not allow multiple servers to connect to a major mode. For example, I can use
;; `texlab', `grammarly', and `lsp-ltex' together with LaTeX files. Eglot also does not support
;; semantic tokens. However, configuring Eglot is simpler and I expect it to receive significant
;; improvements now that it is in the Emacs core.
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

(dolist (dir '("modules" "extras"))
  (push (expand-file-name dir user-emacs-directory) load-path))

;; Splitting the configuration across multiple files is much easier to maintain, and looks less
;; cluttered. The downside is that more files need to be loaded during startup, possibly affecting
;; startup performance.

(require 'init-packages)
(require 'init-core)
(require 'buffer-dired)

(cond
  ((eq sb/minibuffer-completion 'ivy)
    (require 'ivy-counsel))
  ((eq sb/minibuffer-completion 'vertico)
    (require 'vertico-consult)))

(require 'init-spell)
(require 'init-misc)
(require 'linters-formatters)

(require 'init-completion)
(cond
  ((eq sb/capf 'corfu)
    (require 'init-corfu))
  ((eq sb/capf 'company)
    (require 'init-company)))

;; I work a lot over SSH, and `lsp-mode' is poor over Tramp. The alternative I used was to use TUI
;; Emacs. Eglot works better than `lsp-mode' over Tramp, which allows me to continue using GUI
;; Emacs. However, Eglot does not support multiple servers for a major-mode. For example, it will be
;; nice to have TexLab and Grammarly with LaTeX files.
(require 'lsp-eglot)
(require 'init-languages)
(require 'init-functions)

;; Configure appearance-related settings at the end
(require 'init-ui)
(require 'init-keybindings)

(defvar sb/custom-file)
(defvar sb/private-file)

(setq custom-file sb/custom-file)

(let ((gc-cons-threshold most-positive-fixnum))
  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage))
  (when (file-exists-p sb/private-file)
    (load sb/private-file 'noerror 'nomessage)))

;; Mark safe variables

(put 'compilation-read-command 'safe-local-variable #'stringp)

;; (put 'bibtex-completion-bibliography          'safe-local-variable #'listp)
;; (put 'company-bibtex-bibliography             'safe-local-variable #'listp)
;; (put 'company-clang-arguments                 'safe-local-variable #'listp)
;; (put 'counsel-find-file-ignore-regexp         'safe-local-variable #'stringp)
;; (put 'flycheck-checker                        'safe-local-variable #'listp)
;; (put 'flycheck-clang-include-path             'safe-local-variable #'listp)
;; (put 'flycheck-gcc-include-path               'safe-local-variable #'listp)
;; (put 'flycheck-python-pylint-executable       'safe-local-variable #'stringp)
;; (put 'lsp-clients-clangd-args                 'safe-local-variable #'listp)
;; (put 'lsp-latex-root-directory                'safe-local-variable #'stringp)
;; (put 'lsp-pyright-extra-paths                 'safe-local-variable #'listp)
;; (put 'projectile-enable-caching               'safe-local-variable #'stringp)
;; (put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)
;; (put 'projectile-project-root                 'safe-local-variable #'stringp)
;; (put 'pyvenv-activate                         'safe-local-variable #'stringp)
;; (put 'reftex-default-bibliography             'safe-local-variable #'listp)
;; (put 'tags-table-list                         'safe-local-variable #'listp)

;; (use-package server
;;   :straight (:type built-in)
;;   :unless (string-equal "root" (getenv "USER")) ; Only start server if not root
;;   :hook
;;   (emacs-startup-hook . (lambda ()
;;                          (unless (and (fboundp 'server-running-p) (server-running-p))
;;                            (server-start)))))
;;   :config
;;   ;; Hide "When done with a buffer, type C-x 5" message
;;   (when (boundp 'server-client-instructions)
;;     (setq server-client-instructions nil)))

;; (when (eq sb/op-mode 'server)
;;   ;; Start server if not root user
;;   (unless (string-equal "root" (getenv "USER"))
;;     (when (and (fboundp 'server-running-p) (not (server-running-p)))
;;       (server-mode))))

;; (when (or (eq sb/op-mode 'server) (eq sb/op-mode 'daemon))
;;   (setq server-client-instructions nil))

;; https://blog.d46.us/advanced-emacs-startup/
(add-hook
  'emacs-startup-hook
  (lambda ()
    (if (bound-and-true-p sb/disable-package.el)
      (let ((gc-time (float-time gc-elapsed)))
        (message "Emacs ready (init time = %s, gc time = %.2fs, gc count = %d)."
          (emacs-init-time)
          gc-time
          gcs-done))
      (let
        (
          (packages (length package-activated-list))
          (gc-time (float-time gc-elapsed)))
        (message "Emacs ready (init time = %s, packages = %d, gc time = %.2fs, gc count = %d)."
          (emacs-init-time)
          packages
          gc-time
          gcs-done)))))

;;; init.el ends here
