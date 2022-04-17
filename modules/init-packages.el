;;; init-packages.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

;; Bootstrap `straight.el'

(setf straight-profiles `((nil . "straight.lockfile.el")))

(defvar bootstrap-version)
(setq straight-build-dir (format "build/%d%s%d"
                                 emacs-major-version
                                 version-separator
                                 emacs-minor-version))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      straight-disable-native-compile nil)

;; If we omit `:defer', `:hook', `:commands', or `:after', then the package is loaded immediately.
;; We do not need `:commands' with `:hook' or `:bind'. The setting `use-package-always-defer'
;; implies always load features lazily unless told otherwise. This implies we should use
;; `after-init' hook or `:init' instead of `:config', since otherwise packages may not be loaded. Be
;; careful about using `:after' and always deferring loading, because then we will need to specifiy
;; alternate ways of loading the package.
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading

;; Hooks in the `:hook' section run in reverse order. Example:
;; (use-package package-name
;;   :hook
;;   ((x-mode-hook . last)
;;    (x-mode-hook . second)
;;    (x-mode-hook . first)))

;; (when (bound-and-true-p sb/debug-init-file)
;;   (setq debug-on-error                 t
;;         debug-on-event                 'sigusr2
;;         garbage-collection-messages    t
;;         use-package-compute-statistics t ; Use "M-x use-package-report" to see results
;;         use-package-verbose            t
;;         use-package-expand-minimally   nil)
;;   (debug-on-entry 'projectile-remove-known-project))

;; (unless (bound-and-true-p sb/debug-init-file)
;;   (setq ;; use-package-always-defer       t
;;         ;; Avoid printing errors and warnings since the configuration is known to work
;;         ;; use-package-expand-minimally   t
;;         use-package-compute-statistics nil
;;         use-package-verbose            nil))

(setq use-package-enable-imenu-support t
      ;; Avoid manual installations whenever I modify package installations
      use-package-hook-name-suffix     nil)

(use-package gcmh ; Allow GC to happen after a period of idle time
  :straight t
  :diminish
  :commands (gcmh-mode gcmh-idle-garbage-collect)
  :hook (after-init-hook . gcmh-mode)
  :config
  (when (bound-and-true-p sb/debug-init-file)
    (setq gcmh-verbose t)))

;; ;; We can do `package-list-packages', then press `U' and `x'. The only thing missing from paradox
;; ;; is `paradox-upgrade-packages' as a single command.
;; (use-package package
;;   :if sb/EMACS27+
;;   :bind
;;   (("C-c d p" . package-quickstart-refresh)
;;    ("C-c d l" . package-list-packages)))

(use-package f
  :straight t
  :commands (f-exists? f-join f-dirname))

(use-package s
  :straight t
  :commands s-starts-with? s-ends-with?)

(use-package dash
  :straight t
  :commands (-contains? -tree-map))

(use-package no-littering
  :straight t
  :demand t)

(defcustom sb/custom-file
  (no-littering-expand-etc-file-name "custom.el")
  "File to write Emacs customizations."
  :type  'string
  :group 'sb/emacs)

;; NOTE: Make a symlink to "private.el" in "$HOME/.emacs.d/etc".
(defcustom sb/private-file
  (no-littering-expand-etc-file-name "private.el")
  "File to include private information."
  :type  'string
  :group 'sb/emacs)

;; ;; Asynchronously byte compile packages installed with `package.el'
;; (use-package async
;;   :straight t
;;   :functions async-bytecomp-package-mode
;;   :commands async-bytecomp-package-mode
;;   :init (async-bytecomp-package-mode 1))

;; "C-h b" lists all the bindings available in a buffer, "C-h m" shows the keybindings for the major
;; and the minor modes.
(use-package bind-key
  :straight t
  :functions bind-key--remove
  :bind ("C-c d k" . describe-personal-keybindings))

(use-package diminish
  :straight t)

;; Get PATH with "(getenv "PATH")". Set PATH with
;; "(setenv "PATH" (concat (getenv "PATH") ":/home/swarnendu/bin"))".
;; These are alternative ways.
;; (setq exec-path (append exec-path (expand-file-name "node_modules/.bin" sb/user-tmp)))
;; (add-to-list 'exec-path (expand-file-name "node_modules/.bin" sb/user-tmp))
(use-package exec-path-from-shell
  :straight t
  :defines exec-path-from-shell-check-startup-files
  :commands exec-path-from-shell-initialize
  :if (or (daemonp) (memq window-system '(x ns)))
  :init
  ;; "-i" is expensive but Tramp may be unable to find executables without the option
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "NODE_PATH" "JAVA_HOME" "PYTHONPATH"
                                         "LANG" "LC_CTYPE" "LC_ALL" "TERM"))
  (exec-path-from-shell-initialize))

(provide 'init-packages)

;;; init-packages.el ends here
