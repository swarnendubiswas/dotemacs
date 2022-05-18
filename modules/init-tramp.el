;;; init-tramp.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

;; Edit remote file: "/method:user@host#port:filename". Shortcut "/ssh::" will connect to default
;; "user@host#port".
;; Edit local file with sudo: "C-x C-f /sudo::/etc/hosts".
;; Open a remote file with ssh + sudo: "C-x C-f /ssh:host|sudo:root:/etc/passwd".

;; Multihop syntax: "C-x C-f /ssh:bird@bastion|ssh:you@remotehost:/path"
;; Multihop with sudo: "C-x C-f /ssh:you@remotehost|sudo:remotehost:/path/to/file"
;; Multihop with sudo with custom user: "C-x C-f
;; /ssh:you@remotehost|sudo:them@remotehost:/path/to/file"

;; Use bookmarks to speed up remote file access: upon visiting a location with TRAMP, save it as a
;; bookmark with `bookmark-set' ("C-x r m"). To revisit that bookmark, use `bookmark-jump' ("C-x r
;; b") or `bookmark-bmenu-list' ("C-x r l"). Rename the bookmarked location in `bookmark-bmenu-mode'
;; with `R'.
;; https://helpdeskheadesk.net/help-desk-head-desk/2021-05-19/

(defvar tramp-default-user)
(defvar tramp-default-remote-shell)
(defvar tramp-verbose)
(defvar tramp-remote-path)
(defvar tramp-ssh-controlmaster-options)
(defvar sb/minibuffer-completion)

(setq tramp-default-user user-login-name
      ;; Tramp uses SSH when connecting and when viewing a directory, but it will use SCP to copy
      ;; files which is faster than SSH.
      ;; tramp-default-method "ssh"
      tramp-default-remote-shell "/usr/bin/bash"
      remote-file-name-inhibit-cache nil ; Remote files are not updated outside of Tramp
      ;; Disable default options, reuse SSH connections by reading "~/.ssh/config" control master
      ;; settings
      ;; https://emacs.stackexchange.com/questions/22306/working-with-tramp-mode-on-slow-connection-emacs-does-network-trip-when-i-start
      ;; https://puppet.com/blog/speed-up-ssh-by-reusing-connections
      tramp-ssh-controlmaster-options ""
      tramp-verbose 1
      ;; Disable version control for remote files to improve performance
      vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                   vc-ignore-dir-regexp tramp-file-name-regexp))

(defalias 'exit-tramp 'tramp-cleanup-all-buffers)

;; Disable backup
(add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))

(with-eval-after-load "tramp"
  ;; Include this directory in $PATH on remote
  (add-to-list 'tramp-remote-path (expand-file-name ".local/bin" (getenv "HOME")))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; https://www.reddit.com/r/emacs/comments/ukyeb6/how_to_disable_emacs_from_trying_to_connect_to_a/
  (defun sb/intercept-tramp-send-command (vec command &rest args)
    (when (string-match "\\(.projectile\\|/git\\)" command)
      (debug)))
  ;; (advice-add 'tramp-send-command :before 'sb/intercept-tramp-send-command)
  )

;; https://www.gnu.org/software/tramp/
(setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))

(declare-function tramp-cleanup-connection "tramp")
(bind-key "C-S-q" #'tramp-cleanup-connection)

;; (declare-function sb/sshlist "private")

;; (progn
;;   (defun sb/ivy-tramp ()
;;     "Invoke remote hosts with ivy and tramp."
;;     (interactive)
;;     (counsel-find-file (ivy-read "Remote Tramp targets: " (sb/sshlist))))

;;   (bind-key "C-c d t" #'sb/ivy-tramp))

(use-package counsel-tramp
  :if (eq sb/minibuffer-completion 'ivy)
  :bind ("C-c d t" . counsel-tramp))

(when (eq sb/minibuffer-completion 'vertico)
  (progn
    (eval-when-compile
      (if (bound-and-true-p sb/disable-package.el)
          (use-package consult-tramp
            :straight (consult-tramp :type git :host github :repo "Ladicle/consult-tramp"))
        (use-package consult-tramp
          :ensure nil
          :load-path "extras")))

    (declare-function consult-tramp "consult-tramp")

    (unless (fboundp 'consult-tramp)
      (autoload #'consult-tramp "consult-tramp" nil t))

    (bind-keys :package consult-tramp
               ("C-c d t" . consult-tramp))))

;; TODO: SSH into Gcloud
;; https://gist.github.com/jackrusher/36c80a2fd6a8fe8ddf46bc7e408ae1f9
;; Make sure you have set your default project with:
;; "gcloud config set project <project-name>"
;; "C-x C-f /gcssh:compute-instance:/path/to/filename.clj"

;; LATER: Can we shorten long Tramp file names? This does not work with Tramp.
;; (add-to-list 'directory-abbrev-alist
;;              '("/ssh:swarnendu@vindhya.cse.iitk.ac.in:/data/swarnendu/" . "/vindhya/data/swarnendu/"))
;; (add-to-list 'directory-abbrev-alist
;;              '("/ssh:swarnendu@vindhya.cse.iitk.ac.in:/home/swarnendu/" . "/vindhya/home/swarnendu/"))

(provide 'init-tramp)

;;; init-tramp.el ends here