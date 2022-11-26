;;; init-tabs.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(use-package centaur-tabs
  :if (eq sb/tab-bar-handler 'centaur-tabs)
  :commands
  (centaur-tabs-group-by-projectile-project
   centaur-tabs-headline-match)
  :hook
  (emacs-startup-hook . centaur-tabs-mode)
  :bind*
  (("M-<right>" . centaur-tabs-forward-tab)
   ("M-<left>"  . centaur-tabs-backward-tab))
  :custom
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "•") ; Unicode Bullet (0x2022)
  (centaur-tabs-gray-out-icons t)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-show-new-tab-button nil)
  (centaur-tabs-enable-ido-completion nil)
  ;; Other styles like "wave" is not rendered on the terminal, and also does not work well with many
  ;; themes
  (centaur-tabs-style "bar")
  (centaur-tabs-set-bar 'under)
  (centaur-tabs-show-count nil) ; The feature is not useful.
  :config
  ;; ;; Unlike `awesome-tab', the icons do not blend well with all themes.
  ;; (let ((themes '("doom-one"
  ;;                 "doom-nord"
  ;;                 "doom-molokai")))
  ;;   (progn
  ;;     (if (-contains? themes (symbol-name sb/gui-theme))
  ;;         (setq centaur-tabs-set-icons t)
  ;;       (setq centaur-tabs-set-icons nil))))

  ;; (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project))

(use-package awesome-tab
  :preface
  (defun sb/awesome-tab-buffer-groups ()
    "`awesome-tab-buffer-groups' control buffers' group rules.
  Group awesome-tab with mode if buffer is derived from
  `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode'
  `magit-mode'. All buffer name start with * will group to
  \"Emacs\". Other buffer group by `awesome-tab-get-group-name'
  with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      (t
       (awesome-tab-get-group-name (current-buffer))))))
  :straight
  (:type git :host github :repo "manateelazycat/awesome-tab")
  :if (eq sb/tab-bar-handler 'awesome-tab)
  :hook
  (emacs-startup-hook . awesome-tab-mode)
  :bind
  (("M-<right>" . awesome-tab-forward-tab)
   ("M-<left>" . awesome-tab-backward-tab)
   ("M-]" . awesome-tab-ace-jump))
  :custom-face
  (awesome-tab-selected-face ((t (:inherit default :height 1.0))))
  (awesome-tab-unselected-face ((t (:inherit default :height 0.8))))
  :custom
  (awesome-tab-label-fixed-length 14)
  (awesome-tab-cycle-scope 'tabs)
  :config
  ;; The variable is declared with a `defvar', so modifying it with `:custom' will not work.
  (setq awesome-tab-buffer-groups-function #'sb/awesome-tab-buffer-groups))

(provide 'init-tabs)

;;; init-tabs.el ends here
