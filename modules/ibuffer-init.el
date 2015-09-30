;;; ibuffer-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; IBuffer configurations.

;;; Code:

(use-package ibuffer
  :defer t
  :preface
  (defun dotemacs-ibuffer-group-buffers ()
    (ibuffer-switch-to-saved-filter-groups "Default"))

  :config
  (defalias 'list-buffers 'ibuffer) ; turn on ibuffer by default
  (setq ibuffer-expert t
        ;;ibuffer-shrink-to-minimum-size t
        ibuffer-always-show-last-buffer nil
        ibuffer-default-sorting-mode 'alphabetic
        ibuffer-use-header-line t
        ibuffer-display-summary t
        ibuffer-case-fold-search t ; ignore case when searching
        ibuffer-show-empty-filter-groups nil)

  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))

  (add-hook 'ibuffer-hook #'ibuffer-auto-mode)

  ;; Prefer ibuffer sorting based on projects via ibuffer-projectile
  ;; (add-hook 'ibuffer-hook #'dotemacs-ibuffer-group-buffers)
  ;; (add-hook 'ibuffer-hook
  ;;           (lambda ()
  ;;             (ibuffer-do-sort-by-recency)))

  ;; Group ibuffer list by tramp connection
  (use-package ibuffer-tramp
    :ensure t
    :defer t
    :config
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-tramp-set-filter-groups-by-tramp-connection)
                (ibuffer-do-sort-by-alphabetic))))

  ;; use ibuffer-vc to sort buffers by VC status
  (use-package ibuffer-vc
    :ensure t
    :defer t
    :config
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-vc-set-filter-groups-by-vc-root)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic)))))

  (use-package ibuffer-projectile ; group buffers by projectile project
    :ensure t
    :config
    (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups)
    (setq ibuffer-show-empty-filter-groups nil))

  (defhydra hydra-buffer-menu (:color pink)
    "Buffer menu commands"
    ("m" Buffer-menu-mark "mark")
    ("u" Buffer-menu-unmark "unmark")
    ("U" Buffer-menu-backup-unmark "backup-unmark")
    ("d" Buffer-menu-delete "delete")
    ("D" Buffer-menu-delete-backwards "delete-backwards")
    ("s" Buffer-menu-save "save")
    ("~" Buffer-menu-not-modified "not modified")
    ("x" Buffer-menu-execute "execute")
    ("b" Buffer-menu-bury "bury")
    ("g" revert-buffer "revert")
    ("T" Buffer-menu-toggle-files-only "toggle files only")
    ("O" Buffer-menu-multi-occur "multi occur" :color blue)
    ("I" Buffer-menu-isearch-buffers "isearch buffers" :color blue)
    ("R" Buffer-menu-isearch-buffers-regexp "isearch buffers regexp" :color blue)
    ("c" nil "cancel")
    ("v" Buffer-menu-select "select" :color blue)
    ("o" Buffer-menu-other-window "other-window" :color blue)
    ("q" quit-window "quit" :color blue))
  (bind-key "." 'hydra-buffer-menu/body ibuffer-mode-map)

  :bind (;;("C-x C-b" . ibuffer)
         ([remap list-buffers] . ibuffer)))

(provide 'ibuffer-init)

;;; ibuffer-init.el ends here
