;;; ibuffer-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; IBuffer configurations.

;;; Code:

(use-package ibuffer
  :defer t
  :init (defalias 'list-buffers 'ibuffer) ; turn on ibuffer by default
  :config
  (setq ibuffer-expert t
        ;;ibuffer-shrink-to-minimum-size t
        ibuffer-always-show-last-buffer nil
        ibuffer-default-sorting-mode 'recency ; 'major-mode
        ibuffer-sorting-mode 'recency
        ibuffer-use-header-line t
        ibuffer-display-summary t
        ibuffer-show-empty-filter-groups nil)
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-do-sort-by-recency)))
  
  ;; Group ibuffer list by tramp connection
  (use-package ibuffer-tramp
    :load-path "lisp/"
    :config
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-tramp-set-filter-groups-by-tramp-connection)
                (ibuffer-do-sort-by-alphabetic))))

  ;; use ibuffer-vc to sort buffers by VC status
  (use-package ibuffer-vc
    :ensure t
    :config
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-vc-set-filter-groups-by-vc-root)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic)))))

  (use-package ibuffer-projectile
    :ensure t
    :config
    (add-hook 'ibuffer-mode-hook
              (lambda()
                (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-mode-name)
                  (ibuffer-do-sort-by-major-mode)))))
  
  :bind (;;([remap list-buffers] . ibuffer)
         ("C-x C-b" . ibuffer)))

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
(bind-key "C-c b" 'hydra-buffer-menu/body)

(provide 'ibuffer-init)

;;; ibuffer-init.el ends here
