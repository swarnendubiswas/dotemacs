;;; ibuffer-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; IBuffer configurations.

;;; Code:

(use-package ibuffer
  :preface
  (defun dotemacs--ibuffer-group-buffers ()
    (ibuffer-switch-to-saved-filter-groups "Default"))
  :commands ibuffer
  :config
  (defalias 'list-buffers 'ibuffer) ; Turn on ibuffer by default
  (setq ibuffer-expert t
        ibuffer-always-show-last-buffer nil
        ibuffer-default-sorting-mode 'alphabetic ; Options: major-mode
        ibuffer-use-header-line t
        ibuffer-display-summary t
        ibuffer-case-fold-search t ; Ignore case when searching
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
  :bind ([remap list-buffers] . ibuffer))

(use-package ibuffer-tramp ; Group ibuffer list by tramp connection
  :ensure t
  :disabled t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-tramp-set-filter-groups-by-tramp-connection)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-vc ; Sort buffers by VC status
  :ensure t
  :disabled t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-projectile ; Group buffers by projectile project
  :ensure t
  :preface
  (defun dotemacs--ibuffer-customization ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      ;; First do alphabetic sort, then do major-mode sort
      (ibuffer-do-sort-by-alphabetic)
      (ibuffer-do-sort-by-major-mode)))
  :after ibuffer
  :config
  ;; (add-hook 'ibuffer-hook #'dotemacs--ibuffer-customization)
  (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

(provide 'ibuffer-init)

;;; ibuffer-init.el ends here
