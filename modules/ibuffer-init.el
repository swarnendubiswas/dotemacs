;;; ibuffer-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; IBuffer configurations.

;;; Code:

(use-package ibuffer
  :defer t
  :init (defalias 'list-buffers 'ibuffer) ; turn on ibuffer by default
  :config
  (progn
    (setq ibuffer-expert t
          ;;ibuffer-shrink-to-minimum-size t
          ibuffer-always-show-last-buffer nil
          ;;ibuffer-default-sorting-mode 'recency ; 'major-mode
          ;;ibuffer-sorting-mode 'recency
          ibuffer-use-header-line t
          ibuffer-show-empty-filter-groups nil)
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-auto-mode 1)))
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-do-sort-by-recency)))
    ;; (add-hook 'ibuffer-mode-hook
    ;;           (lambda ()
    ;;             (ibuffer-recompile-formats -1)))
    ;;(global-set-key (kbd "C-x C-b") 'ibuffer) ; use ibuffer for buffer list

    ;; Group ibuffer list by tramp connection
    (use-package ibuffer-tramp
      :load-path "lisp/"
      :config
      (eval-after-load 'ibuffer
        '(add-hook 'ibuffer-hook
                   (lambda ()
                     (ibuffer-tramp-set-filter-groups-by-tramp-connection)
                     (ibuffer-do-sort-by-alphabetic))))))
  :bind ("C-x C-b" . ibuffer))

;; use ibuffer-vc to sort buffers by VC status
(use-package ibuffer-vc
  :ensure t
  :defer t
  :config
  (eval-after-load 'ibuffer
    '(add-hook 'ibuffer-hook
               (lambda ()
                 (ibuffer-vc-set-filter-groups-by-vc-root)
                 (unless (eq ibuffer-sorting-mode 'alphabetic)
                   (ibuffer-do-sort-by-alphabetic))))))

(provide 'ibuffer-init)

;;; ibuffer-init.el ends here
