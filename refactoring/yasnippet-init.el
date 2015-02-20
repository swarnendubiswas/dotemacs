;;; yasnippet-init.el --- Part of emacs initialization

;;; Commentary:
;; Initialize yasnippet 

;;; Code:

(use-package yasnippet
             :ensure t
             :defer t
             :diminish yas-minor-mode
             :commands yas-global-mode
             :idle
             (progn
               (yas-global-mode 1)
               )
             )

(provide 'yasnippet-init)

;;; company-init.el ends here
