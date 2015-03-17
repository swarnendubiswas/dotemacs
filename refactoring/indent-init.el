;;; indent-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup indentation in emacs.

;;; Code:

(setq-default fill-column 120
              standard-indent 2 ; set standard indent to 2 rather that 4
              tab-width 2
              indent-tabs-mode nil ; spaces instead of tabs by default
              )

(use-package aggresive-indent
             :ensure t
             :defer t
             :idle (global-aggressive-indent-mode 1)
             )

(use-package auto-indent-mode
             :ensure t
             :defer t
             )

(use-package highlight-indentation
             :ensure t
             :defer t
             )

(use-package indent-guide
             :ensure t
             :defer t
             )

;; indentation
(electric-indent-mode -1) ; intelligent indentation, on by default from Emacs 24.4
;;(auto-indent-global-mode 1) ; auto-indentation minor mode

;; indentation guides
;;(indent-guide-global-mode 1) ; doesn't seem to work well with company-mode and auto-complete-mode
;;(setq indent-guide-delay 0.1) ; show guide lines only in idle-time
(highlight-indentation-mode 1) 

(provide 'indent-init)

;;; indent-init.el ends here
