;;; indent-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup indentation in emacs.

;;; Code:

(setq-default fill-column 120
              standard-indent 2 ; set standard indent to 2 rather that 4
              tab-width 2
              indent-tabs-mode nil ; spaces instead of tabs by default
              )

(provide 'indent-init)

;;; indent-init.el ends here

