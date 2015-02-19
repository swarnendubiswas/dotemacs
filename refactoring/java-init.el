(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))
(autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
(add-hook 'java-mode-hook 'jtags-mode)
