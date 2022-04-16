;; "C-h b" lists all the bindings available in a buffer, "C-h m" shows the keybindings for the major
;; and the minor modes.
(use-package bind-key
  :straight t
  :functions bind-key--remove
  :bind ("C-c d k" . describe-personal-keybindings))

(use-package diminish
  :straight t)

(provide 'init-packages)
