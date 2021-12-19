((nil
  (format-all-formatters
   ("JavaScript" prettier)
   ("Python" (yapf "--style" "google")))))

((nil . ((format-all-formatters . (("Haskell" brittany)
                                   ("Cabal Config" cabal-fmt)
                                   ("YAML" prettier)
                                   ("Emacs Lisp" emacs-lisp)))))
 (haskell-mode . ((fill-column . 100)
                  (format-all-mode . t)))
 (haskell-cable-mode . ((format-all-mode . t)))
 (emacs-lisp-mode . ((format-all-mode . t))))
