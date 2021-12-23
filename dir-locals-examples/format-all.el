(
 (nil . (
         (format-all-formatters . (
                                   ("Haskell" brittany)
                                   ("Cabal Config" cabal-fmt)
                                   ("YAML" prettier)
                                   ("Emacs Lisp" emacs-lisp)
                                   ("C++" clang-format)
                                   ("C" clang-format)
                                   ("Python" (black "--skip-string-normalization"))
                                   ("SQL" (sqlformat "-a" "--keywords" "capitalize"))
                                   ("JavaScript" prettier)
                                   ("Python" (yapf "--style" "google"))
                                   ))
         ))
 (haskell-mode . (
                  (fill-column . 100)
                  (format-all-mode . t)
                  ))
 (haskell-cable-mode . (
                        (format-all-mode . t)
                        ))
 (emacs-lisp-mode . (
                     (format-all-mode . t)
                     ))
 )
