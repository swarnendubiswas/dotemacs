(
 (nil . (
         . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|__pycache__\\|.aux$\\|.bbl$\\|.blg$\\|.cb$\\|.cb2$\\|.dvi$\\|.elc$\\|.fdb_latexmk$\\|.fls$\\|.jar$\\|.lof$\\|.lot$\\|.o$\\|.out$\\|.pdf$\\|.pyc$\\|.rel$\\|.rip$\\|.synctex$\\|.synctex.gz$\\|.tar.gz$\\|.toc$\\|TAGS\\|GPATH\\|GRTAGS\\|GTAGS\\|tramp\\|.metadata\\|.clangd\\|.recommenders\\|auto-save-list\\|eclipse.jdt.ls\\|session*\\|request\\|^workspace")
      (counsel-find-file-ignore-regexp
       (lsp-file-watch-ignored . ("/\\.git$" "/\\.clangd$" "build" "built"))
       (projectile-project-compilation-cmd . "cd bin && cmake .. && cmake --build . && cd Debug && .\\shiolink.exe")
       (projectile-project-compilation-cmd . latexmk)
       (projectile-project-run-cmd . "cd bin\\Debug && .\\shiolink.exe && E:\\nanika\\ssp.exe")
       (projectile-enable-caching . t) ; Enable caching
       (projectile-globally-ignored-files . ("MyBinaryFile")) ; Ignore file
       (projectile-project-name . "your-project-name-here")
       (show-trailing-whitespace . t)
       (eval . (progn BODY))
       (eval . (setq projectile-project-root
                     (locate-dominating-file buffer-file-name
                                             ".dir-locals.el")))
       (eval . (setq-local counsel-etags-project-root (projectile-project-root)
                           tags-table-list (list (projectile-project-root))))
       (eval . (add-hook 'before-save-hook #'delete-trailing-whitespace))
       (eval . (setq compile-command
                     `(format "cd %s && make"
                              (locate-dominating-file buffer-file-name
                                                      ".dir-locals.el"))))
       (eval . (flycheck-mode 0))
       (eval . (progn
                 (defun my-project-specific-function ()
                   ;; ...
                   )))
       ;; https://stackoverflow.com/questions/16237506/wrong-type-argument-listp-eval-after-load-in-dir-locals-el
       (eval . (progn
                 (require 'grep)
                 (add-to-list
                  (make-local-variable 'grep-find-ignored-directories)
                  "blur")))
       ))

 (nil
  (eval
   (lambda ()
     (when (string= (file-name-nondirectory buffer-file-name)
                    "wordlist")
       (text-mode)))))

 (emacs-lisp-mode . (
                     (no-byte-compile . t)
                     ))

 (sh-mode . (
             (eval ignore-errors (require 'shfmt))
             (mode . shfmt-on-save)
             ))

 (python-mode . (
                 (flycheck-pylintrc . "setup.cfg")
                 (lsp-pyls-plugins-pylint-enabled . t)
                 (lsp-pyls-plugins-yapf-enabled . t)
                 (lsp-python-ms-extra-paths . ["/home/swarnendu/prospar-workspace/data-race-framework/src"])
                 (python-shell-exec-path . "/usr/bin/python3")
                 (python-shell-interpreter . "/usr/bin/python3")
                 (pyvenv-activate . "/home/swarnendu/tmp/virtualenvs/2019-sharwari")))

 (c-mode . ((c-file-style . "BSD")))

 (c++-mode . (
              (flycheck-gcc-language-standard . "c++11")
              (flycheck-clang-language-standard . "c++11")
              (eval add-hook 'hack-local-variables-hook (lambda () (when (string= major-mode 'c++-mode) (lsp))))
              (eval . (let (
                            (clang-args (list
                                         "-std=c++11"
                                         (concat "-I" (expand-file-name "tdebug-llvm/llvm/include" (projectile-project-root)))
                                         (concat "-I" (expand-file-name "tdebug-llvm/clang/include" (projectile-project-root)))
                                         (concat "-I" (expand-file-name "tdebug-lib/include" (projectile-project-root)))
                                         (concat "-I" (expand-file-name "spd3-lib/include" (projectile-project-root)))
                                         (concat "-I" (expand-file-name "fasttrack/include" (projectile-project-root)))
                                         (concat "-I" (expand-file-name "newfasttrack/include" (projectile-project-root)))
                                         (concat "-I" (expand-file-name "new_algo/include" (projectile-project-root)))
                                         ))
                            (include-path (list
                                           (expand-file-name "tdebug-llvm/llvm/include" (projectile-project-root))
                                           (expand-file-name "tdebug-llvm/clang/include" (projectile-project-root))
                                           (expand-file-name "tdebug-lib/include" (projectile-project-root))
                                           (expand-file-name "spd3-lib/include" (projectile-project-root))
                                           (expand-file-name "fasttrack/include" (projectile-project-root))
                                           (expand-file-name "newfasttrack/include" (projectile-project-root))
                                           (expand-file-name "new_algo/include" (projectile-project-root))
                                           )))
                        (setq-local company-clang-arguments clang-args
                                    flycheck-clang-args clang-args
                                    flycheck-gcc-args clang-args
                                    flycheck-gcc-include-path include-path
                                    flycheck-clang-include-path include-path)
                        ))
              ))

 (latex-mode . (
                (flycheck-checker . tex-chktex)
                (TeX-master . "/home/swarnendu/plass-workspace/arc-paper/paper.tex")
                (reftex-default-bibliography . "/home/swarnendu/plass-workspace/bib/plass.bib")
                (company-bibtex-bibliography . "/home/swarnendu/plass-workspace/bib/plass.bib")
                (bibtex-completion-bibliography . "/home/swarnendu/plass-workspace/bib/plass.bib")
                (eval . (add-hook 'before-save-hook #'sb/save-buffer-and-run-latexmk)))
             )

 ((markdown-mode . ((eval . (prettier-mode t)))))

 )
