(
 (nil . (
         (counsel-find-file-ignore-regexp . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|__pycache__\\|.aux$\\|.bbl$\\|.blg$\\|.cb$\\|.cb2$\\|.dvi$\\|.elc$\\|.fdb_latexmk$\\|.fls$\\|.jar$\\|.lof$\\|.lot$\\|.o$\\|.out$\\|.pdf$\\|.pyc$\\|.rel$\\|.rip$\\|.synctex$\\|.synctex.gz$\\|.tar.gz$\\|.toc$\\|TAGS\\|GPATH\\|GRTAGS\\|GTAGS\\|tramp\\|.metadata\\|.clangd\\|.recommenders\\|auto-save-list\\|eclipse.jdt.ls\\|session*\\|request\\|^workspace")
         (compile-command . "make")
         (compile-command . "gradle compileTestJava")
         (lsp-enabled-clients . (pyls msplys pyright jedi))
         (lsp-file-watch-ignored-directories . ("/\\.git$" "/\\.clangd$" "build" "built"))
         (projectile-enable-caching . t)
         (projectile-globally-ignored-files . ("MyBinaryFile")) ; Ignore file
         (projectile-project-compilation-cmd . "cd bin && cmake .. && cmake --build . && cd Debug && .\\shiolink.exe")
         (projectile-project-compilation-cmd . latexmk)
         (projectile-project-run-cmd . "cd bin\\Debug && .\\shiolink.exe && E:\\nanika\\ssp.exe")
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
         (eval . (flycheck-mode 0)) ; Enable/disable a minor mode
         (flycheck-disabled-checkers . '(emacs-lisp-checkdoc))
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
         ;; https://emacs.stackexchange.com/questions/42463/load-package-in-dir-locals
         (eval . (use-package smart-tabs
                   :ensure t
                   :config ()))
         (eval .
               (set
                (make-local-variable 'projectile-globally-ignored-files)
                (push "SOME-VALUE" projectile-globally-ignored-files)))
         ))

 (fundamental-mode . (
                      (eval . (minor-mode))
                      (eval . (when (string-match "\.prof$" (buffer-file-name)) (minor-mode)))
                      ))

 (nil
  (eval
   (lambda ()
     (when (string= (file-name-nondirectory buffer-file-name)
                    "wordlist")
       (text-mode)))))

 (emacs-lisp-mode . (
                     (no-byte-compile . t)
                     (show-trailing-whitespace . t)
                     ))

 (sh-mode . (
             (eval ignore-errors (require 'shfmt))
             (mode . shfmt-on-save)
             ))

 (python-mode . (
                 (flycheck-pylintrc . "setup.cfg")
                 (lsp-pyls-plugins-pylint-enabled . t)
                 (lsp-pyls-plugins-yapf-enabled . t)
                 (lsp-pyls-plugins-jedi-environment . "*ABSOLUTE PATH TO YOUR VIRTUALENV*")
                 (lsp-pyright-venv-path . ["./python-src"])
                 (python-shell-exec-path . "/usr/bin/python3")
                 (python-shell-interpreter . "/usr/bin/python3")
                 (pyvenv-activate . "/home/swarnendu/tmp/virtualenvs/2019-sharwari")
                 (eval . (let (
                               (paths
                                (vconcat (list
                                          (expand-file-name "python-src" (projectile-project-root))
                                          ))
                                )
                               )
                           (setq lsp-pyright-extra-paths paths
                                 lsp-python-ms-extra-paths paths)
                           ))
                 ))

 (diff-mode . ((mode . whitespace)))

 (c-mode . (
            (c-file-style . "BSD")
            (c-basic-offset . 2)
            (subdirs . nil) ; Apply C mode settings only to the current directory
            ))

 (c++-mode . (
              (flycheck-gcc-language-standard . "c++11")
              (flycheck-clang-language-standard . "c++11")
              (eval add-hook 'hack-local-variables-hook (lambda () (when (string= major-mode 'c++-mode) (lsp))))
              (eval add-hook 'before-save-hook #'clang-format-buffer nil t)
              ;; https://stackoverflow.com/questions/33063008/define-new-variable-in-dir-locals-el
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
                                           ))
                            (clangd-args (list
                                          "-j=2"
                                          "--background-index"
                                          "--clang-tidy"
                                          "--fallback-style=LLVM"
                                          "--pch-storage=memory"
                                          "--suggest-missing-includes"
                                          "--log=error"
                                          "--compile-commands-dir=build"
                                          ))
                            )
                        (setq-local company-clang-arguments clang-args
                                    flycheck-clang-args clang-args
                                    flycheck-gcc-args clang-args
                                    flycheck-gcc-include-path include-path
                                    flycheck-clang-include-path include-path
                                    lsp-clients-clangd-args clangd-args)
                        ))
              ))

 (latex-mode . (
                (flycheck-checker . tex-chktex)
                (TeX-master . "/home/swarnendu/prospar-workspace/dl-optimizations-draft/paper.tex")
                (reftex-default-bibliography . "/home/swarnendu/prospar-workspace/references/references.bib")
                (company-bibtex-bibliography . "/home/swarnendu/prospar-workspace/references/references.bib")
                (bibtex-completion-bibliography . "/home/swarnendu/prospar-workspace/references/references.bib")
                (lsp-latex-root-directory . "/home/swarnendu/bitbucket/iitkanpur/courses/autumn2020-cs335/endsem")
                ;; (eval . (add-hook 'before-save-hook #'sb/save-buffer-and-run-latexmk))
                ))

 (markdown-mode . (
                   ;; https://emacs.stackexchange.com/questions/41855/dir-locals-not-working
                   (mode . wc) ; if wc is a major mode
                   (eval . (wc-mode)) ; if wc is a major mode
                   (eval . (wc-mode 1)) ; if wc is a minor mode
                   (eval . (prettier-mode t))
                   ))

 (nxml-mode . (
               (nxml-child-indent . 2)
               ))

 (shell-mode . (
                (sh-basic-offset . 2)
                (sh-indentation . 2)
                ))
 )

(eval-after-load 'flycheck
  '(progn
     (setq flycheck-disabled-checkers '(javascript-jshint))
     (setq flycheck-checkers '(javascript-eslint))))


(
 ("src/"
  (nil . (
          (fill-column . 80)
          (eval add-hook 'after-save-hook '(lambda () (compile "make -kC .."))  nil t)
          ))
  )
 ("wwwroot/"
  (nil . (
          (auto-revert-mode . 1)
          ))
  )
 )
