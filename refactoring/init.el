;;; init.el starts here
;; Swarnendu Biswas
;; Mon Feb  9 12:35:49 EST 2015

;; Notes: To evaluate an Sexp, just go to the end of the sexp and type \C-x \C-e, instead of evaluating the whole buffer
;; Init file shouldn't ideally contain calls to "load" or "require", since they cause eager loading and are expensive, a
;; cheaper alternative is to use "autoload".

;; Interesting quotes (inspired from http://www.mygooglest.com/fni/dot-emacs.html):
;;
;;  "Show me your ~/.emacs and I will tell you who you are." -- Bogdan Maryniuk
;;  "People talk about getting used to a new editor, but over time, it is precisely the opposite that should happen -
;;    the editor should get used to us." -- Vivek Haldar in "New frontiers in text editing".
;;  "Emacs is like a laser guided missile. It only has to be slightly mis-configured to ruin your whole day." -- Sean
;;    McGrath
;;  "Emacs outshines all other editing software in approximately the same way that the noonday sun does the stars. It
;;    is not just bigger and brighter; it simply makes everything else vanish." -- Neal Stephenson, "In the Beginning 
;;    was the Command Line"
;;  "Nearly everybody is convinced that every style but their own is ugly and unreadable. Leave out the "but their own"
;;    and they're probably right..." -- Jerry Coffin (on indentation)
;;  "The only real difficulties in programming are cache invalidation and naming things." -- Phil Karlton
;;  "Good code is its own best documentation. As you're about to add a comment, ask yourself, "How can I improve the
;;    code so that this comment isn't needed?" Improve the code and then document it to make it even clearer." -- Steve
;;    McConnell
;;  "What I don't understand is: why should you ever care how your editor looks, unless you're trying to win a
;;    screenshot competition? The primary factor in looking good should be the choice of a good font at a comfortable
;;    size, and a syntax coloring theme that you like. And that is not something specific to an editor. Editors like
;;    Emacs and vi have almost no UI! If Emacs is configured right, the only UI it has is the modeline and the
;;    minibuffer." -- Vivek Haldar in "New frontiers in text editing".

;;; Code:


;; customizing packages
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ))
(setq package-user-dir (expand-file-name "~/.emacs.d/elpa"))
(package-initialize)

;; set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; start customizing functionality

;; saveplace: remember cursor position in files
(setq-default save-place t)


;; ensure that a required set of packages are always installed
(require 'ensure-packages)
;; get a list of currently installed packages (excluding built in packages) with '\C-h v package-activated-list'
(setq ensure-packages
      '(aggressive-indent anzu async auctex-latexmk auctex auto-highlight-symbol auto-indent-mode bash-completion color-theme  ctags ctags-update dash discover-my-major display-theme duplicate-thing epl es-lib f fill-column-indicator fish-mode fixme-mode flex-isearch flx flycheck-color-mode-line flycheck-tip flycheck flymake flymake-shell flymake-easy flyparens ggtags git-rebase-mode git-commit-mode goto-last-change guide-key guide-key-tip pos-tip popwin highlight-indentation highlight-numbers hl-line+ hlinum hungry-delete icomplete+ idle-highlight idle-highlight-mode indent-guide jgraph-mode jtags let-alist  manage-minor-mode fringe-helper math-symbol-lists mic-paren mode-icons names nav org parent-mode pkg-info popup readline-complete rich-minority s sentence-highlight use-package writegood-mode  org-beautify-theme ibuffer-tramp json-mode)
      )
(ensure-packages-install-missing)


;; customize appearance

;;(highlight-changes-mode 1) ; not very useful usually


;; fontification
(global-font-lock-mode 1) ; turn on syntax coloring, on by default since Emacs 22
(setq font-lock-maximum-decoration t ; maximum fontification possible
      jit-lock-defer-time 0.10 ; improve scrolling speed with jit fontification
      font-lock-support-mode 'jit-lock-mode ; jit locking is better than fast-lock and lazy-lock
      jit-lock-stealth-time 10
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5
      )


;; Package specific


;; related to pairing of parentheses, brackets, etc.
(setq show-paren-delay 0
      show-paren-style 'mixed ; 'expression, 'parenthesis, 'mixed
      )
(when (fboundp 'show-paren-mode)
  (show-paren-mode 1) ; highlight matching parentheses when the point is on them
  (make-variable-buffer-local 'show-paren-mode))
;;(show-paren-mode 1) ; highlight matching parentheses when the point is on them
(setq-default flyparens-mode t) ; highlight/track mismatched parentheses

;; indentation
(electric-indent-mode -1) ; intelligent indentation, on by default from Emacs 24.4
;;(auto-indent-global-mode 1) ; auto-indentation minor mode
(use-package aggressive-indent
             :ensure t
             :defer t
             :idle (global-aggressive-indent-mode 1)
             )


;; indentation guides
;;(indent-guide-global-mode 1) ; doesn't seem to work well with company-mode and auto-complete-mode
;;(setq indent-guide-delay 0.1) ; show guide lines only in idle-time
(highlight-indentation-mode 1) 


;; ace-jump-buffer
;; leave out certain buffer based on file name patterns
;; http://scottfrazersblog.blogspot.com/2010/01/emacs-filtered-buffer-switching.html
(defvar my-bs-always-show-regexps '("\\*\\(scratch\\)\\*")
  "*Buffer regexps to always show when buffer switching.")
(defvar my-bs-never-show-regexps '("^\\s-" "^\\*" "TAGS$")
  "*Buffer regexps to never show when buffer switching.")
(defvar my-ido-ignore-dired-buffers nil
  "*If non-nil, buffer switching should ignore dired buffers.")
(defun my-bs-str-in-regexp-list (str regexp-list)
  "Return non-nil if str matches anything in regexp-list."
  (let ((case-fold-search nil))
    (catch 'done
      (dolist (regexp regexp-list)
        (when (string-match regexp str)
          (throw 'done t))))))
(defun my-bs-ignore-buffer (name)
  "Return non-nil if the named buffer should be ignored."
  (or (and (not (my-bs-str-in-regexp-list name my-bs-always-show-regexps))
           (my-bs-str-in-regexp-list name my-bs-never-show-regexps))
      (and my-ido-ignore-dired-buffers
           (save-excursion
             (set-buffer name)
             (equal major-mode 'dired-mode)))))
(setq bs-configurations
      '(("all" nil nil nil nil nil)
        ("files" nil nil nil (lambda (buf) (my-bs-ignore-buffer (buffer-name buf))) nil)))
(setq bs-cycle-configuration-name "files")
(setq-default ajb-bs-configuration "files")




;; whitespace
(setq-default indicate-empty-lines t
              show-trailing-whitespace t)
;; (add-hook 'before-save-hook 'whitespace-cleanup)
;;(setq whitespace-style '(face empty tabs lines-tail trailing))
;;(setq whitespace-style '(face empty tabs lines-tail))
;;(set-face-attribute 'whitespace-line nil
;;                    :background "red1"
;;                    :foreground "yellow"
;;                    :weight 'bold)
;;(global-whitespace-mode t)


;; uniquify
;;(setq uniquify-separator ":")
;; options: post-forward, reverse, forward
(setq uniquify-buffer-name-style 'post-forward-angle-brackets ; emacs 24.4 style ⁖ cat.png<dirName>
      uniquify-after-kill-buffer-p t)


;; spell check
(add-hook 'find-file-hooks 'turn-on-flyspell) 
(setq-default ispell-program-name "/usr/bin/aspell")
;; speed up aspell: ultra | fast | normal
(setq ispell-extra-args '("--sug-mode=normal"))


;; flycheck
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
(global-flycheck-mode 1)


;; rainbow mode
;;(rainbow-mode 1)
;;(rainbow-identifiers-mode 1)
;;(rainbow-delimiters-mode 1)

;; directory navigation
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-nav-49/")
;;(nav-mode) ; always start in navigation mode
;;(nav-disable-overeager-window-splitting)



(icomplete-mode 1) ; incremental minibuffer completion/suggestions
(eval-after-load "icomplete" '(progn (require 'icomplete+)))
(setq icomplete-prospects-height 2
      icomplete-compute-delay 0
      )
;;(icy-mode 1) ; icicles


;; save minibuffer histories across sessions
(setq savehist-additional-variables    
      '(kill-ring search-ring regexp-search-ring)    
      savehist-file "~/.emacs.d/savehist") 
(savehist-mode 1)


;; yasnippet
;;(yas-global-mode 1)
;;(yas-reload-all 1) ; this slows startup


;; specific major mode hooks

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ajb-bs-configuration "files")
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("b98666d27e294e536e61320317c1736e6614317c4bffcd542556b8c6826d45c3" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "87755ed88c91cd87ee37b666b82edeb382b3a3a6191078a56bec558ebf8a58d9" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "e35ef4f72931a774769da2b0c863e11d94e60a9ad97fb9734e8b28c7ee40f49b" "930a202ae41cb4417a89bc3a6f969ebb7fcea5ffa9df6e7313df4f7a2a631434" "acb039d6f2c41b3bd852b448351b2979f44ef488026c95dd5228d2f6da57f574" default)))
 '(diredp-hide-details-initially-flag nil t)
 '(display-time-mode t)
 '(scroll-bar-mode 1)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(vlf-application (quote dont-ask)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

;; temporary stuff

;; (add-to-list 'file-name-handler-alist '("\\.class$" . javap-handler))

;; (defun javap-handler (op &rest args)
;;   "Handle .class files by putting the output of javap in the buffer."
;;   (cond
;;    ((eq op 'get-file-buffer)
;;     (let ((file (car args)))
;;       (with-current-buffer (create-file-buffer file)
;;         (call-process "javap" nil (current-buffer) nil "-c -private"
;;                       "-classpath" (file-name-directory file)
;;                       (file-name-sans-extension
;;                        (file-name-nondirectory file)))
;;         (setq buffer-file-name file)
;;         (setq buffer-read-only t)
;;         (set-buffer-modified-p nil)
;;         (goto-char (point-min))
;;         (java-mode)
;;         (current-buffer))))
;;    ((javap-handler-real op args))))

;; (defun javap-handler-real (operation args)
;;   "Run the real handler without the javap handler installed."
;;   (let ((inhibit-file-name-handlers
;;          (cons 'javap-handler
;;                (and (eq inhibit-file-name-operation operation)
;;                     inhibit-file-name-handlers)))
;;         (inhibit-file-name-operation operation))
;;     (apply operation args)))


;;; init.el ends here
