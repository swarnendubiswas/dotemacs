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


;; automatically load abbreviations table
(setq-default abbrev-file-name "~/.emacs.d/abbrev_defs"
              abbrev-mode t)
(setq save-abbrevs nil ; do not ask to save new abbrevs when quitting
      dabbrev-case-replace nil ; preserve case when expanding
      )
;;(quietly-read-abbrev-file)


;; ensure that a required set of packages are always installed
(require 'ensure-packages)
;; get a list of currently installed packages (excluding built in packages) with '\C-h v package-activated-list'
(setq ensure-packages
      '(ace-jump-buffer ace-jump-mode achievements aggressive-indent anzu async auctex-latexmk auctex auto-highlight-symbol auto-indent-mode auto-compile autodisass-java-bytecode bash-completion bibtex-utils color-theme  ctags ctags-update dash dired+ dired-details dired-details+ dired-rainbow dired-hacks-utils discover-my-major display-theme duplicate-thing epl es-lib f fill-column-indicator fish-mode fixme-mode flex-autopair flex-isearch flx flycheck-color-mode-line flycheck-tip flycheck flymake flymake-shell flymake-easy flyparens ggtags git-rebase-mode git-commit-mode goto-last-change guide-key guide-key-tip pos-tip popwin highlight-indentation highlight-numbers hl-line+ hlinum hungry-delete icomplete+ idle-highlight idle-highlight-mode indent-guide javap-mode jgraph-mode jtags latex-extra latex-pretty-symbols latex-preview-pane let-alist leuven-theme magic-latex-buffer manage-minor-mode fringe-helper math-symbol-lists mic-paren mode-icons names nav org parent-mode pkg-info popup powerline professional-theme rainbow-delimiters rainbow-identifiers rainbow-mode readline-complete rich-minority s sentence-highlight smart-tab smart-tabs-mode smex smooth-scroll tabbar use-package undo-tree vlf writegood-mode yasnippet org-beautify-theme ibuffer-tramp json-mode)
      )
(ensure-packages-install-missing)


;; enable tabbar minor mode
(setq tabbar-use-images nil) ; speed up by not using images
(tabbar-mode 1)


;; customize appearance



;;(highlight-changes-mode 1) ; not very useful usually
(delete-selection-mode 1) ; typing with the mark active will overwrite the marked region
(transient-mark-mode 1) ; enable visual feedback on selections, default since v23
(global-hungry-delete-mode 1) ; erase 'all' consecutive white space characters in a given direction


;; fontification
(global-font-lock-mode 1) ; turn on syntax coloring, on by default since Emacs 22
(setq font-lock-maximum-decoration t ; maximum fontification possible
      jit-lock-defer-time 0.10 ; improve scrolling speed with jit fontification
      font-lock-support-mode 'jit-lock-mode ; jit locking is better than fast-lock and lazy-lock
      jit-lock-stealth-time 10
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5
      )


;; achievements
(setq achievements-idle-time 600) ; seconds
(achievements-mode 1)


;; undo-tree (visualize with C-x u)
(setq undo-tree-mode-lighter ""
      undo-tree-visualizer-timestamps t
      undo-tree-visualizer-diff t
      )
(global-undo-tree-mode 1)


;; ibuffer
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)))
(defalias 'list-buffers 'ibuffer) ; turn on ibuffer by default
(setq ibuffer-expert t
      ibuffer-shrink-to-minimum-size t
      ibuffer-always-show-last-buffer nil
      ibuffer-default-sorting-mode 'recency ; 'major-mode
      ibuffer-sorting-mode 'recency
      ibuffer-use-header-line t)


;; search
(setq search-highlight t ; highlight incremental search
      query-replace-highlight t ; highlight during query
      case-fold-search t ; make search ignore case
      )


;; tramp
(setq tramp-default-method "ssh" ; faster than the default scp
      tramp-default-user "biswass"
      tramp-default-host "sunshine.cse.ohio-state.edu")
;; disable version control
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))


(setq completion-ignore-case t ; ignore case when completing
      read-file-name-completion-ignore-case t ; ignore case when reading a file name completion
      )


;; dim the ignored part of the file name
(file-name-shadow-mode 1)
(setq use-file-dialog nil)


;; desktop save mode
(desktop-save-mode -1) 
(setq-default desktop-restore-frames nil ; no need to restore frames
              desktop-load-locked-desktop nil)


;; fully redraw the display before queued input events are processed
;; don't defer screen updates when performing operations
(setq redisplay-dont-pause t) 


;; Package specific


;; speed up emacs for large files
(use-package vlf
             :ensure t
             :defer t
             :config (setq large-file-warning-threshold 50000000) ; warn when opening files bigger than 50MB
             )
(require 'vlf-setup)


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


;; ace jump mode major function
(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)


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


;; recentf stuff
(setq recentf-max-menu-items 15 ; show in recent menu
      recentf-max-saved-items 50 ; keep track of last xx files
      recentf-auto-cleanup 'never
      recentf-exclude '("/tmp/") ; "/ssh:"
      recentf-filename-handlers '(abbreviate-file-name) ; save file names relative to my current home directory
      ) 
(recentf-mode 1)


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


;; C-x C-j opens dired with the cursor right on the file you're editing, otherwise
;; you can use C-x d, or 'M-x dired'
(require 'dired) ; needed for dired-mode-map
(add-hook 'dired-load-hook ; dired-load-hook
          (lambda ()
            (load "dired-x")))
(autoload 'dired-jump "dired-x"
  "Jump to dired buffer corresponding to current buffer."
  'interactive)
(setq dired-bind-jump t)


;; directory navigation
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-nav-49/")
;;(nav-mode) ; always start in navigation mode
;;(nav-disable-overeager-window-splitting)


;; smart tabs (indent with tabs, align with spaces)
;;(global-smart-tab-mode 1)
;;(autoload 'smart-tabs-mode "smart-tabs-mode"
;;  "Intelligently indent with tabs, align with spaces!")
;;(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
;;(autoload 'smart-tabs-advice "smart-tabs-mode")
;;(autoload 'smart-tabs-insinuate "smart-tabs-mode")
;;(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml)


;; smex
;;(smex-initialize) ; this is slow
(autoload 'smex "smex")
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))

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


;; text mode  hooks

(setq-default major-mode 'text-mode)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook 'flyspell-mode) ; possibly won't work for extensionless .ascii files
(add-hook 'text-mode-hook 'writegood-mode)
(add-hook 'text-mode-hook 'abbrev-mode)
(add-hook 'text-mode-hook 'fci-mode)


;; latex mode hooks
(autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook #'reftex-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
(add-hook 'LaTeX-mode-hook #'writegood-mode)
(add-hook 'LaTeX-mode-hook #'abbrev-mode)
;;(add-hook 'LaTeX-mode-hook (lambda () (yas-reload-all)))
(add-hook 'LaTeX-mode-hook '(lambda () (yas-minor-mode)))
;;(add-hook 'prog-mode-hook '(lambda () (yas-minor-mode)))
(add-hook 'LaTeX-mode-hook #'fci-mode)
(add-hook 'LaTeX-mode-hook #'TeX-PDF-mode) ; compile files to pdf by default
(add-hook 'LaTeX-mode-hook #'auto-highlight-symbol-mode) ; highlight symbol at point

(setq TeX-auto-save t ; enable parse on save, stores parsed information in an "auto" directory
      TeX-parse-self t ; enable parse on load
      TeX-electric-sub-and-superscript t ; automatically insert braces in math mode
      TeX-force-default-mode t ; always use `TeX-default-mode', which defaults to `latex-mode'
      reftex-plug-into-AUCTeX t
      )
(setq-default TeX-master nil) ; query for master file

(auctex-latexmk-setup) ; add support for latexmk


;; shell mode hooks

;; set up shell (not eshell) mode
(setq explicit-shell-file-name "fish"
      explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
      )
(setq comint-process-echoes t)
;; setup auto-completion framework
(push 'company-readline company-backends)
(add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))
(add-hook 'sh-set-shell-hook 'flymake-shell-load) ;; flymake syntax-check for shell scripts
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; programming mode hooks

(add-hook 'prog-mode-hook 'highlight-numbers-mode) ; minor mode to highlight numeric literals
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode) ; enable in programming related-modes (Emacs 24+)
(add-hook 'prog-mode-hook #'aggressive-indent-mode)
(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
;;(add-hook 'prog-mode-hook (lambda () (yas-reload-all)))
(add-hook 'prog-mode-hook '(lambda () (yas-minor-mode)))
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook 'idle-highlight-mode) ; highlight all occurrences of word under the point
(add-hook 'prog-mode-hook #'auto-highlight-symbol-mode) ; highlight symbol at point

;; show the name of the function in the modeline
(add-hook 'prog-mode-hook 'which-function-mode)
;; (add-to-list 'which-func-modes 'java-mode)
;; (add-to-list 'which-func-modes 'c-mode)
;; (add-to-list 'which-func-modes 'c++-mode)
;; (add-to-list 'which-func-modes 'python-mode)
(eval-after-load "which-func"
  '(setq which-func-modes '(java-mode c++-mode c-mode python-mode)))


;; c/c++ hooks
(setq c-default-style "cc-mode"
      c-basic-offset 2)


;; java hooks
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))
(autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
(add-hook 'java-mode-hook 'jtags-mode)


;; python hooks


;; org mode hooks
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'visual-line-mode)
;; turn on soft wrapping mode for org mode
(add-hook 'org-mode-hook 
          (lambda () (setq truncate-lines nil)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode 1)))
(setq org-completion-use-ido t
      org-src-fontify-natively t ; code block fontification using the major-mode of the code
      org-src-preserve-indentation t
      )


;; custom functions

;; kill all non-special buffers but the current one
(defun kill-other-buffers ()
  "Kill all buffers but the current one. Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

;; http://endlessparentheses.com/new-in-emacs-25-1-comment-line.html
(defun comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))


;; keyboard shortcuts

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-c z") 'repeat)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-\\") 'goto-last-change) ; goto-last-change

(global-set-key [f1] 'shell)

(global-set-key [f2] 'split-window-vertically)
(global-set-key [f3] 'split-window-horizontally)
(global-set-key [f4] 'delete-other-windows)
(global-set-key [f7] 'other-window) ; switch to the other buffer

(global-set-key [f6] 'nav-toggle) ; set up a quick key to toggle nav

(global-set-key (kbd "M-/") 'hippie-expand)

(global-unset-key (kbd "C-s")) ; isearch-forward-regexp
(global-set-key (kbd "C-f") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(global-unset-key (kbd "C-x C-s")) ; save-buffer
(global-set-key (kbd "C-s") 'save-buffer)

(global-set-key (kbd "C-c n") 'comment-region)
(global-set-key (kbd "C-c m") 'uncomment-region)
(global-set-key (kbd "C-c ;") #'comment-line)

;; setting up writegood-mode, identify weasel words, passive voice, and duplicate words
(global-set-key (kbd "C-c g") 'writegood-mode)

;; define a keyboard shortcut for duplicating lines
(global-set-key (kbd "C-c C-d") 'duplicate-thing)

;; buffers
(global-set-key (kbd "C-c k") 'kill-other-buffers) ; kill all non-special buffers
(global-set-key (kbd "C-x C-b") 'ibuffer) ; use ibuffer for buffer list
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)

(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
;;(global-set-key (kbd "M-b") 'ace-jump-buffer-with-configuration)
(global-set-key (kbd "M-b") 'ace-jump-buffer)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; dired
(global-set-key (kbd "C-x C-j") #'dired-jump)
(define-key dired-mode-map (kbd "i") 'ido-find-file)
;; jump to home directory
(global-set-key (kbd "M-<home>")
                (lambda () 
                  (interactive)
                  (dired "~/")))
;; M-<up> is nicer in dired if it moves to the fourth line - the first file
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))
(define-key dired-mode-map (kbd "M-<up>") 'dired-back-to-top)

;; M-<down> is nicer in dired if it moves to the last file
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))
(define-key dired-mode-map (kbd "M-<down>") 'dired-jump-to-bottom)

;; M-<left>/<right> is overwritten by 'ahs-backward/forward, which is not useful
(when (auto-highlight-symbol-mode)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
  (define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil)
  )
(add-hook 'org-mode-hook 
          (lambda ()
            (local-set-key (kbd "M-<left>") #'tabbar-backward-tab)
            (local-set-key (kbd "M-<right>") #'tabbar-forward-tab)
            ))
(global-set-key (kbd "M-<left>") 'tabbar-backward-tab)
(global-set-key (kbd "M-<right>") 'tabbar-forward-tab)

;; up and down keys to navigate options, left and right to move through history/directories
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(global-set-key (kbd "C-h C-m") 'discover-my-major)


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
