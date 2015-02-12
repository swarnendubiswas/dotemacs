;;; init.el starts here
;; Swarnendu Biswas
;; Mon Feb  9 12:35:49 EST 2015

;; Notes: To evaluate an Sexp, just go to the end of the sexp and type \C-x \C-e, instead of evaluating the whole buffer
;; Init file shouldn't ideally contain calls to "load" or "require", since they cause eager loading and are expensive, a
;; cheaper alternative is to use "autoload".

;; Interesting quotes (inspired from http://www.mygooglest.com/fni/dot-emacs.html):
;;
;;   "Show me your ~/.emacs and I will tell you who you are." -- Bogdan Maryniuk
;;   "People talk about getting used to a new editor, but over time, it is precisely the opposite that should happen -
;;    the editor should get used to us." -- Vivek Haldar in "New frontiers in text editing".
;;   "Emacs is like a laser guided missile. It only has to be slightly mis-configured to ruin your whole day." -- Sean
;;    McGrath
;;   "Emacs outshines all other editing software in approximately the same way that the noonday sun does the stars. It
;;    is not just bigger and brighter; it simply makes everything else vanish." -- Neal Stephenson, "In the Beginning 
;;    was the Command Line"
;;   "Nearly everybody is convinced that every style but their own is ugly and unreadable. Leave out the "but their own"
;;    and they're probably right..." -- Jerry Coffin (on indentation)
;;   "The only real difficulties in programming are cache invalidation and naming things." -- Phil Karlton
;;   "Good code is its own best documentation. As you're about to add a comment, ask yourself, "How can I improve the
;;    code so that this comment isn't needed?" Improve the code and then document it to make it even clearer." -- Steve
;;    McConnell
;;   "What I don't understand is: why should you ever care how your editor looks, unless you're trying to win a
;;    screenshot competition? The primary factor in looking good should be the choice of a good font at a comfortable
;;    size, and a syntax coloring theme that you like. And that is not something specific to an editor. Editors like
;;    Emacs and vi have almost no UI! If Emacs is configured right, the only UI it has is the modeline and the
;;    minibuffer." -- Vivek Haldar in "New frontiers in text editing".

;;; Code:

;; customizing packages
(add-to-list 'load-path "~/.emacs.d/lisp")
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ))
(package-initialize)


;; user details
(setq user-full-name "Swarnendu Biswas"
      user-mail-address "XXX")


;; start customizing functionality

;; startup
(setq inhibit-default-init t ; disable loading of "default.el" at startup
      inhibit-startup-screen t
      inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode ; *scratch* is in Lisp interaction mode by default, use text mode instead
      ) 
(setq-default major-mode 'text-mode)


;; customize defaults
(setq require-final-newline t) ; always end a file with a newline
(fset 'yes-or-no-p 'y-or-n-p) ; type "y"/"n" instead of "yes"/"no"
(set-face-attribute 'default nil :height 110) ; set font size, value is in 1/10pt, so 100 will give you 10pt
(setq-default fill-column 120
              standard-indent 2 ; set standard indent to 2 rather that 4
              tab-width 2
              indent-tabs-mode nil ; spaces instead of tabs by default
              )


;; we need to paste something from another program, but sometimes we do real paste after some kill
;; action, that will erase the clipboard, so we need to save it to kill ring.
(setq save-interprogram-paste-before-kill t)
(setq x-select-enable-clipboard t) ; enable use of system clipboard across emacs and other applications


;; backup
(setq make-backup-files nil ; stop making backup ~ files
      backup-inhibited t) ; disable backup for a per-file basis, not to be used by major modes


;; auto revert
(global-auto-revert-mode 1) ; auto-refresh all buffers, does not work for remote files
(setq-default auto-revert-interval 5 ; default is 5 s
              auto-revert-verbose nil
              global-auto-revert-non-file-buffers t ; auto-refresh dired buffers
              ) 


;; automatically load abbreviations table
(setq-default abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq save-abbrevs nil) ; do not ask to save new abbrevs when quitting
;;(quietly-read-abbrev-file)
(setq dabbrev-case-replace nil) ; preserve case when expanding
(setq-default abbrev-mode t)


;; ensure that a required set of packages are always installed
(require 'ensure-packages)
;; get a list of currently installed packages (excluding built in packages) with '\C-h v package-activated-list'
(setq ensure-packages
      '(ace-jump-buffer ace-jump-mode achievements aggressive-indent anzu async auctex-latexmk auctex auto-highlight-symbol auto-indent-mode autodisass-java-bytecode bash-completion bibtex-utils color-theme company-auctex company company-math company-quickhelp company-statistics ctags ctags-update dash dired+ dired-details dired-details+ dired-rainbow dired-hacks-utils discover-my-major display-theme duplicate-thing epl es-lib f fill-column-indicator fish-mode fixme-mode flex-autopair flex-isearch flx-ido flx flycheck-color-mode-line flycheck-tip flycheck flymake flymake-shell flymake-easy flyparens ggtags git-rebase-mode git-commit-mode goto-last-change guide-key guide-key-tip pos-tip popwin highlight-indentation highlight-numbers hl-line+ hlinum hungry-delete icomplete+ idle-highlight idle-highlight-mode ido-at-point ido-better-flex ido-hacks ido-ubiquitous ido-vertical-mode ido-yes-or-no indent-guide javap-mode jgraph-mode jtags latex-extra latex-pretty-symbols latex-preview-pane let-alist leuven-theme magic-latex-buffer manage-minor-mode fringe-helper math-symbol-lists mic-paren mode-icons names nav org parent-mode pkg-info popup powerline professional-theme rainbow-delimiters rainbow-identifiers rainbow-mode readline-complete rich-minority s sentence-highlight smart-mode-line-powerline-theme smart-mode-line smart-tab smart-tabs-mode smartparens smex smooth-scroll tabbar undo-tree vlf writegood-mode yasnippet org-beautify-theme direx ibuffer-tramp json-mode)
      )
(ensure-packages-install-missing)


;; enable tabbar minor mode
(setq tabbar-use-images nil) ; speed up by not using images
(tabbar-mode 1)


;; customize appearance

;; better frame titles
;;(setq frame-title-format (concat  "%b - emacs@" (system-name)))
(setq frame-title-format
      (list '(buffer-file-name "%f" "%b") " -- " "GNU Emacs " emacs-version "@" system-name))

;;  line and column numbers
(global-hl-line-mode 1) ; highlight current line, turn it on for all modes by default
(global-linum-mode 1) ; display line numbers in margin
(hlinum-activate) ; extension to linum-mode to highlight current line number in the margin
(column-number-mode 1)

(tooltip-mode -1) ;; tooltips
(tool-bar-mode -1) ; no toolbar with icons
(scroll-bar-mode -1) ; no scroll bars
(menu-bar-mode -1) ; no menu bar
(blink-cursor-mode 1) ;; enable/disable blinking cursor

;; displays the time and date in the mode line
(setq display-time-day-and-date t
      display-time-24hr-format nil)
(display-time)


;; these are two nice themes: leuven and professional
(load-theme 'leuven t)
(set-face-background 'fringe "white") ; hide the fringe mark on the left
(setq-default indicate-empty-lines t ; show empty lines after buffer end
              indicate-buffer-boundaries 'right)


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
(setq undo-tree-mode-lighter "")
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
      tramp-default-user "XXX"
      tramp-default-host "XXX")
;; disable version control
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))


;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)


;; dim the ignored part of the file name
(file-name-shadow-mode 1)


;; desktop save mode
(desktop-save-mode -1) 
(setq-default desktop-restore-frames nil ; no need to restore frames
              desktop-load-locked-desktop nil)


;; fully redraw the display before queued input events are processed
;; don't defer screen updates when performing operations
(setq redisplay-dont-pause t) 


;; Package specific


;; speed up emacs for large files
(setq large-file-warning-threshold 50000000) ; warn when opening files bigger than 50MB
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

;; smart pairing
;; (require 'smartparens-config)
;; (setq sp-base-key-bindings 'paredit)
;; (setq sp-autoskip-closing-pair 'always)
;; (setq sp-hybrid-kill-entire-symbol nil)
;; (sp-use-paredit-bindings)
;; (smartparens-global-mode 1)
;; (show-smartparens-global-mode 1) ; highlight matching pairs


;; indentation
(electric-indent-mode -1) ; intelligent indentation, on by default from Emacs 24.4
;;(auto-indent-global-mode 1) ; auto-indentation minor mode
(global-aggressive-indent-mode 1)


;; indentation guides
;;(indent-guide-global-mode 1) ; doesn't seem to work well with company-mode and auto-complete-mode
;;(setq indent-guide-delay 0.1) ; show guide lines only in idle-time
(highlight-indentation-mode 1) 


;; highlight symbol at point
(global-auto-highlight-symbol-mode 1)


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


;; ido mode
(setq ido-everywhere t
      ido-enable-flex-matching t
      ido-enable-prefix nil
      ido-max-prospects 10
      ido-case-fold t ; ignore case
      ;;ido-use-filename-at-point 'guess ; other options: 'ffap-guesser
      ;;ido-show-dot-for-dired t ; don't show current directory as the first choice
      ido-create-new-buffer 'always ; other options: prompt, never
      ido-save-directory-list-file "~/.emacs.d/.ido.last"
      ido-enable-last-directory-history t
      ido-max-work-directory-list 20
      ido-max-work-file-list 50
      confirm-nonexistent-file-or-buffer nil
      ido-use-faces nil ; disable ido faces to see flx highlights
      ido-use-virtual-buffers t
      ) 
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Compile-Log*" "Flycheck error messages*"
                           "*Messages*" "Async Shell Command"))

(ido-mode 1)
(ido-at-point-mode 1) ;; M-tab to start completion
(flx-ido-mode 1) ; smarter fuzzy matching for ido
(ido-ubiquitous-mode 1) ; allow ido-style completion in more places
(ido-better-flex/enable)
(ido-vertical-mode 1)


;; recentf stuff
(setq recentf-max-menu-items 15 ; show in recent menu
      recentf-max-saved-items 50 ; keep track of last xx files
      recentf-auto-cleanup 'never
      recentf-exclude '("/tmp/") ; "/ssh:"
      recentf-filename-handlers '(abbreviate-file-name) ; save file names relative to my current home directory
      ) 
(recentf-mode 1)


;; smooth scroll
(require 'smooth-scroll)
(smooth-scroll-mode 1)


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


;; fci
;;(define-globalized-minor-mode
;;  global-fci-mode fci-mode (lambda () (fci-mode 1)))
;;(global-fci-mode 1)
;;(setq fci-rule-width 1)
(setq-default fci-rule-column 120)
(setq fci-handle-truncate-lines nil)
;; (defun auto-fci-mode (&optional unused)
;;   (if (> (frame-width) 120)
;;       (fci-mode 1)
;;     (fci-mode 0))
;;   )
;;(add-hook 'after-change-major-mode-hook 'auto-fci-mode)
;;(add-hook 'window-size-change-functions 'auto-fci-mode)


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
;; (add-hook 'emacs-startup-hook ; dired-load-hook
;;           (lambda ()
;;             (load "dired-x")))
(setq dired-auto-revert-buffer t ; revert each dired buffer automatically when you visit it
      dired-recursive-deletes 'always ; single prompt for all n directories
      dired-recursive-copies 'always)
(setq-default diredp-hide-details-initially-flag nil)
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


;; company
(autoload 'company-mode "company" nil t)
;;(require 'company-auctex)
(setq company-dabbrev-downcase nil ;; turn off auto downcasing of things
      company-show-numbers t
      company-minimum-prefix-length 2)
;; invert the navigation direction if the completion popup is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t) 
(global-company-mode 1)
(company-auctex-init)
(company-statistics-mode 1) 


;; smex
;;(smex-initialize) ; this is slow
(autoload 'smex "smex")
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))


;; smart mode line
(setq sml/theme 'light) ; options: dark, light, respectful, automatic, powerline
;;(setq sml/name-width 20)
(setq sml/no-confirm-load-theme t
      sml/shorten-modes t
      sml/shorten-directory t)
(sml/setup)
;; flat-looking mode-line
;;(set-face-attribute 'mode-line nil :box nil)
;;(set-face-attribute 'mode-line-inactive nil :box nil)
;;(set-face-attribute 'mode-line-highlight nil :box nil)


;; anzu mode - show number of searches in the mode line
(global-anzu-mode 1)


(icomplete-mode 1) ; incremental minibuffer completion/suggestions
(eval-after-load "icomplete" '(progn (require 'icomplete+)))
;;(icy-mode 1) ; icicles


;; save minibuffer histories across sessions
(setq savehist-additional-variables    
      '(kill-ring search-ring regexp-search-ring)    
      savehist-file "~/.emacs.d/savehist") 
(savehist-mode 1)


;; yasnippet
;;(yas-global-mode 1)
;;(yas-reload-all 1) ; this slows startup


;; guide-key
(setq guide-key/guide-key-sequence t
      guide-key/recursive-key-sequence-flag t
      guide-key/popup-window-position 'bottom
      )
(guide-key-mode 1)
(setq guide-key-tip/enabled t)


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
(add-hook 'LaTeX-mode-hook 'writegood-mode)
(add-hook 'LaTeX-mode-hook 'abbrev-mode)
(add-hook 'LaTeX-mode-hook (lambda () (yas-reload-all)))
(add-hook 'LaTeX-mode-hook 'yas-minor-mode)
(add-hook 'LaTeX-mode-hook 'fci-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) ; compile files to pdf by default

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
(add-hook 'prog-mode-hook (lambda () (yas-reload-all)))
(add-hook 'prog-mode-hook '(lambda () (yas-minor-mode)))
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook 'idle-highlight-mode) ; highlight all occurrences of word under the point

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
(define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
(define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil)
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
 '(column-number-mode t)
 '(custom-safe-themes (quote (default)))
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
 ;;'(sml/folder ((t (:inherit sml/global :background "#000000" :foreground "white smoke" :weight normal))))
 ;;'(sml/modes ((t (:inherit sml/global :background "#006666" :foreground "white smoke"))))
 )


;;; init.el ends here
