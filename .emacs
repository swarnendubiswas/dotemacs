;; Swarnendu Biswas
;; Wed Nov 12 23:44:13 EST 2014


;; SB: To evaluate an Sexp, just go to the end of the sexp and type \C-x \C-e, instead of evaluating the whole buffer
;; Init file shouldn't ideally contain calls to load or require, since they cause eager loading and are expensive, a
;; cheaper alternative is to use autoload

;; Citations (inspired from http://www.mygooglest.com/fni/dot-emacs.html)
;;
;;   "Show me your ~/.emacs and I will tell you who you are." - Bogdan Maryniuk
;;   "People talk about getting used to a new editor, but over time, it is precisely the opposite that should happen -
;;    the editor should get used to us." - Vivek Haldar
;;   "Emacs is like a laser guided missile. It only has to be slightly mis-configured to ruin your whole day." - Sean
;;    McGrath


;; time your .emacs initialization
;;(require 'cl) ; a rare necessary use of REQUIRE
;;(defvar *emacs-load-start* (current-time))

;; Customizing packages
(add-to-list 'load-path "~/.emacs.d/lisp")
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ;;("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ))
(package-initialize)


;; start customizing functionality

;; startup
(setq inhibit-default-init t) ; disable loading of "default.el" at startup
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t
      initial-scratch-message nil)
;; *scratch* is in Lisp interaction mode by default, make it use text mode by default
(setq initial-major-mode 'text-mode) 
(setq-default major-mode 'text-mode)


;; customize defaults
(setq require-final-newline t) ; always end a file with a newline
(fset 'yes-or-no-p 'y-or-n-p) ; type "y"/"n" instead of "yes"/"no"
(setq x-select-enable-clipboard t) ; enable use of system clipboard across emacs and applications
(set-face-attribute 'default nil :height 109) ; set font size, value is in 1/10pt, so 100 will give you 10pt
(setq column-number-mode t)
(setq-default fill-column 120)
(setq-default standard-indent 2) ; set standard indent to 2 rather that 4
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil) ; spaces instead of tabs by default


;; backup
(setq make-backup-files nil) ; stop making backup ~ files
(setq backup-inhibited t) ; disable backup for a per-file basis, not to be used by major modes


;; auto revert
(global-auto-revert-mode 1) ; auto-refresh all buffers, does not work for remote files
(setq-default auto-revert-interval 30) ; default is 5 s
;;(auto-revert-tail-mode t) ; auto-revert if file grows at the end, also works for remote files
(setq-default auto-revert-verbose nil) 


;; automatically load abbreviations table
(setq-default abbrev-mode t)
(setq-default abbrev-file-name "~/.emacs/abbrev_defs")
(setq save-abbrevs t)


;; tooltips
(tooltip-mode -1)
;;(setq tooltip-use-echo-area t) ; obsolete since 24.1


;; enable tabbar minor mode
(setq tabbar-use-images nil) ; speed up by not using images
(tabbar-mode 1)


;; customize appearance

(global-hl-line-mode 1) ; highlight current line, turn it on for all modes by default
(global-linum-mode 1) ; display line numbers in margin
;;(hlinum-activate 1) ; extension to linum-mode to highlight current line number
;;(setq linum-format " %d ")

(tool-bar-mode -1) ; no toolbar with icons
(scroll-bar-mode -1) ; no scroll bars
(menu-bar-mode -1) ; disable menu bar
;; displays the time and date in the mode line
(setq display-time-day-and-date t
      display-time-24hr-format nil)
(display-time)
(setq frame-title-format (concat  "%b - emacs@" (system-name))) ;; default to better frame titles


;; These are two nice themes, leuven and professional
;;(load-theme 'leuven t) ; set default theme on start up
(load-theme 'professional t)
(set-face-background 'fringe "white") ; Hide the fringe mark on the left
(setq-default indicate-empty-lines t)
(setq-default highlight-changes-mode 1)
(setq-default indicate-buffer-boundaries 'right)


(delete-selection-mode 1) ; typing with the mark active will overwrite the marked region
(transient-mark-mode 1) ; enable visual feedback on selections, default since v23
(global-hungry-delete-mode 1) ; erase 'all' consecutive white space characters in a given direction
(idle-highlight-mode 1) ; idle highlight mode


;; cua mode
;; cua-mode interferes with \C-x which can be useful in LaTeX. There are ways to get around it,
;; for example, press the prefix key twice very quickly, or instead use \C-\S-x
;;(cua-mode t) ; normal cut, copy, paste mode
;;(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;;(setq cua-keep-region-after-copy t) ;; Standard Windows behavior


;; define a keyboard shortcut for duplicating lines
(defun duplicate-line()
  "Duplicate current line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

;; kill all non-special buffers but the current one
(defun kill-other-buffers ()
  "Kill all buffers but the current one. Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))


;; ibuffer
;;(ibuffer-auto-mode 1) ; automatically keeps the buffer list up to date
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)))
(setq ibuffer-expert t)
(defalias 'list-buffers 'ibuffer)
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)


;; fontification
(global-font-lock-mode 1) ; turn on syntax coloring, on by default since Emacs 22
(setq font-lock-maximum-decoration t)
(setq jit-lock-defer-time 0.10) ; improve scrolling speed with jit fontification


;; search
(setq search-highlight t) ; highlight incremental search
(setq query-replace-highlight t) ; highlight during query


;; tramp
(setq tramp-default-method "ssh") ; faster than the default scp
(setq tramp-default-user "xxx"
      tramp-default-host "xxx")


;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; dim the ignored part of the file name
(file-name-shadow-mode 1)

;; start emacs/emacsclient in fullscreen mode

;; this doesn't work on all Gnome versions
;; make emacs start fullscreen, there is also the -fs option, but that covers the desktop panels
;;(defun toggle-fullscreen ()
;;(interactive)
;;(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;'(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;;(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;'(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;;)
;;(toggle-fullscreen)
;;(global-set-key [f11] 'toggle-fullscreen)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
;;(fullscreen)
;;(global-set-key [f11] 'fullscreen)

;; for emacs, just pass -mm or --maximized
;; for emacsclient, use /usr/local/bin/emacsclient -a "" -c -F "((fullscreen . maximized))"
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; use fullboth for fullscreen

;; fully redraw the display before queued input events are processed
(setq redisplay-dont-pause t)


(setq large-file-warning-threshold 50000000) ; warn when opening files bigger than 50MB


;; Package specific

;; First ensure that a required set of packages are always installed
(require 'ensure-packages)
;; Get a list of currently installed packages (excluding built in packages) with '\C-h v package-activated-list'
(setq ensure-packages
      '(ace-jump-buffer aggressive-indent anzu async auctex auctex-latexmk auto-auto-indent autodisass-java-bytecode auto-indent-mode bash-completion bibtex-utils color-theme company company-auctex company-math dash dired+ display-theme duplicate-thing epl es-lib f fill-column-indicator fish-mode fixme-mode flex-autopair flex-isearch flx-ido flx flycheck flycheck-color-mode-line flycheck-tip flymake flymake-shell flymake-easy flyparens goto-last-change guide-key guide-key-tip highlight-indentation highlight-numbers hl-line+ hlinum hungry-delete icicles idle-highlight-mode ido-at-point ido-better-flex ido-hacks ido-ubiquitous ido-yes-or-no indent-guide javap-mode jgraph-mode latex-extra auctex latex-pretty-symbols latex-preview-pane leuven-theme magic-latex-buffer mic-paren mode-icons names nav parent-mode pkg-info popup professional-theme rainbow-mode rainbow-delimiters rainbow-identifiers readline-complete rich-minority s sentence-highlight smart-tabs-mode smooth-scroll smex tabbar writegood-mode yasnippet)
      )
(ensure-packages-install-missing)


;; related to pairing of parentheses, brackets, etc.
(show-paren-mode 1) 
(setq show-paren-style 'parenthesis) ; highlight just brackets, 'expression
;;(autopair-global-mode 1) ; auto pair parentheses seems better than smartparens
;;(smartparens-global-mode 1) ; show paired parentheses
;;(flex-autopair-mode 1) ; this seems to work best, it autocompletes over existing words
;;(paren-activate) ; mic-paren - parentheses matching
(electric-indent-mode 1) ; intelligent indentation, on by default from Emacs 24.4
(electric-pair-mode 1) ; autocomplete brackets/parentheses, seems to be more improved than autopair
(setq-default flyparens-mode t) ; highlight/track mismatched parentheses
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)


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


;; enable ido mode
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess) ; 'ffap-guesser
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
;;(setq ido-show-dot-for-dired t) ; don't show current directory as the first choice
(ido-at-point-mode 1)
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Compile-Log*" "Flycheck error messages*"
                           "*Messages*" "Async Shell Command"))
(setq ido-enable-last-directory-history t)
(setq ido-max-work-directory-list 30)
(setq ido-max-work-file-list 50)
(setq confirm-nonexistent-file-or-buffer nil)

;;(add-hook 'ido-setup-hook
;;          (lambda ()
;;            (define-key ido-completion-map [up] 'previous-history-element)))
(flx-ido-mode 1)
(setq ido-use-faces nil) ; disable ido faces to see flx highlights
(ido-ubiquitous-mode 1) ; allow ido-style completion in more places
(setq ido-use-virtual-buffers t)


;; recentf stuff
(setq recentf-max-menu-items 15) ; show 15 in recent menu, but currently menu bar is disabled
(setq recentf-max-saved-items 100) ; keep track of last 100 files
(setq recentf-auto-cleanup 'never)
;; save file names relative to my current home directory
(setq recentf-filename-handlers '(abbreviate-file-name))
(recentf-mode 1)
(setq recentf-exclude '("/tmp/" "/ssh:"))

;; if you want to use ido with recentf
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))


;; autocomplete ; currently disabled in favor of company
;;(require 'auto-complete)
;;(require 'auto-complete-config)
;;(add-to-list 'load-path "~/.emacs.d/lisp/auto-complete-1.3.1")
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;(ac-config-default)
;;(global-auto-complete-mode 1)
;;(ac-flyspell-workaround) ; seems auto-complete stops working with flymake
;;(add-hook 'c-mode-hook
;;(lambda ()
;;(add-to-list 'ac-sources 'ac-source-c-headers)
;;(add-to-list 'ac-sources 'ac-source-c-header-symbols t)))



;; indentation
(auto-indent-global-mode 1) ; auto-indentation minor mode
(indent-guide-global-mode 1) ; doesn't seem to work well with transient-mark-mode and auto-complete-mode
;;(indent-guide-mode 1)
(highlight-indentation-mode 1)


;; highlight-symbol at point
(highlight-symbol-mode 1)


;; guide-key
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(guide-key-mode 1)  ; Enable guide-key-mode


;; smooth scroll
(require 'smooth-scroll)
(smooth-scroll-mode 1)

;; scroll one line at a time (less "jumpy" than defaults)
;;(setq mouse-wheel-scroll-amount '(10 ((shift) . 10))) ;; one line at a time
;;(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;;(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;;(setq scroll-step 10) ;; keyboard scroll one line at a time


;; whitespace
;; (setq-default indicate-empty-lines t)
;; (show-trailing-whitespace t)
;; (add-hook 'before-save-hook 'whitespace-cleanup)
;;(setq whitespace-style '(face empty tabs lines-tail trailing))
;;(setq whitespace-style '(face empty tabs lines-tail))
;;(set-face-attribute 'whitespace-line nil
;;                    :background "red1"
;;                    :foreground "yellow"
;;                    :weight 'bold)
;;(global-whitespace-mode t)


;; fci
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
;;(global-fci-mode 1)
;;(setq fci-rule-width 1)
;;(setq-default fci-rule-column 120)
;;(setq fci-handle-truncate-lines nil)
(defun auto-fci-mode (&optional unused)
  (if (> (frame-width) 120)
      (fci-mode 1)
    (fci-mode 0))
  )
;;(add-hook 'after-change-major-mode-hook 'auto-fci-mode)
;;(add-hook 'window-size-change-functions 'auto-fci-mode)


;; uniquify
;;(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")
;;(setq uniquify-buffer-name-style 'reverse)
;;(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; emacs 24.4 style ⁖ cat.png<dirName>
(setq uniquify-after-kill-buffer-p t)


;; Spell check
;;(flyspell-mode) ; this is slow and doesn't work perfectly
;;(turn-on-flyspell 1)
;;(turn-on-auto-fill t)
;;(flyspell-issue-message-flag nil)
(add-hook 'find-file-hooks 'turn-on-flyspell) ; Otherwise flyspell isn't enabled as I want it
(setq-default ispell-program-name "/usr/bin/aspell")


;; flycheck
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
;;(add-hook 'after-init-hook #'global-flycheck-mode)
(global-flycheck-mode 1)
;; disable Checkdoc warnings for .emacs
;;(eval-after-load 'flycheck (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))


;; minibuffer completion
;;(require 'icicles)
;;(icy-mode -1)


;; rainbow mode
(rainbow-mode 1)
(rainbow-identifiers-mode 1)
(rainbow-delimiters-mode 1)


;; C-x C-j opens dired with the cursor right on the file you're editing, otherwise
;; you can use C-x d, or 'M-x dired'
;;(require 'dired+)
;;(require 'dired-x)
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))
(setq dired-auto-revert-buffer t) ;; revert each dired buffer automatically when you visit it
(setq dired-recursive-deletes 'always) ; single prompt for all n directories
(setq-default diredp-hide-details-initially-flag nil)


;; smart tabs (indent with tabs, align with spaces)
;;(global-smart-tab-mode 1)
(autoload 'smart-tabs-mode "smart-tabs-mode"
  "Intelligently indent with tabs, align with spaces!")
(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
(autoload 'smart-tabs-advice "smart-tabs-mode")
(autoload 'smart-tabs-insinuate "smart-tabs-mode")
(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml)


;; ergoemacs
;;(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
;;(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
;;(ergoemacs-mode 1)


;; directory navigation
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-nav-49/")
;;(nav-mode) ; always start in navigation mode
;;(nav-disable-overeager-window-splitting)


;; helm
;;(require 'helm-config)
;;(helm-mode t)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)


;; company
(autoload 'company-mode "company" nil t)
(add-hook 'after-init-hook 'global-company-mode)
;;(require 'company-auctex)
(company-auctex-init)
(setq company-dabbrev-downcase nil) ;; turn off auto downcasing of things
(setq company-show-numbers t)
(global-company-mode 1)


;; smex
;;(smex-initialize) ; this is slow
(autoload 'smex "smex")
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ajb-bs-configuration "files" t)
 '(column-number-mode t)
 '(custom-safe-themes (quote ( default)))
 '(diredp-hide-details-initially-flag nil t)
 '(display-time-mode 1)
 '(menu-bar-mode nil)
 '(scroll-bar-mode 1)
 '(show-paren-mode 1)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; mode-line
;; powerline
;; (set-face-attribute ;; 'mode-line nil
;;  :foreground "Black"
;;  :background "DarkOrange"
;;  :box nil
;;  )
;;(require 'powerline)
;;(setq powerline-arrow-shape 'curve) ; options: arrow, curve, arrow14
;;(setq powerline-default-separator-dir '(right . left))
;;(setq powerline-color1 "grey22")
;;(setq powerline-active1 "grey40")

;; smart mode line
;;(setq sml/theme 'powerline) ; options: dark, light, respectful, automatic, powerline
;;(setq sml/name-width 20)
;;(setq sml/modes ((t :foreground "White"))))
;;(setq sml/no-confirm-load-theme t)
;;(sml/setup)
;; flat-looking mode-line
;;(set-face-attribute 'mode-line nil :box nil)
;;(set-face-attribute 'mode-line-inactive nil :box nil)
;;(set-face-attribute 'mode-line-highlight nil :box nil)

;; (setq-default mode-line-format `(
;;                                  "%e" mode-line-front-space
;;                                  ;; Standard info about the current buffer
;;                                  mode-line-mule-info
;;                                  mode-line-client
;;                                  mode-line-modified
;;                                  mode-line-remote
;;                                  mode-line-frame-identification
;;                                  mode-line-buffer-identification mode-line-position
;;                                  (vc-mode vc-mode-line) ; VC information
;;                                  (flycheck-mode flycheck-mode-line) ; Flycheck status
;;                                  ;; Misc information, notably battery state and function name
;;                                  " " mode-line-modes mode-line-end-spaces
;;                                  ))

;;(set-face-attribute 'mode-line nil :foreground "White" :background "grey22" :box nil)
;;(set-face-attribute 'mode-line-highlight nil :foreground "red" )
;; (set-face-background 'modeline "grey40" (current-buffer))
;;(set-face-attribute 'minor-mode-alist "red" )

;; (setq-default mode-line-format
;;               (list
;;                "%e:"
;;                mode-line-front-space
;;                mode-line-mule-info
;;                mode-line-client
;;                mode-line-modified
;;                mode-line-remote
;;                mode-line-frame-identification
;;                mode-line-buffer-identification
;;                ;; " --user: " (getenv "USER")
;;                ;;mode-line-position
;;                "%m"  ;; value of `mode-name'
;;                ;; value of current buffer name
;;                ;;"buffer %b, "
;;                ;; value of current line number
;;                ;;"line %l "
;;                ;;(vc-mode vc-mode-line)
;;                minor-mode-alist
;;                mode-line-end-spaces
;;                ))

;; anzu mode - show number of searches in the mode line
(global-anzu-mode 1)


;;(iswitchb-mode 1) ;; auto completion in minibuffer, obsolete since Emacs 24.4
(icomplete-mode 1) ;; incremental minibuffer completion/suggestions


;; save minibuffer histories across emacs sessions
(setq savehist-additional-variables    
      '(kill-ring search-ring regexp-search-ring)    
      savehist-file "~/.emacs.d/savehist") 
(savehist-mode 1)


;; yasnippet
(yas-global-mode 1)


;; text mode  hooks

(setq-default major-mode 'text-mode)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook 'flyspell-mode) ; possibly won't work for extensionless .ascii files
(add-hook 'text-mode-hook 'writegood-mode)

;; latex mode hooks
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook #'reftex-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
(add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
(add-hook 'LaTeX-mode-hook 'writegood-mode)

(setq TeX-auto-save t) ; enable parse on save, stores parsed information in an "auto" directory
(setq TeX-parse-self t) ; enable parse on load
(setq-default TeX-master nil) ; query for master file
(setq TeX-PDF-mode t) ; compile files to pdf by default

(autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)
(setq reftex-plug-into-AUCTeX t)
;;(TeX-source-correlate-mode 1)
;;(TeX-source-correlate-determine-method 'synctex)
;;(TeX-source-correlate-method-active)

(auctex-latexmk-setup) ; add support for latexmk

(setq TeX-electric-sub-and-superscript t) ; automatically insert braces in math mode

;; preview-latex
(autoload 'LaTeX-preview-setup "preview")
(add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
(latex-preview-pane-enable) ; latex-preview-pane

;; custom hook
(defun compile-candidacy-proposal()
  "Compile my candidacy proposal"
  (message "banana"))
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'compile-candidacy-proposal nil 'make-it-local)))


;; shell mode hooks
;;(require 'readline-complete) ; set up auto-complete in shell mode with company

;; set up shell (not eshell) mode
(setq explicit-shell-file-name "bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
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

;; show the name of the function in the modeline
(which-function-mode 1)
(add-to-list 'which-func-modes 'java-mode)
(add-to-list 'which-func-modes 'c-mode)
(add-to-list 'which-func-modes 'c++-mode)
(add-to-list 'which-func-modes 'python-mode)

;; c/c++ hooks
(setq c-default-style "cc-mode"
      c-basic-offset 2)


;; java hooks
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))

;;(add-to-list 'load-path "~/.emacs.d/jdee-2.4.1/lisp")
;;(load "jde")
;;(require 'cedet)
;;(require 'semantic)
;;(require 'semantic/db-javap)
;;(load "semantic/loaddefs.el")
;;(semantic-mode 1)

;; (require 'malabar-mode)
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))
;; ;; compile on save
;; (add-hook 'malabar-mode-hook
;;           (lambda () 
;;             (add-hook 'after-save-hook 'malabar-compile-file-silently
;;                       nil t)))

;;(load-file "~/cedet-1.0pre6/common/cedet.el")
;;(global-ede-mode 1)                      ; Enable the Project management system

;;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;;(global-semantic-decoration-mode)
;;(global-semantic-highlight-edits-mode)
;;(global-semantic-highlight-func-mode)
;;(global-semantic-idle-completions-mode)
;;(global-semantic-idle-breadcrumbs-mode)


;; python hooks


;; org mode hooks
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'visual-line-mode)
;; turn on soft wrapping mode for org mode
(add-hook 'org-mode-hook 
          (lambda () (setq truncate-lines nil)))
(setq org-completion-use-ido t)
(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)) t)


;; automatically byte compile any emacs-lisp after every change (a save)
;; (add-hook 'emacs-lisp-mode-hook
;;           '(lambda ()
;;              (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))
;;           )

;; automatically byte compile .emacs after every change (a save)
(defun auto-recompile-emacs-file ()
  (interactive)
  (when (and buffer-file-name (string-match "\\.emacs" buffer-file-name))
    (let ((byte-file (concat buffer-file-name "\\.elc")))
      (if (or (not (file-exists-p byte-file))
              (file-newer-than-file-p buffer-file-name byte-file))
          (byte-compile-file buffer-file-name)))))

;;(add-hook 'after-save-hook 'auto-recompile-emacs-file)


;; keyboard shortcuts
(global-set-key "\C-l" 'goto-line)
(global-set-key [f1] 'shell)
(global-set-key [f2] 'split-window-vertically)
(global-set-key [f3] 'split-window-horizontally)
(global-set-key [f4] 'delete-other-windows)
(global-set-key [f6] 'nav-toggle) ; set up a quick key to toggle nav
(global-set-key [f7] 'other-window) ; switch to the other buffer
(global-set-key "\C-c z" 'repeat)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-unset-key (kbd "C-s")) ; isearch-forward-regexp
(global-set-key (kbd "C-f") 'isearch-forward-regexp)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(global-unset-key (kbd "C-x C-s")) ; save-buffer
(global-set-key "\C-s" 'save-buffer)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-c n") 'comment-region)
(global-set-key (kbd "C-c m") 'uncomment-region)
;; setting up writegood-mode, identify weasel words, passive voice, and duplicate words
(global-set-key "\C-cg" 'writegood-mode)
;; define a keyboard shortcut for duplicating lines
;;(global-set-key "\C-c\C-d" 'duplicate-line)
(global-set-key (kbd "C-c C-d") 'duplicate-thing)
;; kill all non-special buffers
(global-set-key (kbd "\C-c k") 'kill-other-buffers)
(global-set-key (kbd "C-x C-b") 'ibuffer) ; use IBuffer for buffer list
(global-set-key "\C-x\C-\\" 'goto-last-change) ; goto-last-change
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key [M-left] 'tabbar-backward-tab)
(global-set-key [M-right] 'tabbar-forward-tab)
;;(global-set-key (kbd "M-b") 'ace-jump-buffer-with-configuration)
(global-set-key (kbd "M-b") 'ace-jump-buffer)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; start emacs server
;;(server-start)


;; compute total running time
;; (message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
;;                                      (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))


