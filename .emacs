;; Swarnendu Biswas
;; Mon Oct 13 11:22:15 EDT 2014

;; SB: To evaluate an Sexp, just go to the end of the sexp and type \C-x \C-e, instead of evaluating the whole buffer

;; Citations (inspired from http://www.mygooglest.com/fni/dot-emacs.html)
;;
;;   "Show me your ~/.emacs and I will tell you who you are."  - Bogdan Maryniuk
;;   "People talk about getting used to a new editor, but over time, it is precisely the opposite that should happen - the editor should get used to us." - Vivek Haldar
;;   "Emacs is like a laser guided missile. It only has to be slightly mis-configured to ruin your whole day." - Sean McGrath

;; Customizing packages
(add-to-list 'load-path "~/.emacs.d/")
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                        ))
(package-initialize)


;; customizing common functionality

;; backup
(setq make-backup-files nil) ; stop making backup ~ files
(setq backup-inhibited t) ; disable backup for a per-file basis, not to be used by major modes

;; startup
(setq inhibit-default-init t) ; disable loading of "default.el" at startup
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t
      initial-scratch-message nil)
(set initial-major-mode 'text-mode) ; *scratch* is in Lisp interaction mode by default

;; defaults
(setq require-final-newline t) ; always end a file with a newline
(set major-mode 'text-mode)
;;(set default-major-mode 'text-mode) ; obsolete variable from Emacs 23.2
(fset 'yes-or-no-p 'y-or-n-p) ; type "y"/"n" instead of "yes"/"no"
(setq x-select-enable-clipboard t) ; enable use of system clipboard across emacs and applications
(set-face-attribute 'default nil :height 109) ; set font size, value is in 1/10pt, so 100 will give you 10pt
(setq column-number-mode t)
(setq-default fill-column 120)
(setq-default standard-indent 2) ; Set standard indent to 2 rather that 4
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil) ; spaces instead of tabs by default

;; auto revert
(global-auto-revert-mode 1) ; auto-refresh all buffers, does not work for remote files
(setq-default auto-revert-interval 30) ; default is 5 s
;;(auto-revert-tail-mode t) ; auto-revert if file grows at the end, also works for remote files
;; All the "Reverting buffer foo" messages are _really_ distracting.
(setq-default auto-revert-verbose nil) ;; quit this message


;; cua mode
;; cua-mode interferes with \C-x which can be useful in LaTeX. There are ways to get around it,
;; for example, press the prefix key twice very quickly, or instead use \C-\S-x
;;(cua-mode t) ; normal cut, copy, paste mode
;;(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;;(setq cua-keep-region-after-copy t) ;; Standard Windows behavior


;; This is already turned on by default since Emacs v23
(setq transient-mark-mode t) ; enable visual feedback on selections
;;(transient-mark-mode 1) ;; No region when it is not highlighted


;; customize appearance

(global-hl-line-mode 1) ; highlight current line, turn it on for all modes by default
(global-linum-mode 1) ; display line numbers in margin
;;(setq linum-format " %d ")
(show-paren-mode 1) ; (setq show-paren-mode t) 
(setq show-paren-style 'parenthesis) ; highlight just brackets
;;(setq show-paren-style 'expression) ; highlight entire bracket expression

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


;; Keyboard shortcuts
(global-set-key "\C-l" 'goto-line)
(global-set-key [f1] 'shell)
(global-set-key [f2] 'split-window-vertically)
(global-set-key [f3] 'split-window-horizontally)
(global-set-key [f4] 'delete-other-windows)
(global-set-key [f7] 'other-window) ; switch to the other buffer
(global-set-key "\C-x z" 'repeat)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-unset-key (kbd "C-s")) ; isearch-forward-regexp
(global-set-key (kbd "C-f") 'isearch-forward-regexp)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(global-unset-key (kbd "C-x C-s")) ; save-buffer
(global-set-key "\C-s" 'save-buffer)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-c n") 'comment-region)
(global-set-key (kbd "C-c m") 'uncomment-region)

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
(global-set-key "\C-c\C-d" 'duplicate-line)

;; kill all non-special buffers but the current one
(defun kill-other-buffers ()
  "Kill all buffers but the current one. Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))
(global-set-key (kbd "\C-c k") 'kill-other-buffers)


;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer) ; Use IBuffer for buffer list
;;(ibuffer-auto-mode) ; automatically keeps the buffer list up to date
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


;; This doesn't work on all Gnome versions
;; make emacs start fullscreen, there is also the -fs option, but that covers the desktop panels
;;(defun toggle-fullscreen ()
;;(interactive)
;(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;'(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;'(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;)
;;(toggle-fullscreen)
;;(global-set-key [f11] 'toggle-fullscreen)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
;;(fullscreen)
;;(global-set-key [f11] 'fullscreen)


(setq large-file-warning-threshold 50000000) ; warn when opening files bigger than 50MB


;; Package specific

;; First ensure that a required set of packages are always installed
(require 'ensure-packages)
;; Get a list of currently installed packages (excluding built in packages) with '\C-h v package-activated-list'
(setq ensure-packages
      '(ac-ispell ac-math auctex-latexmk auto-auto-indent auto-complete-auctex auto-complete-c-headers auto-complete auto-indent-mode bash-completion bibtex-utils color-theme company-auctex company dired+ display-theme es-lib f fill-column-indicator fish-mode fixme-mode flex-autopair flex-isearch flx-ido flx flycheck flymake flymake-shell flymake-easy flyparens highlight-indentation highlight-numbers hl-line+ hlinum hungry-delete icicles idle-highlight ido-at-point ido-better-flex ido-hacks ido-ubiquitous ido-yes-or-no indent-guide jgraph-mode latex-extra auctex latex-pretty-symbols latex-preview-pane leuven-theme magic-latex-buffer mic-paren mode-icons nav parent-mode pkg-info epl popup professional-theme rainbow-mode rainbow-delimiters rainbow-identifiers readline-complete s sentence-highlight smart-mode-line smart-tabs-mode smooth-scroll rich-minority dash smex writegood-mode yasnippet)
      )
(ensure-packages-install-missing)


;; related to pairing of parentheses, brackets, etc.
;;(autopair-global-mode 1) ; auto pair parentheses seems better than smartparens
;;(smartparens-global-mode 1) ; show paired parentheses
;;(flex-autopair-mode 1) ; this seems to work best, it autocompletes over existing words
;;(paren-activate) ; mic-paren - parentheses matching
(electric-indent-mode 1) ; intelligent indentation, on by default from Emacs 24.4
;;(electric-pair-mode 1) ; autocomplete brackets/parentheses
(setq-default flyparens-mode t) ; highlight/track mismatched parentheses

;; recentf stuff
(require 'recentf)
;;(setq recentf-max-menu-items 15) ; show 15 in recent menu, but currently menu bar is disabled
(setq recentf-max-saved-items 100) ; keep track of last 100 files
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(setq recentf-auto-cleanup 'never)
;; save file names relative to my current home directory
(setq recentf-filename-handlers '(abbreviate-file-name))
(recentf-mode 1)

;; if you want to use ido with recentf
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))


;; enable ido mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t) ;;(ido-everywhere 1)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-use-filename-at-point 'guess)
;;(setq ido-show-dot-for-dired t) ; don't show current directory as the first choice
(ido-at-point-mode 1)

;;(add-hook 'ido-setup-hook
;;          (lambda ()
;;            (define-key ido-completion-map [up] 'previous-history-element)))
(flx-ido-mode 1)
(setq ido-use-faces nil) ; disable ido faces to see flx highlights
(ido-ubiquitous-mode 1) ; allow ido-style completion in more places
;;(ido-vertical-mode 1) ; display ido completions in a vertical list


;; autocomplete ; currently disabled in favor of company
;;(require 'auto-complete)
;;(require 'auto-complete-config)
;;(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;(ac-config-default)
;;(global-auto-complete-mode 1)
;;(ac-flyspell-workaround) ; seems auto-complete stops working with flymake
;;(add-hook 'c-mode-hook
;;(lambda ()
;;(add-to-list 'ac-sources 'ac-source-c-headers)
;;(add-to-list 'ac-sources 'ac-source-c-header-symbols t)))


(hlinum-activate) ; extension to linum-mode to highlight current line number


;; indentation
(auto-indent-global-mode 1) ; auto-indentation minor mode
;;(indent-guide-global-mode t) ; doesn't seem to work well with transient-mark-mode and auto-complete-mode
(indent-guide-mode 1)
(highlight-indentation-mode 1)


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
(require 'uniquify) ; default from Emacs 24.4
;;(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")
;;(setq uniquify-buffer-name-style 'reverse)
;;(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; emacs 24.4 style ⁖ cat.png<dirName>


;; Spell check
;;(setq flyspell-mode 1) ; this is slow and doesn't work perfectly
;;(turn-on-flyspell 1)
;;(turn-on-auto-fill t)
;;(flyspell-issue-message-flag nil)
(add-hook 'find-file-hooks 'turn-on-flyspell) ; Otherwise flyspell isn't enabled as I want it
(setq-default ispell-program-name "/usr/bin/aspell")
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)


;; minibuffer completion
(require 'icicles)
(icy-mode 1)

;; rainbow mode
;;(global-rainbow-delimiters-mode) ; use Emacs-wide
(rainbow-mode 1)
(rainbow-identifiers-mode 1)
(rainbow-delimiters-mode 1)


;; C-x C-j opens dired with the cursor right on the file you're editing, otherwise
;; you can use C-x d, or 'M-x dired'
(require 'dired-x)
(setq dired-auto-revert-buffer t) ;; revert each dired buffer automatically when you visit it
(setq dired-recursive-deletes 'always) ; single prompt for all n directories


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
(add-to-list 'load-path "~/.emacs.d/emacs-nav-49/")
;;(nav-mode) ; always start in navigation mode
;;(nav-disable-overeager-window-splitting)
(global-set-key [f6] 'nav-toggle) ; set up a quick key to toggle nav


;; helm
;;(require 'helm-config)
;;(helm-mode t)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)


;; company
(autoload 'company-mode "company" nil t)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-auctex)
(company-auctex-init)
;; turn off auto downcasing of things
(setq company-dabbrev-downcase nil)
(global-company-mode 1)


;; smex
(smex-initialize)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
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
;;(setq sml/theme 'automatic)
(sml/setup)
;; flat-looking mode-line
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'mode-line-highlight nil :box nil)


(global-hungry-delete-mode 1) ; erase 'all' consecutive white space characters in a given direction


(setq-default idle-highlight-mode t) ; idle highlight modes


(iswitchb-mode 1) ;; auto completion in minibuffer
(icomplete-mode 1) ;; minibuffer completion/suggestions


;; save minibuffer histories across emacs sessions
(setq savehist-additional-variables    
      '(search-ring regexp-search-ring)    
      savehist-file "~/.emacs.d/savehist") 
(savehist-mode 1)


;; Setting up writegood-mode, identify weasel words, passive voice, and duplicate words
(require 'writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)


;; discover
;;(global-discover-mode 1)


;; text mode  hooks

(setq-default major-mode 'text-mode)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook 'flyspell-mode) ; possibly won't work for extensionless .ascii files
(add-hook 'text-mode-hook 'writegood-mode)

;; latex mode hooks

;; auctex suggestions
(setq TeX-auto-save t) ; enable parse on save, stores parsed information in an "auto" directory
(setq TeX-parse-self t) ; enable parse on load
(setq-default TeX-master nil) ; query for master file

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t) ; compile files to pdf by default

(require 'auctex-latexmk)
(auctex-latexmk-setup)

;; preview-latex
(autoload 'LaTeX-preview-setup "preview")
(add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
(latex-preview-pane-enable) ; latex-preview-pane
(add-hook 'latex-mode-hook 'magic-latex-buffer)
(add-hook 'latex-mode-hook 'writegood-mode)


;; shell mode hooks
(require 'readline-complete) ; set up auto-complete in shell mode with company

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

;;(add-hook 'prog-mode-hook 'highlight-numbers-mode) ; minor mode to highlight numeric literals
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; enable in programming related-modes (Emacs 24+)


;; show the name of the function in the modeline
(which-function-mode)
(add-to-list 'which-func-modes 'java-mode)
(add-to-list 'which-func-modes 'c-mode)
(add-to-list 'which-func-modes 'c++-mode)
(add-to-list 'which-func-modes 'python-mode)

;; c/c++ hooks

;; java hooks
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))
;;(add-to-list 'load-path "~/.emacs.d/jdee-2.4.1/lisp")
;;(load "jde")
;;(require 'cedet)
(require 'semantic)
;;(require 'semantic/db-javap)
(load "semantic/loaddefs.el")
(semantic-mode 1)

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
(global-semantic-decoration-mode)
(global-semantic-highlight-edits-mode)
(global-semantic-highlight-func-mode)
(global-semantic-idle-completions-mode)
(global-semantic-idle-breadcrumbs-mode)

;; python hooks


;; org mode hooks
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'visual-line-mode)
;; turn on soft wrapping mode for org mode
(add-hook 'org-mode-hook 
          (lambda () (setq truncate-lines nil)))
