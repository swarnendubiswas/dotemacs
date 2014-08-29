                                        ; .emacs

;; SB: To evaluate an Sexp, just go to the end of the sexp and type \C-x \C-e, instead of evaluating the whole buffer

                                        ; Customizing packages
(add-to-list 'load-path "~/.emacs.d/")
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                        ))

(package-initialize)


                                        ; Customizing common functionality

(setq backup-inhibited t) ; disable backup
(setq make-backup-files nil)
(setq require-final-newline t) ; always end a file with a newline
(setq inhibit-default-init t) ; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t
      initial-scratch-message nil)
(set initial-major-mode 'text-mode) ; *scratch* is in Lisp interaction mode by default
(global-auto-revert-mode t) ; auto-refresh all buffers
(fset 'yes-or-no-p 'y-or-n-p) ; type "y"/"n" instead of "yes"/"no"
(setq x-select-enable-clipboard t) ; enable use of system clipboard across emacs and applications
(set-face-attribute 'default nil :height 109) ; set font size, value is in 1/10pt, so 100 will give you 10pt

(cua-mode t) ; normal cut, copy, paste mode
;;(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(setq cua-keep-region-after-copy t) ;; Standard Windows behavior

;; This is already turned on by default since Emacs v23
(setq transient-mark-mode t) ; enable visual feedback on selections
;;(transient-mark-mode 1) ;; No region when it is not highlighted

; Customize appearance

(global-hl-line-mode 1) ; highlight current line, turn it on for all modes by default
;;(global-hl-line-mode) ; highlight current line
(global-linum-mode 1) ; display line numbers in margin
(setq column-number-mode t)
(setq-default fill-column 120)
(show-paren-mode t) ; (setq show-paren-mode t)
(setq standard-indent 2) ; Set standard indent to 2 rather that 4
(setq tab-width 2
      indent-tabs-mode nil)
;;(setq-default indent-tabs-mode nil) ; spaces instead of tabs by default

;;(tool-bar-mode nil) ; no toolbar with icons
(scroll-bar-mode -1) ; no scroll bars
;;(menu-bar-mode nil) ; disable menu bar
; displays the time and date in the mode line
(setq display-time-day-and-date t
      display-time-24hr-format nil)
(display-time)
;; default to better frame titles
(setq frame-title-format 
      (concat  "%b - emacs@" (system-name)))

; set default theme on start up
(load-theme 'leuven t)
;;(setq linum-format " %d ")
(set-face-background 'fringe "white") ; Hide the fringe mark on the left
(setq-default indicate-empty-lines t)
(highlight-changes-mode 1)
(delete-selection-mode t) ; typing with the mark active will overwrite the marked region


; show the name of the function in the modeline
(which-function-mode)
(add-to-list 'which-func-modes 'java-mode)
(add-to-list 'which-func-modes 'c-mode)
(add-to-list 'which-func-modes 'c++-mode)
(add-to-list 'which-func-modes 'python-mode)



(electric-indent-mode) ; intelligent indentation
;;(electric-pair-mode 1) ; autocomplete brackets/parentheses


;;(smartparens-global-mode 1) ; show paired parentheses

                                    
(paren-activate) ; mic-paren - parentheses matching


(auto-indent-global-mode)
;;(indent-guide-global-mode)


;; scroll one line at a time (less "jumpy" than defaults)    
(setq mouse-wheel-scroll-amount '(10 ((shift) . 10))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 10) ;; keyboard scroll one line at a time


; Keyboard shortcuts
(global-set-key "\C-l" 'goto-line) 
(global-set-key [f1] 'shell)
(global-set-key [f2] 'split-window-vertically)
(global-set-key [f3] 'split-window-horizontally)
(global-set-key [f4] 'delete-other-windows)
(global-set-key [f7] 'other-window) ; switch to the other buffer 
(global-set-key "\C-x z" 'repeat)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map "\C-f" 'isearch-repeat-forward)
(global-unset-key (kbd "C-x C-s")) ; save-buffer
(global-set-key "\C-s" 'save-buffer)


; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer) ; Use IBuffer for buffer list
;;(ibuffer-auto-mode) ; automatically keeps the buffer list up to date
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)))
(setq ibuffer-expert t)


; Search
(setq search-highlight t) ; highlight incremental search
(setq query-replace-highlight t) ; highlight during query


; Tramp
(require 'tramp)
(setq tramp-default-method "ssh") ; faster than the default scp
(setq tramp-default-user "biswass"
      tramp-default-host "sunshine.cse.ohio-state.edu")


; define a keyboard shortcut for duplicating lines
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key "\C-c\C-d" 'duplicate-line)


; This doesn't work on all Gnome versions
; make emacs start fullscreen, there is also the -fs option, but that covers the desktop panels
;(defun toggle-fullscreen ()
                                        ;  (interactive)
;(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;'(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;(x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;'(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;)
;(toggle-fullscreen)
;(global-set-key [f11] 'toggle-fullscreen)


(setq large-file-warning-threshold 50000000) ; warn when opening files bigger than 50MB


(defun kill-other-buffers ()
  "Kill all buffers but the current one. Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))
(global-set-key (kbd "\C-c k") 'kill-other-buffers)


; Package specific

                                        ; First ensure that a required set of packages are always installed
(require 'ensure-packages)
                                        ; Get a list of currently installed packages (excluding built in packages) with '\C-h v package-activated-list'
(setq ensure-packages
      '(ac-c-headers ac-ispell ac-math auto-auto-indent auto-complete-auctex auto-complete-c-headers auto-complete auto-indent-mode autopair bash-completion better-defaults bibtex-utils color-theme company-auctex company-c-headers company dired+ display-theme emacs-setup es-lib f fill-column-indicator fish-mode flex-autopair flex-isearch flx-ido flx flycheck flymake flymake-shell flymake-easy highlight-indentation hl-line+ hlinum hungry-delete idle-highlight ido-at-point ido-better-flex ido-hacks ido-ubiquitous ido-vertical-mode ido-yes-or-no indent-guide jgraph-mode latex-extra auctex latex-pretty-symbols latex-preview-pane leuven-theme mic-paren mode-icons nav pkg-info epl popup rainbow-delimiters rainbow-identifiers rainbow-mode readline-complete s sentence-highlight smart-mode-line rich-minority smart-tab smart-tabs-mode smartparens dash smex yasnippet)
      )
(ensure-packages-install-missing)


; recentf stuff
(recentf-mode 1)
(setq recentf-max-menu-items 15) ; show 15 in recent menu
(setq recentf-max-saved-items 100) ; keep track of last 100 files 
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(setq recentf-auto-cleanup 'never)

;; if you want to use ido with recentf
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))


; enable ido mode
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
;;(ido-everywhere 1)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-use-filename-at-point 'guess)
;;(setq ido-show-dot-for-dired t) ; don't show current directory as the first choice
(ido-at-point-mode)

;(add-hook 'ido-setup-hook
;          (lambda ()
;            (define-key ido-completion-map [up] 'previous-history-element)))
(flx-ido-mode 1)
(setq ido-use-faces nil) ; disable ido faces to see flx highlights


; autocomplete
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(global-auto-complete-mode t)

(require 'auto-complete-c-headers)
(add-to-list 'ac-sources 'ac-source-c-headers)


(add-hook 'c-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-c-headers)
            (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))


                                        ; highlight current line number
(require 'hlinum)
(hlinum-activate)


;;(global-smart-tab-mode 1)
(auto-indent-global-mode) ; auto-indentation minor mode
;;(flex-autopair-mode 1)

(require 'indent-guide)
(indent-guide-global-mode)


; whitespace
;; (setq-default indicate-empty-lines t)
;; (show-trailing-whitespace t)
;; (add-hook 'before-save-hook 'whitespace-cleanup)
;(setq whitespace-style '(face empty tabs lines-tail trailing))
;(setq whitespace-style '(face empty tabs lines-tail))
;(set-face-attribute 'whitespace-line nil
;                    :background "red1"
;                    :foreground "yellow"
;                    :weight 'bold)
;(global-whitespace-mode t)


;(add-hook 'java-mode-hook
;          (lambda ()
;            (setq c-basic-offset 2)))


; fci
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
;(global-fci-mode t)
;(setq fci-rule-width 1)
;(setq-default fci-rule-column 120)
;(setq fci-handle-truncate-lines nil)
(defun auto-fci-mode (&optional unused)
  (if (> (frame-width) 120)
      (fci-mode 1)
    (fci-mode 0))
)
;(add-hook 'after-change-major-mode-hook 'auto-fci-mode)
;(add-hook 'window-size-change-functions 'auto-fci-mode)


(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")
;(setq uniquify-buffer-name-style 'reverse) ; another choice


; Spell check
;;(setq flyspell-mode 1) ; this is slow and doesn't work perfectly
;;(turn-on-flyspell 1)
;;(turn-on-auto-fill t)
;;(flyspell-issue-message-flag nil)
(add-hook 'text-mode-hook 'flyspell-mode) ; possibly won't work for extensionless .ascii files
;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'find-file-hooks 'turn-on-flyspell) ; Otherwise flyspell isn't enabled as I want it
(setq-default ispell-program-name "/usr/bin/aspell")
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)


                                        ; rainbow mode
                                        ; To enable it in all programming-related emacs modes (Emacs 24+)
;;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; enable only in programming related-modes
(global-rainbow-delimiters-mode) ; use Emacs-wide


(global-font-lock-mode t)
(setq jit-lock-defer-time 0.05) ; improve scrolling speed with jit fontification


; C-x C-j opens dired with the cursor right on the file you're editing, otherwise
; you can use C-x d, or 'M-x dired'
(require 'dired-x)


; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
;(fullscreen)
;(global-set-key [f11] 'fullscreen)


;;(global-smart-tab-mode 1)


; ergoemacs
;(require 'ergoemacs-mode)
;(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
;(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
;(ergoemacs-mode 1)


                                        ; directory navigation
(add-to-list 'load-path "~/.emacs.d/emacs-nav-49/")
;(nav-mode) ; always start in navigation mode
;(nav-disable-overeager-window-splitting)
(global-set-key [f6] 'nav-toggle) ; set up a quick key to toggle nav


                                        ; preview-latex
(autoload 'LaTeX-preview-setup "preview")
(add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)


(latex-preview-pane-enable) ; latex-preview-pane


                                        ; helm
;(helm-mode t)
;(global-set-key (kbd "C-x C-f") 'helm-find-files)


                                        ; company 
(autoload 'company-mode "company" nil t)
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-auctex)
(company-auctex-init)


                                        ; set up auto-complete in shell mode with company
(require 'readline-complete)
                                        ; set up shell (not eshell) mode
(setq explicit-shell-file-name "bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
(setq comint-process-echoes t)
                                        ; setup auto-completion framework
(push 'company-readline company-backends)
(add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))


; smex
(smex-initialize)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


; flymake syntax-check for shell scripts
(require 'flymake-shell)
(add-hook 'sh-set-shell-hook 'flymake-shell-load)


(autopair-global-mode 1) ; auto pair parentheses


(defalias 'list-buffers 'ibuffer)
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-always-show-last-buffer nil)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-use-header-line t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes (quote ("3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(display-time-mode t)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


                                        ; mode-line
(setq sml/theme 'automatic)
(sml/setup)


(global-hungry-delete-mode) ; erase 'all' consecutive white space characters in a given direction

