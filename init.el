;; Swarnendu Biswas
;; Wed Jan 14 09:55:00 EST 2015

;; Notes: To evaluate an Sexp, just go to the end of the sexp and type \C-x \C-e, instead of evaluating the whole buffer
;; Init file shouldn't ideally contain calls to load or require, since they cause eager loading and are expensive, a
;; cheaper alternative is to use autoload

;; Citations (inspired from http://www.mygooglest.com/fni/dot-emacs.html)
;;
;;   "Show me your ~/.emacs and I will tell you who you are." - Bogdan Maryniuk
;;   "People talk about getting used to a new editor, but over time, it is precisely the opposite that should happen -
;;    the editor should get used to us." - Vivek Haldar
;;   "Emacs is like a laser guided missile. It only has to be slightly mis-configured to ruin your whole day." - Sean
;;    McGrath


;; Customizing packages
(add-to-list 'load-path "~/.emacs.d/lisp")
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
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
(blink-cursor-mode 1) ;; enable/disable blinking cursor


;; backup
(setq make-backup-files nil) ; stop making backup ~ files
(setq backup-inhibited t) ; disable backup for a per-file basis, not to be used by major modes


;; auto revert
(global-auto-revert-mode 1) ; auto-refresh all buffers, does not work for remote files
(setq-default auto-revert-interval 5) ; default is 5 s
(setq-default auto-revert-verbose nil) 


;; automatically load abbreviations table
(setq-default abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq save-abbrevs t)


;; tooltips
(tooltip-mode -1)


;; enable tabbar minor mode
(setq tabbar-use-images nil) ; speed up by not using images
(tabbar-mode 1)


;; customize appearance

;;  line numbers
(global-hl-line-mode 1) ; highlight current line, turn it on for all modes by default
(global-linum-mode 1) ; display line numbers in margin
(hlinum-activate) ; extension to linum-mode to highlight current line number
(column-number-mode 1)

(tool-bar-mode -1) ; no toolbar with icons
(scroll-bar-mode -1) ; no scroll bars
(menu-bar-mode 1) ; keep menu bar enabled
;; displays the time and date in the mode line
(setq display-time-day-and-date t
      display-time-24hr-format nil)
;;(display-time)
(setq frame-title-format (concat  "%b - emacs@" (system-name))) ;; default to better frame titles


;; these are two nice themes: leuven and professional
(load-theme 'professional t)
;;(set-face-background 'fringe "white") ; Hide the fringe mark on the left
(setq-default indicate-empty-lines t)
(highlight-changes-mode 1)
(setq-default indicate-buffer-boundaries 'right)


(delete-selection-mode 1) ; typing with the mark active will overwrite the marked region
(transient-mark-mode 1) ; enable visual feedback on selections, default since v23
(global-hungry-delete-mode 1) ; erase 'all' consecutive white space characters in a given direction


;; kill all non-special buffers but the current one
(defun kill-other-buffers ()
  "Kill all buffers but the current one. Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))


;; ibuffer
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
(setq case-fold-search t) ; make search ignore case


;; tramp
(setq tramp-default-method "ssh") ; faster than the default scp
(setq tramp-default-user "XXX"
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


;; use desktop save mode
(desktop-save-mode 0) 
(setq-default desktop-restore-frames nil) ; no need to restore frames
(setq-default desktop-load-locked-desktop nil)


;; fully redraw the display before queued input events are processed
(setq redisplay-dont-pause t)


;; Package specific

;; First ensure that a required set of packages are always installed
(require 'ensure-packages)
;; Get a list of currently installed packages (excluding built in packages) with '\C-h v package-activated-list'
(setq ensure-packages
      '(ace-jump-buffer ace-jump-mode aggressive-indent anzu async auctex-latexmk auctex auto-auto-indent auto-highlight-symbol auto-indent-mode autodisass-java-bytecode bash-completion bibtex-utils color-theme company-auctex company company-math ctags dash dired+ dired-rainbow dired-hacks-utils display-theme duplicate-thing es-lib f fill-column-indicator fish-mode fixme-mode flex-autopair flex-isearch flx-ido flx flycheck-color-mode-line flycheck-tip  flycheck epl flymake flymake-shell flymake-easy flyparens ggtags goto-last-change guide-key-tip pos-tip guide-key popwin highlight-indentation highlight-numbers highlight-symbol hl-line+ hlinum hungry-delete icicles idle-highlight idle-highlight-mode ido-at-point ido-better-flex ido-hacks ido-ubiquitous ido-yes-or-no indent-guide javap-mode jgraph-mode jtags latex-extra latex-pretty-symbols latex-preview-pane leuven-theme magic-latex-buffer fringe-helper math-symbol-lists mic-paren mode-icons names nav org parent-mode pkg-info popup professional-theme rainbow-delimiters rainbow-identifiers rainbow-mode readline-complete rich-minority s sentence-highlight smart-mode-line-powerline-theme smart-mode-line powerline smart-tab smart-tabs-mode smartparens smex smooth-scroll tabbar vlf writegood-mode yasnippet)
      )
(ensure-packages-install-missing)


;; speed up emacs for large files
(setq large-file-warning-threshold 50000000) ; warn when opening files bigger than 50MB
(require 'vlf-setup)


;; related to pairing of parentheses, brackets, etc.
(show-paren-mode 1) 
(setq show-paren-style 'parenthesis) ; highlight just brackets, 'expression
(setq-default flyparens-mode t) ; highlight/track mismatched parentheses

;; smart pairing for all, from prelude
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(show-smartparens-global-mode 1)


;; indentation
(electric-indent-mode 1) ; intelligent indentation, on by default from Emacs 24.4
(auto-indent-global-mode 1) ; auto-indentation minor mode
(indent-guide-global-mode 1) ; doesn't seem to work well with transient-mark-mode and auto-complete-mode
(highlight-indentation-mode 1) ; there seems to be an error with derived-mode


;; highlight-symbol at point
(global-auto-highlight-symbol-mode 1)
;;(highlight-symbol-mode 1)
;;(idle-highlight-mode 1) ; idle highlight mode


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
;;(setq ido-use-filename-at-point 'guess) ; other options: 'ffap-guesser
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
;;(setq ido-show-dot-for-dired t) ; don't show current directory as the first choice
(ido-at-point-mode 1)
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Compile-Log*" "Flycheck error messages*"
                           "*Messages*" "Async Shell Command"))
(setq ido-enable-last-directory-history t)
(setq ido-max-work-directory-list 30)
(setq ido-max-work-file-list 50)
(setq confirm-nonexistent-file-or-buffer nil)
(flx-ido-mode 1)
(setq ido-use-faces nil) ; disable ido faces to see flx highlights
(ido-ubiquitous-mode 1) ; allow ido-style completion in more places
(setq ido-use-virtual-buffers t)
(ido-better-flex/enable)


;; recentf stuff
(setq recentf-max-menu-items 15) ; show 15 in recent menu, but currently menu bar is disabled
(setq recentf-max-saved-items 100) ; keep track of last 100 files
(setq recentf-auto-cleanup 'never)
;; save file names relative to my current home directory
(setq recentf-filename-handlers '(abbreviate-file-name))
(recentf-mode 1)
(setq recentf-exclude '("/tmp/" "/ssh:"))


;; smooth scroll
(require 'smooth-scroll)
(smooth-scroll-mode 1)


;; whitespace
(setq-default indicate-empty-lines t)
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
;;(setq uniquify-separator ":")
;; options: post-forward, reverse, forward
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; emacs 24.4 style ⁖ cat.png<dirName>
(setq uniquify-after-kill-buffer-p t)


;; Spell check
(add-hook 'find-file-hooks 'turn-on-flyspell) ; Otherwise flyspell isn't enabled as I want it
(setq-default ispell-program-name "/usr/bin/aspell")
;; Speed up aspell: ultra | fast | normal
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
;; (add-hook 'emacs-startup-hook ; dired-load-hook
;;           (lambda ()
;;             (load "dired-x")))
(setq dired-auto-revert-buffer t) ;; revert each dired buffer automatically when you visit it
(setq dired-recursive-deletes 'always) ; single prompt for all n directories
(setq dired-recursive-copies 'always)
(setq-default diredp-hide-details-initially-flag nil)
(autoload 'dired-jump "dired-x"
  "Jump to dired buffer corresponding to current buffer."
  'interactive)
(setq dired-bind-jump t)


;; smart tabs (indent with tabs, align with spaces)
;;(global-smart-tab-mode 1)
(autoload 'smart-tabs-mode "smart-tabs-mode"
  "Intelligently indent with tabs, align with spaces!")
(autoload 'smart-tabs-mode-enable "smart-tabs-mode")
(autoload 'smart-tabs-advice "smart-tabs-mode")
(autoload 'smart-tabs-insinuate "smart-tabs-mode")
(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml)


;; directory navigation
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-nav-49/")
;;(nav-mode) ; always start in navigation mode
;;(nav-disable-overeager-window-splitting)


;; company
(autoload 'company-mode "company" nil t)
;;(require 'company-auctex)
(company-auctex-init)
(setq company-dabbrev-downcase nil) ;; turn off auto downcasing of things
(setq company-show-numbers t)
(setq company-minimum-prefix-length 2)
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
 '(custom-safe-themes (quote (default)))
 '(diredp-hide-details-initially-flag nil t)
 ;;'(display-time-mode t)
 '(scroll-bar-mode 1)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(vlf-application 'dont-ask))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sml/folder ((t (:inherit sml/global :background "#000000" :foreground "white smoke" :weight normal))))
 '(sml/modes ((t (:inherit sml/global :background "#006666" :foreground "white smoke")))))


;; smart mode line
(setq sml/theme 'powerline) ; options: dark, light, respectful, automatic, powerline
;;(setq sml/name-width 20)
(setq sml/no-confirm-load-theme t)
(sml/setup)
;; flat-looking mode-line
;;(set-face-attribute 'mode-line nil :box nil)
;;(set-face-attribute 'mode-line-inactive nil :box nil)
;;(set-face-attribute 'mode-line-highlight nil :box nil)


;; anzu mode - show number of searches in the mode line
(global-anzu-mode 1)


(icomplete-mode 1) ;; incremental minibuffer completion/suggestions


;; save minibuffer histories across emacs sessions
(setq savehist-additional-variables    
      '(kill-ring search-ring regexp-search-ring)    
      savehist-file "~/.emacs.d/savehist") 
(savehist-mode 1)


;; yasnippet
(yas-global-mode 1)
;;(yas-reload-all 1)


;; text mode  hooks

(setq-default major-mode 'text-mode)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook 'flyspell-mode) ; possibly won't work for extensionless .ascii files
(add-hook 'text-mode-hook 'writegood-mode)
(add-hook 'text-mode-hook 'abbrev-mode)

;; latex mode hooks
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
(add-hook 'LaTeX-mode-hook 'yas-minor-mode)

(setq TeX-auto-save t) ; enable parse on save, stores parsed information in an "auto" directory
(setq TeX-parse-self t) ; enable parse on load
(setq-default TeX-master nil) ; query for master file
(setq TeX-PDF-mode t) ; compile files to pdf by default

;; always use `TeX-default-mode', which defaults to `latex-mode'
(setq TeX-force-default-mode t)

(autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)
(setq reftex-plug-into-AUCTeX t)

(auctex-latexmk-setup) ; add support for latexmk

(setq TeX-electric-sub-and-superscript t) ; automatically insert braces in math mode


;; shell mode hooks

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
(add-hook 'prog-mode-hook #'aggressive-indent-mode)
(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook '(lambda () (yas-minor-mode)))

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
(autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
(add-hook 'java-mode-hook 'jtags-mode)


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
;;(global-set-key (kbd "M-b") 'ace-jump-buffer-with-configuration)
(global-set-key (kbd "M-b") 'ace-jump-buffer)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x C-j") #'dired-jump)

;; M-<left>/<right> is overwritten by 'ahs-backward/forward, which is not useful
(define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
(define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil)
(global-set-key (kbd "M-<left>") 'tabbar-backward-tab)
(global-set-key (kbd "M-<right>") 'tabbar-forward-tab)
