;;; init-corfu.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/capf)

;; Corfu is not a completion framework, it is just a front-end for `completion-at-point'.
(use-package
  corfu
  :preface
  (defun sb/corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))

  (defun sb/corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))
  :straight
  (corfu
    :files (:defaults "extensions/*")
    :includes (corfu-quick corfu-echo corfu-indexed corfu-popupinfo corfu-history))
  :if (eq sb/capf 'corfu)
  :hook (emacs-startup-hook . global-corfu-mode)
  :bind
  (:map
    corfu-map
    ([remap move-beginning-of-line] . sb/corfu-beginning-of-prompt)
    ([remap move-end-of-line] . sb/corfu-end-of-prompt)
    ([escape] . corfu-quit))
  :custom
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-auto t "Enable auto completion")
  ;; (corfu-auto-delay 0.1 "Recommended to not use zero for performance reasons")
  ;; (corfu-max-width 60)
  (corfu-bar-width 0 "See if this helps with corfu-terminal wrap around")
  :config
  ;; The goal is to use a smaller prefix for programming languages to get faster auto-completion,
  ;; but the popup wraps around with `corfu-terminal-mode' on TUI Emacs. This mostly happens with
  ;; longish completion entries. Hence, a larger prefix can limit to more precise and smaller
  ;; entries.
  (add-hook 'prog-mode-hook (lambda () (setq-local corfu-auto-prefix 3))))

(use-package
  corfu-info
  :straight nil
  :after corfu
  :bind (:map corfu-map ("M-d" . corfu-info-documentation) ("M-l" . corfu-info-location)))

;; The indexed mode uses numeric prefix arguments, e.g., "C-0 RET" or "C-1 TAB".
(use-package
  corfu-indexed
  :straight nil
  :disabled t
  :after corfu
  :commands corfu-indexed-mode
  :init (corfu-indexed-mode 1)
  :config
  ;; Bind "C-num" and "M-num" for convenience.
  ;; https://github.com/minad/corfu/issues/231

  ;; (use-package loopy-iter
  ;;   :straight (loopy :type git :host github :repo "okamsn/loopy")
  ;;   :demand t)

  ;; (loopy-iter
  ;;  (with (map corfu-map))
  ;;  (numbering i :from 1 :to 9)
  ;;  (let ((idx i))
  ;;    (define-key map (kbd (format "M-%d" i))
  ;;      (lambda () (interactive)
  ;;        (let ((current-prefix-arg idx))
  ;;          (call-interactively #'corfu-insert))))
  ;;    (define-key map (kbd (format "C-%d" i))
  ;;      (lambda () (interactive)
  ;;        (let ((current-prefix-arg idx))
  ;;          (call-interactively #'corfu-complete))))))
  )

(use-package
  corfu-quick
  :straight nil
  :after corfu
  :bind (:map corfu-map ("C-'" . corfu-quick-insert)))

;; We do not need this if we use prescient-based sorting.

(use-package
  corfu-history
  :straight nil
  :after (corfu savehist)
  :commands corfu-history-mode
  :init
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (corfu-history-mode 1))

(use-package
  corfu-echo
  :straight nil
  :after corfu
  :commands corfu-echo-mode
  :init (corfu-echo-mode 1))

(use-package
  corfu-popupinfo
  :straight nil
  :after corfu
  :commands corfu-popupinfo-mode
  :init (corfu-popupinfo-mode 1))

(use-package
  popon
  :straight (:host codeberg :repo "akib/emacs-popon")
  :if (and (eq sb/capf 'corfu) (not (display-graphic-p))))

(use-package
  corfu-terminal
  :straight (:host codeberg :repo "akib/emacs-corfu-terminal")
  :if (and (eq sb/capf 'corfu) (not (display-graphic-p)))
  :hook (corfu-mode-hook . corfu-terminal-mode)
  :custom (corfu-terminal-position-right-margin 10))

;; Here is a snippet to show how to support `company' backends with `cape'.
;; https://github.com/minad/cape/issues/20
;; (fset #'cape-path (cape-company-to-capf #'company-files))
;; (add-hook 'completion-at-point-functions #'cape-path)

(use-package
  cape
  :preface
  ;; https://kristofferbalintona.me/posts/202203130102/
  (defun sb/cape-capf-setup-lsp ()
    "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version. Also add `cape-file' and
`company-yasnippet' backends."
    (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
      (cape-capf-buster #'lsp-completion-at-point))
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t))
  :after corfu
  :demand t
  :commands
  (cape-history ; Complete from Eshell, Comint or minibuffer history
    cape-file
    cape-keyword ; Complete programming language keyword
    cape-tex ; Complete unicode char from TeX command, e.g. \hbar.
    cape-abbrev ; Complete abbreviation at point
    cape-dict ; Complete word from dictionary at point
    cape-line ; Complete current line from other lines in buffer
    cape-symbol ; Elisp symbol
    cape-elisp-block ; Complete Elisp in Org or Markdown code block
    cape-ispell ; Complete word at point with Ispell
    cape-dabbrev ; Complete with Dabbrev at point
    cape-capf-buster
    cape-company-to-capf
    cape-super-capf
    sh-completion-at-point-function
    comint-completion-at-point
    citre-completion-at-point
    TeX--completion-at-point)
  :init
  ;; Initialize for all generic languages that are not specifically handled
  (add-to-list 'completion-at-point-functions #'cape-file 'append)
  (add-to-list 'completion-at-point-functions (cape-super-capf #'cape-dabbrev #'cape-dict) 'append)
  :custom (cape-dabbrev-min-length 3)
  ;; Checking all other buffers for completetion ignoring the major mode seems to be expensive
  (cape-dabbrev-check-other-buffers nil)
  (cape-dict-file
    `
    (,(expand-file-name "wordlist.5" sb/extras-directory)
      ,(expand-file-name "company-dict/text-mode" user-emacs-directory)))
  :config
  ;; https://github.com/minad/cape/issues/53
  ;; Override CAPFS for specific major modes

  (add-hook
    'emacs-lisp-mode-hook
    (lambda ()
      (setq-local completion-at-point-functions
        (list
          (cape-super-capf
            #'elisp-completion-at-point
            #'citre-completion-at-point
            #'cape-file
            #'cape-symbol ; Elisp symbols
            #'cape-dabbrev
            (cape-capf-inside-string #'cape-dict)
            (cape-capf-inside-comment #'cape-dict))))))

  ;; FIXME: How can we simplify the following mess?

  (with-eval-after-load "lsp-mode"
    (add-hook
      'sh-mode-hook
      (lambda ()
        (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
              (list
                (cape-super-capf
                  #'lsp-completion-at-point
                  #'citre-completion-at-point
                  #'cape-file
                  #'cape-dabbrev
                  (cape-capf-inside-string #'cape-dict)
                  (cape-capf-inside-comment #'cape-dict))))))))

    (dolist (lsp-prog-mode '(c++-mode-hook java-mode-hook python-mode-hook))
      (add-hook
        lsp-prog-mode
        (lambda ()
          (add-hook
            'lsp-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                (list
                  (cape-super-capf
                    #'lsp-completion-at-point
                    #'citre-completion-at-point
                    #'cape-file
                    #'cape-keyword
                    #'cape-dabbrev
                    (cape-capf-inside-string #'cape-dict)
                    (cape-capf-inside-comment #'cape-dict)))))))))

    (dolist (modes '(latex-mode-hook LaTeX-mode-hook))
      (add-hook
        modes
        (lambda ()
          (add-hook
            'lsp-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                (list
                  (cape-super-capf
                    ;; #'lsp-completion-at-point #'citre-completion-at-point
                    ;; #'cape-tex ; Leads to unwanted completions
                    #'cape-file #'cape-dabbrev #'cape-dict)))))))))

  ;; (with-eval-after-load "eglot"
  (add-hook
    'sh-mode-hook
    (lambda ()
      (add-hook
        'eglot-managed-mode-hook
        (lambda ()
          (setq-local completion-at-point-functions
            (list
              (cape-super-capf
                #'eglot-completion-at-point
                #'citre-completion-at-point
                ;; #'sh-completion-at-point-function
                #'cape-keyword
                #'cape-file
                #'cape-dabbrev
                (cape-capf-inside-string #'cape-dict)
                (cape-capf-inside-comment #'cape-dict))))))))

  (dolist (prog '(c++-mode-hook java-mode-hook python-mode-hook))
    (add-hook
      prog
      (lambda ()
        (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
              (list
                (cape-super-capf
                  #'eglot-completion-at-point
                  #'citre-completion-at-point
                  #'cape-file
                  #'cape-keyword
                  #'cape-dabbrev
                  (cape-capf-inside-string #'cape-dict)
                  (cape-capf-inside-comment #'cape-dict)))))))))

  (dolist (modes '(latex-mode-hook LaTeX-mode-hook))
    (add-hook
      modes
      (lambda ()
        (add-hook
          'eglot--managed-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
              (list
                (cape-super-capf
                  #'eglot-completion-at-point #'citre-completion-at-point
                  ;; #'cape-tex ; Leads to unwanted completions
                  #'cape-file #'cape-dabbrev #'cape-dict)))))))))

(use-package
  kind-icon
  :if (and (eq sb/corfu-icons 'kind-icon) (display-graphic-p))
  :after corfu
  :demand t
  :commands kind-icon-margin-formatter
  :custom
  (kind-icon-default-face 'corfu-default) ; To compute blended backgrounds correctly
  ;; Prefer smaller icons and a more compact popup
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 0.6))
  (kind-icon-blend-background nil)
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package
  kind-all-the-icons
  :straight (:host github :repo "Hirozy/kind-all-the-icons")
  :if (and (eq sb/corfu-icons 'kind-all-the-icons) (display-graphic-p))
  :after corfu
  :demand t
  :config (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter))

(use-package
  corfu-quick-access
  :straight (:host codeberg :repo "spike_spiegel/corfu-quick-access.el")
  :after corfu
  :init (corfu-quick-access-mode 1))

(provide 'init-corfu)

;;; init-corfu.el ends here
