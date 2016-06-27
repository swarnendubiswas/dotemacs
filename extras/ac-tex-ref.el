;;; ac-tex-ref.el --- auto-complete for tex-mode \ref and \cite labels

;; Copyright 2013, 2014, 2015 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 3
;; Keywords: wp, TeX, LaTeX, auto-complete
;; URL: http://user42.tuxfamily.org/ac-tex-ref/index.html

;; ac-tex-ref.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ac-tex-ref.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This is an auto-complete.el add-on with completion sources for label
;; names in LaTeX \ref{} and \cite{}.
;;
;; Label names and cite names are found from \label{} and \bibitem{}s in the
;; current buffer.  There's no support for multi-file documents.  (AUCTeX
;; and RefTeX have things for that, and for new label creating macros too.)

;;; Install:
;;
;; Put ac-tex-ref.el in one of your `load-path' directories and put
;; `ac-source-tex-ref' and/or `ac-source-tex-cite' into `ac-sources' with
;; something like
;;
;;     (add-hook 'tex-mode-hook
;;               (lambda ()
;;                 (require 'ac-tex-ref)
;;                 (set (make-local-variable 'ac-sources)
;;                      '(ac-source-tex-ref
;;                        ac-source-tex-cite))))
;;
;; auto-complete.el (as of its version 1.3.1) does not enable itself in
;; `tex-mode' by default.  M-x auto-complete-mode can be used to try it
;; temporarily.  To have it permanently add to `ac-modes' as described in
;; the auto-complete.el manual under "Enable auto-complete-mode
;; automatically for specific modes".

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - require 'thingatpt
;; Version 3 - new email


;;; Code:

(require 'thingatpt)

;; Docs:
;; /usr/share/doc/auto-complete-el/doc/manual.txt

;;-----------------------------------------------------------------------------
;; ac-source-tex-ref   \ref{} completion of names from \label{}

(defun ac-tex-ref-labels-list ()
  "Return a list of \label{} names in the current buffer.
This is an internal part of ac-tex-ref.el.
If the buffer contains for example

    \\=\\label{foo}
    \\=\\label{bar}

then the return is a list (\"foo\" \"bar\").  Commented-out
labels are not returned."

  (let (ret)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\
\\(\\\\*%\\)\
\\|\
\\\\label{\\([^}\r\n]+\\)"
                                nil t)
        (cond ((match-beginning 1)
               ;; % comment
               ;; Group 1 is \\\% with preceding backslashes.
               ;; If group 1 is odd length then there's an even backslashes
               ;; which means an unescaped % which is a comment to end-of-line.
               (if (= 1 (logand 1 (- (match-end 1) (match-beginning 1))))
                   (end-of-line)))
              (t
               ;; \label{name}
               (add-to-list 'ret (match-string 2))))))
    (nreverse ret)))

(defun ac-tex-ref-beginning-of-ref-name-position ()
  "Return the position of the start of a \\=\\ref{} name at point.
This is an internal part of ac-tex-ref.el.

If point is in the name part of one of the various \\=\\ref forms
described in `ac-source-tex-ref' then return the position of the
start of that name.  If point is not in such a form then return
nil.

    \\=\\ref{name...
         ^---return

    \\=\\indexentry{text}{name...
                      ^---return

    \\=\\vref[options]{name}
                   ^---return

    \\=\\hyperref[name]{text}
              ^---return

\[] is taken to be options and the text part in {}, except
\\=\\hyperref is [name], followed by the display text."

  (save-excursion
    (let ((orig-point (point)))
      (save-restriction
        (narrow-to-region (line-beginning-position) (point))
        (and (re-search-backward
              "\\\\\
\\(\
\\(\\(lc\\)?name\\)?\
\\([vVcC]\\)?\
\\([fF]ull\\)?\
\\(auto\\)?\
\\(eq\\|th\\|hyper\\)?\
\\(page\\)?\
\[rR]ef\
\\(range\\|s\\)?\
\\|\
\\(glossary\\|index\\)entry\
\\)\
\\*?\
\\(\\[[^]]*\\]\\)?\
\\({[^}]*}\\)?\
\\(\
\{[^}]*\
\\|\
\\[[^]]*\
\\)"
              nil ;; bound
              t)  ;; no error

             (equal (match-end 0) (point-max))

             ;; if options part then point is in the text part of
             ;; \hyperref[name]{text}
             (not (and (match-beginning 11)
                       (string-equal (match-string 1) "hyperref")))

             (progn
               (goto-char (point-max))
               (search-backward ","
                                (match-beginning 13) ;; bound
                                'bound)  ;; point at bound if no match
               (1+ (point))))))))

(defvar ac-source-tex-ref
  '((candidates . ac-tex-ref-labels-list)
    (prefix     . ac-tex-ref-beginning-of-ref-name-position)
    (requires   . 0))
  "auto-complete.el source for LaTeX \\=\\ref{} names.
This source completes the name part of \\=\\ref{} and similar
commands by offering names defined by \\=\\label{} forms in the
buffer.

    \\=\\ref{name}                     \\=\\
    \\=\\pageref{name}                 | LaTeX
    \\=\\indexentry{text}{name}        |
    \\=\\glossaryentry{text}{name}     /

    \\=\\eqref{name}                   amsmath

    \\=\\ref*{name}                    \\=\\
    \\=\\pageref*{name}                | hyperref variations
    \\=\\autoref{name}                 |
    \\=\\autoref*{name}                |
    \\=\\autopageref{name}             |
    \\=\\autopageref*{name}            |
    \\=\\hyperref[name]{...}           /

    \\=\\cref{name,name}               \\=\\
    \\=\\Cref{name}                    | cleveref variations
    \\=\\cpageref{name}                |
    \\=\\Cpageref{name}                |
    \\=\\crefrange{name}{name}         /

    \\=\\vref{name                     varioref variations

    \\=\\thref{name                    ntheorem

The \"requires\" field is 0 so completion is offered immediately
after \\=\\ref{ is typed.  This is convenient for the usual case
of a short to moderately long list of labels.

Names separated by commas in the style of the cleveref package
are completed individually.  For the \"range\" forms of cleveref
each of the two {name} parts are completed.

----
The auto-complete.el home page is
URL `http://cx4a.org/software/auto-complete/'

The ac-tex-ref.el home page is
URL `http://user42.tuxfamily.org/ac-tex-ref/index.html'")


;;-----------------------------------------------------------------------------
;; ac-source-tex-cite   \cite complete from \bibitem

(defun ac-tex-ref-bibitem-list ()
  "Return a list of \\=\\bibitem{} names in the current buffer.
This is an internal part of ac-tex-ref.el.
If the buffer contains for example

    \\=\\bibitem{Foo} ...

then the return is a list (\"Foo\").  Commented-out bibitems are
ignored."

  (let (ret)
    (save-excursion
      (goto-char (point-min))
      ;; no match of empty \bibitem{} which it seems some authors use when
      ;; doing manual bibliographic numbering
      (while (re-search-forward "\
\\(\\\\*%\\)\
\\|\
\\\\bibitem{\\([^}\r\n]+\\)"
                                nil t)

        (cond ((match-beginning 1)
               ;; %
               ;; Group 1 includes preceding backslashes \\\%
               ;; If group 1 is odd length then there's an even backslashes
               ;; which means an unescaped % which is a comment to end-of-line.
               (if (= 1 (logand 1 (- (match-end 1) (match-beginning 1))))
                   (end-of-line)))
              (t
               ;; \bibitem{name}
               (add-to-list 'ret (match-string 2))))))
    (nreverse ret)))

(defun ac-tex-ref-beginning-of-cite-name-position ()
  "Return the position of the start of a \\=\\cite{} name at point.
This is an internal part of ac-tex-ref.el.

    \\=\\cite{name...
          ^---return

If point is in the \"name\" part then return the position of the
start of that name.  If point is not in such a form then return
nil.  Subcitation parts [] are skipped,

    \\=\\cite[subcite]{name...
                   ^---return

Multiple citations with commas in the style of the natbib package
are recognised and the return is the start of the name containing
point

    \\=\\citep{one,two...
               ^---return"

  (save-excursion
    (let ((orig-point (point)))
      (save-restriction
        (narrow-to-region (line-beginning-position) (point))
        (and (thing-at-point-looking-at "\
\\\\\\([Cc]ite\
\\(\\(al\\)?[pt]?\\|author\\fullauthor\\|year\\(par\\)?\\|[pt]?alias\\)\\)\
\\*?\
\\(\\[[^]]*\\]\\)?\
{\
\\([^}\r\n]*,\\)*\
\\([^}\r\n]*\\)"))
        (match-beginning 7)))))

(defvar ac-source-tex-cite
  '((candidates . ac-tex-ref-bibitem-list)
    (prefix     . ac-tex-ref-beginning-of-cite-name-position)
    (requires   . 0))
  "auto-complete.el source for LaTeX \cite{} names.
This source completes the name part of \\=\\cite{} and similar
commands by offering names defined by \\=\\bibitem{} entries in
the current buffer.

    \\=\\cite{name}

    \\=\\citet{name}           \\=\\
    \\=\\citep{name}           | natbib variations
    \\=\\citealt*{name}        |
    \\=\\citealp*{name}        |
    \\=\\citeauthor{name}      |
    etc                    /

The \"requires\" field in the source is 0 so completion is
offered immediately after typing \\=\\cite{.  This is convenient
for the usual case that there's only a short to moderate list of
bibitems.

----
The auto-complete.el home page is
URL `http://cx4a.org/software/auto-complete/'

The ac-tex-ref.el home page is
URL `http://user42.tuxfamily.org/ac-tex-ref/index.html'")


;;-----------------------------------------------------------------------------

;;  LocalWords:  bibitem foo Foo yyy Subcitations subcite pageref eqref
;;  LocalWords:  amsmath indexentry glossaryentry hyperref autoref
;;  LocalWords:  autopageref natbib citep citet citeauthor citealt citealp
;;  LocalWords:  RefTeX Subcitation bibitems unescaped vref cleveref cref
;;  LocalWords:  Cref cpageref Cpageref crefrange ntheorem thref varioref

(provide 'ac-tex-ref)

;;; ac-tex-ref.el ends here
