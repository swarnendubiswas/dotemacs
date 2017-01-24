;;; ido-describe-prefix-bindings.el --- describe-prefix-bindings with ido.  -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2015  Tom Hinton

;; Author: Tom Hinton
;; Maintainer: Tom Hinton <t@larkery.com>
;; Version: 1.0.0
;; Keywords: convenience
;; URL: https://github.com/larkery/ido-describe-prefix-bindings.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Another implementation of descbinds with ido; this one offers a
;; mode to directly replace `describe-prefix-bindings', and allows you
;; to execute the bound command. This means you can press C-x C-h, get
;; a list of keys starting with C-x, search it, and then execute one
;; if you want. It's a bit like guide-key but maybe a little simpler?
;; This works well with things like ido-vertical-mode or ido-grid-mode.

;;; Code:

(defgroup ido-describe-prefix-bindings nil
  "Uses ido or other completing-read to let you interactively choose prefix-bound keys."
  :group 'ido)

(defcustom ido-describe-prefix-bindings-fill t
  "Whether or not to fill the command key, name and description. True is better with
ido-grid-mode, ido-vertical-mode, ivy etc. False is better with plain ido."
  :type 'boolean
  :group 'ido-describe-prefix-bindings)

(defun ido-describe-prefix-bindings-starting-with (key)
  (let* ((buffer (current-buffer))
         (prefix (and key (make-vector (1- (length key)) 0)))
         bindings
         choices
         the-command
         (longest-binding 0)
         (longest-command 0)
         re)

    (dotimes (i (length prefix))
      (aset prefix i (aref key i)))

    (setf re
          (rx-to-string `(sequence
                          bol
                          (minimal-match (group ,(key-description prefix)
                                                (one-or-more any)))
                          (maximal-match (one-or-more "\t"))
                          (group (maximal-match (one-or-more (not blank))))
                          eol)))

    (save-excursion
      (with-current-buffer (get-buffer-create " *ido-describe-prefix-bindings*")
        (let ((indent-tabs-mode t))

          (erase-buffer)
          (insert re)
          (describe-buffer-bindings buffer prefix)
          ;; we want to iterate over the lines and think about them
          (save-match-data
            (goto-char 0)
            (search-forward "---")

            (while (search-forward-regexp re nil t 1)
              (ignore-errors
                (let* ((keyname (s-trim (match-string 1)))
                       (command-name (s-trim (match-string 2)))
                       (command (intern-soft command-name)))
                  (when (and (commandp command)
                             (not (eq 'ignore command))
                             (not (eq 'self-insert-command command))
                             (not (s-blank? keyname))
                             (not (string-match (rx bos (or "<remap>"
                                                            "<compose-last-chars>"
                                                            "<mode-line>"
                                                            "<vertical-line>"
                                                            "<header-line>"
                                                            (seq "<"
                                                                 (zero-or-more any)
                                                                 "mouse-"
                                                                 (one-or-more digit)
                                                                 ">")
                                                            )) keyname)))
                    (setf longest-command (max longest-command (length command-name))
                          longest-binding (max longest-binding (length keyname)))
                    (push (list keyname command
                                ;; get first line
                                (car (split-string (or (documentation command)
                                                       "undocumented") "\n"))
                                ) bindings)))))
            (dolist (x bindings)
              (let ((key (nth 0 x))
                    (command (nth 1 x))
                    (description (nth 2 x)))
                (add-face-text-property 0 (length key) 'font-lock-keyword-face nil key)
                (let ((row (concat
                            (if ido-describe-prefix-bindings-fill
                                (s-pad-right longest-binding " " key)
                              key)
                            "  "
                            (if ido-describe-prefix-bindings-fill
                                (s-pad-right longest-command " " (symbol-name command))
                              (symbol-name command))
                            "  "
                            description)))

                  (push (cons row command) choices))))

            (let* ((result
                    (completing-read (if key
                                         (concat "Prefix " (key-description prefix) ": ")
                                       "All commands:")
                                     choices
                                     nil
                                     t))
                   (command (cdr (assoc result choices))))
              (setf the-command command))))))
    (when the-command (call-interactively the-command))))

(defun ido-describe-prefix-bindings (_blah &rest _rest)
  (interactive)
  (ido-describe-prefix-bindings-starting-with (this-command-keys-vector)))

(defun ido-describe-mode-bindings ()
  (interactive)
  (ido-describe-prefix-bindings-starting-with nil))

;;;###autoload
(define-minor-mode ido-describe-prefix-bindings-mode
  "Uses ido to display and execute prefix bindings"
  :global t
  (if ido-describe-prefix-bindings-mode
      (advice-add 'describe-prefix-bindings :around #'ido-describe-prefix-bindings)
    (advice-remove 'describe-prefix-bindings #'ido-describe-prefix-bindings)))

(provide 'ido-describe-prefix-bindings)

;;; ido-describe-prefix-bindings.el ends here
