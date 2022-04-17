;;; early-init.el --- Early Init File. -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defconst sb/emacs-4MB    (*       4 1024 1024))
(defconst sb/emacs-8MB    (*       8 1000 1024))
(defconst sb/emacs-16MB   (*      16 1000 1024))
(defconst sb/emacs-32MB   (*      32 1000 1024))
(defconst sb/emacs-64MB   (*      64 1024 1024))
(defconst sb/emacs-128MB  (*     128 1024 1024))
(defconst sb/emacs-256MB  (*     256 1024 1024))
(defconst sb/emacs-512MB  (*     512 1024 1024))
(defconst sb/emacs-1GB    (*  1 1024 1024 1024))
(defconst sb/emacs-2GB    (*  2 1024 1024 1024))
(defconst sb/emacs-4GB    (*  4 1024 1024 1024))
(defconst sb/emacs-8GB    (*  8 1024 1024 1024))
(defconst sb/emacs-16GB   (* 16 1024 1024 1024))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (defun reset-gc-cons-threshold ()
            (setq gc-cons-threshold 100000000 gc-cons-percentage 0.1)))

;; optimize: set the file-name-handler to nil since regexing is cpu intensive.
(unless (or (daemonp) noninteractive)
  (let ((default-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (defun reset-file-name-handler-alist ()
                (setq file-name-handler-alist default-file-name-handler-alist)))))

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; Set the right directory to store the native compilation cache
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Inhibit package initialize
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Remove some unneeded UI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(internal-border-width . 0) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;;; early-init.el ends here
