;;; erblog.el --- 
;; Time-stamp: <02/10/01 09:30:37 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erblog.el
;; Package: erblog
;; Author: D. Goel <deego@glue.umd.edu>
;; Version: 99.99
;; Author's homepage: http://deego.gnufans.org/~deego
;; For latest version: 

(defvar erblog-home-page
  "http://deego.gnufans.org/~deego")


 
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 

;; See also:


;; Quick start:
(defvar erblog-quick-start
  "Help..."
)

(defun erblog-quick-start ()
  "Provides electric help regarding variable `erblog-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erblog-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar erblog-introduction
  "Help..."
)

;;;###autoload
(defun erblog-introduction ()
  "Provides electric help regarding variable `erblog-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erblog-introduction) nil) "*doc*"))

;;; Commentary:
(defvar erblog-commentary
  "Help..."
)

(defun erblog-commentary ()
  "Provides electric help regarding variable `erblog-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erblog-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defvar erblog-new-features
  "Help..."
)

(defun erblog-new-features ()
  "Provides electric help regarding variable `erblog-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erblog-new-features) nil) "*doc*"))

;;; TO DO:
(defvar erblog-todo
  "Help..."
)

(defun erblog-todo ()
  "Provides electric help regarding variable `erblog-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erblog-todo) nil) "*doc*"))

(defvar erblog-version "99.99")

;;==========================================
;;; Code:

(defgroup erblog nil 
  "The group erblog"
   :group 'applications)
(defcustom erblog-before-load-hooks nil "" :group 'erblog)
(defcustom erblog-after-load-hooks nil "" :group 'erblog)
(run-hooks 'erblog-before-load-hooks)


(defvar erblog-active-targets nil
  "This stores the list of targets that have had some activity...

The idea is that the operator sets this to nil (see commands
below).. goes away, comes back and examined this variables to find
out which channels have had activity...
")

(defun erblog-log-target (target &rest stuff)
  (unless (member (format "%s" target)
		  erblog-active-targets)
    (progn
      (add-to-list 'erblog-active-targets
		   (format "%s" target))
      (erblog-show-targets))))
    
;; operator bind to C-c s
(defun erblog-show-targets ()
  (interactive)
  (message (format "%s" erblog-active-targets)))

;; bind to C-c r
(defun erblog-reset-targets ()
  (interactive)
  (message "Nulling.. was %s" erblog-active-targets)
  (setq erblog-active-targets nil))

(provide 'erblog)
(run-hooks 'erblog-after-load-hooks)



;;; erblog.el ends here
