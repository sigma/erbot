;;; erbdata.el --- 
;; Time-stamp: <2003-05-23 08:43:58 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbdata.el
;; Package: erbdata
;; Author: D. Goel <deego@gnufans.org>
;; Version: 99.99
;; Author's homepage: http://deego.gnufans.org/~deego
;; For latest version: 

(defvar erbdata-home-page
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
(defvar erbdata-quick-start
  "Help..."
)

(defun erbdata-quick-start ()
  "Provides electric help regarding variable `erbdata-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbdata-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar erbdata-introduction
  "Help..."
)

;;;###autoload
(defun erbdata-introduction ()
  "Provides electric help regarding variable `erbdata-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbdata-introduction) nil) "*doc*"))

;;; Commentary:
(defvar erbdata-commentary
  "Help..."
)

(defun erbdata-commentary ()
  "Provides electric help regarding variable `erbdata-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbdata-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defvar erbdata-new-features
  "Help..."
)

(defun erbdata-new-features ()
  "Provides electric help regarding variable `erbdata-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbdata-new-features) nil) "*doc*"))

;;; TO DO:
(defvar erbdata-todo
  "Help..."
)

(defun erbdata-todo ()
  "Provides electric help regarding variable `erbdata-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbdata-todo) nil) "*doc*"))

(defvar erbdata-version "99.99")

;;==========================================
;;; Code:

(defgroup erbdata nil 
  "The group erbdata"
   :group 'applications)
(defcustom erbdata-before-load-hooks nil "" :group 'erbdata)
(defcustom erbdata-after-load-hooks nil "" :group 'erbdata)
(run-hooks 'erbdata-before-load-hooks)

(defvar erbdata-flames
  '(
    "%s: Are you smoking crack?"
    "%s: Is it larger than a breadbox?"
    "What are you smoking, %s?"
    "You are confused, but this is your normal state. "
    ))





(provide 'erbdata)
(run-hooks 'erbdata-after-load-hooks)



;;; erbdata.el ends here
