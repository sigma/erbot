;;; erbc4.el --- Functions contributed by #emacsers. 
;; Time-stamp: <2003-06-19 08:34:17 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc4.el
;; Package: erbc4
;; Author: D. Goel <deego@glue.umd.edu>
;; Keywords:
;; Version:
;; URL: http://gnufans.net/~deego
;; For latest version:

(defconst erbc4-home-page
  "http://gnufans.net/~deego")


 
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
(defconst erbc4-quick-start
  "Help..."
)

(defun erbc4-quick-start ()
  "Provides electric help from variable `erbc4-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc4-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst erbc4-introduction
  "Help..."
)

;;;###autoload
(defun erbc4-introduction ()
  "Provides electric help from variable `erbc4-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc4-introduction) nil) "*doc*"))

;;; Commentary:
(defconst erbc4-commentary
  "Help..."
)

(defun erbc4-commentary ()
  "Provides electric help from variable `erbc4-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc4-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defconst erbc4-new-features
  "Help..."
)

(defun erbc4-new-features ()
  "Provides electric help from variable `erbc4-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc4-new-features) nil) "*doc*"))

;;; TO DO:
(defconst erbc4-todo
  "Help..."
)

(defun erbc4-todo ()
  "Provides electric help from variable `erbc4-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc4-todo) nil) "*doc*"))

(defconst erbc4-version "0.0-DUMMY")
(defun erbc4-version (&optional arg)
   "Display erbc4's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "erbc4 version %s" erbc4-version))
    (message "erbc4 version %s" erbc4-version)))

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defgroup erbc4 nil
  "The group erbc4."
  :group 'applications)
(defcustom erbc4-before-load-hook nil
  "Hook to run before loading erbc4."
  :group 'erbc4)
(defcustom erbc4-after-load-hook nil
  "Hook to run after loading erbc4."
  :group 'erbc4)
(run-hooks 'erbc4-before-load-hook)

(defcustom erbc4-verbosity 0
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'erbc4)
(defcustom erbc4-interactivity 0
  "How interactive to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'erbc4)
(defcustom erbc4-y-or-n-p-function 'erbc4-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `erbc4-y-or-n-p'."
  :type 'function
  :group 'erbc4)
(defcustom erbc4-n-or-y-p-function 'erbc4-y-or-n-p
  "Function to use for interactivity-dependent `n-or-y-p'.
Format same as that of `erbc4-n-or-y-p'."
  :type 'function
  :group 'erbc4)
(defun erbc4-message (points &rest args)
  "Signal message, depending on POINTS anderbc4-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points erbc4-verbosity))
    (apply #'message args)))
(defun erbc4-y-or-n-p (add prompt)
  "Query or assume t, based on `erbc4-interactivity'.
ADD is added to `erbc4-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbc4-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun erbc4-n-or-y-p (add prompt)
  "Query or assume t, based on `erbc4-interactivity'.
ADD is added to `erbc4-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbc4-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:

(defvar erbnoc-rr-bullet (random 6))

(defun erbc-russian-roulette (&rest ignore)
  (if (>= erbnoc-rr-bullet 5)
      (progn 
	(setq erbnoc-rr-bullet (random 6)) 
	(erbc-describe "rr-bang-kick")) 
    (incf erbnoc-rr-bullet) (erbc-describe "rr-click")))

(defalias 'erbc-RR 'erbc-russian-roulette)
(defalias 'erbc-rr 'erbc-russian-roulette)

(provide 'erbc4)
(run-hooks 'erbc4-after-load-hook)



;;; erbc4.el ends here
