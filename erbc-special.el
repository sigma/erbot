;;; erbc-special.el --- Special/dangerous implementation functions.
;; Many fs-functions can simply be defined in terms of other
;; fs-functions (and always should be!, for security.)
;; This file is for the remaining few, that can't be.

;; Time-stamp: <2004-04-22 21:52:48 deego>
;; Copyright (C) 2004 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc-special.el
;; Package: erbc-special
;; Author: D. Goel <deego@glue.umd.edu>
;; Keywords:
;; Version:
;; URL: http://gnufans.net/~deego
;; For latest version:

(defconst erbc-special-home-page
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
(defconst erbc-special-quick-start
  "Help..."
)

(defun erbc-special-quick-start ()
  "Provides electric help from variable `erbc-special-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc-special-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst erbc-special-introduction
  "Help..."
)

;;;###autoload
(defun erbc-special-introduction ()
  "Provides electric help from variable `erbc-special-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc-special-introduction) nil) "*doc*"))

;;; Commentary:
(defconst erbc-special-commentary
  "Help..."
)

(defun erbc-special-commentary ()
  "Provides electric help from variable `erbc-special-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc-special-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defconst erbc-special-new-features
  "Help..."
)

(defun erbc-special-new-features ()
  "Provides electric help from variable `erbc-special-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc-special-new-features) nil) "*doc*"))

;;; TO DO:
(defconst erbc-special-todo
  "Help..."
)

(defun erbc-special-todo ()
  "Provides electric help from variable `erbc-special-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc-special-todo) nil) "*doc*"))

(defconst erbc-special-version "0.0-DUMMY")
(defun erbc-special-version (&optional arg)
   "Display erbc-special's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "erbc-special version %s" erbc-special-version))
    (message "erbc-special version %s" erbc-special-version)))

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defgroup erbc-special nil
  "The group erbc-special."
  :group 'applications)
(defcustom erbc-special-before-load-hook nil
  "Hook to run before loading erbc-special."
  :group 'erbc-special)
(defcustom erbc-special-after-load-hook nil
  "Hook to run after loading erbc-special."
  :group 'erbc-special)
(run-hooks 'erbc-special-before-load-hook)

(defcustom erbc-special-verbosity 0
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'erbc-special)
(defcustom erbc-special-interactivity 0
  "How interactive to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'erbc-special)
(defcustom erbc-special-y-or-n-p-function 'erbc-special-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `erbc-special-y-or-n-p'."
  :type 'function
  :group 'erbc-special)
(defcustom erbc-special-n-or-y-p-function 'erbc-special-y-or-n-p
  "Function to use for interactivity-dependent `n-or-y-p'.
Format same as that of `erbc-special-n-or-y-p'."
  :type 'function
  :group 'erbc-special)
(defun erbc-special-message (points &rest args)
  "Signal message, depending on POINTS anderbc-special-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points erbc-special-verbosity))
    (apply #'message args)))
(defun erbc-special-y-or-n-p (add prompt)
  "Query or assume t, based on `erbc-special-interactivity'.
ADD is added to `erbc-special-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbc-special-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun erbc-special-n-or-y-p (add prompt)
  "Query or assume t, based on `erbc-special-interactivity'.
ADD is added to `erbc-special-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbc-special-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:



(provide 'erbc-special)
(run-hooks 'erbc-special-after-load-hook)



;;; erbc-special.el ends here
