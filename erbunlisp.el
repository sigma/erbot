;;; erbunlisp.el --- Help Simplify functions for nonlisp channels. 
;; Time-stamp: <2003-05-29 09:03:33 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbunlisp.el
;; Package: erbunlisp
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot


(defconst erbunlisp-home-page
  "http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot")


 
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
(defconst erbunlisp-quick-start
  "Help..."
)

(defun erbunlisp-quick-start ()
  "Provides electric help from variable `erbunlisp-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbunlisp-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst erbunlisp-introduction
  "Help..."
)

;;;###autoload
(defun erbunlisp-introduction ()
  "Provides electric help from variable `erbunlisp-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbunlisp-introduction) nil) "*doc*"))

;;; Commentary:
(defconst erbunlisp-commentary
  "Help..."
)

(defun erbunlisp-commentary ()
  "Provides electric help from variable `erbunlisp-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbunlisp-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defconst erbunlisp-new-features
  "Help..."
)

(defun erbunlisp-new-features ()
  "Provides electric help from variable `erbunlisp-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbunlisp-new-features) nil) "*doc*"))

(defconst erbunlisp-version "0.0-DUMMY")
(defun erbunlisp-version (&optional arg)
   "Display erbunlisp's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "erbunlisp version %s" erbunlisp-version))
    (message "erbunlisp version %s" erbunlisp-version)))

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defgroup erbunlisp nil
  "The group erbunlisp."
  :group 'applications)
(defcustom erbunlisp-before-load-hooks nil
  "Hooks to run before loading erbunlisp."
  :group 'erbunlisp)
(defcustom erbunlisp-after-load-hooks nil
  "Hooks to run after loading erbunlisp."
  :group 'erbunlisp)
(run-hooks 'erbunlisp-before-load-hooks)

(defcustom erbunlisp-verbosity 0
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'erbunlisp)
(defcustom erbunlisp-interactivity 0
  "How interactive to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'erbunlisp)
(defcustom erbunlisp-y-or-n-p-function 'erbunlisp-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `erbunlisp-y-or-n-p'."
  :type 'function
  :group 'erbunlisp)
(defcustom erbunlisp-n-or-y-p-function 'erbunlisp-y-or-n-p
  "Function to use for interactivity-dependent `n-or-y-p'.
Format same as that of `erbunlisp-n-or-y-p'."
  :type 'function
  :group 'erbunlisp)
(defun erbunlisp-message (points &rest args)
  "Signal message, depending on POINTS anderbunlisp-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points erbunlisp-verbosity))
    (apply #'message args)))
(defun erbunlisp-y-or-n-p (add prompt)
  "Query or assume t, based on `erbunlisp-interactivity'.
ADD is added to `erbunlisp-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbunlisp-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun erbunlisp-n-or-y-p (add prompt)
  "Query or assume t, based on `erbunlisp-interactivity'.
ADD is added to `erbunlisp-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbunlisp-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:

(defcustom erbunlisp-list
  '((erbc-remove erbc-forget remove))
  "When you type erbunlisp-install, the first entries will get aliased
to second one. 
When you type erbunlisp-uninstall, the first entries will get aliased
to the third one. "
  :group 'erbunlisp)



(defun erbunlisp-install ()
  (interactive)
  (mapcar 
   (lambda (arg)
     (defalias (first arg)
       (second arg)))
   erbunlisp-list))

(defun erbunlisp-uninstall ()
  (interactive)
  (mapcar 
   (lambda (arg)
     (defalias (first arg) (third arg)))
   erbunlisp-list))


(provide 'erbunlisp)
(run-hooks 'erbunlisp-after-load-hooks)



;;; erbunlisp.el ends here
