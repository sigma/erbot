;;; erbc3.el ---erbot lisp stuff which should be PERSISTENT ACROSS SESSIONS.
;; Time-stamp: <2003-06-19 14:12:25 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc3.el
;; Package: erbc3
;; Author: D. Goel <deego@glue.umd.edu>
;; Keywords:
;; Version:

;; URL: http://gnufans.net/~deego
;; For latest version:

(defconst erbc3-home-page
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
(defconst erbc3-quick-start
  "Help..."
)

(defun erbc3-quick-start ()
  "Provides electric help from variable `erbc3-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc3-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst erbc3-introduction
  "Help..."
)

;;;###autoload
(defun erbc3-introduction ()
  "Provides electric help from variable `erbc3-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc3-introduction) nil) "*doc*"))

;;; Commentary:
(defconst erbc3-commentary
  "Help..."
)

(defun erbc3-commentary ()
  "Provides electric help from variable `erbc3-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc3-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defconst erbc3-new-features
  "Help..."
)

(defun erbc3-new-features ()
  "Provides electric help from variable `erbc3-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc3-new-features) nil) "*doc*"))

;;; TO DO:
(defconst erbc3-todo
  "Help..."
)

(defun erbc3-todo ()
  "Provides electric help from variable `erbc3-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc3-todo) nil) "*doc*"))

(defconst erbc3-version "0.0-DUMMY")
(defun erbc3-version (&optional arg)
   "Display erbc3's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "erbc3 version %s" erbc3-version))
    (message "erbc3 version %s" erbc3-version)))

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defgroup erbc3 nil
  "The group erbc3."
  :group 'applications)
(defcustom erbc3-before-load-hook nil
  "Hook to run before loading erbc3."
  :group 'erbc3)
(defcustom erbc3-after-load-hook nil
  "Hook to run after loading erbc3."
  :group 'erbc3)
(run-hooks 'erbc3-before-load-hook)

(defcustom erbc3-verbosity 0
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'erbc3)
(defcustom erbc3-interactivity 0
  "How interactive to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'erbc3)
(defcustom erbc3-y-or-n-p-function 'erbc3-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `erbc3-y-or-n-p'."
  :type 'function
  :group 'erbc3)
(defcustom erbc3-n-or-y-p-function 'erbc3-y-or-n-p
  "Function to use for interactivity-dependent `n-or-y-p'.
Format same as that of `erbc3-n-or-y-p'."
  :type 'function
  :group 'erbc3)
(defun erbc3-message (points &rest args)
  "Signal message, depending on POINTS anderbc3-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points erbc3-verbosity))
    (apply #'message args)))
(defun erbc3-y-or-n-p (add prompt)
  "Query or assume t, based on `erbc3-interactivity'.
ADD is added to `erbc3-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbc3-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun erbc3-n-or-y-p (add prompt)
  "Query or assume t, based on `erbc3-interactivity'.
ADD is added to `erbc3-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbc3-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:
;; pf stands for persistent functions.
;; pv stands for persistent variables.

(defvar erbnoc-pf-file "~/public_html/data/userfunctions.el")
(defvar erbnoc-pv-file "~/public_html/data/uservariables.el")

(defun fs-pfpv-load ()
  (fs-pf-load)
  (fs-pv-load))

(defun fs-pf-load ()
  (when (file-exists-p erbnoc-pf-file)
	 (ignore-errors (load erbnoc-pf-file))))

(defun fs-pv-load ()
  (when (file-exists-p erbnoc-pv-file)
    (ignore-errors (load erbnoc-pv-file))))

	 

(defun fs-user-function-p (fcn)
  (member 
   fcn 
   (erbutils-functions-in-file erbnoc-pf-file)))


(defun erbnoc-create-defun-new (sexps body)
  (cons body sexps))

(defun erbnoc-create-defun-overwrite (sexps body fcn)
  (cons body
	(remove
	 (first (member-if
		 (lambda (arg) (equal (second arg) fcn))
		 sexps))
	 sexps)))

			 

(defun erbnoc-write-sexps-to-file (file sexps)
  (erbutils-mkback-maybe file)
  (find-file file)
  (widen)
  (delete-region (point-min) (point-max))
  (insert "\n\n\n")
  (insert
   (mapconcat
    (lambda (arg) (pp-to-string arg)) sexps "\n\n\n"))
  (insert "\n\n\n")
  (save-buffer))

(defvar erbnoc-tmp-sexps)
(defvar erbnoc-tmp-newbody)

(defmacro fs-defun (fcn &rest body)
  (erbnoc-write-sexps-to-file
   erbnoc-pf-file
   (erbnoc-create-defun-overwrite
    (erbutils-file-sexps erbnoc-pf-file)
    (cons 'defun (cons fcn body)) fcn))
  (fs-pf-load)
  `(quote ,fcn))




	  


(defun fs-pv-get-variables-values ()
  (let 
      ((vars 
	(apropos-internal "^fs-" 'boundp)))
    (mapcar
     (lambda (v)
       (list 'defvar v 
	     (eval v)))
     vars)))



;;;###autoload
(defun fs-pv-save ()
  (interactive)
  (erbnoc-write-sexps-to-file 
   erbnoc-pv-file 
   (fs-pv-get-variables-values)))




(provide 'erbc3)
(run-hooks 'erbc3-after-load-hook)



;;; erbc3.el ends here
