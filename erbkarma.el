;;; erbkarma.el ---
;; Time-stamp: <2003-05-23 08:43:57 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbkarma.el
;; Package: erbkarma
;; Authors: D. Goel <deego@gnufans.org> 
;; Keywords:
;; Version:
;; Author's homepage: http://deego.gnufans.org/~deego
;; For latest version:

(defconst erbkarma-home-page
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

;;; 2003-01-29 T13:10:42-0500 (Wednesday)    D. Goel
;; removed
;;         Dheeraj Buduru  <dbuduru@yahoo.com>
;; from authors' list at his request. :(

;; See also:


;; Quick start:
(defconst erbkarma-quick-start
  "Help..."
)

(defun erbkarma-quick-start ()
  "Provides electric help from variable `erbkarma-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbkarma-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst erbkarma-introduction
  "Help..."
)

;;;###autoload
(defun erbkarma-introduction ()
  "Provides electric help from variable `erbkarma-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbkarma-introduction) nil) "*doc*"))

;;; Commentary:
(defconst erbkarma-commentary
  "Help..."
)

(defun erbkarma-commentary ()
  "Provides electric help from variable `erbkarma-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbkarma-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defconst erbkarma-new-features
  "Help..."
)

(defun erbkarma-new-features ()
  "Provides electric help from variable `erbkarma-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbkarma-new-features) nil) "*doc*"))

;;; TO DO:
(defconst erbkarma-todo
  "Help..."
)

(defun erbkarma-todo ()
  "Provides electric help from variable `erbkarma-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbkarma-todo) nil) "*doc*"))

(defconst erbkarma-version "0.0-DUMMY")
(defun erbkarma-version (&optional arg)
   "Display erbkarma's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "erbkarma version %s" erbkarma-version))
    (message "erbkarma version %s" erbkarma-version)))

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))
(require 'pp)
(require 'thingatpt)
;;; Code:

(defgroup erbkarma nil
  "The group erbkarma."
  :group 'applications)
(defcustom erbkarma-before-load-hooks nil
  "Hooks to run before loading erbkarma."
  :group 'erbkarma)
(defcustom erbkarma-after-load-hooks nil
  "Hooks to run after loading erbkarma."
  :group 'erbkarma)
(run-hooks 'erbkarma-before-load-hooks)

(defcustom erbkarma-verbosity 0
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'erbkarma)
(defcustom erbkarma-interactivity 0
  "How interactive to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'erbkarma)
(defcustom erbkarma-y-or-n-p-function 'erbkarma-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `erbkarma-y-or-n-p'."
  :type 'function
  :group 'erbkarma)
(defcustom erbkarma-n-or-y-p-function 'erbkarma-y-or-n-p
  "Function to use for interactivity-dependent n-or-y--p.
Format same as that of `erbkarma-n-or-y-p'."
  :type 'function
  :group 'erbkarma)
(defun erbkarma-message (points &rest args)
  "Signal message, depending on POINTS anderbkarma-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points erbkarma-verbosity))
    (apply #'message args)))
(defun erbkarma-y-or-n-p (add prompt)
  "Query or assume t, based on `erbkarma-interactivity'.
ADD is added to `erbkarma-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbkarma-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun erbkarma-n-or-y-p (add prompt)
  "Query or assume t, based on `erbkarma-interactivity'.
ADD is added to `erbkarma-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbkarma-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:


(defcustom erbkarma-file "~/public_html/karma/karma"
  "")

(defcustom erbkarma-min -1000 "")
(defcustom erbkarma-max +1000 "")

(defvar  erbkarma nil
  "stores all karma"
  )


(defun erbkarma-read ()
  (save-window-excursion
   (unless erbkarma
     (setq erbkarma
	   (ignore-errors
	     (find-file erbkarma-file)
	     (goto-char (point-min))
	     (sexp-at-point))))))

(defun erbkarma (&optional entity)
  (cond
   ((not entity) (erbc-karma-best))
   (t
    (unless (stringp entity) 
      (setq entity (format "%s" entity)))
    (erbkarma-read)
    (second
     (assoc entity erbkarma)))))

(defun erbkarma-save ()
  (save-window-excursion
    (find-file erbkarma-file)
    (delete-region (point-min) (point-max))
    (insert (pp-to-string erbkarma))
    (write-file erbkarma-file)
    (kill-buffer (current-buffer))))




(defun erbkarma-increase (entity &optional points)
  (format "%s" entity)
  (erbkarma-tgt-check)
  (unless points (setq points 1))
  (erbkarma-read)
  (let* ((eass (assoc entity erbkarma))
	 (val (if eass (second eass) 0))
	 (newval (+ val points))
	 (removed (remove eass erbkarma)))
    (setq erbkarma
	  (if (= newval 0)
	      removed
	    (cons
	     (list entity newval)
	     removed)))
    (erbkarma-save)
    (format "%s" newval)))

(defun erbkarma-decrease (entity &optional points)
  (erbkarma-tgt-check)
  (unless points (setq points 1))
  (erbkarma-increase entity (- points)))

(defun erbkarma-sort ()
  (setq erbkarma
	(sort
	 erbkarma
	 '(lambda (a b)
	    (> (second a) (second b)))))
  (erbkarma-save))

(defun erbkarma-best (&optional n bottomp)
  (unless n (setq n 5))
  (erbkarma-sort)
  (let ((result (if bottomp
		    (reverse erbkarma)
		  erbkarma)))
    (if (> n (length result))
	result
      (subseq result 0 n))))
(defalias 'erbc-best-karma 'erbc-karma-best)

(defcustom erbkarma-tgt-check-string 
  "^\\(#emacs\\|#gnu\\|#fsf\\|#hurd-bunny\\|deego\\|#wiki\\)$"
  "" :group 'erbkarma
  )

(defun erbkarma-tgt-check ()
  (unless (string-match erbkarma-tgt-check-string erbc-tgt)
    (error 
     "Do it publicly. ")))

(provide 'erbkarma)
(run-hooks 'erbkarma-after-load-hooks)



;;; erbkarma.el ends here
