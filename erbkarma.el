;;; erbkarma.el --- karma is not currently functional, we think..
;; Time-stamp: <2007-11-23 11:30:09 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbkarma.el
;; Package: erbkarma
;; Authors: D. Goel <deego@gnufans.org> 
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot


 
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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


;;; 2004-01-22 T07:18:36-0500 (Thursday)    D. Goel
;; <kensanata> deego: fsbot should get the old silly karma system back.  fsbot
;;             forget all karma points whenever he disconnects.  the only
;;             important thing is that we can say b0ef++ and fsbot responds with
;;             "Noted, kensanata.  One (brownie|karma|wiki|rms|lispy)-point for
;;             b0ef!"

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
   ((not entity) (fs-karma-best))
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
(defalias 'fs-best-karma 'fs-karma-best)

(defcustom erbkarma-tgt-check-string 
  "^\\(#emacs\\|#gnu\\|#fsf\\|#hurd-bunny\\|deego\\|#wiki\\)$"
  "" :group 'erbkarma
  )

(defun erbkarma-tgt-check ()
  (unless (string-match erbkarma-tgt-check-string fs-tgt)
    (error 
     "Do it publicly. ")))

(provide 'erbkarma)
(run-hooks 'erbkarma-after-load-hooks)



;;; erbkarma.el ends here
