;;; erbbdb.el --- 
;; Time-stamp: <2005-01-02 16:43:59 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbbdb.el
;; Package: erbbdb
;; Author: D. Goel <deego@gnufans.org>
;; Version: 99.99
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
 

(defvar erbbdb-home-page
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
(defvar erbbdb-quick-start
  "Help..."
)

(defun erbbdb-quick-start ()
  "Provides electric help regarding variable `erbbdb-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbbdb-quick-start) nil) "*doc*"))

(defvar erbbdb-version "99.99")

;;==========================================
;;; Code:
(ignore-errors (require 'bbdb))
(ignore-errors (require 'bbdb-com))
(ignore-errors (require 'bbdb-hooks))

(require 'erbc)

(defgroup erbbdb nil 
  "The group erbbdb"
   :group 'applications)
(defcustom erbbdb-before-load-hooks nil "." :group 'erbbdb)
(defcustom erbbdb-after-load-hooks nil "" :group 'erbbdb)
(run-hooks 'erbbdb-before-load-hooks)


(defun erbbdb-get-exact-notes (string)
  (erbbdb-get-regexp-notes (concat "^" (regexp-quote 
					(erbbdb-frob-main-entry string)
					) "$")))

(defun erbbdb-get-exact-name (string)
  (erbbdb-get-regexp-name (concat "^" (regexp-quote 
					(erbbdb-frob-main-entry string)
					) "$")))



(defun erbbdb-get-regexp-record (expr)
  "dsfdfdf"
  (let ((records
	 (bbdb-search (bbdb-records)
	  expr)))
    (first records)))

(defun erbbdb-get-record (str)
  (erbbdb-get-regexp-record 
   (concat "^" (regexp-quote 
		(erbbdb-frob-main-entry str)) "$")))

(defun erbbdb-get-regexp-name (expr)
  "used to get exact name, eg: the exact name of tmpa may be TmpA."
  (let ((record (car 
		 ;; this basically does an M-x  bbdb-name 
		 (bbdb-search (bbdb-records)
			      expr))))
    (if record
	(aref record 0)
      nil)))

(defun erbbdb-get-regexp-notes (expr)
  "currently: Assumes that there will be only one match for the expr
in bbdb...  Discards any further matches... 

If the notes are (), we want it to return nil, not a string.. so that
the calling function knows there's (effectively) no such record...

That is why we have the read below..

This of course, also means that the notes field had better contain a
lisp sexp.. and anythign after the sexp gets discarded...

If record exists but no notes exist, \"\" is returned. 
Else the string containing the notes is returned.
If no record exists, then a nil is returned. 
"
  (let ((record (car 
		 ;; this basically does an M-x  bbdb-name 
		 (bbdb-search (bbdb-records)
			      expr))))
    (if record
	(let* ((notes-notes (assq 'notes (bbdb-record-raw-notes record)))
	       (notes-string (cdr notes-notes)))
	  (or notes-string "")
	       ;;(if foo (read foo) nil)
	  )
      nil)))


(defun erbbdb-frob-main-entry (givenname)
    (let* ((sname (format "%s" givenname))
	   ;;(dname (downcase sname))
	   (dname sname)
	   (bname (split-string dname))
	   (name (mapconcat 'identity bname "-")))
      name))

(defun erbbdb-change (givenname notes)
  "also used by other functions in here.."
  
  (bbdb-records)

  (let* ((sname (format "%s" givenname))
	 ;;(dname (downcase sname))
	 (dname sname)
	 (bname (split-string dname))
	 (name (mapconcat 'identity bname "-")))
    ;;(let ((record
    ;;	 (vector 
    ;;	  ;; first name
    ;;	  name
    ;;	  ;;lastname
    ;;	  nil
    ;;	  nil
    ;;	  nil ;;company
    ;;	  nil ;;phones
    ;;	  nil ;; addrs
    ;;	  nil ;;net
    ;;	  (format "%s" notes)
    ;;	;  (make-vector bbdb-cache-length nil))))
    ;;    (bbdb-change-record record t))
    (let* ((record (erbbdb-get-record name)))
      (bbdb-record-set-notes record notes)
      (bbdb-change-record record t)
      (bbdb-save-db))))

(defun erbbdb-save ()
  (when 
      erbbdb-save-p
      (bbdb-save-db)))

(defvar erbbdb-save-p t
  "Should normally be t, except inside special constructions. ")

  
(defun erbbdb-create (name newnotes)
  "also used by other functions in here.."
  (bbdb-records)
  (let ((record
	 (vector 
	  ;; first name
	  name
	  ;;lastname
	  nil
	  nil
	  nil ;;company
	  nil ;;phones
	  nil ;; addrs
	  nil ;;net
	  nil ;; (format "%s" newnotes)
	  (make-vector bbdb-cache-length nil))))
    (bbdb-record-set-notes record nil)
    (mapcar '(lambda (arg)
	       (erbbdb-add name arg))
	    newnotes)
    )
  (bbdb-save-db))

(defun erbbdb-add (name note)
  (bbdb-records)
  (let* ((oldnotes
	  (erbbdb-get-exact-notes name))
	 (newnotes nil))

    ;; should almost always be the case.. except when nil..
    (if (stringp oldnotes)
	(setq oldnotes 
	      (ignore-errors (read oldnotes))))
    (setq newnotes (format "%S" (append oldnotes (list note))))
    (erbbdb-remove-not-really name)
    (erbbdb-change name newnotes)))


(defun erbbdb-remove-not-really (name)
  (erbbdb-change name nil))
(defun erbbdb-remove (givenname)
  "Remove the record implied by givenname from bbdb.."
  ;;(erbbdb-change name nil)
  (bbdb-records)
  (let* ((sname (format "%s" givenname))
	 ;;(dname (downcase sname))
	 (dname sname)
	 (bname (split-string dname))
	 (name (mapconcat 'identity bname "-")))
    (let* ((record (erbbdb-get-record name)))
      (when record
	(bbdb-delete-current-record record t)
	;;(bbdb-record-set-notes record notes)
	;;(bbdb-change-record record t)
	(bbdb-save-db)))))

(provide 'erbbdb)
(run-hooks 'erbbdb-after-load-hooks)



;;; erbbdb.el ends here
