;;; erbjavadoc.el --- Learn terms from a url. 
;; Time-stamp:
;; Copyright (C) 2004 Pete Kazmier
;; Emacs Lisp Archive entry
;; Filename: erbjavadoc.el
;; Package: erbjavadoc
;; Author: Pete Kazmier <pete-erbot-dev@kazmier.com>
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot

(defconst erbtrain-home-page
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
(defconst erbjavadoc-quick-start
  "Help..."
)

(defun erbjavadoc-quick-start ()
  "Provides electric help from variable `erbjavadoc-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbjavadoc-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst erbjavadoc-introduction
  "Help..."
)

;;;###autoload
(defun erbjavadoc-introduction ()
  "Provides electric help from variable `erbjavadoc-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbjavadoc-introduction) nil) "*doc*"))

;;; Commentary:
(defconst erbjavadoc-commentary
  "Help..."
)

(defun erbjavadoc-commentary ()
  "Provides electric help from variable `erbjavadoc-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbjavadoc-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defconst erbjavadoc-new-features
  "Help..."
)

(defun erbjavadoc-new-features ()
  "Provides electric help from variable `erbjavadoc-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbjavadoc-new-features) nil) "*doc*"))

(defconst erbjavadoc-version "0.0-DUMMY")
(defun erbjavadoc-version (&optional arg)
   "Display erbjavadoc's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "erbjavadoc version %s" erbjavadoc-version))
    (message "erbjavadoc version %s" erbjavadoc-version)))

;;==========================================
;;; Requires: 
(require 'cl)
(require 'erburl)

;;; Code:

(defgroup erbjavadoc nil
  "The group erbjavadoc."
  :group 'applications)

(defcustom erbjavadoc-before-load-hooks nil
  "Hooks to run before loading erbjavadoc."
  :group 'erbjavadoc)

(defcustom erbjavadoc-after-load-hooks nil
  "Hooks to run after loading erbjavadoc."
  :group 'erbjavadoc)

(run-hooks 'erbjavadoc-before-load-hooks)

(defcustom erbjavadoc-verbosity 0
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'erbjavadoc)

(defcustom erbjavadoc-interactivity 0
  "How interactive to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'erbjavadoc)

(defcustom erbjavadoc-y-or-n-p-function 'erbjavadoc-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `erbjavadoc-y-or-n-p'."
  :type 'function
  :group 'erbjavadoc)

(defcustom erbjavadoc-n-or-y-p-function 'erbjavadoc-n-or-y-p
  "Function to use for interactivity-dependent n-or-y--p.
Format same as that of `erbjavadoc-n-or-y-p'."
  :type 'function
  :group 'erbjavadoc)

(defun erbjavadoc-message (points &rest args)
  "Signal message, depending on POINTS anderbjavadoc-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points erbjavadoc-verbosity))
    (apply #'message args)))

(defun erbjavadoc-y-or-n-p (add prompt)
  "Query or assume t, based on `erbjavadoc-interactivity'.
ADD is added to `erbjavadoc-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbjavadoc-interactivity))
        t
      (funcall 'y-or-n-p prompt)))

(defun erbjavadoc-n-or-y-p (add prompt)
  "Query or assume t, based on `erbjavadoc-interactivity'.
ADD is added to `erbjavadoc-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbjavadoc-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:

;; I need to persist this var somehow, are there any facilities
;; in erbot to do this?  
(defvar erbjavadoc-scraped-urls '()
  "A list of javadoc urls that have been learned already.  This
is used to prevent users from learning a url more than once.")

;; In the meantime until a better way to persist immutable vars
;; is in place, I'll just write out the value to a file.
(defvar erbjavadoc-data-file "~/public_html/data/state-erbjavadoc.el")

(defun erbjavadoc-load-data ()
  (when (file-exists-p erbjavadoc-data-file)
    (ignore-errors (load erbjavadoc-data-file))))

(defun erbjavadoc-save-data ()
  (erbnoc-write-sexps-to-file erbjavadoc-data-file
                              (list `(setq erbjavadoc-scraped-urls
                                           ',erbjavadoc-scraped-urls))))

(erbjavadoc-load-data)

(defvar erbjavadoc-pages '("allclasses-frame.html" "overview-frame.html")
  "The names of the index pages generated by javadoc.  These names
will be appended to a base url and then these pages will be scraped
for terms.")

(defun erbjavadoc-base-url (url)
  "Returns the base url for a given URL.  Strips off any trailing
filename component and/or trailing slash.  Converts the following:

   http://example.com/test/          -> http://example.com/test
   http://example.com/test/name.html -> http://example.com/test
"
  (let ((p (string-match "/\\([^/]+\\.[^/]+\\)?$" url)))
    (if p 
	(substring url 0 p) 
      url)))

(defun fsi-learn-javadocs (url)
  "Add the Java package and class names as terms in the bot's bbdb
with links to the appropriate pages.  A single URL is passed as the
only argument and can only be learned once until its been forgotten.
It should be noted that this command can only be executed via a user
in IRC because in relies on various vars that are in scope when
erbot.el invokes this function."
  (unless (stringp url) (setq url (format "%s" url)))
  (let ((base (erbjavadoc-base-url url)))
    (if (member base erbjavadoc-scraped-urls)
	"That set of javadocs has already been learned."
      (dolist (page erbjavadoc-pages)
	(let ((pageurl (concat base "/" page)))
          ;; See the docsting for erburl-scrape-terms for more
          ;; information on its arguments.  Lack of closures 
          ;; makes this more complicated than need be.
	  (erburl-scrape-terms pageurl
			       ;; Entry parser callback, we use the
			       ;; standard parser and supply it with
                               ;; the appropriate base url to use and
                               ;; limit the terms learned to terms 
                               ;; that don't contain spaces.
			       (lambda (base &rest not-used)
				 (erburl-href-parser base t))
			       ;; Progress callback, the default is 
			       ;; to use 'message, but we want the
			       ;; progress to be sent back to the 
			       ;; user that invoked the command, so
			       ;; we use erbot-reply.
			       (lambda (msg not-used proc nick tgt)
				 (save-excursion
				   (set-buffer (process-buffer proc))
				   (erbot-reply msg proc nick tgt "" nil)))
			       ;; These arguments are passed as
			       ;; extra parameters to our callback 
			       ;; functions. We need these so that
			       ;; we can invoke erbot-reply.
			       (list base proc nick tgt))))
      (push base erbjavadoc-scraped-urls)
      (erbjavadoc-save-data)
      (format "I'm downloading the javadocs now ..."))))

;; This function should not be made available to users until I can
;; figure out how to make the underlying erburl-forget-terms an
;; asychronous operation.  Currently, if a user invokes this and there
;; are a large number of entries to remove, the operation times out
;; from the top-level timer in erbot (I think)
;;
;; (defun fsi-unlearn-javadocs (url)
;;   "Remove all terms and entries for the URL specified.  This will
;; remove the appropriate entries from the bbdb.  If an entry has more
;; than one definition, only the relevant entry is removed."
;;   (unless (stringp url) (setq url (format "%s" url)))
;;   (let ((base (erbjavadoc-base-url url)))
;;     (if (not (member base erbjavadoc-scraped-urls))
;; 	"This set of javadocs has not been learned."
;;       (let ((count (erburl-forget-terms base)))
;; 	(setq erbjavadoc-scraped-urls (remove base erbjavadoc-scraped-urls))
;;         (erbjavadoc-save-data)
;; 	(format "I have removed %S entries for %S" count base)))))

(defun fsi-learned-javadocs ()
  "Return a list of learned javadocs."
  (cond ((= 0 (length erbjavadoc-scraped-urls))
	 "I have not learned any javadocs.")
	(t
	 (format "I know about the following javadocs: %s" 
		 (mapconcat 'identity erbjavadoc-scraped-urls ", ")))))

(provide 'erbjavadoc)
(run-hooks 'erbjavadoc-after-load-hooks)

;;; erbjavadoc.el ends here