;;; erburl.el --- Learn terms from a url. 
;; Time-stamp:
;; Copyright (C) 2004 Pete Kazmier
;; Emacs Lisp Archive entry
;; Filename: erburl.el
;; Package: erburl
;; Author: Pete Kazmier <pete-erbot-dev@kazmier.com>
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot

(defconst erbtrain-home-page
  "http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot")
 

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
 

;; See also:


;; Quick start:
(defconst erburl-quick-start
  "This library enables one to add and remove entries to your bot's
bbdb that have been \"scraped\" from sources on the web.  When using
this library, you must be running under the uid of your bot.  In
addition, (although I'm not positive), you should make sure that only
one instance of your bot is running to avoid clobbering the bbdb.  I
use this library in the emacs session of my running bot.  Although the
main function of this library is asynchronous, when adding a lot of
terms, the local session is not really useable (you see the progress
though), however the bot still responds to requests from its channels.

With that all said, lets add the terms from a wiki using the wiki page
name as the term.  The note attached to the term will be a string in
the form of \"at URL\" where URL is the URL to the page:

  (erburl-scrape-terms 
   \"http://www.emacswiki.org/cgi-bin/wiki?action=index\")

This library can be used to add terms from any web page because you
can pass your own parser to ERBURL-SCRAPE-TERMS (see the doc string
for the full details).  This includes parsing pages and adding notes
that contain information other than a simple link back to the original
page.

The library also includes a function to remove all entries that
contain a specific URL in the notes of an entry.  It will only remove
the term in its entirety if it does not contain other notes for the
same term.  It should be noted that this function is not asynchronous
and will cause your bot to stop responding on channels if it is
deleting a large number of records.

Finally, for an additional reference to using this library, please see
erbjavadoc which uses this library to provide a command that will
permit users to add javadoc entries from a set of javadoc pages.
"
  )

(defun erburl-quick-start ()
  "Provides electric help from variable `erburl-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erburl-quick-start) nil) "*doc*"))

(defconst erburl-version "0.0dev")

;;==========================================
;;; Requires: 
(require 'cl)
(require 'url)

;;; Code:

(defgroup erburl nil
  "The group erburl."
  :group 'applications)

(defcustom erburl-before-load-hooks nil
  "Hooks to run before loading erburl."
  :group 'erburl)

(defcustom erburl-after-load-hooks nil
  "Hooks to run after loading erburl."
  :group 'erburl)

(run-hooks 'erburl-before-load-hooks)

;;; Real Code:


(defun erburl-scrape-terms (url &optional entry-parser-fn progress-callback cbargs)
  "Scrape terms from URL using the ENTRY-PARSER-FN and add them to the
erbot's botbbdb. Due to the asynchronous nature of this call, messages
are sent to PROGRESS-CALLBACK to report process.

ENTRY-PARSER-FN is called when the contents of the URL have finished
downloading into a buffer.  The contents of the buffer include any
headers that were sent followed by a blank line and then followed by
the actual contents of the URL.  When ENTRY-PARSER-FN is called, this
buffer has already been selected.  ENTRY-PARSER-FN is passed CBARGS as
arguments, and must return a list of entries to be added to the bbdb.
Each entry should be a list of two elements with the term as the first
element and the definition as the second.  The default parser used if
one is not specified is ERBURL-HREF-PARSER (which parses href links).

PROGRESS-CALLBACK is called once after the entries have been added to
the bbdb with a descriptive message indicating how many terms were
added.  It may also be called after the entries have been parsed with
a message indicating that it will take a significant amount of time to
add the entries to the bbdb.  When PROGRESS-CALLBACK is called, it is
passed a message as the first argument and then CBARGS are passed as
additional arguments.  The default callback used if one is not
specified is MESSAGE.

CBARGS are passed as additional argements to both of the callback
functions.
"
  (let ((parser (or entry-parser-fn 'erburl-href-parser))
	(progress (or progress-callback 'message)))
    (url-retrieve url 
		  'erburl-scrape-callback 
		  (list url parser progress cbargs))))

(defun erburl-scrape-callback (url entry-parser-fn progress-callback cbargs)
  "Callback invoked by url-retrieve.  It is invoked in the buffer with
the contents of the retrieved URL.  In addition, this method is passed
two additional callbacks to assist during processing (please refer to
erburl-scrape-terms doc).  Finally, CBARGS is a list of arguments that
will be passed as additional arguments to the callback functions (I
wish elisp supported lexical closures!)"
  (goto-char (point-min))
  (let* ((buffer (current-buffer))
	 (count 0)
	 (entries (apply entry-parser-fn cbargs))
         (delay 0.1)
         (total (length entries))
         (eta (* total delay)))
    (when (> eta 10)
      (apply progress-callback 
             (format "Processing %d entries from %s will take at least %.1f minutes ..."
                     total url (/ eta 60))
             cbargs))
    (erbot-working
     (dolist (entry entries)
       (message "Adding entry for %s" (first entry))
       (sleep-for 0.1)
       ;; I need to find a way to speed this up.  As the bbdb gets
       ;; larger things really start to slow down significantly.
       (when (or (ignore-errors (apply 'fs-set-also entry))
		 (ignore-errors (apply 'fs-set-term entry)))
	 (incf count))))
    (erbbdb-save)
    (apply progress-callback 
           (format "Added %d entries from %s." count url)
	   cbargs)
    (kill-buffer buffer)))

;; This needs to be asynchronous if we are to make an fsi-* version
;; for IRC users to execute because this function is very slow when
;; removing a large number of entries from the bbdb.
(defun erburl-forget-terms (url)
  "Remove all terms and entries for the specified URL.  This will
remove terms from the bbdb entirely unless a particular term has more
than one entry, in which case, only the relevant entry is removed.
Note: this function is not asynchronous and will cause your bot to
stop responding on channels if it is removing a large number of
entries that match the specified URL."
  (unless (string-match "^https?://[^/]+/" url)
    (error "The specified URL is not well-formed"))
  (let ((count 0)
	(regexp (regexp-quote url))
	(erbforget-interactivity -100))
    (erbot-working 
     (setq count (erbforget-sw regexp nil t)))
    (erbbdb-save)
    count))

(defun erburl-href-parser (&optional base terms-with-spaces-p)
  "Returns a list of lists representing the HTML links in the current
buffer.  Each list is composed of a term and a string indicating the
link which is prefixed with BASE if supplied.  If TERMS-WITH-SPACES-P
is non-nil, only links with single word text will be included."
  (let ((entries '())
        (case-fold-search t))
    (while (re-search-forward 
            (if terms-with-spaces-p
              "<a .*?href=\"\\([^\"]+\\)\".*?>\\(?:<[^>]+>\\)*\\([^ <]+\\)\\(?:<[^>]+>\\)*</a>"
              "<a .*?href=\"\\([^\"]+\\)\".*?>\\(?:<[^>]+>\\)*\\([^<]+\\)\\(?:<[^>]+>\\)*</a>")
	    nil t)
      (push (list (match-string 2) 
		  (concat "at " 
			  (when base (concat base "/"))
			  (match-string 1)))
	    entries))
    entries))

(defun erburl-safe-url (url)
  )

(provide 'erburl)
(run-hooks 'erburl-after-load-hooks)

;;; erburl.el ends here
