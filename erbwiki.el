;;; erbwiki.el ---
;; Time-stamp: <2003-05-16 07:36:26 deego>
;; Copyright (C) 2002, 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbwiki.el
;; Package: erbwiki
;; Author: D. Goel <deego@glue.umd.edu>
;; Keywords:
;; Version:
;; Author's homepage: http://deego.gnufans.org/~deego
;; For latest version:

(defconst erbwiki-home-page
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
(defconst erbwiki-quick-start
  "Help..."
)

(defun erbwiki-quick-start ()
  "Provides electric help from variable `erbwiki-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbwiki-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst erbwiki-introduction
  "Help..."
)

;;;###autoload
(defun erbwiki-introduction ()
  "Provides electric help from variable `erbwiki-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbwiki-introduction) nil) "*doc*"))

;;; Commentary:
(defconst erbwiki-commentary
  "Help..."
)

(defun erbwiki-commentary ()
  "Provides electric help from variable `erbwiki-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbwiki-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defconst erbwiki-new-features
  "Help..."
)

(defun erbwiki-new-features ()
  "Provides electric help from variable `erbwiki-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbwiki-new-features) nil) "*doc*"))

;;; TO DO:
(defconst erbwiki-todo
  "Help..."
)

(defun erbwiki-todo ()
  "Provides electric help from variable `erbwiki-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbwiki-todo) nil) "*doc*"))

(defconst erbwiki-version "0.0-DUMMY")
(defun erbwiki-version (&optional arg)
   "Display erbwiki's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "erbwiki version %s" erbwiki-version))
    (message "erbwiki version %s" erbwiki-version)))

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defgroup erbwiki nil
  "The group erbwiki."
  :group 'applications)
(defcustom erbwiki-before-load-hooks nil
  "Hooks to run before loading erbwiki."
  :group 'erbwiki)
(defcustom erbwiki-after-load-hooks nil
  "Hooks to run after loading erbwiki."
  :group 'erbwiki)
(run-hooks 'erbwiki-before-load-hooks)

(defcustom erbwiki-verbosity 0
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'erbwiki)
(defcustom erbwiki-interactivity 0
  "How interactive to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'erbwiki)
(defcustom erbwiki-y-or-n-p-function 'erbwiki-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `erbwiki-y-or-n-p'."
  :type 'function
  :group 'erbwiki)
(defcustom erbwiki-n-or-y-p-function 'erbwiki-y-or-n-p
  "Function to use for interactivity-dependent n-or-y--p.
Format same as that of `erbwiki-n-or-y-p'."
  :type 'function
  :group 'erbwiki)
(defun erbwiki-message (points &rest args)
  "Signal message, depending on POINTS anderbwiki-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points erbwiki-verbosity))
    (apply #'message args)))
(defun erbwiki-y-or-n-p (add prompt)
  "Query or assume t, based on `erbwiki-interactivity'.
ADD is added to `erbwiki-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbwiki-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun erbwiki-n-or-y-p (add prompt)
  "Query or assume t, based on `erbwiki-interactivity'.
ADD is added to `erbwiki-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbwiki-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:

(defcustom erbwiki-index-pages
  '(

    ("ai"
     "http://www.gnufans.net/cgi-bin/ai.pl?"
     "http://www.gnufans.net/cgi-bin/ai.pl?action=index"
     nil
     "aisbot: "
     )

    ("ew"
     "http://www.emacswiki.org/cgi-bin/wiki.pl?"
     "http://www.emacswiki.org/cgi-bin/wiki.pl?action=index"
     nil
     "fsbot: "
     )


    ("fw"
     "http://www.etrumeus.com/ferment/"
     "http://www.etrumeus.com/ferment/TitleIndex?action=titleindex"
     nil
     "wikibot: ")

    
    ("fs"
     "http://www.gnufans.net/fsedu.pl?"
     "http://www.gnufans.net/cgi-bin/fsedu.pl?action=index"
     nil
     "fsbot: "
     )


    ("mb"
     "http://www.usemod.com/cgi-bin/mb.pl?"
     "http://www.usemod.com/cgi-bin/mb.pl?action=index"
     nil
     "wikibot: "
    )

    ("hwh"
     "http://hurd.gnufans.org/bin/view/Hurd/"
     "http://hurd.gnufans.org/bin/view/Hurd/WebTopicList?skin=plain"
     erbwiki-get-fields-spaced
     "hbot: "
     )

    
    ("hwd"
     "http://hurd.gnufans.org/bin/view/Distrib/"
     "http://hurd.gnufans.org/bin/view/Distrib/WebTopicList?skin=plain"
     erbwiki-get-fields-spaced
     "hbot: "
     )


    ("hwmain"
     "http://hurd.gnufans.org/bin/view/Main/"
     "http://hurd.gnufans.org/bin/view/Main/WebTopicList?skin=plain"
     erbwiki-get-fields-spaced
     "hbot: "
     )

    ("hwmach"
     "http://hurd.gnufans.org/bin/view/Mach/"
     "http://hurd.gnufans.org/bin/view/Mach/WebTopicList?skin=plain"
     erbwiki-get-fields-spaced
     "hbot: "
     )


    ("hwmig"
     "http://hurd.gnufans.org/bin/view/Mig/"
     "http://hurd.gnufans.org/bin/view/Mig/WebTopicList?skin=plain"
     erbwiki-get-fields-spaced
     "hbot: "
     )

    
    
    ("hwg"
     "http://hurd.gnufans.org/bin/view/GNU/"
     "http://hurd.gnufans.org/bin/view/GNU/WebTopicList?skin=plain"
     erbwiki-get-fields-spaced
     "hbot: "
     )


    ("hwt"
     "http://hurd.gnufans.org/bin/view/TWiki/"
     "http://hurd.gnufans.org/bin/view/TWiki/WebTopicList?skin=plain"
     erbwiki-get-fields-spaced
     "hbot: "
     )


    ("sl"
     "http://www.sl4.org/bin/wiki.pl?"
     "http://www.sl4.org/bin/wiki.pl?action=index"
     nil
     "aisbot: "
     )

    ;;("sm"
     ;;"http://www.scarymath.org/math.pl?"
     ;;"http://www.scarymath.org/math.pl?action=index"
     ;;nil
     ;;"ScBot: "
     ;;)

    ("so"
     "http://www.gnufans.net/cgi-bin/octave.pl?"
     "http://www.gnufans.net/cgi-bin/octave.pl?action=index"
     nil
     "ScBot: "
     )

    ;;("sp"
    ;; "http://www.scarymath.org/physics.pl?"
    ;; "http://www.scarymath.org/physics.pl?action=index"
    ;; nil
    ;; "ScBot: "
    ;;)


    ;; towniebot
    ("tbm"
     "http://www.nevadamissouri.net/bin/view/Main/"
     "http://www.nevadamissouri.net/bin/view/Main/WebTopicList?skin=plain"
     nil
     "towniebot: ")

    ;; now the big ones: 



    ("twt"
     "http://twiki.org/cgi-bin/view/TWiki/"
     "http://twiki.org/cgi-bin/view/TWiki/WebTopicList?skin=plain"
     nil
     "TWikiBot: "
     )


    ("twp"
     "http://twiki.org/cgi-bin/view/Plugins/"
     "http://twiki.org/cgi-bin/view/Plugins/WebTopicList?skin=plain"
     nil
     "TWikiBot: "
     )


    ("twm"
     "http://twiki.org/cgi-bin/view/Main/"
     "http://twiki.org/cgi-bin/view/Main/WebTopicList?skin=plain"
     nil
     "TwikiBot: "
     )

    ("twc"
     "http://twiki.org/cgi-bin/view/Codev/"
     "http://twiki.org/cgi-bin/view/Codev/WebTopicList?skin=plain"
     nil
     "TWikiBot: "
     )

    ("twsupport"
     "http://twiki.org/cgi-bin/view/Support/"
     "http://twiki.org/cgi-bin/view/Support/WebTopicList?skin=plain"
     nil
     "TWikiBot: "
     )

    ("twsandbox"
     "http://twiki.org/cgi-bin/view/Sandbox/"
     "http://twiki.org/cgi-bin/view/Sandbox/WebTopicList?skin=plain"
     nil
     "TWikiBot: "
     )

    
    )
  "Page storing names of all pages."
  :group 'erbwiki)

(defcustom erbwiki-this-wiki "NONE"
  "Choose this as one of the cars of erbwiki-index-pages
and do your thing  :)  Should mostly be done for you by erbwiki-main
functions. "
  :group 'erbwiki)


(defcustom erbwiki-file-name "~/pub/pub/fsbot-train/wiki-index"
  "Please customize this. 

This filename, appropriately suffixed, stores the wiki's current or
last index. "
  :group 'erbwiki)


(defcustom erbwiki-train-string 
  (concat
   "%s%s is also at %s%s\n"
   "%s%s is at %s%s\n"
   )
  "Don't forget the \n at the end!" 
  :group 'erbwiki
  )

(defcustom erbwiki-train-file-name "~/pub/pub/fsbot-train/wiki-train"
  "Please customize this. 

With appropriate extension, this file stores the commands to be used to
train the bot.  "
  :group 'erbwiki)

(defcustom erbwiki-fetch-wiki-function 'erbwiki-fetch-wiki 
  "
This function should take a file as argument, and write into the file,
a single lisp object.  The lisp object is a list of new pages in the
wiki. "
  :group 'erbwiki)


(defcustom erbwiki-before-train-hooks nil
  "Hooks to run before training..

Users might want to use these hooks to connect if they are not already
connected."
  :group 'erbwiki)
;;;###autoload
(defun erbwiki-doit ()
  ;; not intetractive anymore. 
  (erbwiki-update)
  (erbwiki-train))

;;;###autoload
(defun erbwiki-main-doit-all-one-wiki  (wikiname &rest morewikies)
  "CAUTION: nullifies idledo list. "
  (interactive "sWhich Wiki? ")
  (let* ((wikilists (cons wikiname morewikies))
	 ctr)
    (setq ctr wikilists)
    (while ctr
      (setq erbwiki-this-wiki (pop ctr))
      (erbwiki-update))
    (idledo-nullify)
    (setq ctr wikilists)
    (while ctr
      (setq erbwiki-this-wiki (pop ctr))
      (erbwiki-train)))
  (ignore-errors (idledo-start)))

;;;###autoload
(defun erbwiki-main-main-ew ()
  (interactive)
  (erbwiki-main-doit-all-one-wiki "ew"))

;;;###autoload
(defun erbwiki-main-main-tbm ()
  (interactive)
  (erbwiki-main-doit-all-one-wiki "tbm"))

;;;###autoload
(defun erbwiki-main-main-tw  ()
  (interactive)
  (erbwiki-main-doit-all-one-wiki "twt"
				  ;;"twm"
				  "twc"
				  "twp"
				  "twsandbox"
				  "twsupport"
				  
				  ))


;;;###autoload
(defun erbwiki-main-main-mb  ()
  (interactive)
  (erbwiki-main-doit-all-one-wiki "mb"))


;;;###autoload
(defun erbwiki-main-main-hw  ()
  (interactive)
  (erbwiki-main-doit-all-one-wiki "hwh" "hwd" "hwmain" "hwmach"
				  "hwmig" "hwg"
				  "hwt"
				  ))

;;;###autoload
(defun erbwiki-main-main-all-wikis ()
  (interactive)
  (erbwiki-main-doit-all-one-wiki
   "ew" "hwh" "hwd" "hwmain" "hwmach" "hwmig" "hwg" "hwt" "mb"))


;;;###autoload
(defun erbwiki-train  ()
  ;;(interactive)
  (run-hooks 'erbwiki-before-train-hooks)
  (erbtrain-file 
   (concat erbwiki-train-file-name
	   "-" erbwiki-this-wiki)))
;;;###autoload
(defun erbwiki-update ()
  ;;(interactive)
  (require 'erball)
  (save-window-excursion
   (let 
       ((newfile (concat erbwiki-file-name 
			 ".current-"
			 erbwiki-this-wiki
			 ))
	(lastfile (concat erbwiki-file-name ".previous-"
			  erbwiki-this-wiki
			  ))
	oldfields currentfields newfields
	(train-name
	 (concat erbwiki-train-file-name "-" erbwiki-this-wiki))
	(wiki-string
	 (cadr (assoc erbwiki-this-wiki erbwiki-index-pages)))
	(botname
	 (fifth (assoc erbwiki-this-wiki erbwiki-index-pages)))
	)
     (unless botname (setq botname ", "))
     (when (file-exists-p lastfile) (mkback lastfile))
     (when (file-exists-p newfile) (copy-file newfile lastfile t))
     (funcall erbwiki-fetch-wiki-function newfile)
     (ignore-errors
       (find-file lastfile)
       (goto-char (point-min))
       (setq oldfields (ignore-errors (read (get-file-buffer lastfile)))))
     (find-file newfile)
     (goto-char (point-min))
     (setq currentfields (ignore-errors
			   (read (get-file-buffer newfile))))
     (setq newfields 
	   (set-difference currentfields oldfields
			   :test 'equal
			   ))
     (kill-buffer (get-file-buffer newfile))
     (kill-buffer (get-file-buffer lastfile))
     (when (file-exists-p train-name) 
       (mkback train-name))
     (with-temp-file train-name
       (while newfields
	 (insert (format erbwiki-train-string 
			 botname
			 (car newfields) 
			 wiki-string
			 (car
			  newfields)
			 botname
			 (car newfields)
			 wiki-string
			 (car newfields)
			 ))
	 (pop newfields)))))
  (erbwiki-display)
  )
			
;;;###autoload
(defun erbwiki-display ()
  (interactive)
  (dired (file-name-directory erbwiki-train-file-name))
  (revert-buffer))


(defun erbwiki-fetch-wiki (filename)
  (require 'lines)
  (let* 
      ((wiki-dump-name (expand-file-name "tmp-wiki-dump"
					  temporary-file-directory))
       (thisassoc
	(assoc erbwiki-this-wiki
	       erbwiki-index-pages))
       (wiki-page 
	(cadr thisassoc))
       (index-page 
	(caddr thisassoc))
       (get-fields-fn (cadddr thisassoc))
       fields fieldslist)
    (unless get-fields-fn 
      (setq get-fields-fn 'erbwiki-get-fields))
    (unless (stringp wiki-page)
      (error "index page is not a stringp??"))

    (unless (stringp index-page)
      (error "index page is not a stringp??"))

    ;;(setq index-page (concat wiki-page "action=index"))
    (shell-command (concat "w3m -dump \"" 
			   index-page
			   ;;erbwiki-index-page 
			   "\""
			   " > " wiki-dump-name))
    (setq fields (lines-get-fields-file wiki-dump-name))
    (kill-buffer (get-file-buffer wiki-dump-name))
    (setq fieldslist
	  (funcall get-fields-fn
		   fields))
    (with-temp-file filename
      (insert (format "%S" fieldslist)))))

(defun erbwiki-get-fields (fields)
  "Given the fields as parsed by lines-get-fields, return a list of
the actual wiki fields."
  (let (field)
    (remove-if
     (lambda (arg) (member arg (list '.... '*    
				     '[]
				     'Search:
				     )))
     (erbutils-flatten
      (remove-if 
       (lambda (field) 
	 (or (not (< (length field) 3))
	     (string-match "--" 
			   (format "%s" 
				   (first field)))))
       fields)))))


(defun erbwiki-get-fields-spaced (fields)
  "Given the fields as parsed by lines-get-fields, return a list of
the actual wiki fields."
  (erbutils-flatten
   (mapcar (lambda (field)
	     (if (equal (first field) '*)
		 (mapconcat
		  '(lambda (arg) (format "%s" arg))
		  (cdr field)
		  ""
		  )))
	   fields)))
	       
  
    

(provide 'erbwiki)
(run-hooks 'erbwiki-after-load-hooks)



;;; erbwiki.el ends here
