;;; erbwiki.el --- SECURITY RISK, READ BELOW.
;; Time-stamp: <2007-11-23 11:27:02 deego>
;; Copyright (C) 2002, 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbwiki.el
;; Package: erbwiki
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
;; Thanks: Alex Schroeder

;; USING ERBWIKI.EL TO TRAIN YOUR BOTS ON WIKIS WITH LINES VERSION <
;; 0.3 IS A SECURITY RISK!!  EARLIER LINES.EL CAN BE MADE TO EVAL AN
;; ARBITRARY LISP EXPRESSION, INCLUDING (SHELL-COMMAND "RM -RF"), WE
;; THINK, THOUGH WE HAVEN'T FIGURED OUT HOW.  ANYHOW, USE LINES.EL >
;; 0.3 ONLY. 


(defconst erbwiki-home-page
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

(defconst erbwiki-version "0.0dev")


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

;;; Real Code:

;;<lion> It's like 2 lines of code to pull down the names of all the
;;pages.

;; <lion> import xmlrpclib
;; <lion> srcwiki =xmlrpclib.ServerProxy("http://mywiki.org/?action=xmlrpc2")
;; <lion> allpages = srcwiki.getAllPages()
;; <lion> The wiki has to support the xml-rpc interface.
;; <lion> But for MoinMoin,
;;<lion> it's pretty much ubiquitous
;; http://twistedmatrix.com/wiki/moin/WikiRpc
;; <lion> Bayle Shanks has written a thing called the "InterWiki Gateway."
;; <lion> It's not quite mature yet,
;; <lion> but it's goal is to make it so that the XML-RPC API will work
;; with any wiki.
;; <lion> It'll just handle the back-end stuff of figuring out "what type
;; of wiki
;;        is it" and "how do I scrape the information out."
;; <lion> But it's not quite there yet.


;;TODO: add this wiki:
;;http://www.nanoaging.com/wiki/index.php/Main_Page"
(defcustom erbwiki-index-pages

  '(


    ("si"
     "http://www.gnufans.net/cgi-bin/singularity.pl?"
     "\"http://localhost/cgi-bin/singularity.pl?action=index\""
     nil
     "singbot: "
     )

    ("ai2"
     "http://www.ifi.unizh.ch/ailab/aiwiki/aiw.cgi?"
     "\"http://www.ifi.unizh.ch/ailab/aiwiki/aiw.cgi?action=index\""
     nil
     "singbot: "
     )



    ("sl"
     "http://www.sl4.org/bin/wiki.pl?"
     "\"http://www.sl4.org/bin/wiki.pl?action=index\""
     nil
     "singbot: "
     )




    ("fu"
     "http://futures.wiki.taoriver.net/moin.cgi/"
     "\"http://futures.wiki.taoriver.net/moin.cgi/TitleIndex?action=titleindex&mimetype=text/xml\""
     nil
     "singbot: "
     erbwiki-fetch-wiki-remove-tags
     
     )



    ("ew"
     "http://www.emacswiki.org/cgi-bin/wiki.pl?"
     "\"http://www.emacswiki.org/cgi-bin/wiki.pl?action=index\""
     nil
     "fsbot: "
     )


    ("cw"
     "http://www.emacswiki.org/cgi-bin/community/"

     "\"http://www.emacswiki.org/cgi-bin/community?action=index;raw=1\""
     nil
     "fsbot: "
     )



    ("fw"
     "http://www.etrumeus.com/ferment/"
     "\"http://www.etrumeus.com/ferment/TitleIndex?action=titleindex\""
     nil
     "wikibot: ")

    
    ("fskdfhukdfhjkdfjk"
     "http://www.gnufans.net/fsedu.pl?"
     "\"http://www.gnufans.net/cgi-bin/fsedu.pl?action=index\""
     nil
     "nobot: "
     )


    ("ipfoobar"

     "http://imminst.org/pedia/PageIndex"
     "\"http://new.imminst.org/pedia/\""
     nil
     "singbot: "
     )




    ("hwh"
     "http://hurd.gnufans.org/bin/view/Hurd/"
     "\"http://hurd.gnufans.org/bin/view/Hurd/WebTopicList?skin=plain\""
     erbwiki-get-fields-spaced
     "hbot: "
     )

    
    ("hwd"
     "http://hurd.gnufans.org/bin/view/Distrib/"
     "\"http://hurd.gnufans.org/bin/view/Distrib/WebTopicList?skin=plain\""
     erbwiki-get-fields-spaced
     "hbot: "
     )


    ("hwmain"
     "http://hurd.gnufans.org/bin/view/Main/"
     "\"http://hurd.gnufans.org/bin/view/Main/WebTopicList?skin=plain\""
     erbwiki-get-fields-spaced
     "hbot: "
     )

    ("hwmach"
     "http://hurd.gnufans.org/bin/view/Mach/"
     "\"http://hurd.gnufans.org/bin/view/Mach/WebTopicList?skin=plain\""
     erbwiki-get-fields-spaced
     "hbot: "
     )


    ("hwmig"
     "http://hurd.gnufans.org/bin/view/Mig/"
     "\"http://hurd.gnufans.org/bin/view/Mig/WebTopicList?skin=plain\""
     erbwiki-get-fields-spaced
     "hbot: "
     )

    
    
    ("hwg"
     "http://hurd.gnufans.org/bin/view/GNU/"
     "\"http://hurd.gnufans.org/bin/view/GNU/WebTopicList?skin=plain\""
     erbwiki-get-fields-spaced
     "hbot: "
     )


    ("hwt"
     "http://hurd.gnufans.org/bin/view/TWiki/"
     "\"http://hurd.gnufans.org/bin/view/TWiki/WebTopicList?skin=plain\""
     erbwiki-get-fields-spaced
     "hbot: "
     )



    ;;("sm"
     ;;"http://www.scarymath.org/math.pl?"
     ;;"http://www.scarymath.org/math.pl?action=index"
     ;;nil
     ;;"ScBot: "
     ;;)

    ("so"
     "http://wiki.octave.org/wiki.pl?"
     "\"http://wiki.octave.org/wiki.pl?action=index\""
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
     "\"http://www.nevadamissouri.net/bin/view/Main/WebTopicList?skin=plain\""
     nil
     "towniebot: ")

    ;; now the big ones: 



    ("twt"
     "http://twiki.org/cgi-bin/view/TWiki/"
     "\"http://twiki.org/cgi-bin/view/TWiki/WebTopicList?skin=plain\""
     nil
     "TWikiBot: "
     )


    ("twp"
     "http://twiki.org/cgi-bin/view/Plugins/"
     "\"http://twiki.org/cgi-bin/view/Plugins/WebTopicList?skin=plain\""
     nil
     "TWikiBot: "
     )


    ("twm"
     "http://twiki.org/cgi-bin/view/Main/"
     "\"http://twiki.org/cgi-bin/view/Main/WebTopicList?skin=plain\""
     nil
     "TwikiBot: "
     )

    ("twc"
     "http://twiki.org/cgi-bin/view/Codev/"
     "\"http://twiki.org/cgi-bin/view/Codev/WebTopicList?skin=plain\""
     nil
     "TWikiBot: "
     )

    ("twsupport"
     "http://twiki.org/cgi-bin/view/Support/"
     "\"http://twiki.org/cgi-bin/view/Support/WebTopicList?skin=plain\""
     nil
     "TWikiBot: "
     )

    ("twsandbox"
     "http://twiki.org/cgi-bin/view/Sandbox/"
     "\"http://twiki.org/cgi-bin/view/Sandbox/WebTopicList?skin=plain\""
     nil
     "TWikiBot: "
     )

    
    )
  
"Page storing names of all pages.
As an example, consider this entry:

    (\"ew\"
     \"http://www.emacswiki.org/cgi-bin/wiki.pl?\"
     \"http://www.emacswiki.org/cgi-bin/wiki.pl?action=index\"
     nil
     \"fsbot: \"
     nil
     )

Most entries are obvious.  ew refers to the nick name of the wiki used
when you run the function M-x erbwiki-do-it-all-one-wiki. 

Let's explain the 2 nils above.  The first nil corresponds to the
default function erbwiki-get-fields.  You replace it by another
function, example, erbwiki-get-fields-spaced if you want to use that
instead.  
The second nil corresponds to the function used to dump the wiki,
which by default is erbwiki-fetch-wiki --- that function uses w3m.


"

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
  "This function should take a file as argument, and write into the file,
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
  (interactive)
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
	(fetchfunction
	 (sixth (assoc erbwiki-this-wiki erbwiki-index-pages)))
	)
     (unless botname (setq botname ", "))
     (when (file-exists-p lastfile) (mkback lastfile))
     (when (file-exists-p newfile) (copy-file newfile lastfile t))
     (funcall (or fetchfunction erbwiki-fetch-wiki-function) newfile)
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
     (setq newfields (funcall erbwiki-filter-fields-function newfields))
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
			

(defcustom erbwiki-filter-fields-function
  'erbwiki-filter-fields-default "")

(defun erbwiki-filter-fields-default (fields)
  ;; remove non-ascii characters
  (delete-if 
   (lambda (arg) (string-match "[\200-\377]" (format "%s" arg)))
   (copy-list fields)))

;;;###autoload
(defun erbwiki-display ()
  (interactive)
  (dired (file-name-directory erbwiki-train-file-name))
  (revert-buffer))



(defcustom erbwiki-dump-program "w3m -dump"
  "Also try lynx -dump, curl. ")

(defun erbwiki-fetch-wiki-lynx (filename)
  (let ((erbwiki-dump-program "lynx -dump"))
    (erbwiki-fetch-wiki filename)))
			
(defcustom erbwiki-fetch-wiki-remove-tags-p nil "")

(defun erbwiki-fetch-wiki-remove-tags (f)
  (let ((erbwiki-fetch-wiki-remove-tags-p t))
    (erbwiki-fetch-wiki f)))

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
    ;; We will NOT add " " around the URL before calling ther
    ;; shell-comnd, since the behavior of w3m -dump and lynx -dump
    ;; differs in that case.  Wehn the user wants a quote, she can
    ;; supply it in the name of te url herself..

    (shell-command (concat erbwiki-dump-program  " " 
			   index-page
			   ;;erbwiki-index-page 
			   ""
			   " > " wiki-dump-name))
    (when erbwiki-fetch-wiki-remove-tags-p
      (erbwiki-remove-tags-from-file wiki-dump-name))
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
     (lambda (arg) (member arg (list '*    
				     '[]
				     'Search:
				     )))
     (erbutils-flatten
      (remove-if 
       (lambda (field) 
	 (or 
	  (not (erbutils-listp-proper field))
	  (not (< (length field) 3))
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
	       
  

(defun erbwiki-remove-tags-from-file (file)
  (interactive "fFile: ")
  (find-file file)
  (goto-char (point-min))
  (while 
      ;; accept any regexp greedily containing only tags with no
      ;; spaces, or one starting with ?xml, in which case, allow
      ;; spaces. but still be greedy. 
      (search-forward-regexp "<\\(?:\\?xml.*?\\|[^ \t\n]*?\\)>" nil t)
    (replace-match "\n" nil t))
  (save-buffer))

(provide 'erbwiki)
(run-hooks 'erbwiki-after-load-hooks)



;;; erbwiki.el ends here
