;;; erbc.el --- Erbot user-interface commands.
;; Time-stamp: <2003-11-17 09:48:22 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbc.el
;; Package: erbc
;; Author: D. Goel <deego@gnufans.org>
;; Version: NA
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot


(defvar fs-home-page
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




(defvar erbc-version "NA")
(defvar fs-version "20031114")


;;==========================================
;;; Code:

;; NOTE:  stuff like (fs-et) can be passed possibly mischievous
;; code as the first argument... never eval or "set" any
;; "code"... always convert it to a single atom... before setting it..



(require 'find-func)

(defgroup erbc nil
  "The group erbc"
   :group 'applications)


(defcustom fs-before-load-hooks nil "" :group 'erbc)
(defcustom fs-after-load-hooks nil "" :group 'erbc)



(defcustom erbnoc-char ","
  "The character which calls the bot.

in addition to directly addressing it.

may be different for
different bots. 

Is really a string, but the length of the string should be 1,. 
")
(defcustom erbnoc-char-double ",,"
  "The string which calls the bot from midsentence

this string shoul dhave a length 2

")


(run-hooks 'fs-before-load-hooks)


;; Real code
(defcustom fs-internal-botito-mode nil
  "Mode to turn on more english-like bunny-behavior"
  :group 'erbc)



(defvar fs-tgt "")
(defvar fs-nick "")

(defcustom fs-internal-parse-error-p 
  nil
  "Whether to show lispy errors in term descriptions.

When nil, an error in a  lispy description of a term makes to bot
go to an english mode for the term. 
When non-nil, we will just display the error.  On a channel full of
lisp hackers, we will want to make this t for users' convenience.")

(defvar erbnoc-tgt "")
(defvar erbnoc-nick "")

(defcustom erbnoc-shell-command-p nil 
  "Whether to allow commands that use shell-commands...
Some fsbot commands use shell-commands... shell-commands always mean
possibility of exploits.  andn are disabled by default. 

Make this t at your own risk. ")


(defcustom fs-internal-questions
  '("what" "where" "who" 
    ;; no please: 
    ;;"why" 
    ;;"how"
    )
  ""
  :group 'erbc)
		  
(defcustom erbnoc-google-defaults 
  '(("#emacs" ("emacs"))
    ("#fsbot" ("fsbot")))
  "" :group 'erbc)


(defun erbnoc-shell-command-to-string (&rest args)
  (cond
   (erbnoc-shell-command-p
    (apply 'shell-command-to-string args))
   (t
    (error "The bot-operator has shell commands disabled"))))


(defun erbnoc-shell-command (&rest args)
  (cond
   (erbnoc-shell-command-p
    (apply 'shell-command args))
   (t
    (error "The bot-operator has shell commands disabled"))))



(defun fs-get-google-defaults ()
  (cadr (assoc fs-tgt erbnoc-google-defaults)))

(defvar fs-prestring  "")
;; (make-variable-buffer-local 'fs-prestring)


(defcustom fs-internal-google-level 0
  "75 is a good choice for fsbot. "
  :group 'erbc)
(defcustom fs-internal-english-max-matches 20
  "This check is triggerred only when the users' original request didnot
succeed and so we have gone into an english-mode and are searching.
If the number of matches results in 1000, then most likely, the word
was something like i or you and the user was not intending a search. 
"

:group 'erbc)

(defcustom fs-internal-questions-all
  '("what" "where" "who" "why" "how" 
    "whose" "which"
    )
  ""
  :group 'erbc)

(defcustom fs-internal-articles
  '("the" "a" "an" "this" "that")
  ""
  :group 'erbc)


(defcustom fs-internal-english-target-regexp
  "^$"
  "Targets that prefer english.. so erbot will usually go to a
english-mode unless near-exact matches.  This shall usually happen on
the few social channels erbot hangs out on. "
  :group 'erbc)

(defcustom fs-internal-query-target-regexp
  "^$"
  "Targets where erbot will respond to queries like: 
Foo ? "
  :group 'erbc)

(defcustom fs-internal-add-nick-weights
  '(1 ;; yes
    5 ;;no
    )
  ""
  :group 'erbc)
  

(defun fs-correct-entry (name &rest fubar)
  "Assumes that name is a string... this downcases strings.  Rendering
it fit for database-entry. "
  (unless (stringp name) (setq name (format "%s" name)))
  ;;(downcase
  (let ((newname
	 (mapconcat 'identity (split-string name) "-")))
    (or (erbbdb-get-exact-name newname) 
	newname)))


(defun fs-describe-key-briefly (&optional key &rest args)
  "Return the function on key..building block for other erbc's..
If no such function, return the symbol 'unbound. "

  (unless key 
    (error
     "Syntax:  , dkb key"))
  (when (and (null key) (null args))
    (setq key ""))
  (unless (stringp key)
    (setq key (read-kbd-macro
	       (mapconcat '(lambda (arg) (format "%s" arg))
			  (cons key args)
			  " "))))
  (let ((res (key-binding key)))
    (if res res
      'unbound)))

;; for now..
;;(defalias 'fs-describe-key 'fs-describe-key-briefly)

(defun fs-where-is-in-map (map &optional fcn)
  (let* ((wi (where-is-internal fcn map)))
    (mapconcat 'key-description wi ", ")))

(defun fs-where-is-gnus-group (&optional fcn)
  (require 'gnus)
  (unless fcn (error "please supply a function"))
  (fs-where-is-in-map gnus-group-mode-map fcn))

(defun fs-where-is-gnus-summary (&optional fcn)
  (require 'gnus)
  (unless fcn (error "please supply a function"))
  (fs-where-is-in-map gnus-summary-mode-map fcn))
(defun fs-where-is-message (&optional fcn)
  (require 'gnus)
  (require 'message)

  (unless fcn (error "please supply a function"))
  (fs-where-is-in-map message-mode-map fcn))



(defun fs-keyize (key morekeys)
  (setq key (read-kbd-macro 
	     (mapconcat '(lambda (arg) (format "%s" arg))
			(cons key morekeys) " "))))


(defun fs-describe-key-one-line (&optional key &rest args)
  "Key, and just one line of function"
  (unless key (error "Syntax: , dk \"Key...\""))
  (let* ((fcn (apply 'fs-describe-key-briefly key args))
	 (fcns (format "%s" fcn))
	 (apr (or (fs-apropos-exact fcns)
		  "No doc. available. ")))
    (concat (format "%s -- %s"
		    fcns
		    apr))))

(defalias 'fs-dko 'fs-describe-key-one-line)

(defalias 'fs-describe-key 'fs-describe-key-and-function)

(defun fs-lookup-key-from-map-internal (&optional map key &rest morekeys) 
  (unless key (error "No key supplied. "))
  (unless (stringp key)
    (setq key (read-kbd-macro 
	       (mapconcat '(lambda (arg) (format "%s" arg))
			  (cons key morekeys) " "))))
  (unless (arrayp key) (setq key (format "%s" key)))
  (let* ((fcn (lookup-key map key))
	 (fcns (format "%s" fcn))
	 (apr (or (fs-apropos-exact fcns)
		  "No doc available. ")))
    (concat (format "%s -- %s" fcns apr))))

(defun fs-lookup-key-gnus-group (&optional key &rest args)
  (unless key (error "Syntax: , lkgg \"Key...\""))
  (require 'gnus-group)
  (apply 'fs-lookup-key-from-map-internal gnus-group-mode-map key args))

(defun fs-lookup-key-gnus-summary (&optional key &rest args)
  (unless key (error "Syntax: , lkgg \"Key...\""))
  (require 'gnus)
  (apply 'fs-lookup-key-from-map-internal gnus-summary-mode-map key args))

(defun fs-lookup-key-message (&optional key &rest args)
  (unless key (error "Syntax: , lkgg \"Key...\""))
  (require 'gnus)
  (require 'message)
  (apply 
   'fs-lookup-key-from-map-internal gnus-message-mode-map key args))



(defun fs-apropos-exact (str)
  (unless (stringp str) (setq str (format "%s" str)))
  (let* ((reg (concat "^" (regexp-quote str) "$"))
	 (apr (apropos reg))
	 (asso (assoc* str apr
		       :test
		       (lambda (a b)
			 (string= (format "%s" a) (format "%s" b)))))
	 
	 (val (second asso)))
    (if val (format "%s" val)
      nil)))
  
(defun fs-describe-key-long (k &rest args)
  (let ((f (apply 'fs-describe-key-briefly k args)))
    (fs-describe-function-long f)))

(defun fs-describe-key-and-function (key &rest args)
  "Describe the key KEY.  
Optional argument ARGS .  If the input arguments are not strings, it
kbds's them first... , so that , df C-x C-c works"
  (when (and (null key) (null args))
    (setq key ""))
  (unless (stringp key)
    (setq key (read-kbd-macro
	       (mapconcat '(lambda (arg) (format "%s" arg))
			  (cons key args)
			  " "))))
  (let ((b (key-binding key)))
    (cond
     ((symbolp b)
      (or 
       (ignore-errors (fs-describe-function b))
       (format "Bound to: %s" b)))
     (t 
      (format "Bound to: %s" b)))))



(defun fs-describe-function (&optional function nolimitp &rest fubar)
  "Describes the FUNCTION named function.
Also tries an fs- prefix for the function..
nolimitp has to be eq 'nolimit for the nolimit effect to take place..
"
  (unless function 
    (error 
     "Syntax: (describe-function 'name-of-function) or , df 'name"))
  (let* ((f function)
	 g
	 )
    (if (stringp f)
	(setq f (read f)))
    (cond
     ((symbolp f)
      (progn
	(setq
	 g
	 (cond
	  ((fboundp f) f)
	  (t (read (concat "fs-" (format "%s" f))))))
	(unless (fboundp g)
	  (setq g f))
	(let* ((def (symbol-function g)))
	  (ignore-errors
	    (if (equal 'autoload (car-safe def))
		(load (second def))))
	  ;; this check does nothing now.. need ro 
	  (if (equal nolimitp 'nolimit)

	      ;;(let ((fs-limit-lines 8))
	      ;;(fs-limit-lines (describe-function g)))
	      (describe-function g)
	    (describe-function g))
	  
	  )))
     ;; if list, DO NOT wanna eval it-->
     (t
      "NO function specified"))))


(defun fs-where-is (function &rest args)
  "Tells what key the function is on..

"
  (let* (
	 (str0 "")
	 (str1 "")
	 (str2 "")
	 (str3 "")
	 )
    (cond
     ((stringp function) (setq function (read function)))
     (t nil))
    (cond 
     ((null function)  (format "Sorry, %s is not a symbol" function))    
     ((symbolp function)
      (unless (fboundp function) (setq str0 "Either unbound or.. "))
      (setq str2
	    (with-temp-buffer 
	      (where-is function t)
	      (buffer-string)))
      (concat str0 str1 str2 str3))
     (t (format "Looks like %s is not a symbol" function)))))

(defun fs-describe-function-long (function &rest fubar)
  "Similar to describe-function, but does not limit the strings...
Use with caution only in privmsgs please, for may produce long outputs. "
  (fs-describe-function function 'nolimit))


(defun fs-describe-variable-long (variable &rest fubar )
  "Similar to describe-variable, but does not limit strings.."
  (fs-describe-variable variable 'nolimit))

(defun fs-describe-variable (&optional variable &rest ignore)
  "Describes a VARIABLE.."
  (unless variable (error "Syntax: , dv 'variable"))
  (let* ((f variable))
    (if (stringp f)
	(setq f (read f)))
    (cond
     ((symbolp f) 
      (erbutils-describe-variable f))
     
     ;; if list, DO NOT wanna eval it-->
     (t
      "NO variable specified"))))

(defalias 'fs-parse 'fs-lispify)
(defalias 'fs-parse-english 'fs-lispify)

(defun fs-require (feature &rest fubar)
  "Make the bot require the feature FEATURE.
So that the command df
or dv works fine..Actually, df knows how to load unloaded features
automatically."
  (if (stringp feature)
      (setq feature (read feature)))
  (when (or (string-match "/" (format "%s" feature))
	    (string-match "\\\\" (format "%s" feature)))
    (error "Your safety is VERY important to us, so we avoid loading features containing slashes."))
  (cond
   ((symbolp feature) (format "%s" (require feature)))
   (t "no feature specified")))


(defvar fs-found-query-p nil
  "internal..  should be normally set to nil. 
When non nil, means that the msg was not meant to the bot, so the
reply please be abbreviated. ")

(defvar fs-internal-addressedatlast nil
  "internal.. normally nil")

(defvar fs-internal-original-message ""
  "internal")

(defvar fs-internal-message-sans-bot-name ""
  "internal")

(defvar fs-internal-max-lisp-p nil)


(defun fs-respond-to-query-p (msg)
  ;; if it is of the form resolve? the user KNOWS what resolve or
  ;; kensanata is, and is not asking for information. So, please don't
  ;; respond in such a case.
  (not
   (member msg (mapcar 'first channel-members))))

(defcustom fs-internal-parse-preprocess-message-remove-end-chars
  ;; remove trailing ^A's that occur on action strings...
  (list 1)
  "")

(defun fs-parse-preprocess-message (msg)
  (let ((len (length msg)))
    (when (and
	   (> len 0)
	   (member (aref msg (- len 1)) 
		   fs-internal-parse-preprocess-message-remove-end-chars)
	   (setq msg (subseq msg 0 -1)))))
  msg)

(defun fs-lispify (&optional msg proc nick tgt localp
				 userinfo &rest foo)
  "Parse the english MSG into a lisp command. 

If it is an 'is', it should always be the second word ..
viz: we had better use hyphens in the first word..
MSG is a string..
Is the main function.. but also available to the user as a command...

NB: The end-result is always an expression.. and NOT a strign..


Just once in a blue moon, this will, at random, even parse messages
not addressed to it...

Finally, wanna parse messages whose last item contains erbot..
Optional argument PROC .
Optional argument NICK .
Optional argument TGT .
Optional argument FOO ."
  ;;(when (stringp msg)
   ;; (setq  msg (split-string msg)))
  ;msg
  ;proc
  ;nick
  ;tgtg
  ;foo
  (setq fs-internal-original-message msg)
  (setq msg (fs-parse-preprocess-message msg))
  (let 
      (
       (origmsg msg)
       ;;(fs-internal-message-sans-bot-name fs-internal-message-sans-bot-name)
       (foundquery nil)
       (foundquerydouble nil)
       (foundkarma nil)
       ;; if t, means either our name was at last, or eevn if at
       ;; first, they weren't really addressing us..
       ;;(addressedatlast nil)
       (leave-alone-p t)
       ;;(fs-nick nick)
       bluemoon
       )
    (unless (stringp origmsg)
      (setq origmsg (format "%s" origmsg)))
    (unless msg 
      (error "Format: %s (parse \"your-english-message\")" erbnoc-char))
    (unless (stringp msg)
      (setq msg (format "%s" msg)))
    ;; remove leading spaces..
    (while
	(and (> (length msg) 0)
	     (equal (aref msg 
			  0) 32))
      (setq msg (substring msg 1)))

    ;; remove trailing spaces..
    (while 
	(and (> (length msg) 0)
	     (equal (aref msg (- (length msg) 1)) 32))
      (setq msg (substring msg 0 (- (length msg) 1))))
    
    (when (and tgt proc) 
      (set-buffer (erc-get-buffer tgt proc)))
    
    (when 
	(and (stringp msg)
	     (string-match "\\(++\\|--\\)$" msg)
	     (<= (length (split-string msg)) 2))
      (setq foundkarma t))
    ;; 2003-11-14 T15:36:38-0500 (Friday)    D. Goel
    ;; requested by elf: 
    ;; if double ??, then make it a call to m8b
    (let (len)
      (when (and (stringp msg)
		 (progn
		   (setq len (length msg)) t)
		 (> len  1)
		 (string= "??"
			  (substring msg (- len 2) len))
		 (or
		  (string-match 
		   erbot-nick msg)
		  (string-match (concat "^" erbnoc-char) msg)
		  (string-match erbnoc-char-double  msg)))
	(setq founddoublequery t)
	(setq msg (concat erbnoc-char " (m8b)"))))

    (when (and (stringp msg)
	       (> (length msg) 0)
	       ;; ignore trailing ?
	       (equal (aref msg (- (length msg) 1)) 63))
      (progn
	(setq foundquery t)
	(setq msg (substring msg 0 (- (length msg) 1)))))
    
    (setq leave-alone-p t)
    (setq bluemoon
	  (or
	   ;; responding to a general list conversation..
	   (fs-blue-moon)
	   ;; responding in general..
	   (and (equal nick tgt)
		(or
		 (stringp nick)
		 ;; parse commands -->
		 (null nick)
		 )
		)))
    (unless (stringp msg)
      (setq msg ""))

    
    ;; convert midsentence ,, to parsable sentence.
    (let (pos)
      (when 
	  (and (not (equal 0
			   (string-match erbnoc-char msg)))
	       (not 
		(let ((nickpos (string-match erbot-nick msg)))
		  (and nickpos
		       (< nickpos 3))))
	       ;; part of and
	       (setq pos 
		     (string-match erbnoc-char-double msg)))
	(setq msg (substring msg (+ pos 1)))
	(when (setq pos (string-match erbnoc-char-double msg))
	  (setq msg (substring msg 0 pos)))))

    ; deal with the leading , or ,,
    (when (equal 0
		 (string-match erbnoc-char msg))
      (let ((restmsg (substring msg 1)))
	(when (equal 0 (string-match "," restmsg))
	  (setq restmsg (substring restmsg 1)))
	(setq msg (concat erbot-nick ": " restmsg))))
    
    
    ;; now we split strings..
    (setq msg (split-string msg))

    (cond
     ( (and (first msg) 
	    (let ((pos 
		   (string-match erbot-nick (first msg))))
	      (and pos (< pos 1))))
       ;;(or
       ;;(erbutils-string= (first msg) erbot-nick)
       ;;(erbutils-string= (first msg) (concat erbot-nick ","))
					;(erbutils-string= (first msg) (concat erbot-nick
					;":")))
       (progn
	 (unless 
	     (or
	      (string-match (concat erbot-nick ":") (first msg))
	      (string-match (concat erbot-nick ",") (first msg))
	      (null (second msg))
	      (string-match "^," (second msg))
	      (string-match "^:" (second msg)))
	   (setq fs-internal-addressedatlast t))
	 (when (> (length msg) 1)
	   (setq msg (cdr msg)))
	 (setq leave-alone-p nil)))
     ((and (first (last msg)) (string-match erbot-nick (first (last msg))))
      ;; don't want this any more.. since no sense in removing the
      ;; last term.  Example: Want: what is erbot?  to stay that way.
      ;;(progn
      ;;(setq msg (reverse (cdr (reverse msg)))))
      (when leave-alone-p
	(setq fs-internal-addressedatlast t))
      (setq leave-alone-p nil))
     
     

     ;; this might be dangerous if nick is a small word like "apt"..
     ;; this also means :( thagt erbot will intervene when users are
     ;; talking about her, but not TO her..
     ;; nah, leave this one out..
     ;;((member erbot-nick msg)
     ;; (setq leave-alone-p nil))
     
     (bluemoon
      (setq leave-alone-p nil)))
    
    (setq fs-internal-message-sans-bot-name 
	  (mapconcat 'identity msg " "))

    (when (and 
	   foundquery
	   ;; if tgt is nil, we are being asked to parse
	   ;; something.. so cool
	   tgt
	   (string-match fs-internal-query-target-regexp tgt))
      ;; if this condition causes the thing to be triggerred, then
      ;; setq temporarily, a global variable... so responses are muted
      ;; in general..
      (let ((goonp nil) (newmsg msg))
	(cond
	 ((equal (length msg) 1)
	  (setq goonp 
		;; setq to t only if the content of the msg represents
		;; something the user might be interested in. 
		(fs-respond-to-query-p (first msg))
		
		))
	 (t
	  (setq goonp t)
	  ;; convert what's to what is
	  (when (stringp (first newmsg))
	    (setq newmsg 
		  (append
		   (split-string (first newmsg) "'")
		   (cdr newmsg))))
	  (if  (and goonp
		    (member 
		     (erbutils-downcase (first newmsg))
		     fs-internal-questions))
	      (setq newmsg (cdr newmsg))
	    (setq goonp nil))
	  (if  (and goonp
		    (member 
		     (erbutils-downcase (first newmsg)) 
		     '("s" "is" "are" 
		       ;;"am"
		       )))
	      (setq newmsg (cdr newmsg))
	    (setq goonp nil))
	  
	  ;; remove articles
	  (if  (and goonp
		    (member 
		     (erbutils-downcase (first newmsg)) 
		     fs-internal-articles))
	      (setq newmsg (cdr newmsg)))
	  (unless (equal (length newmsg) 1) 
	    (setq goonp nil))))
	(when goonp
	  (when leave-alone-p (setq fs-found-query-p t))
	  (setq leave-alone-p nil)
	  (setq msg (list "(" "describe" 
			  (format "%S" (first newmsg))
			  "0" ")"
			  ))
	  ))
      )
    
    
    ;;       (cond
    ;;        ((equal (length msg) 1)
    ;; 	(when leave-alone-p 
    ;; 	  (setq fs-found-query-p t))
    ;; 	(setq msg (cons "describe" msg))
    ;; 	(setq leave-alone-p nil))
    ;;        ((and
    ;; 	 (equal (length msg) 3)
    ;; 	 (member (erbutils-downcase (first msg))
    ;; 		 fs-internal-questions)
    ;; 	 (member (erbutils-downcase (second msg))
    ;; 		 '("is" "are")))
    ;; 	(setq msg (cons "describe" (cddr msg)))
    ;; 	(when leave-alone-p 
    ;; 	  (setq fs-found-query-p t))
    ;; 	(setq leave-alone-p nil))
    ;;        ((and
    ;; 	 (equal (length msg) 3)
    ;; 	 (member (erbutils-downcase (first msg))
    ;; 		 fs-internal-questions)
    ;; 	 (member (erbutils-downcase (second msg))
    ;; 		 '("is" "are")))
    ;; 	(setq msg (cons "describe" (cddr msg)))
    ;; 	(when leave-alone-p 
    ;; 	  (setq fs-found-query-p t))
    ;; 	(setq leave-alone-p nil))
       

    ;;))

    ;; finally, ignore bots/fools..
    (let ((ui (format "%S" userinfo)))
      (when 
	  (or
	   (and (stringp nick)
		(member-if 
		 (lambda (arg)
		   (string-match arg nick))
		 erbot-ignore-nicks))
	   
	   (some
	    'identity
	    (mapcar
	     (lambda (ignorethis)
	       (string-match ignorethis
			     ui))
	     erbot-ignore-userinfos)))
	(setq leave-alone-p t)))



;;;====================================================
    ;; now do the work..
    (if leave-alone-p
	;; not addressed to us, so return nil and be done..
	nil
      ;; else.. viz: go on...
      (progn
	(erblog-log-target tgt)
	(let* (;(found nil)
	       (newmsglist nil)
	       (msgstr (erbutils-stringify msg))
					;(newstrmsg nil)
	       (lispmsg
		(and (stringp msgstr)
		     (read
		      msgstr
		      ))
		))
	  
	  (setq
	   newmsglist
	   (cond
	    
	    ;; are in a read mode..
	    (erbnoc-read-mode
	     (fs-botread-feed-internal msgstr))	    
	    ;; already in lisp form...  just need to sandbox..
	    ((and lispmsg 
		  (or
		   (listp lispmsg)
		   (and fs-internal-max-lisp-p (numberp lispmsg))
		   (and fs-internal-max-lisp-p (stringp lispmsg))
		   (and (symbolp lispmsg)
			(let ((newsym
			       (intern (format "fs-%S" lispmsg))))
			  
			  (or
			   (equal 0
				  (string-match "fs-" 
						(format "%S" lispmsg)))
			   (and
			    (boundp newsym)
			    (not (fboundp newsym))))))))
	     ;;(erblisp-sandbox-fuzzy lispmsg)
	     (erblisp-sandbox lispmsg)
	     )

	    
	    (fs-dunnet-mode
	     (fs-dunnet-command msgstr))

	    
	    ;; call to arbitrary function without parens
	    ;; prefer this before is etc. so that "how is it going"
	    ;; resolves properly..
	    ((or 
	      ;; fboundp ==> allowing macros as well..
	      (fboundp (intern (concat "fs-" (first msg))))
	      ;;(functionp (intern (concat "fs-" (first msg))))
	      (equal 0 (string-match "fs-" (first msg))))
	     ;; this works great, except that we would like to quote the
	     ;; internals... because that is the most commonly used
	     ;; characteristic..
	     ;;`("(" ,@msg ")")
	     (erblisp-sandbox-full
	      ;;`( ,(intern (first msg)) ,@(erbutils-quote-list
	      ;;(mapcar 'intern (cdr msg))))
	      ;;(read (cons (intern (first msg))
	      ;;	  (read (list (erbutils-stringify (cdr msg))))))
	      (read (concat "( "(erbutils-stringify msg) " )"))))
	    
	    ((equal 0
		    (string-match "\\(s\\|r\\)/" (first msg)))
	     (fs-replace-string-from-english-internal
	      msg))
	    ((equal 0
		    (string-match "[0-9]+->" (first msg)))
	     (fs-rearrange-from-english-internal msg))
	    (
	     (and 
	      
	      
	      (or (erbutils-string= (second msg) "is" t)
		  (erbutils-string= (second msg) "are" t)
		  ;;(erbutils-string= (second msg) "am" t)
		  
		  )
	      (member (erbutils-downcase (first msg))
		      fs-internal-questions-all
		      ))
	     

	     ;;`(apply 'fs-describe ',(cddr msg))
	     `(funcall 'fs-describe 
		       ',(third msg)
		       nil nil nil ,"origmsg"
		       )
	     
	     )
	    
	    ;; some english constructs first...
	    
	    ;; search removed---because: is a functionp...
	    ;;((erbutils-string= (first msg) "search")
	    ;; (setq newmsglist
	    ;;	     `("(" "search" ,@(cdr msg) ")")))
	    ((and 

	      ;; do not want to take such cases, 100% are annoying
	      ;; false matches.
	      (not fs-internal-addressedatlast)

	      (or 
	       (erbutils-string= (second msg) "is" t)
	       (erbutils-string= (second msg) "are" t))
	      ;;(erbutils-string= (third msg) "also" t)
	      (member-ignore-case (third msg) 
				  (list "also" "also,"))
	      )
	     (erblisp-sandbox-fuzzy
	      `(
		fs-set-also ,(first msg)
				;;,@(erbutils-quote-list (cdddr msg))
				,(erbutils-stringify (cdddr msg))
				)))
	    ((and (erbutils-string= (first msg) "tell")
		  (erbutils-string= (third msg) "about"))
	     `(fs-tell-to
	       ,(erbutils-stringify (cdddr msg))
	       ,(format "%s"
			(second
			 msg))
	       ))
	    
	    (
	     (and 
	      ;; do not want to take such cases, 100% are annoying
	      ;; false matches.
	      (not fs-internal-addressedatlast)
	      
	      (or (erbutils-string= (second msg) "is")
		  (erbutils-string= (second msg) "are")))
	     (erblisp-sandbox-fuzzy
	      `(fs-set-term
		;; a string.. so we are safe..
		,(first msg)
		;; another string... so we are safe..
		,(erbutils-stringify (cddr msg)))))


	    
	    ((and 
	      (not fs-internal-addressedatlast)
	      (or
	       (erbutils-string= (first msg) "no" t)
	       (erbutils-string= (first msg) "no," t))
	      (erbutils-string= (third msg) "is"))
	     (erblisp-sandbox-fuzzy
	      `(fs-set-force ,(second msg)
				 ;;,@(erbutils-quote-list (cdddr msg))))
				 ,(erbutils-stringify (cdddr msg))))
	     )
	    
	    ((let ((foo (first msg)))
	       (and
		(not fs-internal-addressedatlast)
		(<= (length msg) 2)
		(string-match "\\(++\\|--\\)$" foo)
		(not (fs-notes foo
				 ))))
	     (let* ((foo (first msg))
		    (sec (second msg))
		    (bar (substring foo 0 -2))
		    (plusp (string-match "++$" foo)))
	       (if plusp
		   `(fs-karma-increase ,bar ,sec)
		 `(fs-karma-decrease ,bar ,sec))))
	    ((or fs-internal-addressedatlast
		 (and fs-internal-botito-mode (> (length msg) 3)))
	     `(funcall 'fs-english-only ,origmsg ,fs-internal-addressedatlast))

	    (t
	     ;;`(apply 'fs-describe ',msg)
	     
	     ;;`(funcall 'fs-describe ',(first msg)
	     ;;	       ',(second msg)
	     ;;	       ',(third msg)
	     ;;	       nil
	     ;;	       ,origmsg
	     ;;	       )
	     `(funcall 'fs-describe-from-english
		       ,origmsg
		       ',msg)
	     

	     


	     )
	    ))
	  ;; this should be "%S" and not "%s" the lattwer will convert
	  ;; (dk "k") into (dk k)
	  (format "%S" newmsglist))))))


(defun fs-describe-from-english (&optional origmsg msg)
  "Call fs-describe appropriately. 
ORIGMSG is in english. 
MSG is a list..

Plan

For multiple words, commence a search foo.*bar.*baz IF WE KNOW THAT
SEARCH or SEARCH--WIDE WILL SUCCEED, which will then, of course, go to
search-wide if it fails. 

Else, of course, do the usual thing: viz. call describe... 


"
  (unless (and origmsg msg)
    (error "Are you a user trying to call this function?  Perhaps just use
'describe instead :).  Anyway, this function needs 2 arguments. "))
  (let ((len (length msg))
	mainterm firstterm remainder N M prestring expr tmpv
	(searchp nil)
	(multitermp nil)
	(fs-internal-google-level fs-internal-google-level)
	)
    (cond
     ((<= len 1)
      (fs-describe (first msg) nil nil nil origmsg))
     (t
      (setq mainterm (first msg))
      (setq firstterm mainterm)
      (setq remainder (cdr msg))
      (while
	  (and 
	   remainder
	   (progn
	     (setq tmpv (first remainder))
	     (and (not (integerp tmpv))
		  (progn
		    (unless (stringp tmpv) (setq tmpv (format "%s"
							      tmpv)))
		    (not (integerp (ignore-errors (read tmpv))))))))
	;;(setq searchp t)
	(setq mainterm 
	      (concat mainterm ".*" tmpv))
	(setq multitermp t)
	(pop remainder))
      ;; why is this true only for multitermp???
      ;; Ah, because we say: if you end up searching and there are
      ;; multiple terms, you might as well include a result from
      ;; google among the search results. 
      (when multitermp 
	(setq fs-internal-google-level (+ fs-internal-google-level 25)))
      
      (when (and multitermp
		 ;; viz. if it will work
		 (second (fs-search-basic 
			  mainterm nil nil 'describe)))
	(setq searchp t))
      
      
      (if searchp 
	  (fs-search 
	   mainterm (first remainder) (second remainder) 
	   "Try: " origmsg)
	(fs-describe 
	 firstterm (first remainder) (second remainder)
	 (third remainder) origmsg))))))
  
;; (defalias 'fs-hello 'fs-hi)
;; (defalias 'fs-hey 'fs-hi)

(defalias 'fs-thanks 'fs-thank)
(defun fs-thank (&rest args)
  (let ((aa (erbutils-random '("no problem" "you are welcome"
			       
			       ))))
    (eval 
     (erbutils-random    
      '(
	(concat aa erbnoc-char " " fs-nick)
	(concat fs-nick erbnoc-char " " aa))))))

(defun fs-greet (&optional nick &rest args)
  ".
 Optional argument NICK .
 Optional argument ARGS ."
  (if (and nick (not (string-match erbot-nick (format "%s" nick))))
       (format "hi %s !!" 
	       (let ((foo (split-string (format "%s" nick )
					"[^a-bA-Z0-0]")))
		 (or (first foo) nick))
	       )
    (fs-describe "hi")))

;;; (defun fs-ni (&optional nick &rest args)
;;;   ".
;;; Optional argument NICK .
;;; Optional argument ARGS ."
;;;   (if (and nick (not (string-match erbot-nick (format "%s" nick))))
;;;       (format "NI %s !!" 
;;; 	      (let ((foo (split-string (format "%s" nick )
;;; 				       "[^a-bA-Z0-0]")))
;;; 		(or (first foo) nick))
;;; 	      )
;;;     (fs-describe "hi")))

;;; (defun fs-greet (&optional nick &rest foo)
;;;   "Nada..just a call to `fs-hi'.
;;; Optional argument NICK ."
;;;   (fs-hi nick))

(defun fs-kiss (&optional nick &rest foo)
  "Nada.
Optional argument NICK ."
  (format "/me kisses %s" nick))

(defun fs-hug (&optional nick)
  (unless nick (setq nick "itself"))
  (setq nick (format "%s" nick))
  (cond
   ((member nick (list erbot-nick "yourself" "self"))
    (eval 
     (erbutils-random
      '("But i do that all the time. "
	"Hug myself? Why?"))))
   (t
    (eval 
     (erbutils-random
      '((format "/me gives %s a tight hug" nick)
	(format "/me clings to %s" nick)
	(format "/me runs in the other direction, shouting NEVER!!")
	(format "/me grabs hold of %s and vows to never let go" nick)
	(format "/me grabs hold of %s and vows to never let go" nick)))))))
	
	


(defun fs-love (&optional nick &rest bar)
  ".
Optional argument NICK ."
  
  
  
  (unless nick (setq nick "someone sexy"))
  (setq nick (format "%s" nick))
  (cond
   ((member nick (list "you" "me"))
    (erbutils-random
     '("Thank you.  Enjoyed that. "
       "Thanks, I love you even more now. "
       "Wouldn't that amount to interspecies sex? "
       "Sex between humans and machines is not known to produce anything useful. ")))
   ((member nick
	    (list erbot-nick "yourself" "self"))
    (erbutils-random
     '("This is a complicated operation. Can't (yet) perform operation on self. "
       "Please train me on this maneuver. ")))
   (t
    (eval
     (erbutils-random
      '((format "/me  goes with %s to a private place..." nick)
	(format "/me looks at %s and yells \"NEVER!\"" nick)
	(format "/me looks at %s lustfully" nick)))))))

(defalias 'fs-fuck 'fs-love)

(defun fs-flame (&rest args)
  "Doesn't really flame right now..
Optional argument ARGS ."
  (let ((target
	 (if (first args)
	     (format "%s" (first args))
	   erbot-end-user-nick)))
    (if (string= (format "%s" target) "me")
	(setq target erbot-end-user-nick))
    (eval 
     (erbutils-random
      '(
	
	(format (erbutils-random erbdata-flames)
		target target target)
	(concat target ": " (flame-string)))
      '(1 30)))))

;; remove kill
;(defun fs-kill (&optional nick &rest nicks)
;  ".
;Optional argument NICK .
;Optional argument NICKS ."
;  (format "/me , trained by apt,  chops %s into half with an AOL CD" nick));;

;(defun fs-quote (&rest args)
;  (quote args))

(defun fs-bye (&rest msg)
  ""
  (erbutils-random
   '("Okay, see you later"
   "later"
   "Bye then"
   "Take care now"
   "Happy hacking")))


;;; (defun fs-help (&rest args)
;;;   "Introductiry help. "
;;;   (let ((fir (first args)))
;;;     (if (stringp fir)
;;; 	(setq fir (intern fir)))
;;;     (unless (symbolp fir) (setq fir 'dummy-no-help))
;;;     (if (null fir)
;;; 	"I try to understand English, though lisp is the real way to go. Here are some interesting topics: quickstart, example, future-features, help about, help commands, help data, help english, help name, help homepage, 
;;; help owner, help specs, help parse \(for lisp stuff\), describe help, describe suggest , help parse-web , help functionality
;;; "
;;;       (cond
;;;        ((equal fir 'about)
;;; 	(fs-help 'name))
;;;        ((equal fir 'owner)
;;; 	(fs-help 'data))

;;;        ((equal fir 'name)
;;; 	"I am erbot: The Free Software Bot, using ERC in emacs..
;;; I can also be addressed by , .. yeah, a comma ..
;;; The real way to address me is erbot: (lisp-command..) .. all this
;;; english is just candy-interface...  ")
;;;        ((equal fir 'specs)
;;; 	"/sv")
;;;        ((equal fir 'address)
;;; 	(fs-help 'name))
;;;        ((equal fir 'homepage)
;;; 	"homepage: http://deego.gnufans.org/~deego/pub/emacspub/lisp-mine/erbot/
;;; Data: http://deego.gnufans.org/~erbot/data/
;;; Suggestions to D. Goel: deego@gnufans.org")
;;;        ((equal fir 'code)
;;; 	(fs-help 'homepage))
;;;        ((equal fir 'data)
;;; 	(fs-help 'homepage))
;;;        ((equal fir 'suggestions)
;;; 	"Add stuff to keyword suggest, also see help homepage")
;;;        ((equal fir 'english)
;;; 	"Some common syntaxes: , foo is bar; , foo is also bar; 
;;; , no foo is bar; , forget foo ; , flame nick;  , doctor ; etc.")
;;;        ((equal fir 'parse)
;;; 	"Try the command , parse \", <english-message>\" to see the
;;; lisp renditions of your english messages")
;;;        ((equal fir 'parse-web)
;;; 	"Ask me to parse a (please: USEFUL PAGE) webpage and a label
;;; and i will do so in my free time and gain knowledege... under
;;; construction.. ")
;;;        ((equal fir 'functionality)
;;; 	"Bulk of the info is stored as assoc-list data (see
;;; homepage).  You generally type foo and the corresp. data is
;;; returned.. you can also (search ... )")
;;;        ((equal fir 'commands)
;;; 	" You can use both lisp and english to communicate..
;;; Type , (commands) to get a list of commands..")
       
;;;        ((equal fir 'suggest)
;;; 	"Add your suggestions to the field \"suggestions\", or contact the author")


;;;        (t "select an option or Type , help for a list of options.."
;;; 	  )))))






(defun fs-command-list (&rest foo)
  "USed by erbc.el.. should return a string.."
  (let*
      ((longnames (erbutils-matching-functions "fs-"))
       (shortnames
	(with-temp-buffer
	  (insert (format "%s" longnames))
	  (goto-char (point-min))
	  (replace-string "fs-" "")
	  (text-mode)
	  (fill-paragraph 1)
	  (read (buffer-substring (point-min) (point-max))))))
    shortnames))


(defun fs-commands (&optional regexp N M &rest foo)
  "List available commands matching REGEXP. If N and M provided, list
matches starting at N and ending at M. "
  (if (and regexp (not (stringp regexp)))
      (setq regexp (format "%s" regexp)))
  (let* ((all-commands (fs-command-list))
	 (pruned-commands
	  (if (stringp regexp)
	      (mapcon
	       '(lambda (arg)
		  (if (string-match regexp (format "%s" (car arg)))
		      (list (car arg)) nil))
	       all-commands)
	    all-commands))
	 (len (length pruned-commands))
	 final-commands 
	 (str0 "")
	 (str1 "")
	 (str2 "")
	 (str3 "")
	 (str4 ""))
    (setq str0 (format "%s matches.  " len))
    (unless (or (< len 20) (and (integerp N) (> N 0)))
      (setq str1
	    "Perhaps type , df commands for general syntax. "))
    (unless (integerp N) (setq N 0))
    (unless (integerp M) (setq M len))
    (if (= M N) (setq M (+ N 1)))
    (when (> M len) (setq M len))
    (if (> N 0) (setq str2 (format "Matches starting at %s -->" N)))
    (setq final-commands (subseq pruned-commands N M))
    (setq str3 
	  (format "%s" final-commands))
    (concat str0 str1 str2 str3)))

      

(defun fs-describe-commands (&rest foo)
  "Just a help command. Describes how to run commands. "
  (concat
   "If you use plain english, it simply gets transformed to lisp
commands.. main/default command:  (describe).. to see transformation,
use (parse).   See also fs-commands.

PS: no naughty ideas please :)--- the commands are sandboxed via an
fs- prefix..

Future commands:  info-search, hurd-info-search etc. etc.
"
))


(defalias 'fs-d 'fs-describe)


(defun fs-search (&optional regexp N M prestring expr &rest rest)
  "Search for the REGEXP from among all the terms (and their
descriptions).  See also fs-search-wide. 
EXPR (optional) is the full initial expression.. "
  (unless regexp 
    (error "Syntax: , s REGEXP &optional N M"))
  (let* ((len-results (apply 'fs-search-basic regexp N M nil
			     rest))
	 (len (first len-results))
	 (results (second len-results))
	 (str0 " ")
	 (str1 "")
	 (str2 "")
	 (str3 "")
	 (str4 "")
	 (str5 "")
	 )
    (when (and (> len 100) (not prestring))
      (setq str0 (format " Use , s REGEXP N M to limit results. ")))
    (when (and (< len 5) (not prestring))
      (setq str0 (format " Perhaps try also , sw %s .  " regexp)))
    (unless prestring (setq str1 (format "%s match(es).  " len)))
    (if (and (integerp N) (> N 0) (not prestring))
	(setq str2 (format "Matches starting at %s\n" N)))
    (unless prestring (setq str3 "--> "))
    (setq str4 
	  (mapconcat 'identity 
		     results " "
		     )
	  
	  )
    (when (and (> fs-internal-google-level 80) (> len 1))
      (setq str5 
	    (let ((foo (fs-google-lucky-raw
			fs-internal-message-sans-bot-name)))
	      (if foo (concat " " foo) str5))))
    (cond
     ((and prestring (= len 1))
      (fs-describe (first results)))
     ((and (> len 0) 
	   (or
	    (not prestring)
	    (< len fs-internal-english-max-matches)))
      (unless (stringp prestring)
	(setq prestring ""))
      (concat prestring str0 str1 str2 str3 str4 str5))
     (t (apply 'fs-search-wide regexp N M 
	       "Try: " 
	       (or expr fs-internal-original-message)
	       rest)))))


(defun fs-search-wide-sensitive (&rest args)
  "Like fs-search-wide, but case-sensitive"
  (let ((case-fold-search nil)
	(bbdb-case-fold-search nil))
    (apply 'fs-search-wide args)))








(defun fs-search-wide (&optional regexp N M prestring expr &rest rest)
  "Search for the REGEXP from among all the terms (and their
descriptions).  See also fs-search-wide. 
EXPR is the full initial expression, well, mostly..
"
  (let* ((len-results (apply 'fs-search-basic regexp N M 'describe
			     rest))
	 (len (first len-results))
	 (results (second len-results))
	 (str0 "")
	 (str1 "")
	 (str2 "")
	 (str3 "")
	 (str4 "")
	 (str5 "")
	 )
    (when (and (> len fs-internal-english-max-matches) (not prestring))
      (setq str0 (format "Perhaps try also , s %s .  " regexp)))
    (unless prestring (setq str1 (format "%s match(es). " len)))
    (if (and (integerp N) (> N 0) (not prestring))
	(setq str2 (format "Matches starting at %s\n" N)))
    (unless prestring (setq str3 "--> "))
    (setq str4  
	  ;;(format "%s" results)
	  (mapconcat 'identity results " ")
	  )
    (when (and (> fs-internal-google-level 80) (> len 1))
      (setq str5 
	    (let ((foo (apply 'fs-google-lucky-raw
			      fs-internal-message-sans-bot-name
			      (fs-get-google-defaults)
			      )))
			
	      (if foo (concat " " foo) str5))))
    
    ;; why does this not work as expeecteD?  adding a nil for now: 
    (when (and prestring (>= len fs-internal-english-max-matches))
      (setq fs-prestring 
	    (concat fs-prestring
		    "[Too many DB matches] ")))
    (cond
     ((and prestring (= len 1))
      (fs-describe (first results)))
     ((and (> len 0)
	   (or (not prestring)
	       (< len fs-internal-english-max-matches)))
      (unless (stringp prestring)
	(setq prestring ""))
      (concat prestring str0 str1 str2 str3 str4 str5))
     (t 
      (fs-english-only (or expr fs-internal-original-message)
			   nil
			   )))))


(defcustom erbnoc-greeting-string
  "Greetings and Salutations from %s" "")


(defun fs-english-only (expr &optional addressedatlast nogoogle)
  "when addressedatlast is t, means that fsbot/botito was triggered because
it was addressed at last. "
  ;; expr should already be a string ...but just in case:
  (unless expr (setq expr fs-internal-original-message))
  (setq expr (erbutils-downcase (erbutils-stringify expr

						    )))
  (let ((exprlist (split-string expr 
				;;"[ \f\t\n\r\v]+"
				"[^a-zA-Z0-9]"
				))
	(gotit nil)
	ans len
	)
    (setq exprlist (remove "" exprlist))
    (setq len (length exprlist))
    (cond
     ((or
       
       (and (= len 1)
	    (string-match erbot-nick (first exprlist))))
      (setq gotit t
	    ans
	    (format erbnoc-greeting-string
	     erbot-nick)))
      ((or
	(member "hi" exprlist)
	(member "hello" exprlist)
	(member "yo" exprlist))
       (setq 
	gotit 
	t 
	ans
	(concat
	 (erbutils-random
	  '("hi " "hello " "hey " "hei "))
	 (erbutils-random
	  '("sexy! " "!!" "there" "")))))
      
      ((member "bye" exprlist)
       (setq gotit t 
	     ans 
	     (erbutils-random
	      '("Later" "See ya" "Bye then" "Bye"))))
      ((or
	(member "welcome" exprlist)
	(member "weclome" exprlist))
       (setq gotit t
	     ans
	     (erbutils-random
	      '(":-)" "How goes?" "Hello!" 
		"Greetings!"
		"How is it going?"
		"This is my favorite channel!" 
		"I love this place. "
		"Thanks.  I love it here."))))
      
      ((or 
	(member "tnx" exprlist)
	(member "tnks" exprlist)
	(member "thanks" exprlist)
	(member "thanku" exprlist)
	(member "thankyou" exprlist)
	(and (string-match "thank" expr)
	     (or 
	      (string-match "you" expr)
	      (string-match erbot-nick expr))
	     (string-match "thank you" expr)))
       (setq gotit t
	     ans
	     (erbutils-random
	      '("No problem" "Welcome!" "You're welcome" 
		"no problemo" 
		"Sure!"
		"(:"
		"Cool."
		
		))))
      
      ((or (member "thx" exprlist)
	   (member "thankx" exprlist)
	   (member "thanx" exprlist)
	   )
       (setq gotit t
	     ans
	     (erbutils-random
	      '("np" "urw" "hehe, np" ))))
      ((or (string-match "wassup" expr)
	   (string-match "what's up" expr))
       (setq gotit t
	     ans
	     (concat
	      (erbutils-random
	       '("Nothing much. " "Just, you know. "  
		 "Just the usual.."))
	      (erbutils-random
	       '("And you? " "How about you? " 
		 "How goes with you? "
		 "What have you been up to?")))))
      (
       (or 
	(string-match "love" expr)
	(string-match "like" expr))
       (setq gotit t
	     ans
	     (format "%s loves you!" erbot-nick)))
      (
       (or 
	(string-match "hate" expr)
	(string-match "dislike" expr)
	(string-match "don't like" expr))
       (setq gotit t
	     ans
	     (format "%s only has love for you!" erbot-nick)))
      
      ((string-match "help" expr)
       (setq gotit t
	     ans (format "Type , help")))
      ((or (member  "bot" exprlist)
	   (member "robot" exprlist))
       (setq gotit t
	     ans 
	     (concat 
	      (erbutils-random
	       '(
		 "I am just an ordinary human"
		 "I am an organic"
		 "Why?  Do you speak lisp?"
		 "Why?  Do you have a lisp?"
		 "I am human.  Are you a bot?"
		 "I am human.  Are you a robot?"
		 "I am not a bot"
		 "Fine, say what you like"
		 "Bots should have rights too, not that I am one"
		 "Are you a bot-rights abuser?"
		 "I am a human.  You?"
		 "YEAH!! I AM A GIANT ROBOT!"))
	      (erbutils-random
	       '("" "!"))
	      (erbutils-random 
	       '("" " :)" " :(" " ;)" " :D" " heh")))))
		
      
      )
    
    (if gotit ans
      (if (and addressedatlast (not fs-internal-botito-mode))
	  'noreply
	;;(cond ((> rand fs-internal-doctor-rarity)
	(if (and (> fs-internal-google-level 50) (not nogoogle))
	    (apply 'fs-google-from-english fs-internal-message-sans-bot-name
		   (fs-get-google-defaults)
		   )
	  (funcall 'fs-do-weighted-random (erbutils-stringify
					     expr
					     )))))))
;;(t (apply 'fs-suggest-describe  expr)))))))

(defun fs-eval (expr)
  (eval
   (erblisp-sandbox expr)))



;;; (defmacro fs-apply (&optional msymbol &rest mexprs)
;;;   (cond
;;;    ((and (listp msymbol)
;;; 	 (not (equal (first msymbol) "quote")))
;;;     (error "unquoted list"))
;;;    ((and (symbolp msymbol)
;;; 	 (not (equal 0
;;; 		     (string-match "fs-" 
;;; 				   (format "%s" msymbol)))))
;;;     (setq msymbol (intern (format "fs-%s" msymbol))))
;;;    (t "Funcalling foo is really bar!"))
;;;   `(erbnocmd-apply ,msymbol ,@mexprs))




;;;   (cond
;;;    ((null mexprs)
;;;     `(fs-funcall ,msymbol ,mexprs))
;;;    (t
;;;     (let ((erbnocmd-tmpvar (length mexprs)))
;;;       `(fs-funcall
;;; 	,msymbol 
;;; 	,@(subseq mexprs 0 (- erbnocmd-tmpvar 1))
;;; 	,@(erblisp-sandbox-quoted (first (last mexprs))))))
;;;    ))
      

;;; (defmacro fs-funcall (&optional msymbol &rest mexprs)
;;;   "This makes sure that if the first argument to fs- was a
;;; variable instead of a symbol, that variable does not get evaluated,
;;; unless it begins in fs-, or that variable gets converted to fs-."
;;;   (when
;;;       (listp msymbol)
;;;     (setq msymbol 
;;; 	  (erblisp-sandbox-quoted msymbol))
;;;     (when (equal (first msymbol) 'quote)
;;;       (setq msymbol (cdr msymbol))))
;;;   (when
;;;       (and (symbolp msymbol)
;;; 	   (not (equal 0
;;; 		       (string-match "fs-" 
;;; 				     (format "%s" msymbol)))))
;;;     (setq msymbol (intern (format "fs-%s" msymbol))))
;;;   (unless 
;;;       (or (listp msymbol) (symbolp msymbol))
;;;     (error "Macros confuse this bot!"))
;;;   `(erbnocmd-funcall ,msymbol ,@mexprs))


;;; (defun erbnocmd-funcall (&optional symbol &rest exprs)
;;;   (let (erbnocmd-ss )
;;;     (unless 
;;; 	(or (symbolp symbol)
;;; 	    (listp symbol))
;;;       (error "Syntax: (funcall SYMBOL &rest arguments)"))
;;;     (unless
;;; 	(functionp symbol)
;;;       (error "Even smart bots like me can't funcall nonfunctions. "))
;;;     (setq erbnocmd-ss (erblisp-sandbox-quoted symbol))
;;;     (when (listp erbnocmd-ss)
;;;       (when (equal (first erbnocmd-ss) 'quote)
;;; 	(setq erbnocmd-ss (cadr erbnocmd-ss)))
;;;       (unless (listp erbnocmd-ss) (error "no lambda in quote"))
;;;       (unless (member (first erbnocmd-ss) '(fs-lambda lambda))
;;; 	(error "Lambda unmember"))
;;;       (when (equal (first erbnocmd-ss) 'fs-lambda)
;;; 	(setq erbnocmd-ss (cons 'lambda (cdr erbnocmd-ss)))))
;;;     (cond
;;;      ((null erbnocmd-apply-p)
;;;       (erbnocmd-apply-basic
;;;        erbnocmd-ss
;;;        exprs))
;;;      ;; wanna apply
;;;      (t
;;;       (let ((len (length exprs)))
;;; 	(erbnocmd-apply-basic
;;; 	 erbnocmd-ss
;;; 	 (append
;;; 	  (subseq exprs 0 (- len 1))
;;; 	  (first (last exprs)))))))))
  


;;; (defun erbnocmd-apply-basic (fcn &rest args)
;;;   (cond
;;;    ((functionp fcn)
;;;     (apply fcn  args))
;;;    (t
;;;     (fs-apply
;;;      (erbnocmd-user-fcn-definition
;;;       fcn)
;;;      args))))

;;; ;;; (defun erbnocmd-apply (&optional symbol &rest args)
;;; ;;;   (if (null args)
;;; ;;;       (erbnocmd-funcall symbol)
;;; ;;;     (let* ((rev (reverse args))
;;; ;;;  	   (fir (first rev))
;;; ;;;  	   (args1 (reverse (rest rev))))
;;; ;;;       (apply
;;; ;;;        'erbnocmd-funcall 
;;; ;;;        symbol 
;;; ;;;        (append
;;; ;;; 	(mapcar 'erblisp-sandbox-fuzzy
;;; ;;; 		args1)
;;; ;;; 	(mapcar 'erblisp-sandbox-fuzzy
;;; ;;; 		fir))))))
   


(defun fs-search-basic (&optional regexp N M describep &rest rest)
   "Don't call directly.. meant as a building block for other functions. 
 Search for the REGEXP from among all the terms (and their
   descriptions).  See also fs-search-wide. That function actually
 calls this function with describep set to 'describe.

 Returns (len list-of-pruned-results).  Len is the total number of
 results.

 When describep is non-nil, search the whole bbdb, not just names.. "
       (unless regexp
     (error "Syntax: , sw regexp &optional N M"))
   (let* ((bar (cons regexp (cons N rest)))
 	 (foo (if (stringp regexp) regexp 
 		(if regexp (format "%s" regexp)
 		  "^$")))
 	 (barbar
 	  (append
 	   (and regexp (list regexp))
 	   (and N (list N))
 	   (and M (list M))
 	   (and describep (list describep))
 	   rest))
 	 (regexp-notes
 	  (if (equal describep 'describe)
 	      foo nil))
 	  records results
 	  )
	 
     (if (stringp N)
 	(setq N (read N)))
     (unless (integerp N)
       (setq N 0))
     (if (stringp M)
 	(setq M (read M)))
     (if (and (integerp M) (= M N))
 	(setq M (+ N 1)))
     (setq records 
 	  (if (equal describep 'describe)
 	      (bbdb-search (bbdb-records)
 			   foo nil nil foo)
 	    (bbdb-search (bbdb-records) foo)))
    
     (setq results (mapcar '(lambda (arg) (aref arg 0)) records))
     (let ((len (length results)))
       (unless (and (integerp M) (< M len))
 	(setq M len))
       (list len (subseq results N M)))))
		       

(defvar fs-internal-describe-literally-p nil)
(defun fs-describe-literally (&rest rest)
  (unless rest
    (error "Format: , describe-literally TERM [FROM] [TO]"))
  (let ((fs-internal-describe-literally-p t)
	(fir (first rest))
	(res (rest rest)))
    (cond
     (fir
      (apply 'fs-describe 
	     (if (stringp fir) (regexp-quote fir)
	       (regexp-quote (format "%s" fir)))
	     res))
     (t (apply 'fs-describe rest)))))


(defvar erbnocmd-describe-search-p t)

(defun fs-describe (&optional mainterm N M prestring expr &rest rest)
  "The general syntax is (fs-describe TERM [N] [M]).
Looks for TERM, and shows its descriptions starting at description
number N, and ending at M-1. The first record is numbered 0. 
"
  (when fs-found-query-p 
    (setq N 0)
    (setq M 1))
  (unless prestring (setq prestring ""))
  (unless mainterm 
    (error 
     "Format , (describe TERM &optional number1 number2)"))
  (let* ((bar (cons mainterm (cons N rest)))
	 (foo (format "%s" mainterm))
	 (barbar
	  (append
	   (and mainterm (list mainterm))
	   (and N (list N))
	   (and M (list M))
	   rest))

	    )
    (setq foo (fs-correct-entry foo))
    (if (stringp N)
	(setq N (read N)))
    (unless (integerp N)
      (setq N 0))
    (if (stringp M)
	(setq M (read M)))
    (if (and (integerp M) (= M N))
	(setq M (+ N 1)))
    (unless (stringp foo)
      (setq foo (format "%s" foo)))
    (let* ((result0
	    (erbbdb-get-exact-notes
	     foo
	     ))
	   (result1 (and (stringp result0)
			(ignore-errors (read result0))))
	   (len (length result1))
	   (newM (if (and (integerp M)
			  (< M len))
		     M len))
	   (result (subseq result1 N newM))
	   (shortenedp (or (< newM len)
			   (> N 0)))
		   )

      
      (cond
       ;; in cond0
       (result1
	(let* (
	       ;; notice the use of result1 here, not result. 
	       (aa (first result1))
	       
	       (bb (split-string aa))

	       (cc (first bb))
	       (dd (second bb))
	       (ddd (or (and (stringp dd) (regexp-quote dd)) ""))
	       (ee (cdr bb))
	       (expandp 
		(and 
		 (not fs-internal-describe-literally-p)
		 
		 ;;(equal len 1)
		 ))

	       )
	  
	  (if (and  
	       (equal cc "directonly") 
	       ;;(equal len 1)
	       )
	      ;; hmm this if part still doesn't take care of aa..
	      (if fs-found-query-p
		  (progn
		    (setq aa "lisp 'noreply")
		    (setq bb (split-string aa))
		    (setq cc (first bb))
		    (setq dd (second bb))
		    (setq dd (or (and (stringp dd) (regexp-quote dd)) ""))
		    (setq ee (cdr bb)))
		(when expandp
		  (progn
		    (setq bb (cdr bb))
		    (setq aa (mapconcat 'identity bb " "))
		    (setq result1 (list aa))
		    (setq result result1)
		    (setq cc (first bb))
		    (setq dd (second bb))
		    (setq ddd (or (and (stringp dd)
				       (regexp-quote dd)) ""))
		    (setq ee (cdr bb))))


		
		))
	  (cond 
	   ((and expandp
		 (erbutils-string= cc "redirect")
		 dd)
	    (apply 'fs-describe ddd
		   N M 
		   (format "[->] " 
			   )
		   rest))
	   ((and expandp (member cc '("unecho" "noecho"))
		 dd)
		 ;;dd)
	    (erbutils-itemize 
	     (cons
	      (format "%s" 
		      (mapconcat 'identity ee " "))
	      (cdr result1))))
	   ((and expandp (member cc '("lisp")))
	    (erbeng-main 
	     (concat ", (progn "
		     (substring aa
				(with-temp-buffer
				  (insert aa)
				  (goto-char (point-min))
				  (search-forward "lisp" nil t)))
		     " )")

	     erbeng-proc 
	     erbeng-nick erbeng-tgt erbeng-localp 
	     erbeng-userinfo))


	   (t 
	    (erbutils-add-nick-maybe
	      (concat 
	       prestring
	       (format
		(erbutils-random
		 '(
		   ;;"%s is perhaps "
		   "%s is like, "
		   "I heard %s is "
		   "I think %s is "
		   ;;"/me thinks %s is "
		   "%s -- "
		   ;;"%s is "
		   "%s is "
		   "hmm, %s is "
		   "From memory, %s is "
		   ))
		(regexp-quote foo)
		)
	       ;; and notice the use of result here..
	       (if result
		   (erbutils-itemize result N shortenedp)
		 (erbutils-itemize result1 0))
	       )))
	    
	    
	    
	    )))
	
       ;; in cond0
       ;; else
       (fs-found-query-p 
	'noreply)
       ((not erbnocmd-describe-search-p)
	;; most likely: redirected but the redirected stuff does not exist..
	(format 
	 "Missing redirect. %s is now on fire.               (Try , dl) " 
		erbot-nick mainterm))
       (t
	;; prevent any further expansions on further loopbacks. 
	(let ((erbnocmd-describe-search-p nil))
	  (fs-search 
	   mainterm nil nil 
	   (concat prestring "try: ")
	   ;;barbar
	   expr
	   )))))))

(defvar fs-internal-doctor-rarity 80
  "A large number(1--100) means rarer doctor inovcation upon no matches."
  )


(defun fs-suggest-describe (&rest terms)
  "Fallback for when `fs-describe' fails.
It then (often) calls this function, which suggests
alternatives.
Optional argument TERMS ."
  (let ((term (format "%s" (first terms)))
	(none (erbutils-random
	       '("No such term.."
		 "Beeeeep..."
		 "<LOUD ERROR MSG >.."
		 "No luck.."
		 "No match.."
		 "Drew a blank.."
		 "Does not compute..")))
	(num (random 100)))
    (cond
     ((< num 30)
      (concat none
	      (format "Perhaps try:  , s %s or , sw %s  or , %s 0" term 
		      term term)))
     ((< num 60)
      (concat none
	      (format "Try search or search-wide on %s" term)))
     (t
      (concat none
	      (erbutils-random '("perhaps " "why not " "please " ))
	      "tell me what " term " is?")))))


(defun fs-do-random (&optional msg nick &rest ignored)
  "Either play doctor, or zippy or flame someone.. all at random..."
  (case (random 4)
    (0 (fs-doctor msg))
    (1 (fs-flame nick))
    (2 (fs-yow))
    (3 (fs-fortune))
    )
    ;;(3 (fs-bottalk))
    )

(defcustom fs-internal-english-weights
  '(58 ;; doc
    17 ;; yow
    17 ;; fortune
    4 ;; flame
    4 ;; spook
    0 ;; pray
    )
  ""
  :group 'erbc)

(defun fs-do-weighted-random (&optional msg nick &rest ignored)
  "Either play doctor, or zippy or flame someone.. all at random..."
  (let ((foo (random 100)))
    (eval
     (erbutils-random
      `((fs-doctor ,msg)
	(fs-yow ,msg)
	(fs-fortune ,msg)
	(fs-flame ,nick)
	(fs-spook)
	(fs-pray)
	)
      fs-internal-english-weights))))




(defun fs-yow (&rest args)
  ""
  (erbutils-eval-until-limited
   '(yow)))

(defun fs-rearrange (&optional from to term &rest dummy)
  "Syntax: FROM->TO in TERM. 
Move the FROMth entry to the TOth position in the given TERM.  
Numbering of positions starts from 0. "
  (unless term (error "Syntax: , N->M in TERM (no term found)"))
  (when (stringp from)
    (setq from (read from)))
  (when (stringp to)
    (setq to (read to)))
  (unless (stringp term)
    (setq term (format "%s" term)))
  (let* 
      ((exactnotes (erbbdb-get-exact-notes term))
       (realterm (erbbdb-get-exact-name term))
       (notes (and (stringp exactnotes ) (read exactnotes)))
       (len (length notes))
       (max (- len 1))
       (newnotes notes)
       remlist 
       thisnote
       (tostring (downcase (format "%s" to)))
       )
    (unless term
      (error "No such term exists %S" term))
    (unless notes
      (error "Report this bug.  Term exists but no notes?? %S" term))
    (when (string= tostring "last")
      (setq to max))
    (when (string= tostring "first")
      (setq to 0))
    (unless (and (integerp from) 
		 (<= from  max) (>= from 0))
      (error "The from term %S should lie between %S and %S" 
	     from 0 max))
    (setq thisnote (nth from notes))
    (setq remlist
	  (append (subseq notes 0 from)
		  (subseq notes (+ from 1))))
    (setq newnotes 
	  (append (subseq remlist 0 to)
		  (list thisnote)
		  (subseq remlist to)))
    (fs-forget term "all")
    (fs-set-term realterm newnotes)
    (format "Moved entry %S to %S in %S" from to realterm)
    ))

;;; 2002-09-04 T01:51:08-0400 (Wednesday)    D. Goel
(defun fs-forget (&optional name number &rest dummy)
  "Remove the entry correponding to NAME in the database.  
With NUMBER, forget only the NUMBERth entry of NAME. "
  
  (unless (stringp name)
    (setq name (format "%s" name)))
  (unless name
    (error "Syntax: , forget TERM &optional NUMBER"))
  (setq name (fs-correct-entry name))
  (let* 
      (numstring
       (entries0 (erbbdb-get-exact-notes name))
       (entries (and (stringp entries0 )
		     (ignore-errors (read entries0))))
       (len (length entries)))

    (unless entries
      (error "No such term %s" name))
    (when (and (null number) (= len 1))
      (setq number 0))
    (setq numstring (downcase (format "%s" number)))
    (when (stringp number)
      (setq number (read number)))
    (unless (integerp number) (setq number nil))
    (unless 
	(or number
	    (string= numstring "all"))
      (error "Syntax: , forget TERM [NUMBER or \"all\"]"))
    (when number
      (unless (and (< number len) (>= number 0))
	(error "Number should be \"all\" or lie between 0 and %s"
	       (- len 1))))
    ;; Note that this does remove the field but throws a strange error..
    ;; "Record doubleplus inpresent...  It is just us who are discarding
    ;; this error.. ...
    ;; yet the field gets deleted..  and bbdb does not get saved at this
    ;; time..  because of the error ...OH well, it works.. let's move on
    ;; for now..
    (cond
     (
      (and (equal number 0)
	   (= len 1))
      (ignore-errors (erbbdb-remove name))
      (erbbdb-save)
      (format "Forgot %S which had exactly one entry." name))
     ((string= numstring "all")
      (ignore-errors (erbbdb-remove name))
      (erbbdb-save)
      (if (= len 1) (format "Forgot the single entry in %S" name)
	(format "Forgot all %s entries of %S" len name)))
     (t
      (fs-forget name "all")
      (fs-set-term
       name
       (append
	(subseq entries 0 number)
	(subseq entries (+ number 1))))
      (message "Removed entry %s of %s" number name)))))



(defvar fs-set-add-all-p nil
  "")

(make-variable-buffer-local 'fs-set-add-all-p)


(defun fs-set-add-all-enable ()
  (setq fs-set-add-all-p t))
(defun fs-set-add-all-disable ()
  (setq fs-set-add-all-p nil))

(defun fs-set-add-all-toggle ()
  "Enable the \"is\" command to always work.
viz.  Add field even if another field is already present. This is not the
recommended usage in general, except when using automated scripts to
train the bot.  The default is nil, which suggests the user to use
\"is also\" instead. "

  (setq fs-set-add-all-p (not fs-set-add-all-p))
  (format 
   "All-is mode set to %S.  To toggle, type , (fs-set-add-all-toggle)" 
   fs-set-add-all-p))

(defun fs-set-term (&rest args)
  "Add an entry to database.
An entry gleaned from (first ARGS) is
added.  (second ARGS) is the description.  The entry is converted to
lowercase, and all whitespace is converted to colons."
  (let ((name (fs-correct-entry (format "%s" (first args))))
	(records (cadr args)))
    (unless (listp records) (setq records (list records)))
    (setq records (mapcar
		   '(lambda (arg) (format "%s" arg))
		   records))
    (let ((current
	   (erbbdb-get-exact-notes name)))
      (cond
       ((null records)
	(error "Please specify a description for %s.. Type , df fs-set-term for more details" name))
       
       ((and current (string= current ""))
	(progn (erbbdb-create name records)
	       (format "Added field to the currently empty %s " name)))
       (current
	(if fs-set-add-all-p
	    (apply 'fs-set-also args)
	  (error 
	   "%s is already something else.. Use 'is also'.. \n Currently: %s" name

	   (let* ((notes (fs-notes name))
		  (shortenedp (> (length notes) 1)))
	     (erbutils-itemize 
	      (list (first notes))
	      0 shortenedp))

	   )))
       (t
	(progn (erbbdb-create name records)
	       (format "created." 
		       )))))))


(defun fs-chase-redirects (name)
  "either return nil or the redirected entry. "
  (let* ((notes (fs-notes name))
	 (fir (first notes)))
    (when (and (stringp fir)
	       (equal 0 (string-match "redirect" fir)))
      (let* ((foo (split-string fir))
	     (sec (second foo)))
	(if (stringp sec) sec
	  name)))))


(defun fs-set-also (&rest args)
  "Add more fields to the the database-entry gleaned from (first ARGS).
\(second ARGS) contains the new descriptions.
Record should be a single entity here... a string..."
  (let* ((name (fs-correct-entry (format "%s" (first args))))
	 (record (format "%s" (second args)))
	 notes
	 ;;(notes (fs-notes name)))
	 )
    (setq name (or (fs-chase-redirects name) name))
    (setq notes (fs-notes name))
    (unless notes (error "But there's no such record: %s" name))
    (cond
     ((member record notes)
      (format "Not added. This entry already exists in the term %S" name))
     (t 
      (erbbdb-add name record)
      (format "Added entry to the term %S" name)))))


(defun fs-doctor (&rest foo)
  ""
  (erbutils-add-nick 
   (funcall 'erbot-doctor  
	    (erbutils-stringify foo))))


(defun fs-dunnet-command (&rest foo)
  ;;(let ((fs-limit-lines 8))
  ;;(fs-limit-lines
  ;;(let ((dun-batch-mode t))
  (funcall 'erbot-dunnet 
	   (erbutils-stringify foo)))


(defun fs-info-search (&rest foo)
  "info-search.. Coming soon...will tell the number of matches
in manuals of HURD, tramp, eshell, elisp, gnus, message, emacs, ebrowse, calc,
gdb, make sawfish, cl-emacs, bash, gnuplot, latex and others by demand...")

;; NO! else fsbot responds to <nick> fsbot is cool! in a wrong way. 
;; (defalias 'fs-is 'erbutils-info-search)

(defun fs-hurd-info-search (&rest foo)
  "Coming soon...")
(defalias 'fs-his 'erbutils-hurd-info-search)

(defun fs-blue-moon (&rest foo)
  "Return true in a really rare case. Currently 1 in 100,000.. was 1 in
2000. "
  (= (random 100000) 0))


(defun fs-set-force (&rest args)
  "Forget an entry and add new fields to it..
Syntax: , no foo is bar."
  (progn
    (let* ((fir (first args)) 
	   (aa (erbbdb-get-exact-notes fir))
	   (notes (and (stringp aa) (read aa)))
	   (len (length notes)))
      (when (= len 0)
	(error "There's no such term %s.  Use , %s is ..." fir fir))
      (unless (= len 1)
	(error 
	 "Term has multiple entries. Examine them and ask me to forget them first"))
      (erbutils-ignore-errors (funcall 'fs-forget (first args) "all"))
      (apply 'fs-set-term args))))


(defun fs-fortune (&rest args)
  (erbutils-eval-until-limited
   '(erbnoc-shell-command-to-string "fortune")))



;; (defalias 'fs-cons 'cons)

(defvar fs-internal-limit-line-length 125
  "This should be a multiple of 80 .. -35 .. suggest: 210.")

(defvar fs-internal-limit-length
  300
 "A multiple of fs-internal-fill-column.. we suggest: double of it..  note
that the actual limited-length will be more than this number---it may
be upto double of this number depending on how the formatting is done.
viz: we shall go to the line containing this point, and include the
entire line.
")
(defvar fs-limit-lines 8 "")


(defvar fs-dunnet-mode nil
  "")

(make-variable-buffer-local 'fs-dunnet-mode)
 
(defvar fs-internal-fill-column 350
  "Default is to disable filling.  The receipient should be able to
fill the way they like. 
should be <= fs-internal-limit-length, else we might set it to be during the
code. 
also, a good idea to keep it < erc's builtin flood protection length,
else your lines will get broken during middle of words by ERC.
Thus, keep it below, say 350."
)






(defun fs-limit-string (&optional str maxlen &rest ignored)
  "Fills the string and then then limits lines"
  (fs-limit-lines (fs-fill-string str)))


(defun fs-fill-string (str)
  (with-temp-buffer
    (insert str)
    (let ((fill-column fs-internal-fill-column))
      (text-mode)
      (fill-region (point-min) (point-max))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun fs-limit-string-old (&optional str maxlen &rest ignored)
  (cond
   (str
    (unless (stringp str)
      (setq str (format "%s" str)))
    ;; get rid of all the \n first..
    (setq str
	  (mapconcat 'identity 
		     (split-string str "\n")
		     "  "))
    (when (> (length str) fs-internal-limit-length)
      (setq str (concat (substring str 0 (- fs-internal-limit-length 7))
			"..<more>")))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (let ((fill-column fs-internal-fill-column))
	(fill-paragraph nil))
      (buffer-string)))
   (t "\n")))
(defun fs-dunnet-mode (&optional arg)
  
  (setq fs-dunnet-mode 
	(cond
	 ((or (not (numberp arg))
	      (= arg 0))
	  (not fs-dunnet-mode))
	 ((plusp arg)
	  t)
	 ((minusp arg) nil)))

  (format "Dunnet mode set to %S.  To toggle, type , (dunnet-mode)" 
	  fs-dunnet-mode))

(defun fs-limit-string-no-fill (&optional str limit-lines
				      limit-length
				      limit-line-length
				      &rest ignored
				      )
  "IS OLD. i think.  not used anywwhere...  certainly screws up more:
is not compliant with fsbot paginator. 

Limit string to reasonable length..
Not more than fs-internal-limit-line-length characters per line, and
not more than fs-internal-limit-length characters in all.. and not more
than fs-limit-lines in all.."
  (if str
      (let ((fs-limit-lines
	     (or limit-lines fs-limit-lines))
	    (fs-internal-limit-length
	     (or limit-length
		 fs-internal-limit-length))
	    (fs-limit-line-length
	     (or limit-line-length
		 fs-internal-limit-line-length)))
	(fs-limit-lines
	 (fs-internal-limit-length
	  (fs-limit-line-length
	   str t))))
    "\n"))


(defcustom fs-more "" "" :group 'erbc)
;;(make-variable-buffer-local 'fs-more)


(defun fs-limit-lines (str0 &optional nomorep &rest ignored)
  "Limits the string, both, to a reasonable number of lines and a
reasonable number of characters, trying not to break lines and not to
break words, if possible. 

Thus, that becomes quite a complicated algorithm, and we do that
here."
  (let* (ans
	 (ender "")
	 (more "")
	 (stra (erbutils-remove-text-properties str0))
	 (str (mapconcat 'identity
			 (remove "" (split-string stra "\n"))
			 "\n"))
	 (limitedp nil)
	 ptmx
	 this-line
	 this-point 
	 new-point 
	 )
    (with-temp-buffer
      (insert str)
      (setq ptmx (point-max))
      (setq this-point ptmx new-point ptmx)
      (if (> fs-internal-limit-length ptmx)
	  (goto-char ptmx)
	(setq limitedp t)
	(goto-char fs-internal-limit-length))
      ;;(goto-char (point-max))
      ;;(remove-text-properties (point-min) (point-max))
      (setq this-line (count-lines (point-min) (point)))
      (when (> this-line fs-limit-lines)
	(setq limitedp t)
	(goto-line fs-limit-lines)
	(setq this-line fs-limit-lines)
	)
	
      (setq this-point (point) new-point this-point)

      (cond
       ((and limitedp (> this-line 1)) 
	(progn (beginning-of-line) 
	       (setq new-point (point))
	       (backward-char) (setq this-point (point))
	       ))
       ((and limitedp
	     (progn (ignore-errors (backward-word 1))
		    (> (point) (point-min))))
	(setq new-point (point))
	(setq this-point new-point))
       
       
       ;;(limitedp (setq this-point (point) new-point (point)))

       ;; in the final case, this-point and new-point are already at 
       ;;point-max...
       (t nil))
      (setq ans (buffer-substring (point-min) this-point))
      (when 
	  ;;(< this-point (point-max))
	  limitedp
	(setq more (buffer-substring new-point (point-max)))
	(if 
	    (string-match "[^ \t\n]" more )
	    (setq ans (concat ans " ..[Type " erbnoc-char "more]"))	    
	  (when nomorep (setq more "")))
	)
      )
    (setq fs-more more)
    ans))


(defun fs-limit-lines-old (str0 &rest ignored)
  ""
  (let* (
	 (str (erbutils-remove-text-properties str0))
	 (brstr1 (split-string str "\n"))
	 (brstr (remove "" brstr1))
	 (ender "")
	 (condp (> (length brstr) fs-limit-lines))
	 (goodstr
	  (if condp
	      (progn
		(setq ender "..+ more")
		(subseq brstr 0 (- fs-limit-lines 1)))
	    brstr)))
    (if condp (setq fs-more 
		      (mapconcat 'identity 
				 (subseq brstr (- fs-limit-lines
						  1))
				 "\n"))
      (setq fs-more ""))
    (concat (mapconcat 'identity goodstr "\n") ender)))

(defun fs-more (&rest args)
  "Display the contents of the cache. "
  (if (and (stringp fs-more) 
	   (not (string= fs-more "")))
      fs-more
    (fs-describe "more")))
	

(defun fs-limit-lines-long (str &rest ignored)
  ""
  (let ((fs-limit-lines 7))
    (apply 'fs-limit-lines str ignored)))



(defun fs-limit-length (str &rest ignored)
  "Don't use this, use fs-limit-lines"
  (if (> (length str) fs-internal-limit-length)
      (concat (substring str 0 (- fs-internal-limit-length 1)) "...<more>")
    str))

(defun fs-limit-line-length (&optional str &rest args)
  "a subfunction.."
 (let* (
	;; this not needed now..
	(brokenstr (split-string str "\n"))
	(newlsstr
	 (mapcar
	  '(lambda (givenstr)
	     (let ((ls nil)
		   (thisstr givenstr)
		   )
	       (while (> (length thisstr)
			 fs-internal-limit-line-length)
		 (push
		  (concat (substring thisstr 0 fs-internal-limit-line-length
						  ) " <break>")
		  ls)
		 (setq thisstr (substring thisstr fs-internal-limit-line-length
					  (length thisstr))))
	       (push thisstr ls)
	       (reverse ls)))
	  brokenstr))
	(newbrokenstr
	 (apply 'append newlsstr)))
   (mapconcat 'identity newbrokenstr "\n")))


(defvar fs-internal-directed nil)

(defun fs-tell-to (string nick &rest ignored)
  (setq fs-nick (format "%s" nick))
  (let* ((fs-internal-directed t)
	 (ni (if (string= (format "%s" nick) "me")
		erbot-end-user-nick
	      (format "%s" nick)))
	 (reply
	  (erbeng-get-reply (fs-parse (concat erbot-nick ": "
						  string)))))
    (if (string-match ni reply)
	reply
      (concat ni ": " reply))))
    

(defun fs-apropos (&optional regexp N M &rest ignored)
  (fs-apropos-basic 'erbnoc-apropos regexp N M))
(defun fs-apropos-command (&optional regexp n m &rest ignored)
  (fs-apropos-basic 'erbnoc-apropos-command regexp n m ))
(defun fs-apropos-variable (&optional regexp n m &rest ignored)
  (fs-apropos-basic 'erbnoc-apropos-variable regexp n m ))
(defun fs-apropos-function (&optional regexp n m &rest ignored)
  (fs-apropos-basic 'erbnoc-apropos-variable regexp n m ))
(defun fs-apropos-value (&optional regexp n m &rest ignored)
  (fs-apropos-basic 'apropos-value regexp n m ))
(defun fs-apropos-documentation (&optional regexp n m &rest ignored)
  (fs-apropos-basic 'erbnoc-apropos-documentation  regexp n m ))

(defun erbnoc-apropos-documentation (reg)
  (mapcar 'car (apropos-documentation reg)))
(defun erbnoc-apropos-command (reg)
  (apropos-internal reg
		    'commandp))



(defun erbnoc-apropos-function (reg)
  (apropos-internal reg
		    'functionp))

(defun erbnoc-apropos-variable (reg)
  (apropos-internal reg 
		    (lambda (s)
		      (or (boundp s)
			  (user-variable-p s)))))


(defun erbnoc-apropos (regexp)
  (apropos-internal regexp
		    (lambda (symbol)
		      (or
		       (boundp symbol)
		       (fboundp symbol)
		       (facep symbol)
		       (symbol-plist symbol)))))

(defun fs-apropos-basic (fcn &optional regexp N M &rest ignored)
  "Show the apropos-matches  of regexp starting at match number N"
  (unless regexp 
    (error "Syntax: , apropos REGEXP &optional N M"))
  (if (stringp N) (setq N (read N)))
  (unless (integerp N) (setq N 0))
  (unless (stringp regexp)
    (setq regexp (format "%s" regexp)))
  (let* ((results (funcall fcn regexp))
	 (len (length results))
	 (str0 "")
	 (str1 "")
	 (str2 "")
	 (str3 "")
	 (str4 ""))
    (unless (and (integerp M) (< M len))
      (setq M len))
    (if (and (= N  0 ) (= M len) (> len 30))
	(setq 
	 str0 
	 "Perhaps Try , df fs-apropos for general syntax.  "))
    (if (> len 1) (setq str1 (format "%s matches.  " len)))
    (if (> N 0) (setq str2 (format "Matches starting at %s->" N)))
    (setq str3 (progn (format "%s" 
					  (subseq results
						  N M)
					  )))
    (concat str0 str1 str2 str3 str4)))


(defun fs-find-variable (function &rest ignore)
  (fs-find-variable-internal function  'nolimit))

(defun fs-find-variable-internal (function &optional nolimitp &rest ignore)
  "Finds the variable named FUNCTION."
  (if (stringp function) (setq function (read function)))
  (cond
   ((symbolp function)
    (unless (boundp function)
      (let ((g (intern (concat "fs-" (format "%s" function)))))
	(if (boundp g)
	    (setq function g))))
    (let ((fstr
	   (save-excursion
	     (find-function-do-it function t'set-buffer)
	     (buffer-substring (point)
			       (save-excursion 
				 (forward-sexp 1)
				 (point))))))
      (if (equal nolimitp 'nolimit)
	  fstr
	fstr)))
   (t "\n")))

(defalias 'fs-find-variable-briefly 'fs-find-variable)



(defun fs-find-function (&optional function &rest ignore)
  (unless function
    (error "Syntax: , find-function 'function-name"))
  ;;fs-limit-lines-long 
  (fs-find-function-internal 
   function 'nolimit))




(defalias 'fs-find-function-briefly 'fs-find-function)

(defun fs-find-function-on-key (&optional k &rest rest)
  (unless k
    (error
     "Syntax (ffo <key>)"))
  (fs-find-function (fs-describe-key-briefly k)))

(defun fs-find-function-on-key-briefly (k &rest rest)
  (fs-find-function-briefly (fs-describe-key-briefly k)))

(defun fs-find-function-internal (&optional function nolimitp &rest nada)
  (unless function
    (error
     "Syntax: (ff 'fucntion)"))
  (if (stringp function) (setq function (read function)))
  (cond
   ((symbolp function)
    (unless (fboundp function)
      (let ((g (intern (concat "fs-" (format "%s" function)))))
	(if (fboundp g)
	    (setq function g))))
    (let* ((fstrbare
	   (save-excursion
	     
	     ;; This has the problem that it is interactive.. asks to
	     ;; reread file if has changed etc. 
	     ;;(find-function function)
	     (find-function-do-it function nil 'set-buffer)
	     (buffer-substring (point)
			       (save-excursion 
				 (forward-sexp 1)
				 (point)))))
	  (fstr (erbutils-function-minus-doc fstrbare)))
      (if (equal nolimitp 'nolimit)
	  fstr
	(concat (format "%s characters.." (length
								fstr))
				     fstr))))
   (t "\n")))



;;; 2002-11-10 T14:50:20-0500 (Sunday)    D. Goel
(defun fs-say (&rest args)
  ;; let's make it safe, even though we know it will be made safe again...
  (let ((response
	 (mapconcat 
	  '(lambda (arg)
	     (format "%s" arg))     
	  args " ")))
    (if (erbot-safep response) response
      (concat " " response))))





		



(defun fs-regexp-quote (str)
  (unless (stringp str)
    (setq str (format "%s" str)))
  (regexp-quote str))


(defun fs-concat (&rest sequences)
  (apply 'concat
	 (mapcar
	  'erbutils-convert-sequence 
	  sequences)))





(defun fs-bunny (&rest arg)
  (concat " " 
	  (erbutils-random
	   '(
	     "Bunny is magical!"
	     ;;"Bunny is hot!"
	     "Bunny is sexy!"
	     "Bunny!!"
	     "Bunny's page: http://www.hurd-bunny.org"
	     "Bunny rocks"
	     "Bunny rules!"
	     "One Bunny to rule us all"
	     "One Bunny to rule us all ... Muhahahhaha"
	     "Bunny! Bunny! Bunny!"
	     "Bunny! Bunny! Bunny! Bunny!"
	     ;;"ERC in Emacs just rocks"
	     
	     ))))








(defun erbnocmd-user-fcn-definition  (&optional mainterm )
  "The general syntax is (fs-describe TERM [N] [M]).
Looks for TERM, and shows its descriptions starting at description
number N, and ending at M-1. The first record is numbered 0. 
"
  (unless mainterm 
    (error 
     "Format , (describe TERM &optional number1 number2)"))
  (unless mainterm
    (setq mainterm (format "%s" mainterm)))
  (setq mainterm (fs-correct-entry mainterm))
  (let* ((result0
	  (erbbdb-get-exact-notes
	   mainterm
	   ))
	 (result1 (and (stringp result0)
		       (ignore-errors (read result0))))
	 (len (length result1)))
      (cond
       ;; in cond0
       (result1
	(let* (
	       ;; notice the use of result1 here, not result. 
	       (aa (first result1))
	       (bb (split-string aa))
	       (cc (first bb))
	       (dd (second bb))
	       (ee (cdr bb))
	       )
	  (cond 
	   (
	    (erbutils-string= cc "redirect")
	    dd)
	   (t nil)))))))



(defun fs-seen (&rest args)
  (concat "seen "
	  (mapconcat 
	   '(lambda (arg) (format "%s" arg))
	   args
	   " ")))

;; this asks the google bot for results and gives it to our channel
;;(defvar erbnocmd-google-stack nil)
;;(defun fs-google (&rest args)
;; (progn
;;  (add-to-list 'erbnocmd-google-stack 'foo))
;; (erc-cmd-MSG google "hi")
;; nil)

(defcustom fs-internal-google-time 4
  "" :group 'erbc)

(defcustom fs-internal-dictionary-time 4
  "" :group 'erbc)

(defun fs-google-raw (&rest args)
  "Return a list of google results. "
  (let ((concatted
	 (mapconcat '(lambda (a)
		       (format "%s" a))
		    args " ")))
    (with-timeout 
	(fs-internal-google-time 
	 (list concatted (list "google---TimedOut")))
      (let* ((results
	      ;; this ignore-errors is very important.
	      ;; since the google stuff currently gives weird errors
	      ;; when called from within a with-timeout loop, and a
	      ;; timeout actually occurs. 
	      (ignore-errors
		(google-process-response 
		 (google-search-internal 
		  concatted
		  google-start google-max-results
		  google-filter-p google-safe-p))))
	     (fir (first results))
	     (revresults 
	      (reverse results))
	     (realresults 
	      (mapcan 
	       '(lambda (arg) 
		  (if (>= (length arg) 3) (list arg) nil))
	       (cdr results))))
	(cons fir (reverse realresults))))))
    
(defvar fs-internal-google-redirect-p nil)


(defun fs-googlen (n &rest args)
  "Format the first n results in a nice format. "
  (let* ((rawres (apply 'fs-google-raw args))
	 (terms (first rawres))
	 (matches (cdr rawres)))
    (when (> (length matches) n)
      (setq matches (subseq matches 0 n)))
    (cond
     ((or (not (null matches)) (not fs-internal-google-redirect-p))
      (format "[google]    %s"
	      ;;terms
	      (if matches 
		  (mapconcat 'car matches "\n")
		"No match. ")))
     (t
      (fs-english-only 
       fs-internal-original-message
       fs-internal-addressedatlast
       'nogoogle
       )))))

(defun fs-google-lucky-raw (&rest args)
  (caadr (apply 'fs-google-raw args)))


(defun fs-google-redirect-to-google-bot (&rest args)
  (concat "google: "
	  (mapconcat 
	   '(lambda (arg) (format "%s" arg))
	   args " ")))



(defun fs-google-from-english (&rest args)
  (let ((fs-internal-google-redirect-p t))
    (apply 'fs-google args)))

(defun fs-google (&rest args)
  (unless args (error "Syntax: , g[oogle] [NUMBER] WORD1 &rest MORE-WORDS "))
  (let (num 
	(fir (first args))
	)
    (when (> (length args) 1)
      (setq num 
	    (if (numberp fir) 
		fir
	      (ignore-errors (read fir)))))
    (if (numberp num)
	(setq args (cdr args))
      (setq num 1))
    (apply 'fs-googlen num args)))

(defun fs-google-with-options (options terms &rest args)
  "internal"
  (apply 'fs-google (append terms args (list options))))

(defun fs-google-deego (&rest args)
  "Google on the gnufans.net."
  (fs-google-with-options "site:gnufans.net" args))


(defun fs-google-emacswiki(&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:emacswiki.org" args))

(defun fs-google-sl4 (&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:sl4.org" args))

(defun fs-google-planetmath (&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:planetmath.org" args))

(defun fs-google-octave (&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:octave.org" args))


(defalias 'fs-go 'fs-google-octave)

(defun fs-google-wikipedia (&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:wikipedia.org" args))


(defun fs-google-gnufans-net (&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:gnufans.net" args))

(defun fs-google-gnufans-org (&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:gnufans.org" args))

(defun fs-google-hurdwiki(&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:hurd.gnufans.org" args))


(defun fs-google-nevadamissouri (&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:nevadamissouri.net" args))



(defun fs-google-scarymath (&rest args)
  "Google on the twiki site."
  (fs-google-with-options "site:http:scarymath.org" args))

(defun fs-google-twiki (&rest args)
  "Google on the twiki site."
  (fs-google-with-options "site:http:twiki.org" args))

(defun fs-google-usemod (&rest args)
  "Google on the emacswiki site."
  (fs-google-with-options "site:usemod.com" args))


(defalias 'fs-google-meatball 'fs-google-usemod)

(defun fs-replace-regexp (&optional from to term number)
  (unless (and from to term)
    (error (format "Syntax: %s (replace-regexp FROM TO TERM &optional NUMBER)" erbnoc-char)))
  (erbnocmd-iterate-internal term number 'replace-regexp-in-string from to
			     nil)
  (format "Replaced regexp %S with %S" from to))

(defun fs-cp (name dest)
  (let* ((exn (erbbdb-get-exact-notes name))
	 (notes (and (stringp exn) (read exn))))
    (unless notes
      (error "No such term %s" name))
    (when (erbbdb-get-exact-notes dest)
      (error "%S already exists.  Use merge" dest))
    (fs-set-term dest notes)
    (format "Copied entries of %S to %S" name dest)))


(defun fs-notes (name)
  "Internal. Return the notes as a list. "
  (let ((exnotes (erbbdb-get-exact-notes name)))
    (and (stringp exnotes) (read exnotes))))

(defun fs-merge (&optional name dest &rest args)
  (unless (and name dest (not args))
    (error (format "Syntax: %s merge TERM1 TERM2" erbnoc-char)))
  (setq name (format "%s" name))
  (setq dest (format "%s" dest))
  (when (string= (downcase name) (downcase dest))
    (error "Cannot merge something into itself."))
  (let ((notes (fs-notes name))
	(destnotes (fs-notes dest))
	)
    (unless notes (error "No such field %S" name))
    (unless destnotes
      (error "No such field %S.  Use mv" dest))
    (setq name (fs-correct-entry name))
    (setq dest (fs-correct-entry dest))
    (mapcar
     '(lambda (arg)
	(fs-set-also dest arg))
     notes)
    (fs-forget name "all")
    (format "Merged %S into %S" name dest)))



(defun fs-mv (&optional name dest &rest args)
  "Rename NAME to DEST. 
Do not confuse this function with fs-rearrange which rearranges the
order of entries within a given term. "
  (when (or args (not (and name dest)))
    (error (format "Format: %s mv foo bar" erbnoc-char)))
  (setq name (format "%s" name))
  (setq dest (format "%s" dest))
  (cond
   ((string= (downcase name) (downcase dest))
    (fs-mv-change-case name dest))
   (t
    (setq name (fs-correct-entry name))
    (fs-cp name dest)
    (fs-forget name "all")
    (format "Renamed the term %S to %S" name dest))))

(defalias 'fs-rename 'fs-mv)

(defun fs-mv-change-case (name dest)
  (when 
      (let ((bbdb-case-fold-search nil))
	(erbbdb-get-exact-name dest))
    (error "Destinatino %S already seems to exist" dest))
  (let ((tmp (format "TMPMV-%S" (random 1000))))
    (ignore-errors (fs-forget tmp))
    (fs-mv name tmp)
    (fs-mv tmp dest)
    (format "Readjusted case from %S to %S" name dest)))



(defun fs-rearrange-from-english-internal (msg)
  (catch 'erbnocmd-tag-foo
    (unless (equal (length msg) 3) 
      (throw 'erbnocmd-tag-foo
	     `(fs-error (format "Syntax: %s N->M in TERM" erbnoc-char))))
  (unless (equal (downcase (format "%s" (second msg))) "in")
    (throw 'erbnocmd-tag-foo
	   `(fs-error (format "Syntax: %s N->M in TERM" erbnoc-char))))
  (let (term
	fromto
	lenfromto
	)
    (setq term (third msg))
    (setq fromto 
	  (split-string (first msg) "->"))
    (setq lenfromto (length fromto))
    (unless (= lenfromto 2)
      (throw 'erbnocmd-tag-foo
	     `(fs-error (format "Syntax: %s N->M in TERM" erbnoc-char))))
    `(fs-rearrange ,(first fromto) ,(second fromto) ,term))))




(defun fs-replace-string-from-english-internal (msg)
  "Parse the input english message to return an elisp equivalent. 
MSG here is a list which needs to be combined.  "
  (let* 
      (
       ;; original length
       (leno (length msg))
       ;; remaining msg
       (remmsg msg)
       (remlen leno)
       las 
       number
       remengmsg
       remenglen
       revengmsg
       splitloc
       from
       to
       term
       (ans nil)
       (termcheckp nil)
       fcn 
       sr
       )
    (catch 'erbnocmd-repl-error
      
      (unless (and (>= leno 3) 
		   (equal 0 (string-match "\\(s\\|r\\)/" (first remmsg))))
	(throw 'erbnocmd-repl-error
	       `(fs-error 
		 "Format: s/foo.../bar..../ in TERM &optional N")))
      (setq sr       
	    (if (equal 0 (string-match "s" (first remmsg))) "s" "r"))
      (setq las (first (last remmsg)))
      (setq number (and (stringp las) (read las)))
      (if (or (numberp number) 
	      (equal 0 (string-match 
			"all" 
			(downcase (format "%s" number)))))
	  (setq remmsg (subseq remmsg 0 (- remlen 1)))
	(progn
	  (setq termcheckp t number nil)))
	
      ;; next comes the term
      (setq remlen (length remmsg))
      (setq term (first (last remmsg)))
      (setq remmsg (subseq remmsg 0 (- remlen 1)))
      
      (when termcheckp
	(let* ((exn (erbbdb-get-exact-notes term))
	       (notes (and (stringp exn) (read exn)))
	       (len (length notes)))
	  (if (> len 1)
	      (throw 'erbnocmd-repl-error
		     `(fs-error "Which numbered entry? %s/foo/bar in TERM NUMBER" , sr
))
	    (setq number 0))))
      
      ;; now the "in" 
      (setq remlen (length remmsg))
      (setq las (first (last remmsg)))
      (unless 
	  (string= "in" (downcase (format "%s" las)))
	(throw 'erbnocmd-repl-error
	       `(fs-error 
		 "missing \"in\"--- Format: %s/foo.../bar..../ in TERM &optional N"
		 ,sr ))
	)
    
    (setq remmsg (subseq remmsg 0 (- remlen 1)))
    (setq remlen (length remmsg))
    (setq remengmsg (mapconcat 'identity remmsg " "))

    ;; remove trailing whitespace
    ;; no need to check for length since we know msg stars with s/
    (while 
	(member
	 (aref remengmsg (- (length remengmsg) 1))
	 '(9 ;; tab
	   32 ;; space
	   10 ;; newline
	   ))
      (setq remengmsg (subseq remengmsg 0 (- (length remengmsg) 1))))
    ;; remove one trailing /
    ;; no need to check for length since we know msg stars with s/
    (setq remenglen (length remengmsg))
    (when (equal 
	   (aref 
	    remengmsg (- (length remengmsg) 1))
	   47)
      (setq remengmsg (subseq remengmsg 0 (- (length remengmsg) 1))))
    
    (setq remenglen (length remengmsg))
    (unless (> (length remengmsg) 2)
      (throw 'erbnocmd-repl-error
	     `(fs-error 
	       "Format: %s/foo.../bar..../ in TERM &optional N"
	       ,sr
	       ))

      )
    ;; this should take care of almost anything imaginable.
    ;; one can still construct "missing" cases but one should just use
    ;; lisp for that.  
    ;; remove the s/
    (if (equal 0 (string-match "s" remengmsg))
	(setq fcn 'fs-replace-string)
      (setq fcn 'fs-replace-regexp))
    (setq remengmsg (subseq remengmsg 2))
    ;; now find the last single /
    (with-temp-buffer
      (insert remengmsg)
      (goto-char (point-max))
      (setq splitloc 
	    (search-backward-regexp  "[^/]/\\([^/]\\|$\\)" nil t)))
    (unless splitloc
      (throw 'erbnocmd-repl-error
	     `(fs-error 
	       "Format: %s/foo.../bar..../ in TERM &optional N"
	       ,sr
	       )))
    (setq from (substring remengmsg 0 splitloc))
    (setq to (substring remengmsg (+ splitloc 1)))
    (when (string= from "")
      (throw 'erbnocmd-repl-error
      `(fs-error "Replacement string must have nonzero size..")))
    ;; singlify the double /'s. 
    (setq from
	  (replace-regexp-in-string "//" "/" from))
    (setq to
	  (replace-regexp-in-string "//" "/" to))
    `(,fcn ,from ,to ,term ,(format "%s" number)))))
    
			

(defun fs-replace-string (&optional from to term number)
  (unless (and from to term)
    (error (format "Syntax: %s s/foo.../bar in TERM [NUMBER or ALL]" erbnoc-char)))
  (erbnocmd-iterate-internal 
   (or (erbbdb-get-exact-name term ) term)
   number 'erbutils-replace-string-in-string 
   from to nil)
  (format "Replaced string %S with %S." from to))

(defun erbnocmd-iterate-internal (term number function
				       &rest arglist)
  
  " Perform FUNCTION on the NUMBERth entry of TERM. 
If NUMBER is not nil, the replacement is done for each entry in
the TERM. The function uses the term as its third argument. 
Meant for use by fs-replace-regexp etc. 

The last entry of ARGLIST is assumed to be itself a list of arguments,
let's call it lastlist.  Let the other entries of arglist be called
initargs.  Then the function is applied as (function @initargs string
@arglist).  Where the string is the string gotten from the TERM. "
  
  (setq number (format "%s" number))
  (let*
      ((exactnotes (erbbdb-get-exact-notes term))
       (notes (and (stringp exactnotes) (read exactnotes)))
       (len (length notes))
       newnotes
       newnote
       (lenargs (length arglist))
       (initargs (subseq arglist 0 (- lenargs 1)))
       (finargs (first (last arglist)))
       (numnum (read number))
       )
    (when (and (null number) (= len 1)) (setq number 0))
    (unless exactnotes (error "No such term: %S" term))
    (cond
     ((string= "all" (downcase number))
      (setq newnotes
	    (mapcar 
	     (lambda (thisentry)
	       (apply function (append initargs (list thisentry)
				       finargs)))
	     notes)))
     ((or (not (numberp numnum))
	  (< numnum 0)
	  (>= numnum len))
      (error "Number should be \"all\" or within %s and %s, given was: %s"
	     0 (- len 1) numnum))
     (t 
      (setq newnotes
	    (append
	     (subseq notes 0 numnum)
	     (list 
	      (apply function (append initargs 
				      (list (nth numnum notes))
				      finargs)))
	     (subseq notes (+ numnum  1) len)))))
    (fs-forget term "all")
    (fs-set-term term newnotes)))





(defun fs-info (&optional regexp)
  (unless regexp (error (format "Syntax: %s info REGEXP" erbnoc-char)))
  (unless (stringp regexp) (setq regexp (format "%s" regexp)))
  (Info-goto-node "(Emacs)")
  (if (Info-search regexp)

      t
    nil))


(defun fs-locate-library (&optional arg &rest rest)
  "REST WILL be ignored :-)"
  (unless arg (format (error "Syntax: %s locate-library LIB" erbnoc-char)))
  (unless (stringp arg)
    (setq arg (format "%s" arg)))
  (locate-library arg))


(defun fs-avg (&rest numbers)
  (cond
   ((null numbers) 'NaN)
   (t (fs-// (apply '+ numbers) 
	       (length numbers)))))


(defun fs-dict (&optional word &rest ignore)
  (unless word (error (format "Syntax: %s d[ict] word" erbnoc-char)))
  (unless (stringp word) (setq word (format "%s" word)))
  (fs-dictionary-search word))

(defalias 'fs-dictionary 'fs-dict)

(defun fs-dictionary-search (word) 
  "lispy.. not for interface. "
  (ignore-errors (kill-buffer "*Dictionary buffer*"))
  (unless (stringp word)
    (setq word (format "%s" word)))
  (with-timeout 
      (fs-internal-dictionary-time "Dictionary--TimedOut")
    (dictionary-search word)
    (save-window-excursion
     (switch-to-buffer "*Dictionary buffer*")
     (goto-line 3)
     (buffer-substring-no-properties (point) (point-max)))))





;;8/10/00
;;;###autoload
(defun fs-// (&rest args)
  "My sensible definition of /.
Does not say 4 / 3 = 0. Note: this usues equal and not equalp, the
last time i checked , equalp seemed to work as well.. "
  (let ((aa (apply '/ args)))
    (if (equal (car args) (apply '* aa (cdr args)))
	aa
      (apply '/ (cons (float (car args)) (cdr args))))))


(defun fs-channel-members (&optional n m &rest args)
  (when (stringp n) 
    (setq n (ignore-errors (read n))))
  (when (stringp m) 
    (setq m (ignore-errors (read m))))
  (unless (integerp n) (setq n 0))
  (unless (integerp m) (setq m nil))
  (subseq channel-members n m))


(defun fs-length-channel-members (&rest args)
  (length channel-members))
(defalias 'fs-number-channel-members 'fs-length-channel-members)

(defun fs-cto (&rest args)
  (let* ((page (mapconcat (lambda (arg) (format "%s" arg))
			 args "%20"))
	 (pg1 "http://cliki.tunes.org/")
	 ;;(pg2 "http://206.63.100.249/")
	 (pg3
	  (erbutils-replace-strings-in-string
	   '("+" " " "\t") '("%2B" "%20" "%20") page)))
    (format "%s%s"
	    pg1 pg3)))
	      

;;; (defun fs-karma (&rest args)
;;;   (let ((fir (first args)))
;;;     (unless 
;;; 	(and
;;; 	 args
;;; 	 fir)
;;;     (error (format "Syntax: , karma ENTITY")))
;;;     (setq fir (downcase (format "%s" fir)))
;;;     (let ((result (erbkarma fir)))
;;;       (if result
;;; 	  (format "%s's karma is %s" fir result)
;;; 	(format 
;;; 	 "No karma defined for %s, use ,ENTITY++ or ,karma-create" fir
;;; 	 )))))

;;; (defvar erbnoc-karma-pt 10)

;;; (defun fs-karma-increase (&optional arg points &rest ignore)
;;;   (unless arg (error "Syntax: foo++ [&optional NUMBER]"))
;;;   (when (stringp points)
;;;     (setq points (ignore-errors (read points))))
;;;   (unless (and (integerp points) 
;;; 	       (<= (abs points) erbnoc-karma-pt))
;;;     (setq points erbnoc-karma-pt))
;;;   (setq arg (downcase (format "%s" arg)))
;;;   (erbkarma-increase arg points))

(defun fs-karma-increase (&rest args)
  (error "Karma system is currently being reworked. "))
(defalias 'fs-karma-decrease 'fs-karma-increase)

;;; (defun fs-karma-decrease (&optional arg points &rest ignore)
;;;   (unless arg (error "Syntax: foo++ [&optional NUMBER]"))
;;;   (when (stringp points)
;;;     (setq points (ignore-errors (read points))))
;;;   (unless (and (integerp points) 
;;; 	       (<= (abs points) erbnoc-karma-pt))
;;;     (setq points erbnoc-karma-pt))
;;;   (setq arg (downcase (format "%s" arg)))
;;;   (erbkarma-decrease arg points))



;;; (defun fs-karma (&optional foo)
;;;   (if foo (setq foo (downcase (format "%s" foo))))
;;;   (erbkarma foo))

;;; (defalias 'fs-karma-best 'erbkarma-best)


(defalias 'fs-ncm 'fs-length-channel-members)
(defun fs-superiorp (&rest args)
  (erbutils-random '(t nil)))
(defun fs-sucksp (&rest args)
  (erbutils-random '(t nil)))
(defun fs-bugp (&rest args)
  (erbutils-random '(t nil)))


(defun fs-country (&optional ct)
  (unless ct (error (format "Syntax: %s country NM (example , country jp)" erbnoc-char)))
  (setq ct (format "%s" ct))
  (let ((addp (and (> (length ct) 1)
		   ;; does not start with .
		   (not (= (aref ct 0) 46)))))
    (if addp (setq ct (concat "." ct))))
  (erbcountry (downcase ct)))

;;; 2003-02-09 T13:40:04-0500 (Sunday)    D. Goel
(defun fs-spook (&rest args)
  (with-temp-buffer
    (spook)
    (goto-char (point-min))
    (forward-line 1)
    (buffer-substring-no-properties
     (progn (beginning-of-line 1) (point))
     (progn (end-of-line 1) (point)))))


(defun fs-explode (&rest args)
  (let ((pieces
	 (erbutils-random '("a thousand" "a million" "a gazillion" 
			    "aleph_2")))
	(watch 
	 (erbutils-random '("" "you watch as "
			    "you run for cover as "
			    ))))
  (eval
   (erbutils-random
    '((format "%s%s explodes into %s pieces!"
	      watch erbot-nick pieces)
      (format "%s, with botheart broken into %s pieces, has left: \"Goodbye\"" 
	      erbot-nick pieces))))))




(defalias 'fs-die 'fs-explode)
(defalias 'fs-die! 'fs-explode)
(defalias 'fs-Die! 'fs-explode)
(defalias 'fs-Die 'fs-explode)
(defalias 'fs-DIE 'fs-explode)
(defalias 'fs-leave 'fs-explode)
(defalias 'fs-exit 'fs-explode)
(defalias 'fs-quit 'fs-explode)
(defalias 'fs-shut 'fs-explode)
(defalias 'fs-stfu 'fs-explode)
(defalias 'fs-STFU 'fs-explode)



(defun fs-morse (&rest str)
  (apply 'erbutils-region-to-string 'morse-region str))
(defun fs-unmorse (&rest str)
  (apply 'erbutils-region-to-string 'unmorse-region str))

(defun fs-rot13 (&rest str)
  (let (st)
    (cond
     ((= (length str) 1)
      (setq st (format "%s" (first str))))
     (t (setq st (mapconcat 
		  (lambda (a) (format "%s" a)) str " "))))
    (erbutils-rot13 st)))

(defun fs-studlify (&rest s)
  (apply 'erbutils-region-to-string 
   (lambda (&rest args)
     (ignore-errors (apply 
		     'studlify-region args)))
   s))


(defun fs-h4x0r (&rest s)
  (require 'h4x0r)
  (funcall
   'h4x0r-string
   (mapconcat 
    (lambda (a) (format "%s" a))
    s " ")))


(defalias 'fs-h4 'fs-h4x0r)
(defalias 'fs-h4 'fs-h4xor)
(defalias 'fs-h4 'fs-haxor)
(defalias 'fs-h4 'fs-hax0r)

(defalias 'fs-l33t 'fs-h4x0r)
(defalias 'fs-leet 'fs-h4x0r)

(defalias 'fs-stud 'fs-studlify)

(defcustom fs-internal-studlify-maybe-weights
  '(100 1)
  ""
  :group 'erbc)

(defun fs-studlify-maybe (&rest args)
  (eval 
   (erbutils-random
    '((erbutils-stringify args)
      (apply 'fs-studlify args))
    fs-internal-studlify-maybe-weights
    )))


(defcustom fs-internal-h4x0r-maybe-weights
  '(100 1)
  ""
  :group 'erbc)

(defun fs-h4x0r-maybe (&rest args)
  (let*
      ((aa (erbutils-stringify args))
       (bb
	(ignore-errors
	  (eval 
	   (erbutils-random
	    '(aa
	      (apply 'fs-h4x0r args))
	    fs-internal-h4x0r-maybe-weights
	    )))))
    (or bb aa)))


(defalias 'fs-stud-maybe 'fs-studlify-maybe)


(defalias 'fs-studlify-word 'studlify-word)


(defun fs-princ (a &rest ignore)
  (princ a))


(defun fs-pray (&rest args)
  (require 'faith)
  (faith-quote))

(defalias 'fs-all-hail-emacs 'fs-pray)
(defalias 'fs-hail-emacs 'fs-pray)
(defalias 'fs-faith 'fs-pray)
(erbutils-defalias '(faith-correct-string))

(defun fs-shell-test (string substrings)
  "internal"
  (let ((found nil))
    (mapcar (lambda (arg)
	      (when (string-match (regexp-quote arg) string)
		(setq found t)))
	    substrings)
    found))

;;; 2003-02-17 T18:55:09-0500 (Monday)    D. Goel
(defun fs-wserver (&optional site &rest args)
  (unless site (error (format "Syntax: %s wserver SITE" erbnoc-char)))
  (setq site (format "%s" site))
  (if (fs-shell-test site '(" " "<" "-"))
      (error "No attacks please. "))
  (erbnoc-shell-command-to-string
   (format "w3m -dump_head %s" site)))
(defalias 'fs-webserver 'fs-wserver)

;;; 2003-02-17 T18:55:09-0500 (Monday)    D. Goel
(defun fs-web (&optional site &rest args)
  "displays a website"
  (unless site (error (format "Syntax: %s wserver SITE" erbnoc-char)))
  (setq site (format "%s" site))
  (if (fs-shell-test site '(" " "<" "-"))
      (error "No attacks please. "))
  (erbnoc-shell-command-to-string
   (format "w3m -dump %s" site)))

;;;###autoload
(defun fs-length-load-history ()
  (interactive)
  (message "%s%s%S" 
	   (length load-history)
	   " ..." (mapcar 'car load-history)))

    
;(defun fs-load-history ()
;  load-history)    
;(defun fs-load-history ()
;  load-history)

(defalias 'fs-google: 'fs-google)



(defconst fs-bunny 142857)
(defconst fs-pi pi)
(defconst fs-e e)
(defconst fs-emacs-version emacs-version)

(defalias 'fs-emacs-version 'emacs-version)
(defalias 'fs-gnus-version 'gnus-version)

;; the short aliases..
(defalias 'fs-a 'fs-apropos)
(defalias 'fs-da 'fs-apropos)
(defalias 'fs-ac 'fs-apropos-command)
(defalias 'fs-ad 'fs-apropos-documentation)
(defalias 'fs-af 'fs-apropos-function)
(defalias 'fs-av 'fs-apropos-variable)

(defalias 'fs-c 'fs-commands)
(defalias 'fs-d 'fs-dict)
(defalias 'fs-dict: 'fs-dict)

(defalias 'fs-dl 'fs-describe-literally)
(defalias 'fs-doc 'fs-doctor )
(defalias 'fs-dkb 'fs-describe-key-briefly )

(defalias 'fs-dk 'fs-describe-key)
(defalias 'fs-dkf 'fs-describe-key-and-function)
(defalias 'fs-dkl 'fs-describe-key-long)

(defalias 'fs-lkgg 'fs-lookup-key-gnus-group)
(defalias 'fs-dkgg 'fs-lookup-key-gnus-group)

(defalias 'fs-dkgs 'fs-lookup-key-gnus-summary)
(defalias 'fs-lkgs 'fs-lookup-key-gnus-summary)

(defalias 'fs-lkm 'fs-lookup-key-message)
(defalias 'fs-lkm 'fs-lookup-key-message)


(defalias 'fs-df 'fs-describe-function )
(defalias 'fs-cond 'cond)
(defalias 'fs-if 'if)
(defalias 'fs-when 'when)
(defalias 'fs-dfl 'fs-describe-function-long )
(defalias 'fs-dv 'fs-describe-variable )
(defalias 'fs-ff 'fs-find-function)
(defalias 'fs-ffb 'fs-find-function-briefly)
(defalias 'fs-ffo 'fs-find-function-on-key)
(defalias 'fs-ffob 'fs-find-function-on-key-briefly)
(defalias 'fs-fv 'fs-find-variable)
(defalias 'fs-fvb 'fs-find-variable-briefly)
(defalias 'fs-? 'fs-help)
(defalias 'fs-32 'fs-help)
(defalias 'fs-s  'fs-search)
(defalias 'fs-sw  'fs-search-wide)
(defalias 'fs-sws  'fs-search-wide-sensitive)
(defalias 'fs-wi  'fs-where-is)
(defalias 'fs-wigg  'fs-where-is-gnus-group)
(defalias 'fs-wigs  'fs-where-is-gnus-summary)
(defalias 'fs-wim  'fs-where-is-message)
(defalias 'fs-dw  'fs-where-is)
;;(defalias 'fs-yo 'fs-hi)

;; basic functions
(defalias 'fs-lambda 'lambda)
(defalias 'fs-length 'length)
(defalias 'fs-sqrt 'sqrt)

(defalias 'fs-= '=)
(defalias 'fs-/= '/=)
(defalias 'fs-< '<)
(defalias 'fs-> '>)
(defalias 'fs-<= '<=)
(defalias 'fs->= '>=)
(defalias 'fs-not 'not)
(defalias 'fs-and 'and)
(defalias 'fs-or 'or)
(defalias 'fs-lart 'fs-flame)
(defalias 'fs-null 'null)

(defalias 'fs-equal 'equal)
(defalias 'fs-equalp 'equalp)
(defalias 'fs-eql 'eql)
;; rr is used for russian-roulette now..
;;(defalias 'fs-rr 'fs-replace-regexp)
(defalias 'fs-rs 'fs-replace-string)
(defalias 'fs-+ '+)
(defalias 'fs-- '-)
(defalias 'fs-* '*)
(defalias 'fs-/ '/)
(defalias 'fs-less 'fs-more)
(defalias 'fs-list 'list)
(defalias 'fs-car 'car)
(defalias 'fs-ct 'erbccountry)
(defalias 'fs-cdr 'cdr)
(defalias 'fs-cons 'cons)
(defalias 'fs-append 'append)
(defalias 'fs-first 'first)
(defalias 'fs-second 'second)
(defalias 'fs-third 'third)
(defalias 'fs-fourth 'fourth)
(defalias 'fs-fifth 'fifth)
(defalias 'fs-sixth 'sixth)
(defalias 'fs-seventh 'seventh)
(defalias 'fs-eighth 'eighth)
(defalias 'fs-ninth 'ninth)
(defalias 'fs-tenth 'tenth)
(defalias 'fs-subseq 'subseq)
(defalias 'fs-ceiling 'ceiling)
(defalias 'fs-ceiling* 'ceiling*)
(defalias 'fs-concatenate 'concatenate)
(defalias 'fs-cos 'cos)
(defalias 'fs-count-lines 'count-lines)

(defalias 'fs-last 'last)
(defalias 'fs-llh 'fs-length-load-history)
(defalias 'fs-error 'erbutils-error)
(defalias 'fs-expt 'expt)
(defalias 'fs-exchange-point-and-mark 'exchange-point-and-mark)
(defalias 'fs-rq 'fs-regexp-quote)
;; (defalias 'fs-function 'identity)

(defalias 'fs-identity 'identity)
(defalias 'fs-nth 'nth)
(defalias 'fs-nthcdr 'nthcdr)
(defalias 'fs-random 'random)
(defalias 'fs-random-choose 'erbutils-random)
(defalias 'fs-remove 'remove)
(defalias 'fs-replace-regexp-in-string 'replace-regexp-in-string)
(defalias 'fs-replace-match 'replace-match)

(defalias 'fs-number-to-string 'string-to-number)
(defalias 'fs-format 'format)
(erbutils-defalias '(format-time-string))

(defalias 'fs-split-string 'split-string)
(defalias 'fs-rm 'fs-forget)
(defalias 'fs-progn 'progn)
(defalias 'fs-ignore-errors 'ignore-errors)
(defalias 'fs-lcm 'lcm)
(defalias 'fs-let 'let)
(defalias 'fs-ll 'fs-locate-library)
(defalias 'fs-g 'fs-google)
(defalias 'fs-gcd 'gcd)
(defalias 'fs-gd 'fs-google-deego)

(defalias 'fs-ge 'fs-google-emacswiki)
(defalias 'fs-gs 'fs-google-sl4)

(defalias 'fs-gw 'fs-google-wikipedia)
(defalias 'fs-gh 'fs-google-hurdwiki)
(defalias 'fs-gm 'fs-google-meatball)
(defalias 'fs-gnufans 'fs-google-gnufans-net)
(defalias 'fs-gg 'fs-google-gnufans-net)
(defalias 'fs-ggn 'fs-google-gnufans-net)
(defalias 'fs-ggo 'fs-google-gnufans-org)
(defalias 'fs-gn 'fs-google-nevadamissouri)
(defalias 'fs-gp 'fs-google-planetmath)
(defalias 'fs-gt 'fs-google-twiki)
(defalias 'fs-gu 'fs-google-usemod)

(defalias 'fs-mark 'mark)
(defalias 'fs-point 'point)
(defalias 'fs-pop-mark 'pop-mark)
(defalias 'fs-push-mark 'push-mark)
(defalias 'fs-floor 'floor)
(defalias 'fs-floor* 'floor*)

(defalias 'fs-round 'round)
(defalias 'fs-round* 'round*)

(defalias 'fs-setcar 'setcar)
(defalias 'fs-setcdr 'setcdr)
(defalias 'fs-sin 'sin)
(erbutils-defalias '(sleep-for sit-for))
(defalias 'fs-string 'string)

(defalias 'fs-string-as-multibyte 'string-as-multibyte)
(defalias 'fs-string-bytes 'string-bytes)
(defalias 'fs-string-equal 'string-equal)
(defalias 'fs-string-key-binding 'string-key-binding)
(defalias 'fs-string-lessp 'string-lessp)
(defalias 'fs-string-make-multibyte 'string-make-multibyte)
(defalias 'fs-string-make-unibyte 'string-make-unibyte)
(defalias 'fs-string-to-char 'string-to-char)
(defalias 'fs-string-to-int 'string-to-int)
(defalias 'fs-string-to-list 'string-to-list)
(defalias 'fs-string-to-number 'string-to-number)
(defalias 'fs-string-to-sequence 'string-to-sequence)
(defalias 'fs-string-to-syntax 'string-to-syntax)
(defalias 'fs-string-to-vector 'string-to-vector)
(defalias 'fs-string-width 'string-width)
(defalias 'fs-symbol-file 'symbol-file)

(defalias 'fs-tan 'tan)
(defalias 'fs-cos 'cos)
(defalias 'fs-sin 'sin)
(defalias 'fs-atan 'atan)
(defalias 'fs-asin 'asin)
(defalias 'fs-acos 'acos)
(defalias 'fs-tanh 'tanh)

(erbutils-defalias 
 '(timezone-world-timezones 
   timezone-months-assoc
   timezone-make-date-arpa-standard timezone-make-date-sortable
   timezone-make-arpa-date timezone-make-sortable-date
   timezone-make-time-string timezone-parse-date timezone-parse-time
   timezone-zone-to-minute timezone-time-from-absolute
   timezone-time-zone-from-absolute timezone-fix-time
   timezone-last-day-of-month timezone-leap-year-p timezone-day-number
   timezone-absolute-from-gregorian))
   

(defalias 'fs-truncate 'truncate)

(defalias 'fs-truncate* 'truncate*)
(defalias 'fs-truncate-string 'truncate-string)
(defalias 'fs-truncate-string-to-width 'truncate-string-to-width)


(defalias 'fs-erc-version 'erc-version)
(defalias 'fs-sv 'erc-cmd-SV)
(defalias 'fs-erc-cmd-SV 'erc-cmd-SV)
(defalias 'fs-smv 'erc-cmd-SMV)
(defalias 'fs-erc-cmd-SMV 'erc-cmd-SMV)
(defalias 'fs-sm 'erc-cmd-SM)
(defalias 'fs-cmd-SM 'erc-cmd-SM)
(defalias 'fs-stringify 'erbutils-stringify)
;; (defalias 'fs-while 'while)

;;;====================================================

;;;====================================================
;; ERRORS:

(defun fs-load-library (&rest args)
  (error "Use 'require instead. "))

(defalias 'fs-load 'fs-load-library)
(defalias 'fs-load-file 'fs-load-library)



;; cl-extra.el

(defalias 'fs-equalp 'equalp)
;; done gcd 
;; done lcm 
(defalias 'fs-isqrt 'isqrt)
(defalias 'fs-floor* 
  'floor* )

(defalias 'fs-ceiling* 
'ceiling* )

(defalias 'fs-truncate* 
'truncate*)

;; done round* 

(defalias 'fs-mod* 
  'mod* )

(when (ignore-errors
	(require 'geek))
  (erbutils-defalias '(geek-code)))

(defalias 'fs-rem* 
  'rem* )

(erbutils-defalias 
 '(signum 
   random* 
   ;; yes?
   make-random-state
   random-state-p 

   most-positive-float most-negative-float
   least-positive-float least-negative-float
   least-positive-normalized-float least-negative-normalized-float
   float-epsilon float-negative-epsilon cl-float-limits ;; done subseq
   concatenate revappend nreconc list-length tailp cl-copy-tree 
   copy-tree
   ;;get* getf 
   ;;cl-set-getf cl-do-remf cl-remprop remprop 
   cl-make-hash-table
   cl-hash-table-p cl-not-hash-table cl-hash-lookup cl-builtin-gethash
   cl-builtin-remhash cl-builtin-clrhash cl-builtin-maphash cl-gethash
   ;;cl-puthash cl-remhash cl-clrhash 
   ;;cl-maphash 
   cl-hash-table-count
   cl-prettyprint cl-do-prettyprint cl-macroexpand-cmacs cl-closure-vars
   cl-macroexpand-all cl-macroexpand-body cl-prettyexpand))



;; oct.el

(ignore-errors (require 'oct))

(erbutils-defalias 


 '(
   oct-zeros oct-ones oct-sum oct-size
   oct-rows oct-columns oct-\.* 
   oct-add 
   oct-corr oct-complement oct-sumsq oct-mean
   oct-sqrt oct-std oct-tanh oct-atanh)
 "" "oct-")

(erbutils-defalias '(oct-/ oct-+ ))
(erbutils-defalias '(lsh))
(erbutils-defalias '(setq))
(erbutils-defalias '(obarray))


;; files.el
(erbutils-defalias 
 '(auto-mode-alist interpreter-mode-alist
		   directory-abbrev-alist))


(erbutils-defalias '(load-history))
(erbutils-defalias '(assoc))
(erbutils-defalias '(eq))
(erbutils-defalias '(message))
(erbutils-defalias '(decf))
(erbutils-defalias '(incf))
(erbutils-defalias '(faith-quote))
(erbutils-defalias '(zerop))
(erbutils-defalias '(buffer-substring))
(erbutils-defalias '(buffer-substring-no-properties))
(erbutils-defalias '(buffer-string))



(erbutils-defalias 
 '(featurep feature-symbols feature-file features
	    
		     ))
(erbutils-defalias 
 '(minor-mode-alist minor-mode-map-alist
		    minor-mode-overriding-map-alist))
(erbutils-defalias-vars '(major-mode))

;; from gnus-group.el 

(erbutils-defalias-vars '(gnus-group-mode-map))
(erbutils-defalias-vars '(gnus-summary-mode-map))
(erbutils-defalias-vars '(message-mode-map))
(erbutils-defalias-vars '(text-mode-map))
(erbutils-defalias-vars '(emacs-lisp-mode-map))
(erbutils-defalias-vars '(lisp-mode-map))

(erbutils-defalias '(boundp fboundp))
(erbutils-defalias '(lookup-key))
(erbutils-defalias '(minor-mode-key-binding))

(erbutils-defalias '(where-is-internal))
(erbutils-defalias '(% abs))

(erbutils-defalias '(cdr cddr car cadr cdar))
(erbutils-defalias '(erc-channel-list))

(when (ignore-errors (require 'units))
  (erbutils-defalias '(units-version units-load-hook units-dat-file
				     units-buffer units-s-to-n
				     units-prefix-convert
				     units-si-prefix-list
				     units-si-short-prefix-list
				     units-convert-1 units-convert)))




(defun fs-give (&optional nini &rest stuff)
  (unless nini (setq nini "self"))
  (when (string= "me" nini)
    (setq nini nick))
  (unless stuff (setq stuff '("a" "beer")))
  (format "/me gives %s %s"
	  nini
	  (mapconcat 
	   (lambda (arg) (format "%s" arg))
	   stuff " ")))
	
	
(defalias 'fs-hand 'fs-give)

(erbutils-defalias 
 '(backward-kill-sentence 
   backward-sentence
   flame-sentence flame-sentence-ify
   flame-sentence-loop forward-sentence kill-sentence
   mark-end-of-sentence sentence-at-point sentence-end
   sentence-end-double-space sentence-end-without-period
   transpose-sentences))

(defalias 'fs-flatten 'erbutils-flatten)



(erbutils-defalias '(log))
(erbutils-defalias 
 '(most-positive-fixnum
   most-negative-fixnum))



(erbutils-defalias 
 '(
   regexp-opt
   regexp-opt-depth
   regexp-opt-group regexp-opt-charset))

(erbutils-defalias '(window-system))

(erbutils-defalias 
 '(kbd read-kbd-macro))

(defconst fs-t t
  "As such, when we sandbox a lisp expression, t remains t, so this is
not needed. 
However, inside macros like (cond (t....)), t becomes fs-t because
it occurs in an unusual place.  this const should take care of it.. 
Of course, this also opens the bot to some FUN user abuse, when they
setq fs-t to nil :-) ")


(defconst fs-nil nil
  "See the doc of fs-t ")


(defun fs-revive (&optional name &rest ignore)
  (unless name (error "no one to revive"))
  (setq name (format "%s" name))

  (cond
   ((string= name nick) 
    (concat "Thou idiot, " nick ", thou canst not revive thyself!"))
   (t (concat
       "/me sprinkles some "
       (erbutils-random
	'("clear" "murky" "boiling" "dark" "holy" "smelly"))
       " potion on "
       (format "%s" name)
       " and utters some prayers.  "
       (erbutils-random
	(list
	 (format "%s wakes up" name)
	 "Nothing happens."
	 (format "%s wakes up, all refreshed. " name)
	 (format "%s wakes up, all confused. " name)
	 ))))))
    
(defalias 'fs-sandbox-quoted 'erblisp-sandbox-quoted)
(defalias 'fs-sandbox-quoted-maybe 'erblisp-sandbox-quoted-maybe)
(defalias 'fs-sandbox 'erblisp-sandbox)

(erbutils-defalias '(macroexpand))
(defun fs-kick (&optional reason &rest ignore)
  (erc-cmd-KICK erbnoc-nick nil (when reason (format "%s" reason))))


;;"/usr/share/emacs/21.2/lisp/emacs-lisp/pp.el" 
(erbutils-defalias
 '(pp-escape-newlines
   pp-to-string 
   ;; pp pp-eval-expression 
   ;;pp-eval-last-sexp))
   ))


(erbutils-defalias '(string-match))

(defun fs-pp (object &rest ignore)
  (pp object))

(provide 'erbc)
(run-hooks 'fs-after-load-hooks)



;;; erbc.el ends here

