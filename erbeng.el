;;; erbeng.el --- 
;; Time-stamp: <2005-09-02 14:18:08 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbeng.el
;; Package: erbeng
;; Author: D. Goel <deego@gnufans.org>
;; Version: 0.0DEV
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
 

(defvar erbeng-home-page
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


(defvar erbeng-version "0.0dev")

;;==========================================
;;; Code:

(require 'cl)
(defgroup erbeng nil 
  "The group erbeng"
   :group 'applications)
(defcustom erbeng-before-load-hooks nil "" :group 'erbeng)
(defcustom erbeng-after-load-hooks nil "" :group 'erbeng)

(defcustom erbeng-reply-timeout 20
  "Time after which the bot times out...")

(run-hooks 'erbeng-before-load-hooks)





(defvar erbeng-msg nil )
(defvar erbeng-proc nil)
(defvar erbeng-nick nil)
(defvar erbeng-tgt nil)
(defvar erbeng-localp nil)
(defvar erbeng-userinfo nil)


(defvar erbot-show-type-p t
  "Whether to show type of non-string objects when replying...

The old behavior was equivalent to having this as nil.")


;;;###autoload
(defun erbeng-main (msg proc nick tgt localp userinfo)
  " The main function: Takes a line of message and generates a reply to it. 
The result is a string.  If the result is 'noreply, that means: Do NOT reply...
The last field localp is here for historical reasons, and shall be
ignored...

One very important criterion here should be:

erbot should learn to avoid runaway talks with other bots.  For this
reason: 

 [a] it should take a break every now and then, say: a 1-minute break
after every 1000 commands.  It should probably announce its break.
AND/OR 
 [b] It should learn to reply only 99 out of 100 times.  Moreover,
before it shuts up, it should let any humans know what it is doing.
tgt, nick and sspec will probably mostly remain unused...  

proc == name of the process in the channel
tgt == channel
nick == who from
userninfo looks like (\"deego\" \"user\" \"24-197......\")
sspec looks like: [\"PRIVMSG\"
\"deego!~user@24-197-159-102.charterga.net\" \"#testopn\" \"hi erbot\" \nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil\ nil nil nil nil nil nil nil nil]

"
  (let* 
      (
       (erbeng-nick nick)
       (erbeng-msg msg)
       (erbeng-proc proc)
       (erbeng-tgt tgt)
       (erbeng-localp localp)
       (erbeng-userinfo userinfo)
       (fs-found-query-p nil)
       (fs-internal-addressedatlast nil)
       (fs-internal-message-sans-bot-name fs-internal-message-sans-bot-name)
       (fsi-prestring fsi-prestring)
       tmpvar
       parsed-msg rep
       (fs-msg fs-msg)
       (fs-msglist fs-msglist)
       (fs-msgsansbot fs-msgsansbot)
       (fs-msglistsansbot fs-msglistsansbot)
       (fs-lispa fs-lispa)
       (fs-lispb fs-lispb)
       (fs-lispc fs-lispc)
       (fs-lispd fs-lispd)
       (fs-lispe fs-lispe)
       )
    ;;(concat nick ": " "/leave Test.. one big big test...")
    ;;(erbutils-ignore-errors
    
    ;; this can also modify fs-found-query
    (setq parsed-msg 
	  (or (condition-case tmpvar
		  (fs-parse msg proc nick tgt localp userinfo)
		(error 
		 ;;"(error \"Please supply a completed lisp form\")"
		 ;; Note that this could be bad: 
		 ;; someone may not even be referring to the bot here:
		 (if 
		     fs-internal-parse-error-p
		     (format "(error %S )" 
			     (error-message-string tmpvar))
		   (format "(fs-english-only %S)" msg))
		 
		 ))
	      (and (featurep 'erbmsg)
		   erbot-erbmsg-p
		   (erbmsg-parse msg proc nick tgt localp userinfo))))
    
    ;;(if (and (first parsed-msg) erbot-nick
    ;;        (string= (first parsed-msg)
    ;;           erbot-nick))
    ;; parsed-msg will never be null if the msg was addressed to fsbot..
    (if
	parsed-msg
	(progn
	  (setq rep 
		;;(erbutils-ignore-errors
		(with-timeout 
		    (erbeng-reply-timeout
		     "overall timeout")
		  (erbutils-ignore-errors
		   (erbeng-get-reply parsed-msg proc nick tgt )))
		)
	  (cond
	   ((string= "noreply" (format "%s" rep)) 'noreply)
	   ((and (stringp rep) (not (equal rep ""))) (format "%s%s"
							     fsi-prestring
							     rep))
	   (t 
	    (cond
	     (erbot-show-type-p
	      (format "%s%S  ..(%s)" fsi-prestring rep (type-of rep)))
	     ((equal "" rep) "EMPTY STRING RETURNED")
	     (t (format "%s%S" fsi-prestring rep))))))
      
      'noreply)))



(defun erbeng-lisp-object-p (msg) 
  (setq msg (ignore-errors (erbn-read msg)))
  (and (listp msg)
       (let ((fir (format "%s" (first msg))))
   (or
    (string-match "concat" fir)
    (string-match "regexp-quote" fir)
   ;; want to allow fs-rq to show the regexp without quoting..
   ;;(string-match "fs-rq" fir)
    ))))



;(defun erbeng-init-parse (msg)
;  (if (equal 0 (string-match "," msg))
;      (setq msg (concat "erbot " 
;     (substring msg 1 (length msg)))))
;  (let ((spl (split-string msg)))
;    (if (> (length spl) 0)
; (erbeng-init-frob-split-string spl)
;      nil)));;;

;;; ;(defun erbeng-init-frob-split-string (spl)
;;; ;  "spl is the split string ..;;;;

;;; ;now, we do not need to split wrt commas... in fact, that will be
;;; ;dangerous, and can spoil the meanings of commas inside parse
;;; ;commands...;;

;;; ;converts all accepted formats to look like this:
  

;;; ; \(\"erbot\" \"foo\" \"bar\"\)

;;; ;"
;;;  ; (let* ((do-again t)
;;;   ; (new-spl
;;;    ; (cond
;;;     ; ;; , foo bar
;;;      ;((string= (first spl) ",")
;;;       (cons erbot-nick (cdr spl)))
;;;      ((equal 
;;;        (string-match "," (first spl)) 0)
;;;       (cons erbot-nick
;;;       (append (split-string (first spl) ",")
;;;         (cdr spl))))
;;;      ((equal
;;;        ;; erbot:
;;;        (string-match (concat erbot-nick ":") (first spl)) 0)
;;;       (append (split-string (first spl) ":")
;;;         (cdr spl)))
;;;      ((equal 
;;;        ;; fdbot,
;;;        (string-match (concat erbot-nick ",") (first spl)) 0)
;;;       (append (split-string (first spl) ",")
;;;         (cdr spl)))
;;;      (t (progn (setq do-again nil) spl)))))
;;;     (if do-again
;;;   (erbeng-init-frob-split-string new-spl)
;;;       ;; removed the extra ""  etc. and all , ; erc. etc. 
;;;       (split-string
;;;        (mapconcat 'identity
;;;       new-spl " ")
;;;        "[ \f\t\n\r\v,;]+"))))




(defun erbeng-get-reply (msg &optional proc nick tgt &rest foo)
  "  ;; now assumes that the msg is (a string) in lisp format... and this just
  ;; evals it.."
  (eval (erbn-read msg)))
;  (let* (
;  (lispmsg 
;   (erbeng-read (erbutils-stringify msg))))
;    (if (and lispmsg (listp lispmsg))
; (erblisp-process-msg proc nick tgt 
;          lispmsg)
;      (let ((englispmsg (fs-parse-english msg proc nick)))
; (erblisp-process-msg proc nick tgt englispmsg)))))



(defun erbeng-read (msg)
  (ignore-errors (erbn-read msg)))










;; proposed register syntax..
(defun erbeng-register-syntax (fsbot-command priority check &optional
					     remap)

"If CHECK is a function, then it is passed the user input as a
string...  If it claims a match, it should return (t arglist).
Arglist is a list of arguments to be passed to the FSBOT-COMMAND.

If CHECK is a regexp, it is matched against the user input string.  If
it is a match, all the submatches 1....n (NOT 0) are passed to the
function as arguments in that order, except that you can remap using
the optional REMAP list.  That list is a list of numbers, like, say (0
1 2 4).<-- this tells us that the arguments to be passed to the
fsbot-command are the regexp-matches 0,1,2 4 in that order. If REMAP
is not supplied, you can see that the default value is more-or-less
equivalent to (1 2 3 4....)

"
nil)


(provide 'erbeng)
(run-hooks 'erbeng-after-load-hooks)



;;; erbeng.el ends here
