;;; erbot.el --- Another robot for ERC.
;; Time-stamp: <2003-05-23 08:43:33 deego>
;; Emacs Lisp Archive entry
;; Filename: erbot.el
;; Package: erbot
;; Authors:  David Edmunston (dme@dme.org) 
;; Modified by: D. Goel <deego@gnufans.org>
;; Version: 0.0
;; Author's homepage: http://deego.gnufans.org/~deego
;; Maintainer: Deepak Goel <deego@gnufans.org>

;; For latest version: 
(defvar erbot-home-page
  "http://deego.gnufans.org/~deego/pub/emacspub/lisp-mine/erbot/")

;; Version: 
;; Keywords: ERC, IRC, chat, robot, bot

;; Copyright (C) 2002 Deepak Goel, FSF

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.




;; See also:
;; erc-robot.el from which this was derived...







;; Thanks for erbot's/erbot's behavior and their data go to a lot
;; of people on #emacs, like: 
;; kensanata (Alex Schroeder)
;; resolve (Damien Elmes)
;; bpt  (Brian P. Templeton)
;; forcer (Jorgen "forcer" Schaefer)
;; and many others

;; and also to bot(s): 
;; apt on debian, for english syntax examples.

;; Thanks for code go to: 
;; David Edmonsdon (who wrote erc-robot.el which is what this started
;; out from).
;; Nick Hober (who wrote google.el)





;; Quick start:
(defvar erbot-quick-start
  "Add (erbot-install) to .emacs.. and join channels.. 
for a more detailed example, look at fsbot's .emacs here:
http://deego.gnufans.org/~fsbot/dotemacs-fsbot ..

If you want to join channels automatically, have a look at the
function M-x erbot-join-servers ...  viz. start emacs and type M-x
erbot-join-servers..

Note that bbdb-case-fold-search should be t <-- this one is quite
important, as should erc-auto-query be, for this bot to function
properly.

"
)

(defun erbot-quick-start ()
  "Provides electric help regarding variable `erbot-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbot-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar erbot-introduction
  "Help..."
)

;;;###autoload
(defun erbot-introduction ()
  "Provides electric help regarding variable `erbot-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbot-introduction) nil) "*doc*"))

;;; Commentary:
(defvar erbot-commentary
  "Help..."
)



;;; David E's Commentary:

;; erbot is a derivative of David's erc-robot.el --- that code was
;; copied over on 2002-09-02 into erbot.el.  Erbot seeks to make the
;; bot similar to apt on #debian.. viz: English style.. yet allowing
;; access to commands via the "cmd" command.  Erbot shall seek to
;; save all its information periodically, and publicly...


;; Erc-robot implements a simple robot for ERC.

;; Installation:

;; The robot uses hooks to gain access to ERC.  The following need to
;; be executed after ERC has loaded:

;;     (load-library "erbot")


;; It is particularly important that the remote robot function is added
;; to the tail of the PRIVMSG hook.

;; Robot commands are declared using the list "erbot-commands".
;; XXX better description of the functions.
;; An example might be:

;; (setq erbot-commands
;;       '(
;; 	("cmds" t (lambda (args)
;; 		  (concat "commands available: "
;; 			  (mapconcat
;; 			   (lambda (e)
;; 			     (car e))
;; 			   erbot-commands " "))))
;; 	("hello" t (lambda (args) "hello to you too !"))
;; 	("zippy" t (lambda (args) (erc-replace-regexp-in-string "\n" " " (yow))))
;; 	("music" t (lambda (args) (concat "now playing: "
;; 					(let ((track (dme:now-playing)))
;; 					  (if track
;; 					      track
;; 					    "nothing.")))))
;; 	("echo" t (lambda (args) args))
;;	; only i'm allowed to talk to my doctor !
;; 	("doctor" nil erc-doctor)
;; 	("version" t (lambda (args) (erc-version)))
;;       ))


; compatability
;(if (featurep 'xemacs)
;    (defun erc-replace-regexp-in-string
;      (regexp rep string &optional fixedcase literal subexp start)
;      (replace-in-string string regexp rep literal))

(defalias 'erc-replace-regexp-in-string 'replace-regexp-in-string)



(defun erbot-commentary ()
  "Provides electric help regarding variable `erbot-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbot-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defvar erbot-new-features
  "Help..."
)

(defun erbot-new-features ()
  "Provides electric help regarding variable `erbot-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbot-new-features) nil) "*doc*"))

;;; TO DO:
(defvar erbot-todo
  "Current shortcomings:"

)

(defun erbot-todo ()
  "Provides electric help regarding variable `erbot-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbot-todo) nil) "*doc*"))

(defvar erbot-version "0.0")

;;==========================================
;;; Code:
(require 'cl)

(defcustom erbot-before-load-hooks nil "" :group 'erbot)
(defcustom erbot-after-load-hooks nil "" :group 'erbot)


(defcustom erbot-ignore-nicks '("^apt[0-9]?$" "bot" "google" "serv")
  "A list of REGEXPS. 
Nicks matching these regexps will be ignored by the bot, viz. not
generate replies. 

I would suggest including atleast bot, google and serv here to prevent
infinite chat loops with other bots.  :)
" 

:group 'erbot)

(defcustom erbot-ignore-userinfos "" "list of regex's" :group 'erbot)
(run-hooks 'erbot-before-load-hooks)


(defgroup erbot nil 
  "The group erbot"
   :group 'applications)

(defcustom erbot-nick "fsbot" 
"Changing this in the middle of things
may have unspecified and unpleasant results..."
:group 'erbot)

(defvar erbot-end-user-nick "dummy-nick"
  "just a temporary variable..")

(defvar erbot-end-user-nick-latest "dummy-end-user-nick-latest"
  "just a temporary variable..")





(defcustom erbot-servers-channels
      '(("irc.openprojects.net"
         ("#testopn"
            ))
	(".gnome.org"
         ("#testgnome")
	 ;; optional but: 
	 6667
	 ))
      "Servers and channels ..."
      :group 'erbot)



;  (defalias 'erc-replace-regexp-in-string 'replace-regexp-in-string))


(defface erbot-face '((t (:foreground "yellow")))
  "Face used for your robot's output."
  :group 'erc-faces)

(defcustom erbot-commands nil
  "A list of robot commands and the functions which implement them."
  :group 'erc
  :type '(repeat (list string (choice (const nil) (const t) string) function))
  )

; This function is used by the example above.
(defun erbot-doctor (args)
  "Glue the doctor into the ERC robot."
  (let* ((dbuf "*doctor*")
	 (docbuf (get-buffer dbuf))
	 outpoint
	 res)
    (if (not docbuf)
	(progn
	  (doctor)
	  (setq docbuf (get-buffer dbuf))
	  (bury-buffer docbuf)))
    (save-excursion
      (set-buffer docbuf)
      (goto-char (point-max))
      (insert args)
      (goto-char (point-max))
      (setq outpoint (point))
      (doctor-ret-or-read 1)
      (doctor-ret-or-read 1)
      (goto-char outpoint)
      (re-search-forward "^.")
      (setq outpoint (- (point) 1))
      (re-search-forward "^$")
      (erc-replace-regexp-in-string
       "\n" " " (buffer-substring outpoint (point)))
    )))



(defun erbot-dunnet (arg)
  "Glue the dunnet into the ERC robot."
  (save-excursion
    (let ((freshp nil)
	  outpoint res ans 
	  (pre "")
	  full
	  )
      (when (or (not (boundp 'dun-dead)) dun-dead
		(not (get-buffer "*dungeon*"))
		)
	(setq freshp t)
	(setq dun-dead nil))
      (when freshp (dunnet))
      (set-buffer "*dungeon*")
      (goto-char (point-max))
      (when (string-match "save" arg)
	(setq arg "save ~/pub/dunnet/dunnet.game")
	(setq pre "Will save to ~/pub/dunnet/dunnet.game"))
      (cond 
       ((string-match "^.?more" arg)
	(setq ans (erbc-more)))
       (t
	(unless freshp (insert arg))
	(goto-char (point-max))
	(setq outpoint (if freshp (point-min) (point)))
	(unless freshp (dun-parse 1))
	(setq ans
	      (buffer-substring-no-properties 
	       outpoint (- (point-max) 1)))
	(when (equal arg "quit")
	  (when (kill-buffer "*dungeon*")))))
      (setq full (concat pre ans))
      (when 
	  (string-match  
	   "I don't understand that"
	   full)
	(setq 
	 full
	 (concat 
	  full
	  " I am in dunnet mode.  For regular fsbot, type , (dunnet-mode)")))
      full)))

(defvar erbot-quiet-p nil)
(defun erbot-quiet ()
  (interactive)
  (setq erbot-quiet-p 
	(not erbot-quiet-p))
  (message "set to %S" erbot-quiet-p))

;; A very very main function..
(defun erbot-remote (proc parsed)
  "Implements a simple robot for erc.  Messages to the robot are of the form:
\"nick: !command args\", where:
nick	- the nickname of the user who is the target of the command,
command	- the specific command,
args	- arguments to the command (optional)."
  (set-buffer (process-buffer proc))
  (let* ((sspec (aref parsed 1))
	 (userinfo (erc-parse-user sspec))
	 (nick (nth 0 userinfo))
	 (tgta (aref parsed 2))
	 (tgt (if (equalp tgta erbot-nick)
		  nick
		tgta))
	 (msg (aref parsed 3))
	 (erbot-end-user-nick nick)
	 )
    ;; changing the structure here..
    ;; also changing erbot-command to erbot-reply..
    ;; from now on, erend-main will take care of what to reply.. 
    ;; erbot-reply will simply take the reply and reply that...
    ;; should not be setq.. else other invocations may change it..
    ;;(setq erbot-end-user-nick nick)
    
    (setq erbot-end-user-nick-latest erbot-end-user-nick)
    (setq erbc-tgt tgt)
    (setq erbc-nick nick)
    (let ((msgg
	   (erbeng-main msg proc nick tgt nil userinfo)))	   
      ;; erbot-reply needs a correct buffer...
      (set-buffer (process-buffer proc))
      (unless erbot-quiet-p
	(erbot-reply 
	 msgg
	 proc nick tgt msg nil
	 ))))
  nil)


(defun erbot-frob-with-init-string (reply)
  (cond
   ((or (not (stringp reply)) (string= erbot-init-string "")) reply)
   (t
    (with-temp-buffer
      (insert reply)
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
	(replace-match 
	 (concat "\n" erbot-init-string) nil t))
      (concat erbot-init-string (buffer-string))))))

(defvar erbot-init-string ""
  "The basic init string.. should be concated to ALL lines of
replies... right at last.. the values it will hold will look like /msg
foo, and will be set by erbc-parse-english, when that function
determines it appropriate..
Currently: we do not use it, since we have found a better way to do
those things..

")

;; this one is probably never used any more... just to make sure,
;; introduced an error command..
;(defun erbot-local (str)
;  "Funnel text typed by the local user to the local robot.  See
;\"erbot-remote\" for details of the command format."
;  (error "foo")
;  (erbot-command erc-process (erc-current-nick) (buffer-name) str t))

(defcustom erbot-reply-p t
  "when nil, don't reply")

(defun erbot-toggle-reply ()
  (interactive)
  (setq erbot-reply-p (not erbot-reply-p))
  (message "erbot-reply-p set to %S" erbot-reply-p)
  )
(defun erbot-reply (main-reply proc from tgt msg locally-generated)
  "Robot worker.  Should do nothing when main-reply is nil. 

"
  (unless (stringp main-reply)
    (setq main-reply (format "%S" main-reply)))
  (let ((me (or (erc-current-nick) erbot-nick))
	;;(if (and erbot-commands
	;;	     (string-match (concat "^" (regexp-quote me)
	;;				   ": !\\([^ ]+\\) ?\\(.*\\)") msg))
	;;					; this is a robot command to me.
	;;	(let* ((cmd (substring msg (match-beginning 1) (match-end 1)))
	;;	       (args (substring msg (match-beginning 2)))
	;;	       (l (assoc cmd erbot-commands))
	;;	       (allowed-users (nth 1 l))
	;;	       (function (nth 2 l))
	;;	       (permitted (or (eq t allowed-users)
	;;			      (and (eq nil allowed-users) locally-generated)
	;;			      (and (stringp allowed-users)
	;;				   (string-match allowed-users
	;;						 (regexp-quote from)))))
	
	
	
	;;(reply (concat from ": " main-reply))
	;; my frobbing of reply..
	(reply 
	 (erbot-frob-with-init-string main-reply))
	
	
	(rep-buffer (erc-get-buffer tgt proc)))
    ;;(if permitted
    ;;				  (if l
    ;;				      (funcall function args)
    ;;(concat "unknown command: " cmd
    ;;					    ": try \"cmds\""))
    ;;				(concat "no access to command \"" cmd
    ;;					"\" for " from ".")))))
    (when (and reply
	       (not (erbot-safep reply)))
      (setq reply (concat " " reply)))
    (erc-log reply)
    
    
    (unless
	(or
	 (null erbot-reply-p)
	 (equal main-reply 'noreply)
	 (equal main-reply "noreply"))
      ;; now we are actually gonna reply. 
      (save-excursion
	(setq reply (erbc-limit-lines reply))
	(if rep-buffer (set-buffer rep-buffer)
	;;; this alternative reply somehow never gets sent out..
	  ;;(setq reply (concat "msg " from " " 
	  ;;		      "No private msgs.. try #testopn"))
	  ;;(set-buffer (erc-get-buffer tgt proc))
	  (progn
	    (ding t)
	    (message "WTF? no rep-buffer? "))
	  )
	(let* ((inhibit-read-only t)
	       (lines (split-string reply "\n"))
	       (multiline-p (< 1 (length lines)))
	       p)
	  (mapc
	   (lambda (line)
	     (goto-char (point-max))
	     (setq p (re-search-backward (erc-prompt)))
	     ;;(insert (erc-format-timestamp) "<" me "> ")
	     (insert ;;(erc-format-timestamp) 
	      "<" me "> ")
	     (erc-put-text-property 0 (length line) 'face
				    'erbot-face line)
	     (insert line "\n")
	     (save-excursion
	       (save-match-data
		 (save-restriction
		   (narrow-to-region p (point))
		   (run-hook-with-args 'erc-send-modify-hook)
		   (run-hook-with-args 'erc-send-post-hook))))
	     (set-marker (process-mark erc-process) (point))
	     (set-marker erc-insert-marker (point))
	     (goto-char (point-max))
	     
	     (erc-process-input-line (concat line "\n") t multiline-p))
	   lines))))))


;;;###autoload
(defun erbot-install ()
  (interactive)
  (add-hook 'erc-server-PRIVMSG-hook 'erbot-remote t)
  ;; Do we need this local command thing...?
  ;;(add-hook 'erc-send-completed-hook 'erbot-local t)
  
  (add-hook 'erc-server-376-hook
            'erbot-autojoin-channels)
  )


;;;###autoload
(defun erbot-autojoin-channels (server nick)
  ;;(interactive)
  (dolist (l erbot-servers-channels)
    (when (string-match (car l) (process-name server))
      (dolist (chan (cadr l))
        (erc-send-command (concat "join " chan))))))



(defun erbot-get-servers ()
  (mapcar '(lambda (arg) (list (car arg) (caddr arg)))
	  erbot-servers-channels))

;;;###autoload
(defun erbot-join-servers (&optional server port nick
				   user-full-name
				   not-connect-arg passwd)
  "Try to never join if already joined..."
  (interactive)
  (require 'erc)
  (if (null server)
      (mapcar
       '(lambda (arg)
	  (erbot-join-servers
	   (car arg) (cadr arg) nick user-full-name not-connect-arg passwd)
	  (sit-for 1)
	  )
       
       ;; get the list of servers
       (erbot-get-servers)

       )
    (progn
      ;;(if (null server)
      ;;	  (setq server erc-server))
      ;; 2002-08-21 T11:22:35-0400 (Wednesday)    D. Goel
      (setq erc-current-server-my server)

      (if (null port) (setq port erc-port))
      (setq nick (or erbot-nick (erc-compute-nick nick)))
      (let* (
	     (foo 'bar)
	     ;(nick
	     ; (if (erc-already-logged-in server port nick)
	     ;;	  (read-from-minibuffer
	     ;;	   (erc-format-message 'nick-in-use ?n nick)
	     ;;	   nick
	     ;;	   nil nil 'erc-nick-history-list)
	     ;;	nick)))
	     )
	(if (and passwd (string= "" passwd))
	    (setq passwd nil))
	;; 	(while (erc-already-logged-in server port nick)
	;; 	  (setq nick (read-from-minibuffer
	;; 		      (erc-format-message 'nick-in-use ?n nick)
	;; 		      nick
	;; 		      nil nil 'erc-nick-history-list)))

	(run-hook-with-args 'erc-before-connect server port nick)
	(unless (erc-already-logged-in server port nick)
	  (erc
	   server port nick user-full-name (not not-connect-arg) passwd))))))



(defun erbot-safep (reply)
  (or
   (not (string-match "^/" reply))
   (equal 0 (string-match "^/me " reply))))


(defun erbot-dunnet-install ()
  "Defines some dunnet specific aliases. "
  (interactive)
  (require 'dunnet)
  (defalias 'dun-read-line 'erbc-botread)
  ;;(defalias 'dun-mprinc
  ;;'erbc-dun-mprinc))
  )



(provide 'erbot)
(run-hooks 'erbot-after-load-hooks)



;;; erbot.el ends here
