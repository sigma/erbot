;;; erbmsg.el --- memoserv-esque functions for Erbot
;; $Id: erbmsg.el,v 1.21 2005/01/08 17:53:08 deego Exp $
;; Copyright (C) 2004 Sebastian Freundt
;; Emacs Lisp Archive entry
;; Filename: erbmsg.el
;; Package: erbmsg
;; Authors: Sebastian Freundt <freundt@math.tu-berlin.de> 
;; Keywords: memo, message, 
;; Version: still conceptional
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErbMsg
;; For latest version:

(defconst erbot-home-page
  "http://savannah.nongnu.org/projects/erbot")
(defconst erbmsg-version
  "Version 0.2 $Revision: 1.21 $")

 
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

;;; Comments:

;; - To automagically save the whole message table with each incoming message
;;   put following to your .erbot:
;;
;;   (add-hook 'erbmsg-new-msg-post-hook 'erbmsg-regular-dump)
;;
;; - To clean up message cookies with every flushed message, add
;;
;;   (add-hook 'erbmsg-flush-post-hook 'erbmsg-garbage-cleanse-cookies)


;;; TODOs that have been done:

;; 2004/06/22:
;; - added dump routines to dump message hash tables to hard disk
;; - added routines for restoring from dumped message files
;; - added interval within erbot does not notify on channel joins
;; - added erbmsg-new-msg-(pre|post)-hook
;; 2004/04/09:
;; - added support for multiple recipients (see fs-memo for syntax)
;; - abstracted fs-memo stuff to two defuns (erbmsg-memo-parse-msg and erbmsg-memorize-msg)
;; 2004/04/01:
;; - added hooks
;; 2004/03/31:
;; - store which channel the memo came from
;; - added garbage collection function (erbmsg-garbage-cleanse-cookies) to
;;   clean up erbmsg-msg-cookie-hash-table from unreferenced cookies

;;; TODO:
;; - functionality to forget the erbmsg-question-* pile effectively
;; - save erbmsg-msg-hash-table across sessions
;; - expire cookies in erbmsg-msg-cookie-hash-table some time (after 3 notifications?)

;;; Data


(defvar erbmsg-msg-hash-table (make-hash-table :test 'equal)
  "This is a hash-table holding all the messages via cookies.")

(defvar erbmsg-internal-msg-cookie nil
  "Message cookie for internal communication.")

(defvar erbmsg-msg-cookie-hash-table (make-hash-table :test 'equal)
  "This is the hash-table for message cookies, the actual
messages are saved here")

(defgroup erbmsg nil
  "The erbmsg module for erbot"
  :group 'erbot)

(defcustom erbmsg-default-magic-words nil
  "List of default magic words for messages with magic words."
  :group 'erbmsg)


;;; dump settings

(defcustom erbmsg-dump-file "~/public_html/data/messages.dump"
  "File to dump message hash tables to."
  :group 'erbmsg)

(defcustom erbmsg-auto-restore-message-tables t
  "Whether to automagically restore contents of `erbmsg-dump-file'."
  :group 'erbmsg
  :type 'boolean)

(defcustom erbmsg-auto-dump-message-tables nil
  "Whether to automagically dump hash tables to `erbmsg-dump-file'."
  :group 'erbmsg
  :type 'boolean)



;;; uncomment this to normalize to UTC
;;(set-time-zone-rule "UTC0")

(defvar erbmsg-after-load-hook nil
  "Hook called after `erbmsg' has been loaded.")

(defvar erbmsg-new-msg-pre-hook nil
  "Hook called before a new message has been posted.
The raw message is passed as argument.")
(defvar erbmsg-new-msg-post-hook
  (when erbmsg-auto-dump-message-tables
    '(erbmsg-regular-dump))
  "Hook called after a new message has been posted.
The parsed message \(split to nicks and actual message text\)
is passed as argument.")
(defvar erbmsg-flush-pre-hook nil
  "Hook called before erbmsg-flush-pending-msgs is called.")
(defvar erbmsg-flush-post-hook nil
  "Hook called before erbmsg-flush-pending-msgs is called.")


;;; this is too useful to not add it here
(add-hook 'erbmsg-flush-post-hook 'erbmsg-garbage-cleanse-cookies)


;; interface functions
(defun fs-memo (&rest msg)
  "Specify your message and the nick to dedicate to here, as in:

#somechan> ,memo somenick hello somenick, don't forget

Allowed syntaxes:
,memo [to|for] <nick> msg
,memo [to|for] <nick> <nick> <nick>: msg

Note: magic words are not currently implemented."
  (or (and erbot-erbmsg-p
           msg
           (let* ((msg-raw (erbutils-stringize msg))
                  (nicks+msg (erbmsg-memo-parse-msg msg-raw)))
             (run-hook-with-args 'erbmsg-new-msg-pre-hook msg-raw)
             (mapc (lambda (nick+msg)
                     (let* ((nick (car nick+msg))
                            (msg (nth 1 nick+msg)))
                       (erbmsg-memorize-msg nick msg)))
                   nicks+msg)
             (run-hook-with-args 'erbmsg-new-msg-post-hook nicks+msg)
             "msg memorized for delivery"))
      'noreply))
(defalias 'fs-msg-wmw 'fs-memo) ;; just for compatibility
(defalias 'fs-msg-with-magic-words 'fs-memo)


(defun erbmsg-memo-parse-msg (raw-msg)
  "Parses MSG for any of the allowed memo syntaxes and returns a list
\(\(nick msg) (nick msg) ...)"
  (let* ((nick-msg (cond ((string-match "^\\(?:to\\|for\\)?\\b\\(.+\\)\\b:\\(.*\\)" raw-msg)
                          (cons (match-string 1 raw-msg) (match-string 2 raw-msg)))
                         ((string-match "^\\(?:to\\|for\\)?\\(?:\\s-\\|\\b\\)\\(\\S-+\\)\\s-\\(.*\\)" raw-msg)
                          (cons (match-string 1 raw-msg) (match-string 2 raw-msg)))
                         (t nil)))
         (nicks (split-string (replace-regexp-in-string ",\\|\\band\\b" "" (car nick-msg))))
         (msg (replace-regexp-in-string "^\\s-+" "" (cdr nick-msg))))
    (mapcar (lambda (nick)
              (list nick msg))
            nicks)))
;;(erbmsg-memo-parse-msg "hroptatyr and deego: huhu! :)")

(defun erbmsg-memorize-msg (nick msg &optional magic-words)
  "Memorizes NICKs MSG."
  (let* ((nicks-ht (or (gethash nick erbmsg-msg-hash-table)
                       (puthash nick
                                (make-hash-table :test 'equal)
                                erbmsg-msg-hash-table)))
         (cnick fs-nick)
         (cchan fs-tgt)
         (ctime (current-time))
         ;; composition of the new memo
         (newmsg (vector cnick cchan msg ctime magic-words))
         (newcookie (erbmsg-generate-msg-cookie newmsg))
         ;; now memos from that user already in the system
         (cmsgs (gethash cnick nicks-ht)))
    (add-to-list 'cmsgs newcookie)
    (puthash cnick cmsgs nicks-ht)))



(defun fs-memos (&rest line)
  "This is redundant but more clean than in `erbmsg-parse'."
  (and erbot-erbmsg-p
       (let* ((linecar (car line))
              (internalp (and erbmsg-internal-msg-cookie
                              (eq linecar ':internal)
                              (eq erbmsg-internal-msg-cookie (cadr line))))
              (nick (or (and internalp
                             (car (cdr-safe (cdr-safe line))))
                        fs-nick))
              (fromnicks (and (null internalp)
                              (mapcar (lambda (s) (format "%s" s)) line)))
              (nicks-ht (gethash nick erbmsg-msg-hash-table))
              pending-msgs)
         (and nicks-ht
              (maphash (lambda (fromnick msg-cookies)
                         (setq pending-msgs
                               (append pending-msgs (or (and (null fromnicks)
                                                            msg-cookies)
                                                       (and (member fromnick fromnicks)
                                                            msg-cookies)))))
                       nicks-ht))
         (or (and pending-msgs 
                  (let ((msg-cookie))
                    (format "erm, %s, %s msgs pending, see them? %s"
                            nick
                            (length pending-msgs)
                            (erbmsg-question `((notice (erbmsg-notice-pending-msgs ,nick ',pending-msgs))
                                               (query (erbmsg-query-pending-msgs ,nick ',pending-msgs))
                                               (post (erbmsg-post-pending-msgs ,nick ',pending-msgs))
                                               (flush (erbmsg-flush-pending-msgs ,nick ',pending-msgs))
                                               (no (ignore))
                                               (memo-help (erbmsg-help)))
                                             nick))))
             (and (null internalp)
                  (format ":( no msgs for you, %s" nick))))))
(defalias 'fs-msg-mymsgs 'fs-memos)
(defalias 'fs-mymemos 'fs-memos)
(defalias 'fs-msgs 'fs-msg-mymsgs)
(defalias 'fs-mymsgs 'fs-msg-mymsgs)

(defun fsi-erbmsg-version (&rest ignore)
  "Spits out `erbmsg-version'."
  erbmsg-version)
(defalias 'fs-msg-version 'fs-erbmsg-version)


(defcustom erbmsg-notify-on-join-timeout 2
  "Interval in seconds to wait between notification on channel joins."
  :group 'erbmsg)

(defvar erbmsg-last-nicks-join nil
  "List of nicks with last join time.")

(defun erbmsg-notify-msg-on-JOIN (process parsed)
  "Notifies users about left messages
when joining the channel"
  (and erbot-erbmsg-p
       (let* ((usernickhost (if erbot-on-new-erc-p
                                (erc-response.sender parsed)
                              (aref parsed 1)))
              (channel (if erbot-on-new-erc-p
                           (nth 0 (erc-response.command-args parsed))
                         (aref parsed 2)))
              (nick (car (erc-parse-user usernickhost)))
              (last-access (cdr-safe (assoc nick erbmsg-last-nicks-join))))
         (set-alist 'erbmsg-last-nicks-join nick (current-time))
         (setq erbmsg-internal-msg-cookie (random))
         (let* ((msgs (fs-msg-mymsgs :internal erbmsg-internal-msg-cookie nick)))
           (and msgs
                (or (null last-access)
                    (> (- (nth 1 (current-time)) (nth 1 last-access))
                       erbmsg-notify-on-join-timeout))
                (erc-message "PRIVMSG"
                             (format "%s %s"
                                     channel
                                     msgs)))
           'noreply))))
(if erbot-on-new-erc-p
    (add-hook 'erc-server-JOIN-functions 'erbmsg-notify-msg-on-JOIN)
  (add-hook 'erc-server-JOIN-hook 'erbmsg-notify-msg-on-JOIN))



(defun erbmsg-parse (msg proc nick tgt localp userinfo)
  "When having (require 'erbmsg) and (setq erbot-erbmsg-p t) 
this function is called with every message typed.

It checks for `nick' being in `erbmsg-msg-hash-table',
if so, i.e. `nick' is about to have messages pending for delivery,
it will be checked here if `nick' says the ~magic words~,
if that's also the case, the message will be spit out.

Currently this function also plays the role as question handler,
see erbmsg-question part below :)."
  (let* ((nicks-ht (gethash nick erbmsg-msg-hash-table))
         (pending-msgs)

         ;; now the stuff for question handling
         (nicks-q-ht (gethash nick erbmsg-question-hash-table))
         (pending-actions))

    ;; erbmsg-question handling
    (and nicks-q-ht
         (maphash (lambda (keyword action-forms)
                    (and (string-match (format "\\b%S\\b" keyword)
                                       msg)
                         (let ((func (intern (format "fs-%s" keyword))))
                           (and (fboundp func)
                                (funcall func)))))
                  nicks-q-ht))))


(defun erbmsg-generate-msg-cookie (message)
  "Generates a message cookie for `message' and returns it."
  (let* ((msg-cookie (format "%.4x%.4x"
                             (mod (random) 65536) (mod (random) 65536))))
    (puthash msg-cookie message erbmsg-msg-cookie-hash-table)
    msg-cookie))

(defun erbmsg-get-msgs (msg-cookies)
  "Gets messages by `msg-cookie'."
  (mapcar (lambda (msg-cookie)
            (gethash msg-cookie erbmsg-msg-cookie-hash-table))
          msg-cookies))


;; reply functions
(defun erbmsg-notice-pending-msgs (nick msg-cookies)
  "NOTICEs all `msgs' to the user `nick'."
  (erbmsg-send-pending-msgs nick msg-cookies "NOTICE" nick))

(defun erbmsg-query-pending-msgs (nick msg-cookies)
  "PRIVMSGs all `msgs' to the user `nick'."
  (erbmsg-send-pending-msgs nick msg-cookies "PRIVMSG" nick))

(defun erbmsg-post-pending-msgs (nick msg-cookies)
  "Publically post all `msgs' to current channel"
  (erbmsg-send-pending-msgs nick msg-cookies "PRIVMSG" fs-tgt))

(defun erbmsg-send-pending-msgs (nick msg-cookies &optional method target)
  "PRIVMSGs all `msgs' to the user `nick',
instead of PRIVMSG you may specify another sending method."
  (let ((msgs (erbmsg-get-msgs msg-cookies))
        (method (or method "PRIVMSG"))
        (target (or target fs-nick)))
    (and msgs
         (mapc (lambda (msg)
                 (or (and msg
                          (let ((msgfrom (aref msg 0))
                                (msgchan (aref msg 1))
                                (msgtext (aref msg 2))
                                (msgtime (aref msg 3)))
                            (erc-message method
                                         (format "%s %s@%s %s: %s"
                                                 target
                                                 msgfrom
                                                 msgchan
                                                 (format-time-string "%D %T (%Z)" msgtime)
                                                 msgtext))))
                     (erc-message method (format "%s invalid message cookie" target))))
               msgs))))

(defun erbmsg-flush-pending-msgs (nick msg-cookies)
  "Flushes all pending messages for user `nick'."
  (run-hook-with-args 'erbmsg-flush-pre-hook nick msg-cookies)
  (erbmsg-flush-msg-cookies msg-cookies)
  (remhash nick erbmsg-msg-hash-table)
  (remhash nick erbmsg-question-hash-table)
  (erc-send-message "flushed")
  (run-hook-with-args 'erbmsg-flush-post-hook nick msg-cookies))

(defun erbmsg-flush-msg-cookie (msg-cookie)
  "Flushes `msg-cookie'."
  (remhash msg-cookie erbmsg-msg-cookie-hash-table))
(defun erbmsg-flush-msg-cookies (msg-cookies)
  "Flushes a collection of `msg-cookies'."
  (mapc 'erbmsg-flush-msg-cookie msg-cookies))


(defun erbmsg-help (&rest ignore)
  "Spits out some detour to the wiki help page."
  (erc-send-message "help? whom to help? see http://www.emacswiki.org/cgi-bin/wiki/ErbMsg"))





;; garbage collection

(defun erbmsg-garbage-cleanse-cookies (&rest ignore)
  "Collects garbage from `erbmsg-msg-cookie-hash-table' when
there's no referring entry in `erbmsg-msg-hash-table'."
  (maphash (lambda (cookie-k cookie-v)
             (let ((cookie cookie-k)
                   (referred))
               (catch 'ref-exists-p
                 (maphash (lambda (memo-k memo-v)
                            (maphash (lambda (from cookie-list)
                                       (and (member cookie cookie-list)
                                            (setq referred t)
                                            (throw 'ref-exists-p t)))
                                     memo-v))
                          erbmsg-msg-hash-table))
               (unless referred
                 (remhash cookie erbmsg-msg-cookie-hash-table))))
           erbmsg-msg-cookie-hash-table))
;; erbmsg-msg-cookie-hash-table
;; (erbmsg-garbage-cleanse-cookies)





;;; just some tricks to create gazillions of msgs w/o IRC
;; (clrhash erbmsg-msg-hash-table)
;; (puthash "hroptatyr" (make-hash-table :test 'equal) erbmsg-msg-hash-table)
;; (puthash "asathor" '("22224444" "33336666") (gethash "hroptatyr" erbmsg-msg-hash-table))







;;; this will get more abstract and move to an own modules soon :)
(defvar erbmsg-question-hash-table (make-hash-table :test 'equal)
  "Hash table to hold who may be about to have the choice.")

(defvar erbmsg-question-verbosity nil
  "Controls how talkative erbot is when being in question mode.")

(defvar erbmsg-question-handler nil
  "command temporarily bound to certain users.")

(defun erbmsg-question (choices nick)
  "Declares choices for interactively control erbot's
more complex tasks.

`choices' is an alist (action action-forms),
`action-forms' will be eval'd if nick uses the magic word once again."
  (let* ((nicks-ht (puthash nick (make-hash-table :test 'equal) erbmsg-question-hash-table)))
    (mapc (lambda (choice)
            (let* ((magic-word (car choice))
                   (action-forms (cdr choice))
                   (internal-name (intern (format "fs-%s" magic-word))))
              (and ;;(not (fboundp internal-name))
                   (fset internal-name
                         `(lambda (&rest ignore)
                            (and (erbmsg-question-user-allowed-p fs-nick ',magic-word)
                                 (mapc 'eval ',action-forms))
                            (erbmsg-question-user-answer fs-nick ',magic-word))))
              (puthash magic-word action-forms nicks-ht)))
          choices)
    (format "[type %s]"
            (mapconcat (lambda (choice)
                         (format "%s%s"
                                 erbn-char
                                 (car choice)))
                       choices "/"))))

;;(symbol-function 'fs-flush)


(defun erbmsg-question-user-allowed-p (nick erbot-command)
  "Tests whether the user `nick' is allowed to use `erbot-command',
i.e. if the user has been offered such an action."
  (let* ((nicks-ht (gethash nick erbmsg-question-hash-table))
         (command-p (and nicks-ht
                         (gethash erbot-command nicks-ht))))
    (null (null command-p))))
(defun erbmsg-question-user-answer (nick erbot-command &optional answer)
  "Tests whether the user `nick' is allowed to use `erbot-command',
if so return 'noreply, if not return an according answer."
  (or (and (not (erbmsg-question-user-allowed-p nick erbot-command))
           'noreply) ;;; "You are currently not allowed to use this function. :(")
      'noreply))


;;; dumping code
;; this code is to make erbot remember messages after restarts

(defun erbmsg-regular-dump (&rest ignore)
  "Fun wrapper to call `erbmsg-dump-tables'."
  (interactive)
  (erbmsg-dump-tables))

(defun erbmsg-dump-tables (&optional file)
  "Dumps known message hash tables to a buffer in order to save it."
  (interactive "Ferbmsg dump file: ")
  (let ((file (or file
                  erbmsg-dump-file)))
    (with-temp-buffer
      (mapc (lambda (htable)
              (insert (format "[%s \n [\n" htable))
              (maphash
               (lambda (key val)
                 (insert
                  (format "  [%S %s]\n" key
                          (cond
                           ((hash-table-p val)
                            (let (valstring)
                              (maphash
                               (lambda (k2 v2)
                                 (setq valstring
                                       (format "%s[%S %S]"
                                               (or valstring "") k2 v2)))
                               val)
                              (format "(%s)" valstring)))
                           (t (format "%S" val))))))
               (eval htable))
              (insert (format " ]\n]\n")))
            '(erbmsg-msg-hash-table erbmsg-msg-cookie-hash-table))
      (write-file erbmsg-dump-file))))

(defun erbmsg-restore-tables (&optional file)
  "Restores known message hash tables from FILE or `erbmsg-dump-file'."
  (interactive "ferbmsg dump file: ")
  (let* ((file (or file
                   erbmsg-dump-file))
         (file-vector
          (and (file-readable-p file)
               (with-temp-buffer
                 (insert-file-contents file)
                 (eval (read (format "(setq file-vector '(%s))"
                                     (buffer-string))))))))
    (mapvector
     (lambda (tablevector)
       (let ((table (aref tablevector 0))
             (vector (aref tablevector 1)))
         (mapvector
          (lambda (keyval)
            (let ((key (aref keyval 0))
                  (val (aref keyval 1)))
              (cond ((listp val)
                     (let ((nickht (make-hash-table :test 'equal)))
                       (mapc
                        (lambda (htvec)
                          (let ((k2 (aref htvec 0))
                                (v2 (aref htvec 1)))
                            (puthash k2 v2 nickht)))
                        val)
                       (puthash key nickht (eval table))))
                    (t (puthash key val (eval table))))))
          vector)))
     file-vector)))

(when (and erbot-erbmsg-p
           erbmsg-auto-restore-message-tables
           ;;(eq (hash-table-count erbmsg-msg-hash-table) 0)
           )
  (erbmsg-restore-tables))


(provide 'erbmsg)

;;; erbmsg.el ends here

;; Local variables:
;; indent-tab-mode: nil
;; End:
