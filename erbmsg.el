;;; erbmsg.el --- memoserv-esque functions for Erbot
;; Time-stamp: <03/05/28 23:19:49 freundt>
;; Copyright (C) 2004 Sebastian Freundt
;; Emacs Lisp Archive entry
;; Filename: erbmsg.el
;; Package: erbmsg
;; Authors: Sebastian Freundt <freundt@math.tu-berlin.de> 
;; Keywords: memo, message, 
;; Version: still conceptional
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
;; For latest version:

(defconst erbauth-home-page
  "http://savannah.nongnu.org/projects/erbot")


 
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

;; still to write

;;; TODOs that have been done:

;;; TODO:
;; - functionality to forget the erbmsg-question-* pile
;; - save erbmsg-msg-hash-table across sessions
;; - allow multiple message between two users

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
  :group 'applications)

(defcustom erbmsg-default-magic-words nil
  "List of default magic words for messages with magic words."
  :group 'erbmsg)


(defun fs-memo (nick &rest msg)
  "Specify your message and the nick to dedicate to here, as in:

#somechan> ,memo somenick hello somenick, don't forget

Note: magic words are not currently implemented."
  (and erbot-erbmsg-p
       (let* ((nick (erbutils-stringize `(,nick)))
              (nicks-ht (or (gethash nick erbmsg-msg-hash-table)
                            (puthash nick
                                     (make-hash-table :test 'equal)
                                     erbmsg-msg-hash-table)))
              (msg (erbutils-stringize msg))
              (cnick fs-nick)
              (ctime (current-time))
              (magic-words)
              ;; composition of the new memo
              (newmsg (vector cnick msg ctime magic-words))
              (newcookie (erbmsg-generate-msg-cookie newmsg))
              ;; now memos from that user already in the system
              (cmsgs (gethash cnick nicks-ht)))
         (add-to-list 'cmsgs newcookie)
         (puthash nick cmsgs nicks-ht))
       "msg memorized for delivery"))
(defalias 'fs-msg-wmw 'fs-memo) ;; just for compatibility


(defun fs-msg-mymsgs (&rest line)
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
(defalias 'fs-memos 'fs-msg-mymsgs)
(defalias 'fs-mymemos 'fs-msg-mymsgs)
(defalias 'fs-msgs 'fs-msg-mymsgs)
(defalias 'fs-mymsgs 'fs-msg-mymsgs)


(defun erbmsg-notify-msg-on-JOIN (process parsed)
  "Notifies users about left messages
when joining the channel"
  (and erbot-erbmsg-p
       (let* ((usernickhost (aref parsed 1))
              (channel (aref parsed 2))
              (nick (erc-parse-user usernickhost)))
         (setq erbmsg-internal-msg-cookie (random))
         (let* ((msgs (fs-msg-mymsgs nick erbmsg-internal-msg-cookie)))
           (and msgs
                (erc-message "PRIVMSG"
                             (format "%s %s"
                                     channel
                                     msgs)))))))
(add-hook 'erc-server-JOIN-hook 'erbmsg-notify-msg-on-JOIN)



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
    (and nicks-ht nil
         (maphash (lambda (fromnick msg-data)
                    (let ((magic-words (aref msg-data 2)))
                      (add-to-list 'magic-words (format "me?ss?a?ge?s? ?\\(?:from\\)? %s" fromnick))
                      (nconc magic-words erbmsg-default-magic-words)
                      (and (string-match
                            (concat "^\\(?:"
                                    (mapconcat 'identity magic-words "\\|")
                                    "\\)")
                            msg)
                           (add-to-list 'pending-msgs (vconcat (vector fromnick) msg-data))
                           )))
                  nicks-ht))

    ;; erbmsg-question handling
    (and nicks-q-ht
         (maphash (lambda (keyword action-forms)
                    (and (string-match (format "\\b%S\\b" keyword)
                                       msg)
                         (let ((func (intern (format "fs-%s" keyword))))
                           (and (fboundp func)
                                (funcall func)))))
                  nicks-q-ht))

    ;; again the erbmsg stuff 
    (and pending-msgs nil
         (let ((msg-cookie (erbmsg-generate-msg-cookie pending-msgs)))
           (format "'%S"
                   (format "erm, %s msgs pending, see them? %s"
                           (length pending-msgs)
                           (erbmsg-question `((notice (erbmsg-notice-pending-msgs ,nick ',msg-cookie))
                                              (query (erbmsg-query-pending-msgs ,nick ',msg-cookie))
                                              (post (erbmsg-post-pending-msgs ,nick ',msg-cookie))
                                              (flush (erbmsg-flush-pending-msgs ,nick ',msg-cookie))
                                              (no (ignore))
                                              (memo-help (erbmsg-help)))
                                            nick)))))))


(defun erbmsg-generate-msg-cookie (message)
  "Generates a message cookie for `message' and returns it."
  (let* ((msg-cookie (format "%.8x" (random))))
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
                                (msgtext (aref msg 1))
                                (msgtime (aref msg 2)))
                            (erc-message method
                                         (format "%s %s %s: %s"
                                                 target
                                                 msgfrom
                                                 (format-time-string "%D %T (%Z)" msgtime)
                                                 msgtext))))
                     (erc-message method (format "%s invalid message cookie" target))))
               msgs))))

(defun erbmsg-flush-pending-msgs (nick msg-cookies)
  "Flushes all pending messages for user `nick'."
  (erbmsg-flush-msg-cookies msg-cookies)
  (remhash nick erbmsg-msg-hash-table)
  (remhash nick erbmsg-question-hash-table)
  (erc-send-message "flushed"))

(defun erbmsg-flush-msg-cookie (msg-cookie)
  "Flushes `msg-cookie'."
  (remhash msg-cookie erbmsg-msg-cookie-hash-table))
(defun erbmsg-flush-msg-cookies (msg-cookies)
  "Flushes a collection of `msg-cookies'."
  (mapc 'erbmsg-flush-msg-cookie msg-cookies))


(defun erbmsg-help (&rest ignore)
  "Spits out some detour to the wiki help page."
  (erc-send-message "help? whom to help? see http://www.emacswiki.org/cgi-bin/wiki/ErbMsg"))






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
                                 erbnoc-char
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


(provide 'erbmsg)

;; Local variables:
;; indent-tab-mode: nil
;; End:
