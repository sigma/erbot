;;; erbtrain.el --- Train erbot (erbot).. 
;; Time-stamp: <2007-11-23 11:30:00 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbtrain.el
;; Package: erbtrain
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
;; Thanks: Michael Olson


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
(defconst erbtrain-quick-start
  "Install idledo.el (tested with 0.2) and start idledo, join IRC as
yourself through ERC (tested with CVS).

Customize erbtrain-buffer to the buffer of the channel in which you want
to train a bot. 

Create bot-parsable strings in a file. 

Then, M-x erbtrain to set up erbtrain which will then feed the strings
to the bot in that channel slowly. 
"
)
(defun erbtrain-quick-start ()
  "Provides electric help from variable `erbtrain-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbtrain-quick-start) nil) "*doc*"))
(defconst erbtrain-version "NA")

;;==========================================
;;; Requires: 
(eval-when-compile (require 'cl))
(ignore-errors (require 'idledo))
;;; Code:

(defgroup erbtrain nil
  "The group erbtrain."
  :group 'applications)
(defcustom erbtrain-before-load-hooks nil
  "Hooks to run before loading erbtrain."
  :group 'erbtrain)
(defcustom erbtrain-after-load-hooks nil
  "Hooks to run after loading erbtrain."
  :group 'erbtrain)
(run-hooks 'erbtrain-before-load-hooks)


;;; Real Code:

(defcustom erbtrain-buffer "#fsbot"
  ""
  :group 'erbtrain)

(defvar erbtrain-list nil)

(defvar erbtrain-idledo-interval-small 2)
(defvar erbtrain-idledo-interval 120)
(defvar erbtrain-idledo-interval-subsequent 10)

;;;###autoload
(defun erbtrain-file-through-irc  (file)
  (interactive "f")
  (setq idledo-interval-small erbtrain-idledo-interval-small)
  (setq idledo-interval erbtrain-idledo-interval)
  (setq idledo-interval-subsequent erbtrain-idledo-interval-subsequent)
  (save-window-excursion
    (find-file file)
    (let ((allstrings (buffer-substring-no-properties
		       (point-min) (point-max))))
      (setq allstrings (split-string allstrings "\n"))
      (setq erbtrain-list allstrings)
      (erbtrain-resume))))

;;;###autoload
(defalias  'erbtrain-file 'erbtrain-file-through-irc)


(defvar erbtrain-local-buffer "*erbtrain-log*")

(defun erbtrain-file-locally (file)
  "EXPERIMENTAL. Can use this when you ARE su'ed so that you are in the bot's account.

su to your bot's account and then use this... This has the minor
advantage of being much faster. "
  (interactive "f")
  (unless
      (yes-or-no-p 
       (concat "Are you really logged in as the bot? "))
    (error "Please use M-x erbtrain-file instead. "))
  (save-window-excursion
    (find-file file)
    (let ((allstrings (buffer-substring-no-properties
		       (point-min) (point-max))))
      (setq allstrings (split-string allstrings "\n"))
      (mapcar 'erbtrain-local allstrings)))
  (display-buffer erbtrain-local-buffer))


(defun erbtrain-local (str)
  "See the doc for erbtrain-file-locally. "
  ;;(require 'erball)
  (let* ((strlisp1 (ignore-errors (fs-parse str)))
	 (strlisp (ignore-errors 
		   (if (stringp strlisp1)
		       (erbn-read strlisp1) nil)))
	 (result (ignore-errors (fs-eval strlisp))))
    (erbtrain-local-log str strlisp result)))

(defun erbtrain-local-log (str expr result)
  (save-excursion 
    (set-buffer (get-buffer-create erbtrain-local-buffer))
    (goto-char (point-max))
    (insert "\n")
    (let ((msg (concat str "\n" "=> " (format "%S" expr) "\n" "==> " 
		       (format "%S" result) "\n\n")))
      (message "%s" msg)
      (insert msg))))

    


(defun erbtrain-resume ()
  (interactive)
  (let* ((len (length erbtrain-list))
	 (toolongp (> len 3000))
	 ls)
    (if toolongp 
	(progn
	  (setq ls (subseq erbtrain-list 0 3001))
	  (setq erbtrain-list (subseq erbtrain-list 3001)))
      (setq ls erbtrain-list)
      (setq erbtrain-list nil)
      )
    (when toolongp
      (y-or-n-p 
       (format "Too LOONG list (%S).  Type M-x erbtrain-resume later. ok?"
	       len)))
    (mapcar
     'erbtrain-idle 
     (cons 
      ;; so that we prevent duplicate entries. 
      ;; this should already be the default, but just to ensure..
      ", (fs-set-add-all-disable)"
      ls
      ;;(list ", (fs-set-add-all-disable)")
      ))
    (message "Added %S idledo's" (length ls))))


(defun erbtrain-idle (str)
  "sets up a string to idly fed to the bot.."
  (idledo-add-action-at-end
   `(erbtrain ,str)))

(defun erbtrain-idle-now (str)
  "sets up a string to idly fed to the bot.."
  (idledo-add-action
   `(erbtrain ,str)))


(defun erbtrain-buggy (str)
  (delete-other-windows)
  (let ((buf ;;(buffer-name)
	 (window-buffer)))
    (display-buffer erbtrain-buffer)
    (let ((win (get-buffer-window erbtrain-buffer)))
      (if win (select-window win)
	(switch-to-buffer erbtrain-buffer)))
    (goto-char (point-max))
    (insert str)
    (erc-send-current-line)
    (let ((bufwindow (get-buffer-window buf)))
      (if bufwindow
	  (select-window bufwindow)
	(switch-to-buffer buf)))))



;;; 2003-01-13 T17:26:24-0500 (Monday)    D. Goel
;;;###autoload
(defun erbtrain (str)
  (delete-other-windows)
  (let ((buf (get-buffer erbtrain-buffer)))
    (cond
     (buf
      (switch-to-buffer buf)
      (goto-char (point-max))
      (insert str)
      (erc-send-current-line))
     (t 
      (beep)
      (message  "No buffer!  Trying to recreate the idledo. ")
      (sit-for 0.3)
      (idledo-add-action
       `(erbtrain ,str))))))


;;;====================================================
;; OT: the foll.  has nothing to do with training the bot, but is a way to
;; keep ERC connection alive: 

;(defvar erbtrain-keep-alive-string 
;  "/ping #fsbot")

;;;###autoload
(defcustom erbtrain-keep-alive-p t
  ""
  :group 'erbtrain)


;;; (defun erbtrain-keep-alive-kick-once-old ()
;;;   (interactive)
;;;   (let ((erc-flood-protect nil))
;;;     (save-window-excursion
;;;       (when  erbtrain-keep-alive-p
;;; 	(erbtrain erbtrain-keep-alive-string)))))

(defvar erbtrain-keep-alive-buffer "#somechannel")



;;; 2003-02-05 T13:22:11-0500 (Wednesday)    D. Goel
;; should do it like this:
;; <delYsid> (erc-with-all-buffers-of-server nil #'erc-server-buffer-p
;;					  (lambda () \...))

(defun erbtrain-keep-alive-kick-once ()
  (interactive)
  (let ((erc-flood-protect nil))
    (mapcar 
     (lambda (arg)
       (save-window-excursion
	 (let ((bufname (buffer-name-p-my arg)))
	   (when bufname
	     (switch-to-buffer arg)
	     ;;(erc-cmd-PING "nickserv")
	     (when (erc-process-alive) (erc-send-command "PING"))))))
     (if (listp erbtrain-keep-alive-buffer) erbtrain-keep-alive-buffer
       (list erbtrain-keep-alive-buffer)))))

(defvar erbtrain-keep-alive-timer nil)

;;;###autoload
(defun erbtrain-keep-alive ()
  (interactive)
  ;;(idledo-nullify)
  (setq erbtrain-keep-alive-timer 
	(run-with-timer 30
			10
			'erbtrain-keep-alive-kick-once))
	;;(setq erbtrain-keep-alive-active-p 
	;;t)
  
  (message "Started erbtrain-keep-alive. "))


;;;====================================================

(defun erbtrain-utils-teach-acronyms (&optional botstring)
  "Teach the the bot from BOTSTRING some acronyms.
You will need wtf.el (from the contrib directory) for this
function to work.
See also the new function fs-wtf. 
Don't forget to connect to irc before running this function."
  (interactive)
  (idledo-nullify)
  (require 'wtf)
  (unless botstring (setq botstring ", "))
  (setq erbtrain-list
        (mapcar #'(lambda (ref)
                    (concat botstring (car ref)
                            " is short for "
                            (upcase-initials (cdr ref))))
                wtf-alist))
  (erbtrain-resume)
  (ignore-errors (idledo-start)))


(provide 'erbtrain)
(run-hooks 'erbtrain-after-load-hooks)



;;; erbtrain.el ends here
