;;; erbtrain.el --- Train erbot (erbot).. 
;; Time-stamp: <2003-05-23 08:43:56 deego>
;; Copyright (C) 2002 D. Goel
;; Emacs Lisp Archive entry
;; Filename: erbtrain.el
;; Package: erbtrain
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version:
;; Author's homepage: http://deego.gnufans.org/~deego
;; For latest version:

(defconst erbtrain-home-page
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

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst erbtrain-introduction
  "Help..."
)

;;;###autoload
(defun erbtrain-introduction ()
  "Provides electric help from variable `erbtrain-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbtrain-introduction) nil) "*doc*"))

;;; Commentary:
(defconst erbtrain-commentary
  "Help..."
)

(defun erbtrain-commentary ()
  "Provides electric help from variable `erbtrain-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbtrain-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defconst erbtrain-new-features
  "Help..."
)

(defun erbtrain-new-features ()
  "Provides electric help from variable `erbtrain-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbtrain-new-features) nil) "*doc*"))

;;; TO DO:
(defconst erbtrain-todo
  "Help..."
)

(defun erbtrain-todo ()
  "Provides electric help from variable `erbtrain-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbtrain-todo) nil) "*doc*"))

(defconst erbtrain-version "0.0-DUMMY")
(defun erbtrain-version (&optional arg)
   "Display erbtrain's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "erbtrain version %s" erbtrain-version))
    (message "erbtrain version %s" erbtrain-version)))

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

(defcustom erbtrain-verbosity 0
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'erbtrain)
(defcustom erbtrain-interactivity 0
  "How interactive to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'erbtrain)
(defcustom erbtrain-y-or-n-p-function 'erbtrain-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `erbtrain-y-or-n-p'."
  :type 'function
  :group 'erbtrain)
(defcustom erbtrain-n-or-y-p-function 'erbtrain-y-or-n-p
  "Function to use for interactivity-dependent n-or-y--p.
Format same as that of `erbtrain-n-or-y-p'."
  :type 'function
  :group 'erbtrain)
(defun erbtrain-message (points &rest args)
  "Signal message, depending on POINTS anderbtrain-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points erbtrain-verbosity))
    (apply #'message args)))
(defun erbtrain-y-or-n-p (add prompt)
  "Query or assume t, based on `erbtrain-interactivity'.
ADD is added to `erbtrain-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbtrain-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun erbtrain-n-or-y-p (add prompt)
  "Query or assume t, based on `erbtrain-interactivity'.
ADD is added to `erbtrain-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbtrain-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:

(defcustom erbtrain-buffer "#fsbot"
  ""
  :group 'erbtrain)

(defvar erbtrain-list nil)

;;;###autoload
(defun erbtrain-file  (file)
  (interactive "f")
  (setq idledo-interval-small 2)
  (save-window-excursion
    (find-file file)
    (let ((allstrings (buffer-substring-no-properties
		       (point-min) (point-max))))
      (setq allstrings (split-string allstrings "\n"))
      (setq erbtrain-list allstrings)
      (erbtrain-resume))))


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
      ", (erbc-set-add-all-disable)"
      ls
      ;;(list ", (erbc-set-add-all-disable)")
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
  (switch-to-buffer erbtrain-buffer)
  (goto-char (point-max))
  (insert str)
  (erc-send-current-line))


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
	     (switch-to-buffer bufname)
	     ;;(erc-cmd-PING "nickserv")
	     (when (erc-process-alive) (erc-send-command "PING"))))))
     (if (listp erbtrain-keep-alive-buffer) erbtrain-keep-alive-buffer
       (list erbtrain-keep-alive-buffer)))))

(defvar erbtrain-keep-alive-timer nil)

;;;###autoload
(defun erbtrain-keep-alive ()
  (interactive)
  (idledo-nullify)
  (setq erbtrain-keep-alive-timer 
	(run-with-timer 30
			10
			'erbtrain-keep-alive-kick-once))
	;;(setq erbtrain-keep-alive-active-p 
	;;t)
  
  (message "Started erbtrain-keep-alive. "))













(provide 'erbtrain)
(run-hooks 'erbtrain-after-load-hooks)



;;; erbtrain.el ends here
