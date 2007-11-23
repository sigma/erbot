;;; erbp.el --- not yet functional, personal erbot-interface, stolen from dunnet.el
;; we should perhaps remove this file, is not in use -- DG.

;; Copyright (C) 1992, 1993, 2001 Free Software Foundation, Inc.

;; Author: Ron Schnell <ronnie@driver-aces.com>
;; Created: 25 Jul 1992
;; Version: 0.0dev
;; Keywords: games

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; For starters, namespaces: dun, dunnet, erbpeon, mostly get mapped
;; to erbp, erbpne and erbpeon respectively.  
;; room-->erbp-room

;;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
;;;  The log file should be set for your system, and it must
;;;  be writable by all.

;;; Code:

(defgroup erbpnet nil
  "Text adventure for Emacs."
  :prefix "erbp-"
  :group 'games)

(defconst erbp-version "0.0dev")

(defcustom erbp-log-file "/usr/local/erbpnet.score"
  "Name of file to store score information for erbpnet."
  :type 'file
  :group 'erbpnet)

(if nil
    (eval-and-compile (setq byte-compile-warnings nil)))

(eval-when-compile
 (require 'cl))

;;;; Mode definitions for interactive mode

(defun erbp-mode ()
  "Major mode for running erbpnet."
  (interactive)
  (text-mode)
  (make-local-variable 'scroll-step)
  (setq scroll-step 2)
  (use-local-map erbpeon-mode-map)
  (setq major-mode 'erbp-mode)
  (setq mode-name "Erbpeon"))

(defun erbp-parse (arg)
  "Function called when return is pressed in interactive mode to parse line."
  (interactive "*p")
  (beginning-of-line)
  (setq beg (+ (point) 1))
  (end-of-line)
  (if (and (not (= beg (point))) (not (< (point) beg))
	   (string= ">" (buffer-substring (- beg 1) beg)))
      (progn
	(setq line (downcase (buffer-substring beg (point))))
	(princ line)
	(if (eq (erbp-vparse erbp-ignore erbp-verblist line) -1)
	    (erbp-mprinc "I don't understand that.\n")))
    (goto-char (point-max))
    (erbp-mprinc "\n"))
    (erbp-messages))
    
(defun erbp-messages ()
  (if erbp-dead
      (text-mode)
    (if (eq erbpeon-mode 'erbpeon)
	(progn
	  (if (not (= erbp-room erbp-current-room))
	      (progn
		(erbp-describe-room erbp-current-room)
		(setq erbp-room erbp-current-room)))
	  (erbp-fix-screen)
	  (erbp-mprinc ">")))))


;;;###autoload
(defun erbpnet ()
  "Switch to *erbpeon* buffer and start game."
  (interactive)
  (switch-to-buffer "*erbpeon*")
  (erbp-mode)
  (setq erbp-dead nil)
  (setq erbp-room 0)
  (erbp-messages))

;;;;
;;;; This section contains all of the verbs and commands.
;;;;

;;; Give long description of room if haven't been there yet.  Otherwise
;;; short.  Also give long if we were called with negative room number.

(defun erbp-describe-room (erbp-room)
  (if (and (not (member (abs erbp-room) erbp-light-rooms)) 
	   (not (member obj-lamp erbp-inventory)))
      (erbp-mprincl "It is pitch dark.  You are likely to be eaten by a grue.")
    (erbp-mprincl (cadr (nth (abs erbp-room) erbp-rooms)))
    (if (and (and (or (member erbp-room erbp-visited) 
		      (string= erbp-mode "erbp-superb")) (> erbp-room 0))
	     (not (string= erbp-mode "long")))
	nil
      (erbp-mprinc (car (nth (abs erbp-room) erbp-rooms)))
    (erbp-mprinc "\n"))
    (if (not (string= erbp-mode "long"))
	(if (not (member (abs erbp-room) erbp-visited))
	    (setq erbp-visited (append (list (abs erbp-room)) erbp-visited))))
    (dolist (xobjs (nth erbp-current-room erbp-room-objects))
      (if (= xobjs obj-special)
	  (erbp-special-object)
	(if (>= xobjs 0)
	    (erbp-mprincl (car (nth xobjs erbp-objects)))
	  (if (not (and (= xobjs obj-bus) erbp-inbus))
	      (progn
		(erbp-mprincl (car (nth (abs xobjs) erbp-perm-objects)))))))
      (if (and (= xobjs obj-jar) erbp-jar)
	  (progn
	    (erbp-mprincl "The jar contains:")
	    (dolist (x erbp-jar)
	      (erbp-mprinc "     ")
	      (erbp-mprincl (car (nth x erbp-objects)))))))
    (if (and (member obj-bus (nth erbp-current-room erbp-room-objects)) erbp-inbus)
	(erbp-mprincl "You are on the bus."))))

;;; There is a special object in the erbp-room.  This object's description,
;;; or lack thereof, depends on certain conditions.

(defun erbp-special-object ()
  (if (= erbp-current-room computer-room)
      (if erbp-computer
	  (erbp-mprincl 
"The panel lights are flashing in a seemingly organized pattern.")
	(erbp-mprincl "The panel lights are steady and motionless.")))

  (if (and (= erbp-current-room red-room) 
	   (not (member obj-towel (nth red-room erbp-room-objects))))
      (erbp-mprincl "There is a hole in the floor here."))

  (if (and (= erbp-current-room marine-life-area) erbp-black)
      (erbp-mprincl 
"The room is lit by a black light, causing the fish, and some of 
your objects, to give off an eerie glow."))
  (if (and (= erbp-current-room fourth-vermont-intersection) erbp-hole)
      (progn
	(if (not erbp-inbus)
	    (progn
	      (erbp-mprincl"You fall into a hole in the ground.")
	      (setq erbp-current-room vermont-station)
	      (erbp-describe-room vermont-station))
	  (progn
	    (erbp-mprincl 
"The bus falls down a hole in the ground and explodes.")
	    (erbp-die "burning")))))

  (if (> erbp-current-room endgame-computer-room)
      (progn
	(if (not erbp-correct-answer)
	    (erbp-endgame-question)
	  (erbp-mprincl "Your question is:")
	  (erbp-mprincl erbp-endgame-question))))

  (if (= erbp-current-room sauna)
      (progn
	(erbp-mprincl (nth erbp-sauna-level '(
"It is normal room temperature in here."
"It is luke warm in here."
"It is comfortably hot in here."
"It is refreshingly hot in here."
"You are dead now.")))
	(if (= erbp-sauna-level 3) 
	    (progn
	      (if (or (member obj-rms erbp-inventory)
		      (member obj-rms (nth erbp-current-room erbp-room-objects)))
		  (progn
		    (erbp-mprincl 
"You notice the wax on your statuette beginning to melt, until it completely
melts off.  You are left with a beautiful diamond!")
		    (if (member obj-rms erbp-inventory)
			(progn
			  (erbp-remove-obj-from-inven obj-rms)
			  (setq erbp-inventory (append erbp-inventory 
						      (list obj-diamond))))
		      (erbp-remove-obj-from-room erbp-current-room obj-rms)
		      (erbp-replace erbp-room-objects erbp-current-room
				   (append (nth erbp-current-room erbp-room-objects)
					   (list obj-diamond))))))
	      (if (or (member obj-floppy erbp-inventory)
		      (member obj-floppy (nth erbp-current-room erbp-room-objects)))
		  (progn
		    (erbp-mprincl
"You notice your floppy disk beginning to melt.  As you grab for it, the 
disk bursts into flames, and disintegrates.")
		    (erbp-remove-obj-from-inven obj-floppy)
		    (erbp-remove-obj-from-room erbp-current-room obj-floppy))))))))


(defun erbp-die (murderer)
  (erbp-mprinc "\n")
  (if murderer
      (erbp-mprincl "You are dead."))
  (erbp-do-logfile 'erbp-die murderer)
  (erbp-score nil)
  (setq erbp-dead t))

(defun erbp-quit (args)
  (erbp-die nil))

;;; Print every object in player's inventory.  Special case for the jar,
;;; as we must also print what is in it.

(defun erbp-inven (args)
  (erbp-mprinc "You currently have:")
  (erbp-mprinc "\n")
  (dolist (curobj erbp-inventory)
    (if curobj
	(progn
	  (erbp-mprincl (cadr (nth curobj erbp-objects)))
	  (if (and (= curobj obj-jar) erbp-jar)
	      (progn
		(erbp-mprincl "The jar contains:")
		(dolist (x erbp-jar)
		  (erbp-mprinc "     ")
		  (erbp-mprincl (cadr (nth x erbp-objects))))))))))

(defun erbp-shake (obj)
  (let (objnum)
    (when (setq objnum (erbp-objnum-from-args-std obj))
      (if (member objnum erbp-inventory)
	  (progn
;;;	If shaking anything will do anything, put here.
	    (erbp-mprinc "Shaking ")
	    (erbp-mprinc (downcase (cadr (nth objnum erbp-objects))))
	    (erbp-mprinc " seems to have no effect.")
	    (erbp-mprinc "\n")
	    )
	(if (and (not (member objnum (nth erbp-current-room erbp-room-silents)))
		 (not (member objnum (nth erbp-current-room erbp-room-objects))))
	    (erbp-mprincl "I don't see that here.")
;;;     Shaking trees can be deadly
	  (if (= objnum obj-tree)
	      (progn
		(erbp-mprinc
 "You begin to shake a tree, and notice a coconut begin to fall from the air.
As you try to get your hand up to block it, you feel the impact as it lands
on your head.")
		(erbp-die "a coconut"))
	    (if (= objnum obj-bear)
		(progn
		  (erbp-mprinc
"As you go up to the bear, it removes your head and places it on the ground.")
		  (erbp-die "a bear"))
	      (if (< objnum 0)
		  (erbp-mprincl "You cannot shake that.")
		(erbp-mprincl "You don't have that.")))))))))


(defun erbp-drop (obj)
  (if erbp-inbus
      (erbp-mprincl "You can't drop anything while on the bus.")
  (let (objnum ptr)
    (when (setq objnum (erbp-objnum-from-args-std obj))
      (if (not (setq ptr (member objnum erbp-inventory)))
	  (erbp-mprincl "You don't have that.")
	(progn
	  (erbp-remove-obj-from-inven objnum)
	  (erbp-replace erbp-room-objects erbp-current-room
		   (append (nth erbp-current-room erbp-room-objects)
			   (list objnum)))
	  (erbp-mprincl "Done.")
	  (if (member objnum (list obj-food obj-weight obj-jar))
	      (erbp-drop-check objnum))))))))

;;; Dropping certain things causes things to happen.

(defun erbp-drop-check (objnum)
  (if (and (= objnum obj-food) (= erbp-room bear-hangout)
	   (member obj-bear (nth bear-hangout erbp-room-objects)))
      (progn
	(erbp-mprincl
"The bear takes the food and runs away with it. He left something behind.")
	(erbp-remove-obj-from-room erbp-current-room obj-bear)
	(erbp-remove-obj-from-room erbp-current-room obj-food)
	(erbp-replace erbp-room-objects erbp-current-room
		 (append (nth erbp-current-room erbp-room-objects)
			 (list obj-key)))))

  (if (and (= objnum obj-jar) (member obj-nitric erbp-jar) 
	   (member obj-glycerine erbp-jar))
      (progn
	(erbp-mprincl 
	 "As the jar impacts the ground it explodes into many pieces.")
	(setq erbp-jar nil)
	(erbp-remove-obj-from-room erbp-current-room obj-jar)
	(if (= erbp-current-room fourth-vermont-intersection)
	    (progn
	      (setq erbp-hole t)
	      (setq erbp-current-room vermont-station)
	      (erbp-mprincl 
"The explosion causes a hole to open up in the ground, which you fall
through.")))))

  (if (and (= objnum obj-weight) (= erbp-current-room maze-button-room))
      (erbp-mprincl "A passageway opens.")))

;;; Give long description of current erbp-room, or an object.
      
(defun erbp-examine (obj)
  (let (objnum)
    (setq objnum (erbp-objnum-from-args obj))
    (if (eq objnum obj-special)
	(erbp-describe-room (* erbp-current-room -1))
      (if (and (eq objnum obj-computer)
	       (member obj-pc (nth erbp-current-room erbp-room-silents)))
	  (erbp-examine '("pc"))
	(if (eq objnum nil)
	    (erbp-mprincl "I don't know what that is.")
	  (if (and (not (member objnum 
				(nth erbp-current-room erbp-room-objects)))
		   (not (and (member obj-jar erbp-inventory)
			     (member objnum erbp-jar)))
		   (not (member objnum 
				(nth erbp-current-room erbp-room-silents)))
		   (not (member objnum erbp-inventory)))
	      (erbp-mprincl "I don't see that here.")
	    (if (>= objnum 0)
		(if (and (= objnum obj-bone) 
			 (= erbp-current-room marine-life-area) erbp-black)
		    (erbp-mprincl 
"In this light you can see some writing on the bone.  It says:
For an explosive time, go to Fourth St. and Vermont.")
		  (if (nth objnum erbp-physobj-desc)
		      (erbp-mprincl (nth objnum erbp-physobj-desc))
		    (erbp-mprincl "I see nothing special about that.")))
	      (if (nth (abs objnum) erbp-permobj-desc)
		  (progn
		    (erbp-mprincl (nth (abs objnum) erbp-permobj-desc)))
		(erbp-mprincl "I see nothing special about that.")))))))))

(defun erbp-take (obj)
    (setq obj (erbp-firstword obj))
    (if (not obj)
	(erbp-mprincl "You must supply an object.")
      (if (string= obj "all")
	  (let (gotsome)
	    (if erbp-inbus
		(erbp-mprincl "You can't take anything while on the bus.")
	      (setq gotsome nil)
	      (dolist (x (nth erbp-current-room erbp-room-objects))
		(if (and (>= x 0) (not (= x obj-special)))
		    (progn
		      (setq gotsome t)
		      (erbp-mprinc (cadr (nth x erbp-objects)))
		      (erbp-mprinc ": ")
		      (erbp-take-object x))))
	      (if (not gotsome)
		  (erbp-mprincl "Nothing to take."))))
	(let (objnum)
	  (setq objnum (cdr (assq (intern obj) erbp-objnames)))
	  (if (eq objnum nil)
	      (progn
		(erbp-mprinc "I don't know what that is.")
		(erbp-mprinc "\n"))
	    (if (and erbp-inbus (not (and (member objnum erbp-jar)
					 (member obj-jar erbp-inventory))))
		(erbp-mprincl "You can't take anything while on the bus.")
	      (erbp-take-object objnum)))))))

(defun erbp-take-object (objnum)
  (if (and (member objnum erbp-jar) (member obj-jar erbp-inventory))
      (let (newjar)
	(erbp-mprincl "You remove it from the jar.")
	(setq newjar nil)
	(dolist (x erbp-jar)
	  (if (not (= x objnum))
	      (setq newjar (append newjar (list x)))))
	(setq erbp-jar newjar)
	(setq erbp-inventory (append erbp-inventory (list objnum))))
    (if (not (member objnum (nth erbp-current-room erbp-room-objects)))
	(if (not (member objnum (nth erbp-current-room erbp-room-silents)))
	    (erbp-mprinc "I do not see that here.")
	  (erbp-try-take objnum))
      (if (>= objnum 0)
	  (progn
	    (if (and (car erbp-inventory) 
		     (> (+ (erbp-inven-weight) (nth objnum erbp-object-lbs)) 11))
		(erbp-mprinc "Your load would be too heavy.")
	      (setq erbp-inventory (append erbp-inventory (list objnum)))
	      (erbp-remove-obj-from-room erbp-current-room objnum)
	      (erbp-mprinc "Taken.  ")
	      (if (and (= objnum obj-towel) (= erbp-current-room red-room))
		  (erbp-mprinc 
		   "Taking the towel reveals a hole in the floor."))))
	(erbp-try-take objnum)))
    (erbp-mprinc "\n")))

(defun erbp-inven-weight ()
  (let (total)
    (setq total 0)
    (dolist (x erbp-jar)
      (setq total (+ total (nth x erbp-object-lbs))))
    (dolist (x erbp-inventory)
      (setq total (+ total (nth x erbp-object-lbs)))) total))

;;; We try to take an object that is untakable.  Print a message
;;; depending on what it is.

(defun erbp-try-take (obj)
  (erbp-mprinc "You cannot take that."))

(defun erbp-dig (args)
  (if erbp-inbus
      (erbp-mprincl "Digging here reveals nothing.")
  (if (not (member 0 erbp-inventory))
      (erbp-mprincl "You have nothing with which to dig.")
    (if (not (nth erbp-current-room erbp-diggables))
	(erbp-mprincl "Digging here reveals nothing.")
      (erbp-mprincl "I think you found something.")
      (erbp-replace erbp-room-objects erbp-current-room
	       (append (nth erbp-current-room erbp-room-objects)
		       (nth erbp-current-room erbp-diggables)))
      (erbp-replace erbp-diggables erbp-current-room nil)))))

(defun erbp-climb (obj)
  (let (objnum)
    (setq objnum (erbp-objnum-from-args obj))
    (cond ((not objnum)
	   (erbp-mprincl "I don't know what that object is."))
	  ((and (not (eq objnum obj-special))
		(not (member objnum (nth erbp-current-room erbp-room-objects)))
		(not (member objnum (nth erbp-current-room erbp-room-silents)))
		(not (and (member objnum erbp-jar) (member obj-jar erbp-inventory)))
		(not (member objnum erbp-inventory)))
	   (erbp-mprincl "I don't see that here."))
	  ((and (eq objnum obj-special)
		(not (member obj-tree (nth erbp-current-room erbp-room-silents))))
	   (erbp-mprincl "There is nothing here to climb."))
	  ((and (not (eq objnum obj-tree)) (not (eq objnum obj-special)))
	   (erbp-mprincl "You can't climb that."))
	  (t
	   (erbp-mprincl
	    "You manage to get about two feet up the tree and fall back down.  You
notice that the tree is very unsteady.")))))

(defun erbp-eat (obj)
  (let (objnum)
    (when (setq objnum (erbp-objnum-from-args-std obj))
      (if (not (member objnum erbp-inventory))
	  (erbp-mprincl "You don't have that.")
	(if (not (= objnum obj-food))
	    (progn
	      (erbp-mprinc "You forcefully shove ")
	      (erbp-mprinc (downcase (cadr (nth objnum erbp-objects))))
	      (erbp-mprincl " down your throat, and start choking.")
	      (erbp-die "choking"))
	  (erbp-mprincl "That tasted horrible.")
	  (erbp-remove-obj-from-inven obj-food))))))

(defun erbp-put (args)
    (let (newargs objnum objnum2 obj)
      (setq newargs (erbp-firstwordl args))
      (if (not newargs)
	  (erbp-mprincl "You must supply an object")
	(setq obj (intern (car newargs)))
	(setq objnum (cdr (assq obj erbp-objnames)))
	(if (not objnum)
	    (erbp-mprincl "I don't know what that object is.")
	  (if (not (member objnum erbp-inventory))
	      (erbp-mprincl "You don't have that.")
	    (setq newargs (erbp-firstwordl (cdr newargs)))
	    (setq newargs (erbp-firstwordl (cdr newargs)))
	    (if (not newargs)
		(erbp-mprincl "You must supply an indirect object.")
	      (setq objnum2 (cdr (assq (intern (car newargs)) erbp-objnames)))
	      (if (and (eq objnum2 obj-computer) (= erbp-current-room pc-area))
		  (setq objnum2 obj-pc))
	      (if (not objnum2)
		  (erbp-mprincl "I don't know what that indirect object is.")
		(if (and (not (member objnum2 
				      (nth erbp-current-room erbp-room-objects)))
			 (not (member objnum2 
				      (nth erbp-current-room erbp-room-silents)))
			 (not (member objnum2 erbp-inventory)))
		    (erbp-mprincl "That indirect object is not here.")
		  (erbp-put-objs objnum objnum2)))))))))

(defun erbp-put-objs (obj1 obj2)
  (if (and (= obj2 obj-drop) (not erbp-nomail))
      (setq obj2 obj-chute))

  (if (= obj2 obj-disposal) (setq obj2 obj-chute))

  (if (and (= obj1 obj-cpu) (= obj2 obj-computer))
      (progn
	(erbp-remove-obj-from-inven obj-cpu)
	(setq erbp-computer t)
	(erbp-mprincl
"As you put the CPU board in the computer, it immediately springs to life.
The lights start flashing, and the fans seem to startup."))
    (if (and (= obj1 obj-weight) (= obj2 obj-button))
	(erbp-drop '("weight"))
      (if (= obj2 obj-jar)                 ;; Put something in jar
	  (if (not (member obj1 (list obj-paper obj-diamond obj-emerald
				      obj-license obj-coins obj-egg
				      obj-nitric obj-glycerine)))
	      (erbp-mprincl "That will not fit in the jar.")
	    (erbp-remove-obj-from-inven obj1)
	    (setq erbp-jar (append erbp-jar (list obj1)))
	    (erbp-mprincl "Done."))
	(if (= obj2 obj-chute)                 ;; Put something in chute
	    (progn
	      (erbp-remove-obj-from-inven obj1)
	      (erbp-mprincl 
"You hear it slide down the chute and off into the distance.")
	      (erbp-put-objs-in-treas (list obj1)))
	  (if (= obj2 obj-box)              ;; Put key in key box
	      (if (= obj1 obj-key)
		  (progn
		    (erbp-mprincl
"As you drop the key, the box begins to shake.  Finally it explodes
with a bang.  The key seems to have vanished!")
		    (erbp-remove-obj-from-inven obj1)
		    (erbp-replace erbp-room-objects computer-room (append
							(nth computer-room
							     erbp-room-objects)
							(list obj1)))
		    (erbp-remove-obj-from-room erbp-current-room obj-box)
		    (setq erbp-key-level (1+ erbp-key-level)))
		(erbp-mprincl "You can't put that in the key box!"))

	    (if (and (= obj1 obj-floppy) (= obj2 obj-pc))
		(progn
		  (setq erbp-floppy t)
		  (erbp-remove-obj-from-inven obj1)
		  (erbp-mprincl "Done."))

	      (if (= obj2 obj-urinal)                   ;; Put object in urinal
		  (progn
		    (erbp-remove-obj-from-inven obj1)
		    (erbp-replace erbp-room-objects urinal (append 
						  (nth urinal erbp-room-objects)
						   (list obj1)))
		    (erbp-mprincl
		     "You hear it plop down in some water below."))
		(if (= obj2 obj-mail)
		    (erbp-mprincl "The mail chute is locked.")
		  (if (member obj1 erbp-inventory)
		      (erbp-mprincl 
"I don't know how to combine those objects.  Perhaps you should
just try dropping it.")
		    (erbp-mprincl"You can't put that there.")))))))))))

(defun erbp-type (args)
  (if (not (= erbp-current-room computer-room))
      (erbp-mprincl "There is nothing here on which you could type.")
    (if (not erbp-computer)
	(erbp-mprincl 
"You type on the keyboard, but your characters do not even echo.")
      (erbp-unix-interface))))

;;; Various movement directions

(defun erbp-n (args)
  (erbp-move north))

(defun erbp-s (args)
  (erbp-move south))

(defun erbp-e (args)
  (erbp-move east))

(defun erbp-w (args)
  (erbp-move west))

(defun erbp-ne (args)
  (erbp-move northeast))

(defun erbp-se (args)
  (erbp-move southeast))

(defun erbp-nw (args)
  (erbp-move northwest))

(defun erbp-sw (args)
  (erbp-move southwest))

(defun erbp-up (args)
  (erbp-move up))

(defun erbp-down (args)
  (erbp-move down))

(defun erbp-in (args)
  (erbp-move in))

(defun erbp-out (args)
  (erbp-move out))

(defun erbp-go (args)
  (if (or (not (car args)) 
	  (eq (erbp-doverb erbp-ignore erbp-verblist (car args) 
			  (cdr (cdr args))) -1))
      (erbp-mprinc "I don't understand where you want me to go.\n")))

;;; Uses the erbpeon-map to figure out where we are going.  If the
;;; requested direction yields 255, we know something special is
;;; supposed to happen, or perhaps you can't go that way unless
;;; certain conditions are met.

(defun erbp-move (dir)
  (if (and (not (member erbp-current-room erbp-light-rooms)) 
	   (not (member obj-lamp erbp-inventory)))
      (progn
	(erbp-mprinc 
"You trip over a grue and fall into a pit and break every bone in your
body.")
	(erbp-die "a grue"))
    (let (newroom)
      (setq newroom (nth dir (nth erbp-current-room erbpeon-map)))
      (if (eq newroom -1)
	  (erbp-mprinc "You can't go that way.\n")
	(if (eq newroom 255)
	    (erbp-special-move dir)
	  (setq erbp-room -1)
	  (setq erbp-lastdir dir)
	  (if erbp-inbus
	      (progn
		(if (or (< newroom 58) (> newroom 83))
		    (erbp-mprincl "The bus cannot go this way.")
		  (erbp-mprincl 
		   "The bus lurches ahead and comes to a screeching halt.")
		  (erbp-remove-obj-from-room erbp-current-room obj-bus)
		  (setq erbp-current-room newroom)
		  (erbp-replace erbp-room-objects newroom
			   (append (nth newroom erbp-room-objects)
				   (list obj-bus)))))
	    (setq erbp-current-room newroom)))))))

;;; Movement in this direction causes something special to happen if the
;;; right conditions exist.  It may be that you can't go this way unless
;;; you have a key, or a passage has been opened.

;;; coding note: Each check of the current room is on the same 'if' level,
;;; i.e. there aren't else's.  If two rooms next to each other have
;;; specials, and they are connected by specials, this could cause
;;; a problem.  Be careful when adding them to consider this, and
;;; perhaps use else's.

(defun erbp-special-move (dir)
  (if (= erbp-current-room building-front)
      (if (not (member obj-key erbp-inventory))
	  (erbp-mprincl "You don't have a key that can open this door.")
	(setq erbp-current-room old-building-hallway))
    (if (= erbp-current-room north-end-of-cave-passage)
	(let (combo)
	  (erbp-mprincl 
"You must type a 3 digit combination code to enter this room.")
	  (erbp-mprinc "Enter it here: ")
	  (setq combo (erbp-read-line))
	  (if (not erbp-batch-mode)
	      (erbp-mprinc "\n"))
	  (if (string= combo erbp-combination)
	      (setq erbp-current-room gamma-computing-center)
	    (erbp-mprincl "Sorry, that combination is incorrect."))))

    (if (= erbp-current-room bear-hangout)
	(if (member obj-bear (nth bear-hangout erbp-room-objects))
	    (progn
	      (erbp-mprinc 
"The bear is very annoyed that you would be so presumptuous as to try
and walk right by it.  He tells you so by tearing your head off.
")
	      (erbp-die "a bear"))
	  (erbp-mprincl "You can't go that way.")))

    (if (= erbp-current-room vermont-station)
	(progn
	  (erbp-mprincl
"As you board the train it immediately leaves the station.  It is a very
bumpy ride.  It is shaking from side to side, and up and down.  You
sit down in one of the chairs in order to be more comfortable.")
	  (erbp-mprincl
"\nFinally the train comes to a sudden stop, and the doors open, and some
force throws you out.  The train speeds away.\n")
	  (setq erbp-current-room museum-station)))

    (if (= erbp-current-room old-building-hallway)
	(if (and (member obj-key erbp-inventory)
		 (> erbp-key-level 0))
	    (setq erbp-current-room meadow)
	  (erbp-mprincl "You don't have a key that can open this door.")))

    (if (and (= erbp-current-room maze-button-room) (= dir northwest))
	(if (member obj-weight (nth maze-button-room erbp-room-objects))
	    (setq erbp-current-room 18)
	  (erbp-mprincl "You can't go that way.")))

    (if (and (= erbp-current-room maze-button-room) (= dir up))
	(if (member obj-weight (nth maze-button-room erbp-room-objects))
	    (erbp-mprincl "You can't go that way.")
	  (setq erbp-current-room weight-room)))

    (if (= erbp-current-room classroom)
	(erbp-mprincl "The door is locked."))

    (if (or (= erbp-current-room lakefront-north) 
	    (= erbp-current-room lakefront-south))
	(erbp-swim nil))

    (if (= erbp-current-room reception-area)
	(if (not (= erbp-sauna-level 3))
	    (setq erbp-current-room health-club-front)
	  (erbp-mprincl
"As you exit the building, you notice some flames coming out of one of the
windows.  Suddenly, the building explodes in a huge ball of fire.  The flames
engulf you, and you burn to death.")
	  (erbp-die "burning")))

    (if (= erbp-current-room red-room)
	(if (not (member obj-towel (nth red-room erbp-room-objects)))
	    (setq erbp-current-room long-n-s-hallway)
	  (erbp-mprincl "You can't go that way.")))

    (if (and (> dir down) (> erbp-current-room gamma-computing-center) 
	     (< erbp-current-room museum-lobby))
	(if (not (member obj-bus (nth erbp-current-room erbp-room-objects)))
	    (erbp-mprincl "You can't go that way.")
	  (if (= dir in)
	      (if erbp-inbus
		  (erbp-mprincl
		   "You are already in the bus!")
		(if (member obj-license erbp-inventory)
		    (progn
		      (erbp-mprincl 
		       "You board the bus and get in the driver's seat.")
		      (setq erbp-nomail t)
		      (setq erbp-inbus t))
		  (erbp-mprincl "You are not licensed for this type of vehicle.")))
	    (if (not erbp-inbus)
		(erbp-mprincl "You are already off the bus!")
	      (erbp-mprincl "You hop off the bus.")
	      (setq erbp-inbus nil))))
      (if (= erbp-current-room fifth-oaktree-intersection)
	  (if (not erbp-inbus)
	      (progn
		(erbp-mprincl "You fall down the cliff and land on your head.")
		(erbp-die "a cliff"))
	    (erbp-mprincl
"The bus flies off the cliff, and plunges to the bottom, where it explodes.")
	    (erbp-die "a bus accident")))
      (if (= erbp-current-room main-maple-intersection)
	  (progn
	    (if (not erbp-inbus)
		(erbp-mprincl "The gate will not open.")
	      (erbp-mprincl
"As the bus approaches, the gate opens and you drive through.")
	      (erbp-remove-obj-from-room main-maple-intersection obj-bus)
	      (erbp-replace erbp-room-objects museum-entrance 
		       (append (nth museum-entrance erbp-room-objects)
			       (list obj-bus)))
	      (setq erbp-current-room museum-entrance)))))
    (if (= erbp-current-room cave-entrance)
	(progn
	  (erbp-mprincl
"As you enter the room you hear a rumbling noise.  You look back to see
huge rocks sliding down from the ceiling, and blocking your way out.\n")
	  (setq erbp-current-room misty-room)))))

(defun erbp-long (args)
  (setq erbp-mode "long"))

(defun erbp-turn (obj)
  (let (objnum direction)
    (when (setq objnum (erbp-objnum-from-args-std obj))
      (if (not (or (member objnum (nth erbp-current-room erbp-room-objects))
		   (member objnum (nth erbp-current-room erbp-room-silents))))
	  (erbp-mprincl "I don't see that here.")
	(if (not (= objnum obj-dial))
	    (erbp-mprincl "You can't turn that.")
	  (setq direction (erbp-firstword (cdr obj)))
	  (if (or (not direction) 
		  (not (or (string= direction "clockwise")
			   (string= direction "counterclockwise"))))
	      (erbp-mprincl "You must indicate clockwise or counterclockwise.")
	    (if (string= direction "clockwise")
		(setq erbp-sauna-level (+ erbp-sauna-level 1))
	      (setq erbp-sauna-level (- erbp-sauna-level 1)))
	    
	    (if (< erbp-sauna-level 0)
		(progn
		  (erbp-mprincl 
		   "The dial will not turn further in that direction.")
		  (setq erbp-sauna-level 0))
	      (erbp-sauna-heat))))))))

(defun erbp-sauna-heat ()
  (if (= erbp-sauna-level 0)
      (erbp-mprincl 
       "The temperature has returned to normal room temperature."))
  (if (= erbp-sauna-level 1)
      (erbp-mprincl "It is now luke warm in here.  You are perspiring."))
  (if (= erbp-sauna-level 2)
      (erbp-mprincl "It is pretty hot in here.  It is still very comfortable."))
  (if (= erbp-sauna-level 3)
      (progn
	(erbp-mprincl 
"It is now very hot.  There is something very refreshing about this.")
	(if (or (member obj-rms erbp-inventory) 
		(member obj-rms (nth erbp-current-room erbp-room-objects)))
	    (progn
	      (erbp-mprincl 
"You notice the wax on your statuette beginning to melt, until it completely
melts off.  You are left with a beautiful diamond!")
	      (if (member obj-rms erbp-inventory)
		  (progn
		    (erbp-remove-obj-from-inven obj-rms)
		    (setq erbp-inventory (append erbp-inventory 
						(list obj-diamond))))
		(erbp-remove-obj-from-room erbp-current-room obj-rms)
		(erbp-replace erbp-room-objects erbp-current-room
			 (append (nth erbp-current-room erbp-room-objects)
				 (list obj-diamond))))))
	(if (or (member obj-floppy erbp-inventory)
		(member obj-floppy (nth erbp-current-room erbp-room-objects)))
	    (progn
	      (erbp-mprincl
"You notice your floppy disk beginning to melt.  As you grab for it, the 
disk bursts into flames, and disintegrates.")
	      (if (member obj-floppy erbp-inventory)
		  (erbp-remove-obj-from-inven obj-floppy)
		(erbp-remove-obj-from-room erbp-current-room obj-floppy))))))

  (if (= erbp-sauna-level 4)
      (progn
	(erbp-mprincl 
"As the dial clicks into place, you immediately burst into flames.")
	(erbp-die "burning"))))

(defun erbp-press (obj)
  (let (objnum)
    (when (setq objnum (erbp-objnum-from-args-std obj))
      (if (not (or (member objnum (nth erbp-current-room erbp-room-objects))
		   (member objnum (nth erbp-current-room erbp-room-silents))))
	  (erbp-mprincl "I don't see that here.")
	(if (not (member objnum (list obj-button obj-switch)))
	    (progn
	      (erbp-mprinc "You can't ")
	      (erbp-mprinc (car line-list))
	      (erbp-mprincl " that."))
	  (if (= objnum obj-button)
	      (erbp-mprincl
"As you press the button, you notice a passageway open up, but
as you release it, the passageway closes."))
	  (if (= objnum obj-switch)
	      (if erbp-black
		  (progn
		    (erbp-mprincl "The button is now in the off position.")
		    (setq erbp-black nil))
		(erbp-mprincl "The button is now in the on position.")
		(setq erbp-black t))))))))

(defun erbp-swim (args)
  (if (not (member erbp-current-room (list lakefront-north lakefront-south)))
      (erbp-mprincl "I see no water!")
    (if (not (member obj-life erbp-inventory))
	(progn
	  (erbp-mprincl 
"You dive in the water, and at first notice it is quite cold.  You then
start to get used to it as you realize that you never really learned how
to swim.")
	  (erbp-die "drowning"))
      (if (= erbp-current-room lakefront-north)
	  (setq erbp-current-room lakefront-south)
	(setq erbp-current-room lakefront-north)))))


(defun erbp-score (args)
  (if (not erbp-endgame)
      (let (total)
	(setq total (erbp-reg-score))
	(erbp-mprinc "You have scored ")
	(erbp-mprinc total)
	(erbp-mprincl " out of a possible 90 points.") total)
    (erbp-mprinc "You have scored ")
    (erbp-mprinc (erbp-endgame-score))
    (erbp-mprincl " endgame points out of a possible 110.")
    (if (= (erbp-endgame-score) 110)
	(erbp-mprincl 
"\n\nCongratulations.  You have won.  The wizard password is 'moby'"))))

(defun erbp-help (args)
  (erbp-mprincl
"Welcome to erbpnet (2.01), by Ron Schnell (ronnie@driver-aces.com).
Here is some useful information (read carefully because there are one
or more clues in here):
- If you have a key that can open a door, you do not need to explicitly
  open it.  You may just use 'in' or walk in the direction of the door.

- If you have a lamp, it is always lit.

- You will not get any points until you manage to get treasures to a certain
  place.  Simply finding the treasures is not good enough.  There is more
  than one way to get a treasure to the special place.  It is also
  important that the objects get to the special place *unharmed* and
  *untarnished*.  You can tell if you have successfully transported the
  object by looking at your score, as it changes immediately.  Note that
  an object can become harmed even after you have received points for it.
  If this happens, your score will decrease, and in many cases you can never
  get credit for it again.

- You can save your game with the 'save' command, and use restore it
  with the 'restore' command.

- There are no limits on lengths of object names.

- Directions are: north,south,east,west,northeast,southeast,northwest,
                  southwest,up,down,in,out.

- These can be abbreviated: n,s,e,w,ne,se,nw,sw,u,d,in,out.

- If you go down a hole in the floor without an aid such as a ladder,
  you probably won't be able to get back up the way you came, if at all.

- To run this game in batch mode (no emacs window), use:
     emacs -batch -l erbpnet
NOTE: This game *should* be run in batch mode!

If you have questions or comments, please contact ronnie@driver-aces.com
My home page is http://www.driver-aces.com/ronnie.html
"))

(defun erbp-flush (args)
  (if (not (= erbp-current-room bathroom))
      (erbp-mprincl "I see nothing to flush.")
    (erbp-mprincl "Whoooosh!!")
    (erbp-put-objs-in-treas (nth urinal erbp-room-objects))
    (erbp-replace erbp-room-objects urinal nil)))

(defun erbp-piss (args)
  (if (not (= erbp-current-room bathroom))
      (erbp-mprincl "You can't do that here, don't even bother trying.")
    (if (not erbp-gottago)
	(erbp-mprincl "I'm afraid you don't have to go now.")
      (erbp-mprincl "That was refreshing.")
      (setq erbp-gottago nil)
      (erbp-replace erbp-room-objects urinal (append 
					    (nth urinal erbp-room-objects)
					    (list obj-URINE))))))


(defun erbp-sleep (args)
  (if (not (= erbp-current-room bedroom))
      (erbp-mprincl
"You try to go to sleep while standing up here, but can't seem to do it.")
    (setq erbp-gottago t)
    (erbp-mprincl
"As soon as you start to doze off you begin dreaming.  You see images of
workers digging caves, slaving in the humid heat.  Then you see yourself
as one of these workers.  While no one is looking, you leave the group
and walk into a room.  The room is bare except for a horseshoe
shaped piece of stone in the center.  You see yourself digging a hole in
the ground, then putting some kind of treasure in it, and filling the hole
with dirt again.  After this, you immediately wake up.")))

(defun erbp-break (obj)
  (let (objnum)
    (if (not (member obj-axe erbp-inventory))
	(erbp-mprincl "You have nothing you can use to break things.")
      (when (setq objnum (erbp-objnum-from-args-std obj))
	(if (member objnum erbp-inventory)
	    (progn
	      (erbp-mprincl
"You take the object in your hands and swing the axe.  Unfortunately, you miss
the object and slice off your hand.  You bleed to death.")
	      (erbp-die "an axe"))
	  (if (not (or (member objnum (nth erbp-current-room erbp-room-objects))
		       (member objnum 
			       (nth erbp-current-room erbp-room-silents))))
	      (erbp-mprincl "I don't see that here.")
	    (if (= objnum obj-cable)
		(progn
		  (erbp-mprincl 
"As you break the ethernet cable, everything starts to blur.  You collapse
for a moment, then straighten yourself up.
")
		  (erbp-replace erbp-room-objects gamma-computing-center
			   (append 
			    (nth gamma-computing-center erbp-room-objects)
			    erbp-inventory))
		  (if (member obj-key erbp-inventory)
		      (progn
			(setq erbp-inventory (list obj-key))
			(erbp-remove-obj-from-room 
			 gamma-computing-center obj-key))
		    (setq erbp-inventory nil))
		  (setq erbp-current-room computer-room)
		  (setq erbp-ethernet nil)
		  (erbp-mprincl "Connection closed.")
		  (erbp-unix-interface))
	      (if (< objnum 0)
		  (progn
		    (erbp-mprincl "Your axe shatters into a million pieces.")
		    (erbp-remove-obj-from-inven obj-axe))
		(erbp-mprincl "Your axe breaks it into a million pieces.")
		(erbp-remove-obj-from-room erbp-current-room objnum)))))))))

(defun erbp-drive (args)
  (if (not erbp-inbus)
      (erbp-mprincl "You cannot drive when you aren't in a vehicle.")
    (erbp-mprincl "To drive while you are in the bus, just give a direction.")))

(defun erbp-superb (args)
  (setq erbp-mode 'erbp-superb))

(defun erbp-reg-score ()
  (let (total)
    (setq total 0)
    (dolist (x (nth treasure-room erbp-room-objects))
      (setq total (+ total (nth x erbp-object-pts))))
    (if (member obj-URINE (nth treasure-room erbp-room-objects))
	(setq total 0)) total))

(defun erbp-endgame-score ()
  (let (total)
    (setq total 0)
    (dolist (x (nth endgame-treasure-room erbp-room-objects))
      (setq total (+ total (nth x erbp-object-pts)))) total))

(defun erbp-answer (args)
  (if (not erbp-correct-answer)
      (erbp-mprincl "I don't believe anyone asked you anything.")
    (setq args (car args))
    (if (not args)
	(erbp-mprincl "You must give the answer on the same line.")
      (if (erbp-members args erbp-correct-answer)
	  (progn
	    (erbp-mprincl "Correct.")
	    (if (= erbp-lastdir 0)
		(setq erbp-current-room (1+ erbp-current-room))
	      (setq erbp-current-room (- erbp-current-room 1)))
	    (setq erbp-correct-answer nil))
	(erbp-mprincl "That answer is incorrect.")))))

(defun erbp-endgame-question ()
(if (not erbp-endgame-questions)
    (progn
      (erbp-mprincl "Your question is:")
      (erbp-mprincl "No more questions, just do 'answer foo'.")
      (setq erbp-correct-answer '("foo")))
  (let (which i newques)
    (setq i 0)
    (setq newques nil)
    (setq which (random (length erbp-endgame-questions)))
    (erbp-mprincl "Your question is:")
    (erbp-mprincl (setq erbp-endgame-question (car 
					     (nth which 
						  erbp-endgame-questions))))
    (setq erbp-correct-answer (cdr (nth which erbp-endgame-questions)))
    (while (< i which)
      (setq newques (append newques (list (nth i erbp-endgame-questions))))
      (setq i (1+ i)))
    (setq i (1+ which))
    (while (< i (length erbp-endgame-questions))
      (setq newques (append newques (list (nth i erbp-endgame-questions))))
      (setq i (1+ i)))
    (setq erbp-endgame-questions newques))))

(defun erbp-power (args)
  (if (not (= erbp-current-room pc-area))
      (erbp-mprincl "That operation is not applicable here.")
    (if (not erbp-floppy)
	(erbp-dos-no-disk)
      (erbp-dos-interface))))

(defun erbp-feed (args)
  (let (objnum)
    (when (setq objnum (erbp-objnum-from-args-std args))
      (if (and (= objnum obj-bear) 
	       (member obj-bear (nth erbp-current-room erbp-room-objects)))
	  (progn
	    (if (not (member obj-food erbp-inventory))
		(erbp-mprincl "You have nothing with which to feed it.")
	      (erbp-drop '("food"))))
	(if (not (or (member objnum (nth erbp-current-room erbp-room-objects))
		     (member objnum erbp-inventory)
		     (member objnum (nth erbp-current-room erbp-room-silents))))
	    (erbp-mprincl "I don't see that here.")
	  (erbp-mprincl "You cannot feed that."))))))


;;;;
;;;;  This section defines various utility functions used
;;;;  by erbpnet.
;;;;


;;; Function which takes a verb and a list of other words.  Calls proper
;;; function associated with the verb, and passes along the other words.

(defun erbp-doverb (erbp-ignore erbp-verblist verb rest)
  (if (not verb)
      nil
    (if (member (intern verb) erbp-ignore)
	(if (not (car rest)) -1
	  (erbp-doverb erbp-ignore erbp-verblist (car rest) (cdr rest)))
      (if (not (cdr (assq (intern verb) erbp-verblist))) -1
	(setq erbp-numcmds (1+ erbp-numcmds))
	(eval (list (cdr (assq (intern verb) erbp-verblist)) (quote rest)))))))


;;; Function to take a string and change it into a list of lowercase words.

(defun erbp-listify-string (strin)
  (let (pos ret-list end-pos)
    (setq pos 0)
    (setq ret-list nil)
    (while (setq end-pos (string-match "[ ,:;]" (substring strin pos)))
      (setq end-pos (+ end-pos pos))
      (if (not (= end-pos pos))
	  (setq ret-list (append ret-list (list 
					   (downcase
					    (substring strin pos end-pos))))))
      (setq pos (+ end-pos 1))) ret-list))

(defun erbp-listify-string2 (strin)
  (let (pos ret-list end-pos)
    (setq pos 0)
    (setq ret-list nil)
    (while (setq end-pos (string-match " " (substring strin pos)))
      (setq end-pos (+ end-pos pos))
      (if (not (= end-pos pos))
	  (setq ret-list (append ret-list (list 
					   (downcase
					    (substring strin pos end-pos))))))
      (setq pos (+ end-pos 1))) ret-list))

(defun erbp-replace (list n number)
  (rplaca (nthcdr n list) number))


;;; Get the first non-ignored word from a list.

(defun erbp-firstword (list)
  (if (not (car list))
      nil
    (while (and list (member (intern (car list)) erbp-ignore))
      (setq list (cdr list)))
    (car list)))

(defun erbp-firstwordl (list)
  (if (not (car list))
      nil
    (while (and list (member (intern (car list)) erbp-ignore))
      (setq list (cdr list)))
    list))

;;; parse a line passed in as a string  Call the proper verb with the
;;; rest of the line passed in as a list.

(defun erbp-vparse (erbp-ignore erbp-verblist line)
  (erbp-mprinc "\n")
  (setq line-list (erbp-listify-string (concat line " ")))
  (erbp-doverb erbp-ignore erbp-verblist (car line-list) (cdr line-list)))

(defun erbp-parse2 (erbp-ignore erbp-verblist line)
  (erbp-mprinc "\n")
  (setq line-list (erbp-listify-string2 (concat line " ")))
  (erbp-doverb erbp-ignore erbp-verblist (car line-list) (cdr line-list)))

;;; Read a line, in window mode

(defun erbp-read-line ()
  (let (line)
    (setq line (read-string ""))
    (erbp-mprinc line) line))

;;; Insert something into the window buffer

(defun erbp-minsert (string)
  (if (stringp string)
      (insert string)
    (insert (prin1-to-string string))))

;;; Print something out, in window mode

(defun erbp-mprinc (string)
  (if (stringp string)
      (insert string)
    (insert (prin1-to-string string))))

;;; In window mode, keep screen from jumping by keeping last line at
;;; the bottom of the screen.

(defun erbp-fix-screen ()
  (interactive)
  (forward-line (- 0 (- (window-height) 2 )))
  (set-window-start (selected-window) (point))
  (end-of-buffer))

;;; Insert something into the buffer, followed by newline.

(defun erbp-minsertl (string)
  (erbp-minsert string)
  (erbp-minsert "\n"))

;;; Print something, followed by a newline.

(defun erbp-mprincl (string)
  (erbp-mprinc string)
  (erbp-mprinc "\n"))

;;; Function which will get an object number given the list of
;;; words in the command, except for the verb.

(defun erbp-objnum-from-args (obj)
  (let (objnum)
    (setq obj (erbp-firstword obj))
    (if (not obj)
	obj-special
      (setq objnum (cdr (assq (intern obj) erbp-objnames))))))

(defun erbp-objnum-from-args-std (obj)
  (let (result)
  (if (eq (setq result (erbp-objnum-from-args obj)) obj-special)
      (erbp-mprincl "You must supply an object."))
  (if (eq result nil)
      (erbp-mprincl "I don't know what that is."))
  (if (eq result obj-special)
      nil
    result)))

;;; Take a short room description, and change spaces and slashes to dashes.

(defun erbp-space-to-hyphen (string)
  (let (space)
    (if (setq space (string-match "[ /]" string))
	(progn
	  (setq string (concat (substring string 0 space) "-"
			       (substring string (1+ space))))
	  (erbp-space-to-hyphen string))
      string)))

;;; Given a unix style pathname, build a list of path components (recursive)

(defun erbp-get-path (dirstring startlist)
  (let (slash pos)
    (if (= (length dirstring) 0)
	startlist
      (if (string= (substring dirstring 0 1) "/")
	  (erbp-get-path (substring dirstring 1) (append startlist (list "/")))
	(if (not (setq slash (string-match "/" dirstring)))
	    (append startlist (list dirstring))
	  (erbp-get-path (substring dirstring (1+ slash))
		    (append startlist
			    (list (substring dirstring 0 slash)))))))))


;;; Is a string a member of a string list?

(defun erbp-members (string string-list)
  (let (found)
    (setq found nil)
    (dolist (x string-list)
      (if (string= x string)
	  (setq found t))) found))

;;; Function to put objects in the treasure room.  Also prints current
;;; score to let user know he has scored.

(defun erbp-put-objs-in-treas (objlist)
  (let (oscore newscore)
    (setq oscore (erbp-reg-score))
    (erbp-replace erbp-room-objects 0 (append (nth 0 erbp-room-objects) objlist))
    (setq newscore (erbp-reg-score))
    (if (not (= oscore newscore))
	(erbp-score nil))))

;;; Load an encrypted file, and eval it.

(defun erbp-load-d (filename)
  (let (old-buffer result)
    (setq result t)
    (setq old-buffer (current-buffer))
    (switch-to-buffer (get-buffer-create "*loadc*"))
    (erase-buffer)
    (condition-case nil
	(insert-file-contents filename)
      (error (setq result nil)))
    (unless (not result)
      (condition-case nil
	  (erbp-rot13)
	(error (yank)))
      (eval-current-buffer)
      (kill-buffer (current-buffer)))
      (switch-to-buffer old-buffer)
    result))

;;; Functions to remove an object either from a room, or from inventory.

(defun erbp-remove-obj-from-room (erbp-room objnum)
  (let (newroom)
    (setq newroom nil)
    (dolist (x (nth erbp-room erbp-room-objects))
      (if (not (= x objnum))
	  (setq newroom (append newroom (list x)))))
    (rplaca (nthcdr erbp-room erbp-room-objects) newroom)))

(defun erbp-remove-obj-from-inven (objnum)
  (let (new-inven)
    (setq new-inven nil)
    (dolist (x erbp-inventory)
      (if (not (= x objnum))
	  (setq new-inven (append new-inven (list x)))))
    (setq erbp-inventory new-inven)))


(let ((i 0) (lower "abcdefghijklmnopqrstuvwxyz") upper)
  (setq erbp-translate-table (make-vector 256 0))
  (while (< i 256)
    (aset erbp-translate-table i i)
    (setq i (1+ i)))
  (setq lower (concat lower lower))
  (setq upper (upcase lower))
  (setq i 0)
  (while (< i 26)
    (aset erbp-translate-table (+ ?a i) (aref lower (+ i 13)))
    (aset erbp-translate-table (+ ?A i) (aref upper (+ i 13)))
      (setq i (1+ i))))
  
(defun erbp-rot13 ()
  (let (str len (i 0))
    (setq str (buffer-substring (point-min) (point-max)))
    (setq len (length str))
    (while (< i len)
      (aset str i (aref erbp-translate-table (aref str i)))
      (setq i (1+ i)))
    (erase-buffer)
    (insert str)))

;;;;
;;;; This section defines the globals that are used in erbpnet.
;;;;
;;;; IMPORTANT
;;;; All globals which can change must be saved from 'save-game.  Add
;;;; all new globals to bottom of file.

(setq erbp-visited '(27))
(setq erbp-current-room 1)
(setq erbp-exitf nil)
(setq erbp-badcd nil)
(defvar erbpeon-mode-map nil)
(setq erbpeon-mode-map (make-sparse-keymap))
(define-key erbpeon-mode-map "\r" 'erbp-parse)
(defvar erbpeon-batch-map (make-keymap))
(if (string= (substring emacs-version 0 2) "18")
    (let (n)
      (setq n 32)
      (while (< 0 (setq n (- n 1)))
	(aset erbpeon-batch-map n 'erbpeon-nil)))
  (let (n)
    (setq n 32)
    (while (< 0 (setq n (- n 1)))
      (aset (car (cdr erbpeon-batch-map)) n 'erbpeon-nil))))
(define-key erbpeon-batch-map "\r" 'exit-minibuffer)
(define-key erbpeon-batch-map "\n" 'exit-minibuffer)
(setq erbp-computer nil)
(setq erbp-floppy nil)
(setq erbp-key-level 0)
(setq erbp-hole nil)
(setq erbp-correct-answer nil)
(setq erbp-lastdir 0)
(setq erbp-numsaves 0)
(setq erbp-jar nil)
(setq erbp-dead nil)
(setq room 0)
(setq erbp-numcmds 0)
(setq erbp-wizard nil)
(setq erbp-endgame-question nil)
(setq erbp-logged-in nil)
(setq erbpeon-mode 'erbpeon)
(setq erbp-unix-verbs '((ls . erbp-ls) (ftp . erbp-ftp) (echo . erbp-echo) 
		       (exit . erbp-uexit) (cd . erbp-cd) (pwd . erbp-pwd)
		       (rlogin . erbp-rlogin) (uncompress . erbp-uncompress)
		       (cat . erbp-cat) (zippy . erbp-zippy)))

(setq erbp-dos-verbs '((dir . erbp-dos-dir) (type . erbp-dos-type)
		      (exit . erbp-dos-exit) (command . erbp-dos-spawn)
		      (b: . erbp-dos-invd) (c: . erbp-dos-invd)
		      (a: . erbp-dos-nil)))


(setq erbp-batch-mode nil)

(setq erbp-cdpath "/usr/toukmond")
(setq erbp-cdroom -10)
(setq erbp-uncompressed nil)
(setq erbp-ethernet t)
(setq erbp-restricted 
      '(erbp-room-objects erbpeon-map erbp-rooms 
			 erbp-room-silents erbp-combination))
(setq erbp-ftptype 'ascii)
(setq erbp-endgame nil)
(setq erbp-gottago t)
(setq erbp-black nil)

(setq erbp-rooms '(
	      (
"You are in the treasure room.  A door leads out to the north."
               "Treasure room"
	       )
	      (
"You are at a dead end of a dirt road.  The road goes to the east.
In the distance you can see that it will eventually fork off.  The
trees here are very tall royal palms, and they are spaced equidistant
from each other."
	       "Dead end"
	       )
	      (
"You are on the continuation of a dirt road.  There are more trees on
both sides of you.  The road continues to the east and west."
               "E/W Dirt road"
	       )
	      (
"You are at a fork of two passages, one to the northeast, and one to the
southeast.  The ground here seems very soft. You can also go back west."
               "Fork"
	       )
	      (
"You are on a northeast/southwest road."
               "NE/SW road"
	       )
	      (
"You are at the end of the road.  There is a building in front of you
to the northeast, and the road leads back to the southwest."
               "Building front"
	       )
	      (
"You are on a southeast/northwest road."
               "SE/NW road"
	       )
	      (
"You are standing at the end of a road.  A passage leads back to the
northwest."
               "Bear hangout"
	       )
	      (
"You are in the hallway of an old building.  There are rooms to the east
and west, and doors leading out to the north and south."
               "Old Building hallway"
	       )
	      (
"You are in a mailroom.  There are many bins where the mail is usually
kept.  The exit is to the west."
               "Mailroom"
	       )
	      (
"You are in a computer room.  It seems like most of the equipment has
been removed.  There is a VAX 11/780 in front of you, however, with
one of the cabinets wide open.  A sign on the front of the machine
says: This VAX is named 'pokey'.  To type on the console, use the
'type' command.  The exit is to the east."
               "Computer room"
	       )
	      (
"You are in a meadow in the back of an old building.  A small path leads
to the west, and a door leads to the south."
               "Meadow"
	       )
	      (
"You are in a round, stone room with a door to the east.  There
is a sign on the wall that reads: 'receiving room'."
               "Receiving room"
	       )
	      (
"You are at the south end of a hallway that leads to the north.  There
are rooms to the east and west."
               "Northbound Hallway"
	       )
	      (
"You are in a sauna.  There is nothing in the room except for a dial
on the wall.  A door leads out to west."
               "Sauna"
               )
	      (
"You are at the end of a north/south hallway.  You can go back to the south,
or off to a room to the east."
               "End of N/S Hallway"
	       )
	      (
"You are in an old weight room.  All of the equipment is either destroyed
or completely broken.  There is a door out to the west, and there is a ladder
leading down a hole in the floor."
               "Weight room"                 ;16
	       )
	      (
"You are in a maze of twisty little passages, all alike.
There is a button on the ground here."
               "Maze button room"
	       )
	      (
"You are in a maze of little twisty passages, all alike."
               "Maze"
	       )
	      (
"You are in a maze of thirsty little passages, all alike."
               "Maze"    ;19
	       )
	      (
"You are in a maze of twenty little passages, all alike."
               "Maze"
	       )
	      (
"You are in a daze of twisty little passages, all alike."
               "Maze"   ;21
	       )
	      (
"You are in a maze of twisty little cabbages, all alike."
               "Maze"   ;22
	       )
	      (
"You are in a reception area for a health and fitness center.  The place
appears to have been recently ransacked, and nothing is left.  There is
a door out to the south, and a crawlspace to the southeast."
               "Reception area"
	       )
	      (
"You are outside a large building to the north which used to be a health
and fitness center.  A road leads to the south."
               "Health Club front"
	       )
	      (
"You are at the north side of a lake.  On the other side you can see
a road which leads to a cave.  The water appears very deep."
               "Lakefront North"
	       )
	      (
"You are at the south side of a lake.  A road goes to the south."
               "Lakefront South"
	       )
	      (
"You are in a well-hidden area off to the side of a road.  Back to the
northeast through the brush you can see the bear hangout."
               "Hidden area"
	       )
	      (
"The entrance to a cave is to the south.  To the north, a road leads
towards a deep lake.  On the ground nearby there is a chute, with a sign
that says 'put treasures here for points'."
               "Cave Entrance"                      ;28
	       )
	      (
"You are in a misty, humid room carved into a mountain.
To the north is the remains of a rockslide.  To the east, a small
passage leads away into the darkness."              ;29
               "Misty Room"
	       )
	      (
"You are in an east/west passageway.  The walls here are made of
multicolored rock and are quite beautiful."
               "Cave E/W passage"                   ;30
	       )
	      (
"You are at the junction of two passages. One goes north/south, and
the other goes west."
               "N/S/W Junction"                     ;31
	       )
	      (
"You are at the north end of a north/south passageway.  There are stairs
leading down from here.  There is also a door leading west."
               "North end of cave passage"         ;32
	       )
	      (
"You are at the south end of a north/south passageway.  There is a hole
in the floor here, into which you could probably fit."
               "South end of cave passage"         ;33
	       )
	      (
"You are in what appears to be a worker's bedroom.  There is a queen-
sized bed in the middle of the room, and a painting hanging on the
wall.  A door leads to another room to the south, and stairways
lead up and down."
               "Bedroom"                          ;34
	       )
	      (
"You are in a bathroom built for workers in the cave.  There is a
urinal hanging on the wall, and some exposed pipes on the opposite
wall where a sink used to be.  To the north is a bedroom."
               "Bathroom"        ;35
	       )
	      (
"This is a marker for the urinal.  User will not see this, but it
is a room that can contain objects."
               "Urinal"          ;36
	       )
	      (
"You are at the northeast end of a northeast/southwest passageway.
Stairs lead up out of sight."
               "NE end of NE/SW cave passage"       ;37
	       )
	      (
"You are at the junction of northeast/southwest and east/west passages."
               "NE/SW-E/W junction"                      ;38
	       )
	      (
"You are at the southwest end of a northeast/southwest passageway."
               "SW end of NE/SW cave passage"        ;39
	       )
	      (
"You are at the east end of an E/W passage.  There are stairs leading up
to a room above."
               "East end of E/W cave passage"    ;40
	       )
	      (
"You are at the west end of an E/W passage.  There is a hole on the ground
which leads down out of sight."
               "West end of E/W cave passage"    ;41
	       )
	      (
"You are in a room which is bare, except for a horseshoe shaped boulder
in the center.  Stairs lead down from here."     ;42
               "Horseshoe boulder room"
	       )
	      (
"You are in a room which is completely empty.  Doors lead out to the north
and east."
               "Empty room"                      ;43
	       )
	      (
"You are in an empty room.  Interestingly enough, the stones in this
room are painted blue.  Doors lead out to the east and south."  ;44
               "Blue room"
	       )
	      (
"You are in an empty room.  Interestingly enough, the stones in this
room are painted yellow.  Doors lead out to the south and west."    ;45
               "Yellow room"
	       )
	      (
"You are in an empty room.  Interestingly enough, the stones in this room
are painted red.  Doors lead out to the west and north."
               "Red room"                                 ;46
	       )
	      (
"You are in the middle of a long north/south hallway."     ;47
               "Long n/s hallway"
	       )
	      (
"You are 3/4 of the way towards the north end of a long north/south hallway."
               "3/4 north"                                ;48
	       )
	      (
"You are at the north end of a long north/south hallway.  There are stairs
leading upwards."
               "North end of long hallway"                 ;49
	       )
	      (
"You are 3/4 of the way towards the south end of a long north/south hallway."
               "3/4 south"                                 ;50
	       )
	      (
"You are at the south end of a long north/south hallway.  There is a hole
to the south."
               "South end of long hallway"                 ;51
	       )
	      (
"You are at a landing in a stairwell which continues up and down."
               "Stair landing"                             ;52
	       )
	      (
"You are at the continuation of an up/down staircase."
               "Up/down staircase"                         ;53
	       )
	      (
"You are at the top of a staircase leading down.  A crawlway leads off
to the northeast."
               "Top of staircase."                        ;54
	       )
	      (
"You are in a crawlway that leads northeast or southwest."
               "NE crawlway"                              ;55
	       )
	      (
"You are in a small crawlspace.  There is a hole in the ground here, and
a small passage back to the southwest."
               "Small crawlspace"                         ;56
	       )
	      (
"You are in the Gamma Computing Center.  An IBM 3090/600s is whirring
away in here.  There is an ethernet cable coming out of one of the units,
and going through the ceiling.  There is no console here on which you
could type."
               "Gamma computing center"                   ;57
	       )
	      (
"You are near the remains of a post office.  There is a mail drop on the
face of the building, but you cannot see where it leads.  A path leads
back to the east, and a road leads to the north."
               "Post office"                             ;58
	       )
	      (
"You are at the intersection of Main Street and Maple Ave.  Main street
runs north and south, and Maple Ave runs east off into the distance.
If you look north and east you can see many intersections, but all of
the buildings that used to stand here are gone.  Nothing remains except
street signs.
There is a road to the northwest leading to a gate that guards a building."
               "Main-Maple intersection"                       ;59
	       )
	      (
"You are at the intersection of Main Street and the west end of Oaktree Ave."
               "Main-Oaktree intersection"   ;60
	       )
	      (
"You are at the intersection of Main Street and the west end of Vermont Ave."
               "Main-Vermont intersection"  ;61
	       )
	      (
"You are at the north end of Main Street at the west end of Sycamore Ave." ;62
               "Main-Sycamore intersection"
	       )
	      (
"You are at the south end of First Street at Maple Ave." ;63
               "First-Maple intersection"
	       )
	      (
"You are at the intersection of First Street and Oaktree Ave."  ;64
               "First-Oaktree intersection"
	       )
	      (
"You are at the intersection of First Street and Vermont Ave."  ;65
               "First-Vermont intersection"
	       )
	      (
"You are at the north end of First Street at Sycamore Ave."  ;66
               "First-Sycamore intersection"
	       )
	      (
"You are at the south end of Second Street at Maple Ave."  ;67
               "Second-Maple intersection"
	       )
	      (
"You are at the intersection of Second Street and Oaktree Ave."  ;68
               "Second-Oaktree intersection"
	       )
	      (
"You are at the intersection of Second Street and Vermont Ave."  ;69
               "Second-Vermont intersection"
	       )
	      (
"You are at the north end of Second Street at Sycamore Ave."  ;70
               "Second-Sycamore intersection"
	       )
	      (
"You are at the south end of Third Street at Maple Ave."  ;71
               "Third-Maple intersection"
	       )
	      (
"You are at the intersection of Third Street and Oaktree Ave."  ;72
               "Third-Oaktree intersection"
	       )
	      (
"You are at the intersection of Third Street and Vermont Ave."  ;73
               "Third-Vermont intersection"
	       )
	      (
"You are at the north end of Third Street at Sycamore Ave."  ;74
               "Third-Sycamore intersection"
	       )
	      (
"You are at the south end of Fourth Street at Maple Ave."  ;75
               "Fourth-Maple intersection"
	       )
	      (
"You are at the intersection of Fourth Street and Oaktree Ave."  ;76
               "Fourth-Oaktree intersection"
	       )
	      (
"You are at the intersection of Fourth Street and Vermont Ave."  ;77
               "Fourth-Vermont intersection"
	       )
	      (
"You are at the north end of Fourth Street at Sycamore Ave."  ;78
               "Fourth-Sycamore intersection"
	       )
	      (
"You are at the south end of Fifth Street at the east end of Maple Ave."  ;79
               "Fifth-Maple intersection"
	       )
	      (
"You are at the intersection of Fifth Street and the east end of Oaktree Ave.
There is a cliff off to the east."
               "Fifth-Oaktree intersection"  ;80
	       )
	      (
"You are at the intersection of Fifth Street and the east end of Vermont Ave."
               "Fifth-Vermont intersection"  ;81
	       )
	      (
"You are at the north end of Fifth Street and the east end of Sycamore Ave."
               "Fifth-Sycamore intersection"  ;82
	       )
	      (
"You are in front of the Museum of Natural History.  A door leads into
the building to the north, and a road leads to the southeast."
               "Museum entrance"                  ;83
	       )
	      (
"You are in the main lobby for the Museum of Natural History.  In the center
of the room is the huge skeleton of a dinosaur.  Doors lead out to the
south and east." 
               "Museum lobby"                     ;84
	       )
	      (
"You are in the geological display.  All of the objects that used to
be on display are missing.  There are rooms to the east, west, and 
north."
               "Geological display"               ;85
	       )
	      (
"You are in the marine life area.  The room is filled with fish tanks,
which are filled with dead fish that have apparently died due to
starvation.  Doors lead out to the south and east."
               "Marine life area"                   ;86
	       )
	      (
"You are in some sort of maintenance room for the museum.  There is a
switch on the wall labeled 'BL'.  There are doors to the west and north."
               "Maintenance room"                   ;87
	       )
	      (
"You are in a classroom where school children were taught about natural
history.  On the blackboard is written, 'No children allowed downstairs.'
There is a door to the east with an 'exit' sign on it.  There is another
door to the west."
               "Classroom"                          ;88
	       )
	      (
"You are at the Vermont St. subway station.  A train is sitting here waiting."
               "Vermont station"                    ;89
	       )
	      (
"You are at the Museum subway stop.  A passage leads off to the north."
               "Museum station"                     ;90
	       )
	      (
"You are in a north/south tunnel."
               "N/S tunnel"                          ;91
	       )
	      (
"You are at the north end of a north/south tunnel.  Stairs lead up and
down from here.  There is a garbage disposal here."
               "North end of N/S tunnel"             ;92
               )
	      (
"You are at the top of some stairs near the subway station.  There is
a door to the west."
               "Top of subway stairs"           ;93
	       )
	      (
"You are at the bottom of some stairs near the subway station.  There is
a room to the northeast."
               "Bottom of subway stairs"       ;94
	       )
	      (
"You are in another computer room.  There is a computer in here larger
than you have ever seen.  It has no manufacturers name on it, but it
does have a sign that says: This machine's name is 'endgame'.  The
exit is to the southwest.  There is no console here on which you could
type."
               "Endgame computer room"         ;95
	       )
	      (
"You are in a north/south hallway."
               "Endgame N/S hallway"           ;96
	       )
	      (
"You have reached a question room.  You must answer a question correctly in
order to get by.  Use the 'answer' command to answer the question."
               "Question room 1"              ;97
	       )
	      (
"You are in a north/south hallway."
               "Endgame N/S hallway"           ;98
	       )
	      (
"You are in a second question room."
               "Question room 2"               ;99
	       )
	      (
"You are in a north/south hallway."
               "Endgame N/S hallway"           ;100
	       )
	      (
"You are in a third question room."
               "Question room 3"               ;101
	       )
	      (
"You are in the endgame treasure room.  A door leads out to the north, and
a hallway leads to the south."
               "Endgame treasure room"         ;102
	       )
	      (
"You are in the winner's room.  A door leads back to the south."
               "Winner's room"                 ;103
	       )
	      (
"You have reached a dead end.  There is a PC on the floor here.  Above
it is a sign that reads:
          Type the 'reset' command to type on the PC. 
A hole leads north."
               "PC area"                       ;104
               )            
))

(setq erbp-light-rooms '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 24 25 26 27 28 58 59
		     60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76
		     77 78 79 80 81 82 83))

(setq erbp-verblist '((die . erbp-die) (ne . erbp-ne) (north . erbp-n) 
		     (south . erbp-s) (east . erbp-e) (west . erbp-w)
		     (u . erbp-up) (d . erbp-down) (i . erbp-inven)
		     (inventory . erbp-inven) (look . erbp-examine) (n . erbp-n)
		     (s . erbp-s) (e . erbp-e) (w . erbp-w) (se . erbp-se)
		     (nw . erbp-nw) (sw . erbp-sw) (up . erbp-up) 
		     (down . erbp-down) (in . erbp-in) (out . erbp-out)
		     (go . erbp-go) (drop . erbp-drop) (southeast . erbp-se)
		     (southwest . erbp-sw) (northeast . erbp-ne)
		     (northwest . erbp-nw) (save . erbp-save-game)
		     (restore . erbp-restore) (long . erbp-long) (dig . erbp-dig)
		     (shake . erbp-shake) (wave . erbp-shake)
		     (examine . erbp-examine) (describe . erbp-examine) 
		     (climb . erbp-climb) (eat . erbp-eat) (put . erbp-put)
		     (type . erbp-type)  (insert . erbp-put)
		     (score . erbp-score) (help . erbp-help) (quit . erbp-quit) 
		     (read . erbp-examine) (verbose . erbp-long) 
		     (urinate . erbp-piss) (piss . erbp-piss)
		     (flush . erbp-flush) (sleep . erbp-sleep) (lie . erbp-sleep) 
		     (x . erbp-examine) (break . erbp-break) (drive . erbp-drive)
		     (board . erbp-in) (enter . erbp-in) (turn . erbp-turn)
		     (press . erbp-press) (push . erbp-press) (swim . erbp-swim)
		     (on . erbp-in) (off . erbp-out) (chop . erbp-break)
		     (switch . erbp-press) (cut . erbp-break) (exit . erbp-out)
		     (leave . erbp-out) (reset . erbp-power) (flick . erbp-press)
		     (superb . erbp-superb) (answer . erbp-answer)
		     (throw . erbp-drop) (l . erbp-examine) (take . erbp-take)
		     (get . erbp-take) (feed . erbp-feed)))

(setq erbp-inbus nil)
(setq erbp-nomail nil)
(setq erbp-ignore '(the to at))
(setq erbp-mode 'moby)
(setq erbp-sauna-level 0)

(defconst north 0)
(defconst south 1)
(defconst east 2)
(defconst west 3)
(defconst northeast 4)
(defconst southeast 5)
(defconst northwest 6)
(defconst southwest 7)
(defconst up 8)
(defconst down 9)
(defconst in 10)
(defconst out 11)

(setq erbpeon-map '(
;		      no  so  ea  we  ne  se  nw  sw  up  do  in  ot
		    ( 96  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;0
		    ( -1  -1   2  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;1
		    ( -1  -1   3   1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;2
		    ( -1  -1  -1   2   4   6  -1  -1  -1  -1  -1  -1 ) ;3
		    ( -1  -1  -1  -1   5  -1  -1   3  -1  -1  -1  -1 ) ;4
		    ( -1  -1  -1  -1  255 -1  -1   4  -1  -1  255 -1 ) ;5
		    ( -1  -1  -1  -1  -1   7   3  -1  -1  -1  -1  -1 ) ;6
		    ( -1  -1  -1  -1  -1  255  6  27  -1  -1  -1  -1 ) ;7
		    ( 255  5   9  10  -1  -1  -1   5  -1  -1  -1   5 ) ;8
		    ( -1  -1  -1   8  -1  -1  -1  -1  -1  -1  -1  -1 ) ;9
		    ( -1  -1   8  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;10
		    ( -1   8  -1  58  -1  -1  -1  -1  -1  -1  -1  -1 ) ;11
		    ( -1  -1  13  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;12
		    ( 15  -1  14  12  -1  -1  -1  -1  -1  -1  -1  -1 ) ;13
		    ( -1  -1  -1  13  -1  -1  -1  -1  -1  -1  -1  -1 ) ;14
		    ( -1  13  16  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;15
		    ( -1  -1  -1  15  -1  -1  -1  -1  -1  17  16  -1 ) ;16
		    ( -1  -1  17  17  17  17 255  17 255  17  -1  -1 ) ;17
		    ( 18  18  18  18  18  -1  18  18  19  18  -1  -1 ) ;18
		    ( -1  18  18  19  19  20  19  19  -1  18  -1  -1 ) ;19
		    ( -1  -1  -1  18  -1  -1  -1  -1  -1  21  -1  -1 ) ;20
		    ( -1  -1  -1  -1  -1  20  22  -1  -1  -1  -1  -1 ) ;21
		    ( 18  18  18  18  16  18  23  18  18  18  18  18 ) ;22
		    ( -1 255  -1  -1  -1  19  -1  -1  -1  -1  -1  -1 ) ;23
		    ( 23  25  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;24
		    ( 24 255  -1  -1  -1  -1  -1  -1  -1  -1 255  -1 ) ;25
		    (255  28  -1  -1  -1  -1  -1  -1  -1  -1 255  -1 ) ;26
		    ( -1  -1  -1  -1   7  -1  -1  -1  -1  -1  -1  -1 ) ;27
		    ( 26 255  -1  -1  -1  -1  -1  -1  -1  -1  255 -1 ) ;28
		    ( -1  -1  30  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;29
		    ( -1  -1  31  29  -1  -1  -1  -1  -1  -1  -1  -1 ) ;30
		    ( 32  33  -1  30  -1  -1  -1  -1  -1  -1  -1  -1 ) ;31
		    ( -1  31  -1  255 -1  -1  -1  -1  -1  34  -1  -1 ) ;32
		    ( 31  -1  -1  -1  -1  -1  -1  -1  -1  35  -1  -1 ) ;33
		    ( -1  35  -1  -1  -1  -1  -1  -1  32  37  -1  -1 ) ;34
		    ( 34  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;35
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;36
		    ( -1  -1  -1  -1  -1  -1  -1  38  34  -1  -1  -1 ) ;37
		    ( -1  -1  40  41  37  -1  -1  39  -1  -1  -1  -1 ) ;38
		    ( -1  -1  -1  -1  38  -1  -1  -1  -1  -1  -1  -1 ) ;39
		    ( -1  -1  -1  38  -1  -1  -1  -1  42  -1  -1  -1 ) ;40
		    ( -1  -1  38  -1  -1  -1  -1  -1  -1  43  -1  -1 ) ;41
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  40  -1  -1 ) ;42
		    ( 44  -1  46  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;43
		    ( -1  43  45  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;44
		    ( -1  46  -1  44  -1  -1  -1  -1  -1  -1  -1  -1 ) ;45
		    ( 45  -1  -1  43  -1  -1  -1  -1  -1  255 -1  -1 ) ;46
		    ( 48  50  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;47
		    ( 49  47  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;48
		    ( -1  48  -1  -1  -1  -1  -1  -1  52  -1  -1  -1 ) ;49
		    ( 47  51  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;50
		    ( 50  104 -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;51
		    ( -1  -1  -1  -1  -1  -1  -1  -1  53  49  -1  -1 ) ;52
		    ( -1  -1  -1  -1  -1  -1  -1  -1  54  52  -1  -1 ) ;53
		    ( -1  -1  -1  -1  55  -1  -1  -1  -1  53  -1  -1 ) ;54
		    ( -1  -1  -1  -1  56  -1  -1  54  -1  -1  -1  54 ) ;55
		    ( -1  -1  -1  -1  -1  -1  -1  55  -1  31  -1  -1 ) ;56
		    ( -1  -1  32  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;57
		    ( 59  -1  11  -1  -1  -1  -1  -1  -1  -1  255 255) ;58
		    ( 60  58  63  -1  -1  -1  255 -1  -1  -1  255 255) ;59
		    ( 61  59  64  -1  -1  -1  -1  -1  -1  -1  255 255) ;60
		    ( 62  60  65  -1  -1  -1  -1  -1  -1  -1  255 255) ;61
		    ( -1  61  66  -1  -1  -1  -1  -1  -1  -1  255 255) ;62
		    ( 64  -1  67  59  -1  -1  -1  -1  -1  -1  255 255) ;63
		    ( 65  63  68  60  -1  -1  -1  -1  -1  -1  255 255) ;64
		    ( 66  64  69  61  -1  -1  -1  -1  -1  -1  255 255) ;65
		    ( -1  65  70  62  -1  -1  -1  -1  -1  -1  255 255) ;66
		    ( 68  -1  71  63  -1  -1  -1  -1  -1  -1  255 255) ;67
		    ( 69  67  72  64  -1  -1  -1  -1  -1  -1  255 255) ;68
		    ( 70  68  73  65  -1  -1  -1  -1  -1  -1  255 255) ;69
		    ( -1  69  74  66  -1  -1  -1  -1  -1  -1  255 255) ;70
		    ( 72  -1  75  67  -1  -1  -1  -1  -1  -1  255 255) ;71
		    ( 73  71  76  68  -1  -1  -1  -1  -1  -1  255 255) ;72
		    ( 74  72  77  69  -1  -1  -1  -1  -1  -1  255 255) ;73
		    ( -1  73  78  70  -1  -1  -1  -1  -1  -1  255 255) ;74
		    ( 76  -1  79  71  -1  -1  -1  -1  -1  -1  255 255) ;75
		    ( 77  75  80  72  -1  -1  -1  -1  -1  -1  255 255) ;76
		    ( 78  76  81  73  -1  -1  -1  -1  -1  -1  255 255) ;77
		    ( -1  77  82  74  -1  -1  -1  -1  -1  -1  255 255) ;78
		    ( 80  -1  -1  75  -1  -1  -1  -1  -1  -1  255 255) ;79
		    ( 81  79  255 76  -1  -1  -1  -1  -1  -1  255 255) ;80
		    ( 82  80  -1  77  -1  -1  -1  -1  -1  -1  255 255) ;81
		    ( -1  81  -1  78  -1  -1  -1  -1  -1  -1  255 255) ;82
		    ( 84  -1  -1  -1  -1  59  -1  -1  -1  -1  255 255) ;83
		    ( -1  83  85  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;84
		    ( 86  -1  87  84  -1  -1  -1  -1  -1  -1  -1  -1 ) ;85
		    ( -1  85  88  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;86
		    ( 88  -1  -1  85  -1  -1  -1  -1  -1  -1  -1  -1 ) ;87
		    ( -1  87 255  86  -1  -1  -1  -1  -1  -1  -1  -1 ) ;88
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 255  -1 ) ;89
		    ( 91  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;90
		    ( 92  90  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;91
		    ( -1  91  -1  -1  -1  -1  -1  -1  93  94  -1  -1 ) ;92
		    ( -1  -1  -1  88  -1  -1  -1  -1  -1  92  -1  -1 ) ;93
		    ( -1  -1  -1  -1  95  -1  -1  -1  92  -1  -1  -1 ) ;94
		    ( -1  -1  -1  -1  -1  -1  -1  94  -1  -1  -1  -1 ) ;95
		    ( 97   0  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;96
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;97
		    ( 99  97  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;98
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;99
		    ( 101 99  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;100
		    ( -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;101
		    ( 103 101 -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;102
		    ( -1  102 -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;103
		    ( 51  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1 ) ;104
		    )
;		      no  so  ea  we  ne  se  nw  sw  up  do  in  ot
)


;;; How the user references *all* objects, permanent and regular.
(setq erbp-objnames '(
		 (shovel . 0) 
		 (lamp . 1)
		 (cpu . 2) (board . 2) (card . 2) (chip . 2)
		 (food . 3) 
		 (key . 4) 
		 (paper . 5) (slip . 5)
		 (rms . 6) (statue . 6) (statuette . 6)  (stallman . 6)
		 (diamond . 7)
		 (weight . 8)
		 (life . 9) (preserver . 9)
		 (bracelet . 10) (emerald . 10) 
		 (gold . 11)
		 (platinum . 12)
		 (towel . 13) (beach . 13)
		 (axe . 14)
		 (silver . 15)
		 (license . 16)
		 (coins . 17)
		 (egg . 18)
		 (jar . 19)
		 (bone . 20)
		 (acid . 21) (nitric . 21)
		 (glycerine . 22)
		 (ruby . 23)
		 (amethyst . 24) 
		 (mona . 25)
		 (bill . 26) 
		 (floppy . 27) (disk . 27)
		 
		 (boulder . -1)
		 (tree . -2) (trees . -2) (palm . -2) 
		 (bear . -3)
		 (bin . -4) (bins . -4)
		 (cabinet . -5) (computer . -5) (vax . -5) (ibm . -5) 
		 (protoplasm . -6)
		 (dial . -7) 
		 (button . -8) 
		 (chute . -9) 
		 (painting . -10)
		 (bed . -11)
		 (urinal . -12)
		 (URINE . -13)
		 (pipes . -14) (pipe . -14) 
		 (box . -15) (slit . -15) 
		 (cable . -16) (ethernet . -16) 
		 (mail . -17) (drop . -17)
		 (bus . -18)
		 (gate . -19)
		 (cliff . -20) 
		 (skeleton . -21) (dinosaur . -21)
		 (fish . -22)
		 (tanks . -23) (tank . -23)
		 (switch . -24)
		 (blackboard . -25)
		 (disposal . -26) (garbage . -26)
		 (ladder . -27)
		 (subway . -28) (train . -28) 
		 (pc . -29) (drive . -29) (coconut . -30) (coconuts . -30)
		 (lake . -32) (water . -32)
))

(dolist (x erbp-objnames)
  (let (name)
    (setq name (concat "obj-" (prin1-to-string (car x))))
    (eval (list 'defconst (intern name) (cdr x)))))

(defconst obj-special 255)

;;; The initial setup of what objects are in each room.
;;; Regular objects have whole numbers lower than 255.
;;; Objects that cannot be taken but might move and are
;;; described during room description are negative.
;;; Stuff that is described and might change are 255, and are
;;; handled specially by 'erbp-describe-room. 

(setq erbp-room-objects (list nil 

        (list obj-shovel)                     ;; treasure-room
        (list obj-boulder)                    ;; dead-end
        nil nil nil
        (list obj-food)                       ;; se-nw-road
        (list obj-bear)                       ;; bear-hangout
        nil nil
        (list obj-special)                    ;; computer-room
        (list obj-lamp obj-license obj-silver);; meadow
        nil nil
        (list obj-special)                    ;; sauna
        nil 
        (list obj-weight obj-life)            ;; weight-room
        nil nil
        (list obj-rms obj-floppy)             ;; thirsty-maze
        nil nil nil nil nil nil nil 
        (list obj-emerald)                    ;; hidden-area
        nil
        (list obj-gold)                       ;; misty-room
        nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
        (list obj-towel obj-special)          ;; red-room
        nil nil nil nil nil
        (list obj-box)                        ;; stair-landing
        nil nil nil
        (list obj-axe)                        ;; smal-crawlspace
        nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
        nil nil nil nil nil
        (list obj-special)                    ;; fourth-vermont-intersection
        nil nil
        (list obj-coins)                      ;; fifth-oaktree-intersection
        nil
        (list obj-bus)                        ;; fifth-sycamore-intersection
        nil
        (list obj-bone)                       ;; museum-lobby
        nil
        (list obj-jar obj-special obj-ruby)   ;; marine-life-area
        (list obj-nitric)                     ;; maintenance-room
        (list obj-glycerine)                  ;; classroom
        nil nil nil nil nil
        (list obj-amethyst)                   ;; bottom-of-subway-stairs
        nil nil
        (list obj-special)                    ;; question-room-1
        nil
        (list obj-special)                    ;; question-room-2
        nil
        (list obj-special)                    ;; question-room-three
        nil
        (list obj-mona)                       ;; winner's-room
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
nil))

;;; These are objects in a room that are only described in the
;;; room description.  They are permanent.

(setq erbp-room-silents (list nil
        (list obj-tree obj-coconut)            ;; dead-end
        (list obj-tree obj-coconut)            ;; e-w-dirt-road
        nil nil nil nil nil nil
        (list obj-bin)                         ;; mailroom
        (list obj-computer)                    ;; computer-room
        nil nil nil
        (list obj-dial)                        ;; sauna
        nil
        (list obj-ladder)                      ;; weight-room
        (list obj-button obj-ladder)           ;; maze-button-room
        nil nil nil
        nil nil nil nil
	(list obj-lake)                        ;; lakefront-north
	(list obj-lake)                        ;; lakefront-south
	nil
        (list obj-chute)                       ;; cave-entrance
        nil nil nil nil nil
        (list obj-painting obj-bed)            ;; bedroom
        (list obj-urinal obj-pipes)            ;; bathroom
        nil nil nil nil nil nil
        (list obj-boulder)                     ;; horseshoe-boulder-room
        nil nil nil nil nil nil nil nil nil nil nil nil nil nil
        (list obj-computer obj-cable)          ;; gamma-computing-center
        (list obj-mail)                        ;; post-office
        (list obj-gate)                        ;; main-maple-intersection
        nil nil nil nil nil nil nil nil nil nil nil nil nil
        nil nil nil nil nil nil nil
        (list obj-cliff)                       ;; fifth-oaktree-intersection
        nil nil nil
        (list obj-dinosaur)                    ;; museum-lobby
        nil
        (list obj-fish obj-tanks)              ;; marine-life-area
        (list obj-switch)                      ;; maintenance-room
        (list obj-blackboard)                  ;; classroom
        (list obj-train)                       ;; vermont-station
        nil nil
        (list obj-disposal)                    ;; north-end-of-n-s-tunnel
        nil nil
        (list obj-computer)                    ;; endgame-computer-room
        nil nil nil nil nil nil nil nil 
	(list obj-pc)                          ;; pc-area
	nil nil nil nil nil nil
))
(setq erbp-inventory '(1))

;;; Descriptions of objects, as they appear in the room description, and
;;; the inventory.

(setq erbp-objects '(
		("There is a shovel here." "A shovel")                ;0
		("There is a lamp nearby." "A lamp")                  ;1
		("There is a CPU card here." "A computer board")      ;2
		("There is some food here." "Some food")              ;3
		("There is a shiny brass key here." "A brass key")    ;4
		("There is a slip of paper here." "A slip of paper")  ;5
		("There is a wax statuette of Richard Stallman here." ;6
		 "An RMS statuette")
		("There is a shimmering diamond here." "A diamond")   ;7
		("There is a 10 pound weight here." "A weight")       ;8
		("There is a life preserver here." "A life preserver");9
		("There is an emerald bracelet here." "A bracelet")   ;10
		("There is a gold bar here." "A gold bar")            ;11
		("There is a platinum bar here." "A platinum bar")    ;12
		("There is a beach towel on the ground here." "A beach towel")
		("There is an axe here." "An axe") ;14
		("There is a silver bar here." "A silver bar")  ;15
		("There is a bus driver's license here." "A license") ;16
		("There are some valuable coins here." "Some valuable coins")
		("There is a jewel-encrusted egg here." "A valuable egg") ;18
		("There is a glass jar here." "A glass jar") ;19
		("There is a dinosaur bone here." "A bone") ;20
		("There is a packet of nitric acid here." "Some nitric acid")
		("There is a packet of glycerine here." "Some glycerine") ;22
		("There is a valuable ruby here." "A ruby") ;23
		("There is a valuable amethyst here." "An amethyst") ;24
		("The Mona Lisa is here." "The Mona Lisa") ;25
		("There is a 100 dollar bill here." "A $100 bill") ;26
		("There is a floppy disk here." "A floppy disk") ;27
	       )
)

;;; Weight of objects

(setq erbp-object-lbs 
      '(2 1 1 1 1 0 2 2 10 3 1 1 1 0 1 1 0 1 1 1 1 0 0 2 2 1 0 0))
(setq erbp-object-pts 
      '(0 0 0 0 0 0 0 10 0 0 10 10 10 0 0 10 0 10 10 0 0 0 0 10 10 10 10 0))


;;; Unix representation of objects.
(setq erbp-objfiles '(
		 "shovel.o" "lamp.o" "cpu.o" "food.o" "key.o" "paper.o"
		 "rms.o" "diamond.o" "weight.o" "preserver.o" "bracelet.o"
		 "gold.o" "platinum.o" "towel.o" "axe.o" "silver.o" "license.o"
		 "coins.o" "egg.o" "jar.o" "bone.o" "nitric.o" "glycerine.o"
		 "ruby.o" "amethyst.o"
		 ))

;;; These are the descriptions for the negative numbered objects from
;;; erbp-room-objects

(setq erbp-perm-objects '(
		     nil
		     ("There is a large boulder here.")
		     nil
		     ("There is a ferocious bear here!")
		     nil
		     nil
		     ("There is a worthless pile of protoplasm here.")
		     nil
		     nil
		     nil
		     nil
		     nil
		     nil
		     ("There is a strange smell in this room.")
		     nil
		     (
"There is a box with a slit in it, bolted to the wall here."
                     )
		     nil
		     nil
		     ("There is a bus here.")
		     nil
		     nil
		     nil
))


;;; These are the descriptions the user gets when regular objects are
;;; examined.

(setq erbp-physobj-desc '(
"It is a normal shovel with a price tag attached that says $19.99."
"The lamp is hand-crafted by Geppetto."
"The CPU board has a VAX chip on it.  It seems to have
2 Megabytes of RAM onboard."
"It looks like some kind of meat.  Smells pretty bad."
nil
"The paper says: Don't forget to type 'help' for help.  Also, remember
this word: 'worms'"
"The statuette is of the likeness of Richard Stallman, the author of the
famous EMACS editor.  You notice that he is not wearing any shoes."
nil
"You observe that the weight is heavy."
"It says S. S. Minnow."
nil
nil
nil
"It has a picture of snoopy on it."
nil
nil
"It has your picture on it!"
"They are old coins from the 19th century."
"It is a valuable Fabrege egg."
"It is a a plain glass jar."
nil
nil
nil
nil
nil
                     )
)

;;; These are the descriptions the user gets when non-regular objects
;;; are examined.

(setq erbp-permobj-desc '(
		     nil
"It is just a boulder.  It cannot be moved."
"They are palm trees with a bountiful supply of coconuts in them."
"It looks like a grizzly to me."
"All of the bins are empty.  Looking closely you can see that there
are names written at the bottom of each bin, but most of them are
faded away so that you cannot read them.  You can only make out three
names:
                   Jeffrey Collier
                   Robert Toukmond
                   Thomas Stock
"
                      nil
"It is just a garbled mess."
"The dial points to a temperature scale which has long since faded away."
nil
nil
"It is a velvet painting of Elvis Presley.  It seems to be nailed to the
wall, and you cannot move it."
"It is a queen sized bed, with a very firm mattress."
"The urinal is very clean compared with everything else in the cave.  There
isn't even any rust.  Upon close examination you realize that the drain at the
bottom is missing, and there is just a large hole leading down the
pipes into nowhere.  The hole is too small for a person to fit in.  The 
flush handle is so clean that you can see your reflection in it."
nil
nil
"The box has a slit in the top of it, and on it, in sloppy handwriting, is
written: 'For key upgrade, put key in here.'"
nil
"It says 'express mail' on it."
"It is a 35 passenger bus with the company name 'mobytours' on it."
"It is a large metal gate that is too big to climb over."
"It is a HIGH cliff."
"Unfortunately you do not know enough about dinosaurs to tell very much about
it.  It is very big, though."
"The fish look like they were once quite beautiful."
nil
nil
nil
nil
"It is a normal ladder that is permanently attached to the hole."
"It is a passenger train that is ready to go."
"It is a personal computer that has only one floppy disk drive."
		    )
)

(setq erbp-diggables 
      (list nil nil nil (list obj-cpu) nil nil nil nil nil nil nil
		  nil nil nil nil nil nil nil nil nil nil      ;11-20
		  nil nil nil nil nil nil nil nil nil nil      ;21-30
		  nil nil nil nil nil nil nil nil nil nil      ;31-40
		  nil (list obj-platinum) nil nil nil nil nil nil nil nil))

(setq erbp-room-shorts nil)
(dolist (x erbp-rooms)
  (setq erbp-room-shorts  
		     (append erbp-room-shorts (list (downcase 
						    (erbp-space-to-hyphen
						     (cadr x)))))))

(setq erbp-endgame-questions '(
			  (
"What is your password on the machine called 'pokey'?" "robert")
			  (
"What password did you use during anonymous ftp to gamma?" "foo")
			  (
"Excluding the endgame, how many places are there where you can put
treasures for points?" "4" "four")
			  (
"What is your login name on the 'endgame' machine?" "toukmond"
)
			  (
"What is the nearest whole dollar to the price of the shovel?" "20" "twenty")
			  (
"What is the name of the bus company serving the town?" "mobytours")
			  (
"Give either of the two last names in the mailroom, other than your own."
"collier" "stock")
			  (
"What cartoon character is on the towel?" "snoopy")
			  (
"What is the last name of the author of EMACS?" "stallman")
			  (
"How many megabytes of memory is on the CPU board for the Vax?" "2")
			  (
"Which street in town is named after a U.S. state?" "vermont")
			  (
"How many pounds did the weight weigh?" "ten" "10")
			  (
"Name the STREET which runs right over the subway stop." "fourth" "4" "4th")
			  (
"How many corners are there in town (excluding the one with the Post Office)?"
                  "24" "twentyfour" "twenty-four")
			  (
"What type of bear was hiding your key?" "grizzly")
			  (
"Name either of the two objects you found by digging." "cpu" "card" "vax"
"board" "platinum")
			  (
"What network protocol is used between pokey and gamma?" "tcp/ip" "ip" "tcp")
))

(let (a)
  (setq a 0)
  (dolist (x erbp-room-shorts)
    (eval (list 'defconst (intern x) a))
    (setq a (+ a 1))))



;;;;
;;;; This section defines the UNIX emulation functions for erbpnet.
;;;;

(defun erbp-unix-parse (args)
  (interactive "*p")
  (beginning-of-line)
  (let (beg esign)
    (setq beg (+ (point) 2))
    (end-of-line)
    (if (and (not (= beg (point)))
	     (string= "$" (buffer-substring (- beg 2) (- beg 1))))
	(progn
	  (setq line (downcase (buffer-substring beg (point))))
	  (princ line)
	  (if (eq (erbp-parse2 nil erbp-unix-verbs line) -1)
	      (progn
		(if (setq esign (string-match "=" line))
		    (erbp-doassign line esign)		
		  (erbp-mprinc (car line-list))
		  (erbp-mprincl ": not found.")))))
      (goto-char (point-max))
      (erbp-mprinc "\n"))
    (if (eq erbpeon-mode 'unix)
	(progn
	  (erbp-fix-screen)
	  (erbp-mprinc "$ ")))))

(defun erbp-doassign (line esign)
  (if (not erbp-wizard)
      (let (passwd)
	(erbp-mprinc "Enter wizard password: ")
	(setq passwd (erbp-read-line))
	(if (not erbp-batch-mode)
	    (erbp-mprinc "\n"))
	(if (string= passwd "moby")
	    (progn
	      (setq erbp-wizard t)
	      (erbp-doassign line esign))
	  (erbp-mprincl "Incorrect.")))

    (let (varname epoint afterq i value)
      (setq varname (substring line 0 esign))
      (if (not (setq epoint (string-match ")" line)))
	  (if (string= (substring line (1+ esign) (+ esign 2))
		       "\"")
	      (progn
		(setq afterq (substring line (+ esign 2)))
		(setq epoint (+
			      (string-match "\"" afterq)
			      (+ esign 3))))
	    
	    (if (not (setq epoint (string-match " " line)))
		(setq epoint (length line))))
	(setq epoint (1+ epoint))
	(while (and
		(not (= epoint (length line)))
		(setq i (string-match ")" (substring line epoint))))
	  (setq epoint (+ epoint i 1))))
      (setq value (substring line (1+ esign) epoint))
      (erbp-eval varname value))))

(defun erbp-eval (varname value)
  (let (eval-error)
    (switch-to-buffer (get-buffer-create "*erbpeon-eval*"))
    (erase-buffer)
    (insert "(setq ")
    (insert varname)
    (insert " ")
    (insert value)
    (insert ")")
    (setq eval-error nil)
    (condition-case nil
	(eval-current-buffer)
      (error (setq eval-error t)))
    (kill-buffer (current-buffer))
    (switch-to-buffer "*erbpeon*")
    (if eval-error
	(erbp-mprincl "Invalid syntax."))))
  

(defun erbp-unix-interface ()
  (erbp-login)
  (if erbp-logged-in
      (progn
	(setq erbpeon-mode 'unix)
	(define-key erbpeon-mode-map "\r" 'erbp-unix-parse)
	(erbp-mprinc "$ "))))

(defun erbp-login ()
  (let (tries username password)
    (setq tries 4)
    (while (and (not erbp-logged-in) (> (setq tries (- tries 1)) 0))
      (erbp-mprinc "\n\nUNIX System V, Release 2.2 (pokey)\n\nlogin: ")
      (setq username (erbp-read-line))
      (if (not erbp-batch-mode)
	  (erbp-mprinc "\n"))
      (erbp-mprinc "password: ")
      (setq password (erbp-read-line))
      (if (not erbp-batch-mode)
	  (erbp-mprinc "\n"))
      (if (or (not (string= username "toukmond"))
	      (not (string= password "robert")))
	  (erbp-mprincl "login incorrect")
	(setq erbp-logged-in t)
	(erbp-mprincl "
Welcome to Unix\n
Please clean up your directories.  The filesystem is getting full.
Our tcp/ip link to gamma is a little flaky, but seems to work.
The current version of ftp can only send files from your home
directory, and deletes them after they are sent!  Be careful.

Note: Restricted bourne shell in use.\n")))
  (setq erbpeon-mode 'erbpeon)))

(defun erbp-ls (args)
  (if (car args)
      (let (ocdpath ocdroom)
	(setq ocdpath erbp-cdpath)
	(setq ocdroom erbp-cdroom)
	(if (not (eq (erbp-cd args) -2))
	    (erbp-ls nil))
	(setq erbp-cdpath ocdpath)
	(setq erbp-cdroom ocdroom))
    (if (= erbp-cdroom -10)
	(erbp-ls-inven))
    (if (= erbp-cdroom -2)
	(erbp-ls-rooms))
    (if (= erbp-cdroom -3)
	(erbp-ls-root))
    (if (= erbp-cdroom -4)
	(erbp-ls-usr))
    (if (> erbp-cdroom 0)
	(erbp-ls-room))))

(defun erbp-ls-root ()
  (erbp-mprincl "total 4
drwxr-xr-x  3 root     staff           512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 usr
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 rooms"))

(defun erbp-ls-usr ()
  (erbp-mprincl "total 4
drwxr-xr-x  3 root     staff           512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..
drwxr-xr-x  3 toukmond restricted      512 Jan 1 1970 toukmond"))

(defun erbp-ls-rooms ()
  (erbp-mprincl "total 16
drwxr-xr-x  3 root     staff           512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..")
  (dolist (x erbp-visited)
    (erbp-mprinc
"drwxr-xr-x  3 root     staff           512 Jan 1 1970 ")
    (erbp-mprincl (nth x erbp-room-shorts))))

(defun erbp-ls-room ()
  (erbp-mprincl "total 4
drwxr-xr-x  3 root     staff           512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..
-rwxr-xr-x  3 root     staff          2048 Jan 1 1970 description")
  (dolist (x (nth erbp-cdroom erbp-room-objects))
    (if (and (>= x 0) (not (= x 255)))
	(progn
	  (erbp-mprinc "-rwxr-xr-x  1 toukmond restricted        0 Jan 1 1970 ")
	  (erbp-mprincl (nth x erbp-objfiles))))))

(defun erbp-ls-inven ()
  (erbp-mprinc "total 467
drwxr-xr-x  3 toukmond restricted      512 Jan 1 1970 .
drwxr-xr-x  3 root     staff          2048 Jan 1 1970 ..")
  (dolist (x erbp-unix-verbs)
    (if (not (eq (car x) 'IMPOSSIBLE))
	(progn
	  (erbp-mprinc"
-rwxr-xr-x  1 toukmond restricted    10423 Jan 1 1970 ")
	  (erbp-mprinc (car x)))))
  (erbp-mprinc "\n")
  (if (not erbp-uncompressed)
      (erbp-mprincl
"-rwxr-xr-x  1 toukmond restricted        0 Jan 1 1970 paper.o.Z"))
  (dolist (x erbp-inventory)
    (erbp-mprinc 
"-rwxr-xr-x  1 toukmond restricted        0 Jan 1 1970 ")
    (erbp-mprincl (nth x erbp-objfiles))))

(defun erbp-echo (args)
  (let (nomore var)
    (setq nomore nil)
    (dolist (x args)
	    (if (not nomore)
		(progn
		  (if (not (string= (substring x 0 1) "$"))
		      (progn
			(erbp-mprinc x)
			(erbp-mprinc " "))
		    (setq var (intern (substring x 1)))
		    (if (not (boundp var))
			(erbp-mprinc " ")
		      (if (member var erbp-restricted)
			  (progn
			    (erbp-mprinc var)
			    (erbp-mprinc ": Permission denied")
			    (setq nomore t))
			(eval (list 'erbp-mprinc var))
			(erbp-mprinc " ")))))))
	    (erbp-mprinc "\n")))


(defun erbp-ftp (args)
  (let (host username passwd ident newlist)
    (if (not (car args))
	(erbp-mprincl "ftp: hostname required on command line.")
      (setq host (intern (car args)))
      (if (not (member host '(gamma erbp-endgame)))
	  (erbp-mprincl "ftp: Unknown host.")
	(if (eq host 'erbp-endgame)
	    (erbp-mprincl "ftp: connection to endgame not allowed")
	  (if (not erbp-ethernet)
	      (erbp-mprincl "ftp: host not responding.")
	    (erbp-mprincl "Connected to gamma. FTP ver 0.9 00:00:00 01/01/70")
	    (erbp-mprinc "Username: ")
	    (setq username (erbp-read-line))
	    (if (string= username "toukmond")
		(if erbp-batch-mode
		    (erbp-mprincl "toukmond ftp access not allowed.")
		  (erbp-mprincl "\ntoukmond ftp access not allowed."))
	      (if (string= username "anonymous")
		  (if erbp-batch-mode
		      (erbp-mprincl
		       "Guest login okay, send your user ident as password.")
		    (erbp-mprincl 
		     "\nGuest login okay, send your user ident as password."))
		(if erbp-batch-mode
		    (erbp-mprinc "Password required for ")
		  (erbp-mprinc "\nPassword required for "))
		(erbp-mprincl username))
	      (erbp-mprinc "Password: ")
	      (setq ident (erbp-read-line))
	      (if (not (string= username "anonymous"))
		  (if erbp-batch-mode
		      (erbp-mprincl "Login failed.")
		    (erbp-mprincl "\nLogin failed."))
		(if erbp-batch-mode
		   (erbp-mprincl 
		    "Guest login okay, user access restrictions apply.")
		  (erbp-mprincl 
		   "\nGuest login okay, user access restrictions apply."))
		(erbp-ftp-commands)
		(setq newlist 
'("What password did you use during anonymous ftp to gamma?"))
		(setq newlist (append newlist (list ident)))
		(rplaca (nthcdr 1 erbp-endgame-questions) newlist)))))))))
  
(defun erbp-ftp-commands ()
    (setq erbp-exitf nil)
    (let (line)
      (while (not erbp-exitf)
	(erbp-mprinc "ftp> ")
	(setq line (erbp-read-line))
	(if 
	    (eq
	     (erbp-parse2 nil 
		    '((type . erbp-ftptype) (binary . erbp-bin) (bin . erbp-bin)
		      (send . erbp-send) (put . erbp-send) (quit . erbp-ftpquit)
		      (help . erbp-ftphelp)(ascii . erbp-fascii)
		      ) line)
	     -1)
	    (erbp-mprincl "No such command.  Try help.")))
      (setq erbp-ftptype 'ascii)))

(defun erbp-ftptype (args)
  (if (not (car args))
      (erbp-mprincl "Usage: type [binary | ascii]")
    (setq args (intern (car args)))
    (if (eq args 'binary)
	(erbp-bin nil)
      (if (eq args 'ascii)
	  (erbp-fascii 'nil)
	(erbp-mprincl "Unknown type.")))))

(defun erbp-bin (args)
  (erbp-mprincl "Type set to binary.")
  (setq erbp-ftptype 'binary))

(defun erbp-fascii (args)
  (erbp-mprincl "Type set to ascii.")
  (setq erbp-ftptype 'ascii))

(defun erbp-ftpquit (args)
  (setq erbp-exitf t))

(defun erbp-send (args)
  (if (not (car args))
      (erbp-mprincl "Usage: send <filename>")
    (setq args (car args))
    (let (counter foo)
      (setq foo nil)
      (setq counter 0)

;;; User can send commands!  Stupid user.


      (if (assq (intern args) erbp-unix-verbs)
	  (progn
	    (rplaca (assq (intern args) erbp-unix-verbs) 'IMPOSSIBLE)
	    (erbp-mprinc "Sending ")
	    (erbp-mprinc erbp-ftptype)
	    (erbp-mprinc " file for ")
	    (erbp-mprincl args)
	    (erbp-mprincl "Transfer complete."))

	(dolist (x erbp-objfiles)
	  (if (string= args x)
	      (progn
		(if (not (member counter erbp-inventory))
		    (progn
		      (erbp-mprincl "No such file.")
		      (setq foo t))
		  (erbp-mprinc "Sending ")
		  (erbp-mprinc erbp-ftptype)
		  (erbp-mprinc " file for ")
		  (erbp-mprinc (downcase (cadr (nth counter erbp-objects))))
		  (erbp-mprincl ", (0 bytes)")
		  (if (not (eq erbp-ftptype 'binary))
		      (progn
			(if (not (member obj-protoplasm
					 (nth receiving-room 
					      erbp-room-objects)))
			    (erbp-replace erbp-room-objects receiving-room
				     (append (nth receiving-room 
						  erbp-room-objects)
					     (list obj-protoplasm))))
			(erbp-remove-obj-from-inven counter))
		    (erbp-remove-obj-from-inven counter)
		    (erbp-replace erbp-room-objects receiving-room
			     (append (nth receiving-room erbp-room-objects)
				     (list counter))))
		  (setq foo t)
		  (erbp-mprincl "Transfer complete."))))
	  (setq counter (+ 1 counter)))
	(if (not foo)
	    (erbp-mprincl "No such file."))))))

(defun erbp-ftphelp (args)
  (erbp-mprincl 
   "Possible commands are:\nsend    quit    type   ascii  binary   help"))

(defun erbp-uexit (args)
  (setq erbpeon-mode 'erbpeon)
  (erbp-mprincl "\nYou step back from the console.")
  (define-key erbpeon-mode-map "\r" 'erbp-parse)
  (if (not erbp-batch-mode)
      (erbp-messages)))

(defun erbp-pwd (args)
  (erbp-mprincl erbp-cdpath))

(defun erbp-uncompress (args)
  (if (not (car args))
      (erbp-mprincl "Usage: uncompress <filename>")
    (setq args (car args))
    (if (or erbp-uncompressed
	    (and (not (string= args "paper.o"))
		 (not (string= args "paper.o.z"))))
	(erbp-mprincl "Uncompress command failed.")
      (setq erbp-uncompressed t)
      (setq erbp-inventory (append erbp-inventory (list obj-paper))))))

(defun erbp-rlogin (args)
  (let (passwd)
    (if (not (car args))
	(erbp-mprincl "Usage: rlogin <hostname>")
      (setq args (car args))
      (if (string= args "endgame")
	  (erbp-rlogin-endgame)
	(if (not (string= args "gamma"))
	    (if (string= args "pokey")
		(erbp-mprincl "Can't rlogin back to localhost")
	      (erbp-mprincl "No such host."))
	  (if (not erbp-ethernet)
	      (erbp-mprincl "Host not responding.")
	    (erbp-mprinc "Password: ")
	    (setq passwd (erbp-read-line))
	    (if (not (string= passwd "worms"))
		(erbp-mprincl "\nlogin incorrect")
	      (erbp-mprinc 
"\nYou begin to feel strange for a moment, and you lose your items."
)
	      (erbp-replace erbp-room-objects computer-room 
		       (append (nth computer-room erbp-room-objects) 
			       erbp-inventory))
	      (setq erbp-inventory nil)
	      (setq erbp-current-room receiving-room)
	      (erbp-uexit nil))))))))
  
(defun erbp-cd (args)
  (let (tcdpath tcdroom path-elements room-check)
    (if (not (car args))
	(erbp-mprincl "Usage: cd <path>")
      (setq tcdpath erbp-cdpath)
      (setq tcdroom erbp-cdroom)
      (setq erbp-badcd nil)
      (condition-case nil
	  (setq path-elements (erbp-get-path (car args) nil))
	(error (erbp-mprincl "Invalid path")
	       (setq erbp-badcd t)))
      (dolist (pe path-elements)
	      (unless erbp-badcd
		      (if (not (string= pe "."))
			  (if (string= pe "..")
			      (progn
				(if (> tcdroom 0)                  ;In a room
				    (progn
				      (setq tcdpath "/rooms")
				      (setq tcdroom -2))
					;In /rooms,/usr,root
				  (if (or 
				       (= tcdroom -2) (= tcdroom -4) 
				       (= tcdroom -3))
				      (progn
					(setq tcdpath "/")
					(setq tcdroom -3))
				    (if (= tcdroom -10)       ;In /usr/toukmond
					(progn
					  (setq tcdpath "/usr")
					  (setq tcdroom -4))))))
			    (if (string= pe "/")
				(progn
				  (setq tcdpath "/")
				  (setq tcdroom -3))
			      (if (= tcdroom -4)
				  (if (string= pe "toukmond")
				      (progn
					(setq tcdpath "/usr/toukmond")
					(setq tcdroom -10))
				    (erbp-nosuchdir))
				(if (= tcdroom -10)
				    (erbp-nosuchdir)
				  (if (> tcdroom 0)
				      (erbp-nosuchdir)
				    (if (= tcdroom -3)
					(progn
					  (if (string= pe "rooms")
					      (progn
						(setq tcdpath "/rooms")
						(setq tcdroom -2))
					    (if (string= pe "usr")
						(progn
						  (setq tcdpath "/usr")
						  (setq tcdroom -4))
					      (erbp-nosuchdir))))
				      (if (= tcdroom -2)
					  (progn
					    (dolist (x erbp-visited)
						    (setq room-check 
							  (nth x 
							      erbp-room-shorts))
						    (if (string= room-check pe)
							(progn
							  (setq tcdpath 
						 (concat "/rooms/" room-check))
							  (setq tcdroom x))))
					    (if (= tcdroom -2)
						(erbp-nosuchdir)))))))))))))
      (if (not erbp-badcd)
	  (progn
	    (setq erbp-cdpath tcdpath)
	    (setq erbp-cdroom tcdroom)
	    0)
      -2))))

(defun erbp-nosuchdir ()
  (erbp-mprincl "No such directory.")
  (setq erbp-badcd t))

(defun erbp-cat (args)
  (let (doto checklist)
    (if (not (setq args (car args)))
	(erbp-mprincl "Usage: cat <ascii-file-name>")
      (if (string-match "/" args)
	  (erbp-mprincl "cat: only files in current directory allowed.")
	(if (and (> erbp-cdroom 0) (string= args "description"))
	    (erbp-mprincl (car (nth erbp-cdroom erbp-rooms)))
	  (if (setq doto (string-match "\\.o" args))
	      (progn
		(if (= erbp-cdroom -10)
		    (setq checklist erbp-inventory)
		  (setq checklist (nth erbp-cdroom erbp-room-objects)))
		(if (not (member (cdr 
				  (assq (intern 
					 (substring args 0 doto)) 
					erbp-objnames))
				 checklist))
		    (erbp-mprincl "File not found.")
		  (erbp-mprincl "Ascii files only.")))
	    (if (assq (intern args) erbp-unix-verbs)
		(erbp-mprincl "Ascii files only.")
	      (erbp-mprincl "File not found."))))))))
  
(defun erbp-zippy (args)
  (erbp-mprincl (yow)))

(defun erbp-rlogin-endgame ()
  (if (not (= (erbp-score nil) 90))
      (erbp-mprincl 
       "You have not achieved enough points to connect to endgame.")
    (erbp-mprincl"\nWelcome to the endgame.  You are a truly noble adventurer.")
    (setq erbp-current-room treasure-room)
    (setq erbp-endgame t)
    (erbp-replace erbp-room-objects endgame-treasure-room (list obj-bill))
    (erbp-uexit nil)))


(random t)
(setq tloc (+ 60 (random 18)))
(erbp-replace erbp-room-objects tloc 
	     (append (nth tloc erbp-room-objects) (list 18)))

(setq tcomb (+ 100 (random 899)))
(setq erbp-combination (prin1-to-string tcomb))

;;;;
;;;; This section defines the DOS emulation functions for erbpnet
;;;;

(defun erbp-dos-parse (args)
  (interactive "*p")
  (beginning-of-line)
  (let (beg)
    (setq beg (+ (point) 3))
    (end-of-line)
    (if (not (= beg (point)))
	(let (line)
	  (setq line (downcase (buffer-substring beg (point))))
	  (princ line)
	  (if (eq (erbp-parse2 nil erbp-dos-verbs line) -1)
	      (progn
		(sleep-for 1)
		(erbp-mprincl "Bad command or file name"))))
      (goto-char (point-max))
      (erbp-mprinc "\n"))
    (if (eq erbpeon-mode 'dos)
	(progn
	  (erbp-fix-screen)
	  (erbp-dos-prompt)))))

(defun erbp-dos-interface ()
  (erbp-dos-boot-msg)
  (setq erbpeon-mode 'dos)
  (define-key erbpeon-mode-map "\r" 'erbp-dos-parse)
  (erbp-dos-prompt))

(defun erbp-dos-type (args)
  (sleep-for 2)
  (if (setq args (car args))
      (if (string= args "foo.txt")
	  (erbp-dos-show-combination)
	(if (string= args "command.com")
	    (erbp-mprincl "Cannot type binary files")
	  (erbp-mprinc "File not found - ")
	  (erbp-mprincl (upcase args))))
    (erbp-mprincl "Must supply file name")))

(defun erbp-dos-invd (args)
  (sleep-for 1)
  (erbp-mprincl "Invalid drive specification"))

(defun erbp-dos-dir (args)
  (sleep-for 1)
  (if (or (not (setq args (car args))) (string= args "\\"))
      (erbp-mprincl "
 Volume in drive A is FOO        
 Volume Serial Number is 1A16-08C9
 Directory of A:\\

COMMAND  COM     47845 04-09-91   2:00a
FOO      TXT        40 01-20-93   1:01a
        2 file(s)      47845 bytes
                     1065280 bytes free
")
    (erbp-mprincl "
 Volume in drive A is FOO        
 Volume Serial Number is 1A16-08C9
 Directory of A:\\

File not found")))


(defun erbp-dos-prompt ()
  (erbp-mprinc "A> "))

(defun erbp-dos-boot-msg ()
  (sleep-for 3)
  (erbp-mprinc "Current time is ")
  (erbp-mprincl (substring (current-time-string) 12 20))
  (erbp-mprinc "Enter new time: ")
  (erbp-read-line)
  (if (not erbp-batch-mode)
      (erbp-mprinc "\n")))

(defun erbp-dos-spawn (args)
  (sleep-for 1)
  (erbp-mprincl "Cannot spawn subshell"))

(defun erbp-dos-exit (args)
  (setq erbpeon-mode 'erbpeon)
  (erbp-mprincl "\nYou power down the machine and step back.")
  (define-key erbpeon-mode-map "\r" 'erbp-parse)
  (if (not erbp-batch-mode)
      (erbp-messages)))

(defun erbp-dos-no-disk ()
  (sleep-for 3)
  (erbp-mprincl "Boot sector not found"))


(defun erbp-dos-show-combination ()
  (sleep-for 2)
  (erbp-mprinc "\nThe combination is ")
  (erbp-mprinc erbp-combination)
  (erbp-mprinc ".\n"))

(defun erbp-dos-nil (args))


;;;;
;;;; This section defines the save and restore game functions for erbpnet.
;;;;

(defun erbp-save-game (filename)
  (if (not (setq filename (car filename)))
      (erbp-mprincl "You must supply a filename for the save.")
    (if (file-exists-p filename)
	(delete-file filename))
    (setq erbp-numsaves (1+ erbp-numsaves))
    (erbp-make-save-buffer)
    (erbp-save-val "erbp-current-room")
    (erbp-save-val "erbp-computer")
    (erbp-save-val "erbp-combination")
    (erbp-save-val "erbp-visited")
    (erbp-save-val "erbp-diggables")
    (erbp-save-val "erbp-key-level")
    (erbp-save-val "erbp-floppy")
    (erbp-save-val "erbp-numsaves")
    (erbp-save-val "erbp-numcmds")
    (erbp-save-val "erbp-logged-in")
    (erbp-save-val "erbpeon-mode")
    (erbp-save-val "erbp-jar")
    (erbp-save-val "erbp-lastdir")
    (erbp-save-val "erbp-black")
    (erbp-save-val "erbp-nomail")
    (erbp-save-val "erbp-unix-verbs")
    (erbp-save-val "erbp-hole")
    (erbp-save-val "erbp-uncompressed")
    (erbp-save-val "erbp-ethernet")
    (erbp-save-val "erbp-sauna-level")
    (erbp-save-val "erbp-room-objects")
    (erbp-save-val "erbp-room-silents")
    (erbp-save-val "erbp-inventory")
    (erbp-save-val "erbp-endgame-questions")
    (erbp-save-val "erbp-endgame")
    (erbp-save-val "erbp-cdroom")
    (erbp-save-val "erbp-cdpath")
    (erbp-save-val "erbp-correct-answer")
    (erbp-save-val "erbp-inbus")
    (if (erbp-compile-save-out filename)
	(erbp-mprincl "Error saving to file.")
      (erbp-do-logfile 'save nil)
      (switch-to-buffer "*erbpeon*")
      (princ "")
      (erbp-mprincl "Done."))))

(defun erbp-make-save-buffer ()
  (switch-to-buffer (get-buffer-create "*save-erbpeon*"))
  (erase-buffer))

(defun erbp-compile-save-out (filename)
  (let (ferror)
    (setq ferror nil)
    (condition-case nil
	(erbp-rot13)
      (error (setq ferror t)))
    (if (not ferror)
	(progn
	  (goto-char (point-min))))
    (condition-case nil
        (write-region 1 (point-max) filename nil 1)
        (error (setq ferror t)))
    (kill-buffer (current-buffer))
    ferror))
    

(defun erbp-save-val (varname)
  (let (value)
    (setq varname (intern varname))
    (setq value (eval varname))
    (erbp-minsert "(setq ")
    (erbp-minsert varname)
    (erbp-minsert " ")
    (if (or (listp value)
	    (symbolp value))
	(erbp-minsert "'"))
    (if (stringp value)
	(erbp-minsert "\""))
    (erbp-minsert value)
    (if (stringp value)
	(erbp-minsert "\""))
    (erbp-minsertl ")")))


(defun erbp-restore (args)
  (let (file)
    (if (not (setq file (car args)))
	(erbp-mprincl "You must supply a filename.")
      (if (not (erbp-load-d file))
	  (erbp-mprincl "Could not load restore file.")
	(erbp-mprincl "Done.")
	(setq room 0)))))


(defun erbp-do-logfile (type how)
  (let (ferror newscore)
    (setq ferror nil)
    (switch-to-buffer (get-buffer-create "*score*"))
    (erase-buffer)
    (condition-case nil
	(insert-file-contents erbp-log-file)
      (error (setq ferror t)))
    (unless ferror
	    (goto-char (point-max))
	    (erbp-minsert (current-time-string))
	    (erbp-minsert " ")
	    (erbp-minsert (user-login-name))
	    (erbp-minsert " ")
	    (if (eq type 'save)
		(erbp-minsert "saved ")
	      (if (= (erbp-endgame-score) 110)
		  (erbp-minsert "won ")
		(if (not how)
		    (erbp-minsert "quit ")
		  (erbp-minsert "killed by ")
		  (erbp-minsert how)
		  (erbp-minsert " "))))
	    (erbp-minsert "at ")
	    (erbp-minsert (cadr (nth (abs room) erbp-rooms)))
	    (erbp-minsert ". score: ")
	    (if (> (erbp-endgame-score) 0)
		(erbp-minsert (setq newscore (+ 90 (erbp-endgame-score))))
	      (erbp-minsert (setq newscore (erbp-reg-score))))
	    (erbp-minsert " saves: ")
	    (erbp-minsert erbp-numsaves)
	    (erbp-minsert " commands: ")
	    (erbp-minsert erbp-numcmds)
	    (erbp-minsert "\n")
	    (write-region 1 (point-max) erbp-log-file nil 1))
    (kill-buffer (current-buffer))))


;;;;
;;;; These are functions, and function re-definitions so that erbpeon can
;;;; be run in batch mode.


(defun erbp-batch-mprinc (arg)
   (if (stringp arg)
       (send-string-to-terminal arg)
     (send-string-to-terminal (prin1-to-string arg))))


(defun erbp-batch-mprincl (arg)
   (if (stringp arg)
       (progn
           (send-string-to-terminal arg)
           (send-string-to-terminal "\n"))
     (send-string-to-terminal (prin1-to-string arg))
     (send-string-to-terminal "\n")))

(defun erbp-batch-parse (erbp-ignore erbp-verblist line)
  (setq line-list (erbp-listify-string (concat line " ")))
  (erbp-doverb erbp-ignore erbp-verblist (car line-list) (cdr line-list)))

(defun erbp-batch-parse2 (erbp-ignore erbp-verblist line)
  (setq line-list (erbp-listify-string2 (concat line " ")))
  (erbp-doverb erbp-ignore erbp-verblist (car line-list) (cdr line-list)))

(defun erbp-batch-read-line ()
  (read-from-minibuffer "" nil erbpeon-batch-map))


(defun erbp-batch-loop ()
  (setq erbp-dead nil)
  (setq room 0)
  (while (not erbp-dead)
    (if (eq erbpeon-mode 'erbpeon)
	(progn
	  (if (not (= room erbp-current-room))
	      (progn
		(erbp-describe-room erbp-current-room)
		(setq room erbp-current-room)))
	  (erbp-mprinc ">")
	  (setq line (downcase (erbp-read-line)))
	  (if (eq (erbp-vparse erbp-ignore erbp-verblist line) -1)
	      (erbp-mprinc "I don't understand that.\n"))))))

(defun erbp-batch-dos-interface ()
  (erbp-dos-boot-msg)
  (setq erbpeon-mode 'dos)
  (while (eq erbpeon-mode 'dos)
    (erbp-dos-prompt)
    (setq line (downcase (erbp-read-line)))
    (if (eq (erbp-parse2 nil erbp-dos-verbs line) -1)
	(progn
	  (sleep-for 1)
	  (erbp-mprincl "Bad command or file name"))))
  (goto-char (point-max))
  (erbp-mprinc "\n"))

(defun erbp-batch-unix-interface ()
    (erbp-login)
    (if erbp-logged-in
	(progn
	  (setq erbpeon-mode 'unix)
	  (while (eq erbpeon-mode 'unix)
	    (erbp-mprinc "$ ")
	    (setq line (downcase (erbp-read-line)))
	    (if (eq (erbp-parse2 nil erbp-unix-verbs line) -1)
		(let (esign)
		  (if (setq esign (string-match "=" line))
		      (erbp-doassign line esign)		
		    (erbp-mprinc (car line-list))
		    (erbp-mprincl ": not found.")))))
	  (goto-char (point-max))
	  (erbp-mprinc "\n"))))

(defun erbpeon-nil (arg)
  "noop"
  (interactive "*p")
  nil)

(defun erbp-batch-erbpeon ()
  (load "erbp-batch")
  (setq erbp-visited '(27))
  (erbp-mprinc "\n")
  (erbp-batch-loop))

(unless (not noninteractive)
  (fset 'erbp-mprinc 'erbp-batch-mprinc)
  (fset 'erbp-mprincl 'erbp-batch-mprincl)
  (fset 'erbp-vparse 'erbp-batch-parse)
  (fset 'erbp-parse2 'erbp-batch-parse2)
  (fset 'erbp-read-line 'erbp-batch-read-line)
  (fset 'erbp-dos-interface 'erbp-batch-dos-interface)
  (fset 'erbp-unix-interface 'erbp-batch-unix-interface)
  (erbp-mprinc "\n")
  (setq erbp-batch-mode t)
  (erbp-batch-loop))

(provide 'erbpnet)

;;; erbpnet.el ends here
