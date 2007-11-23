;;; erbc4.el --- Russian Roulette 
;; Time-stamp: <2007-11-23 11:30:12 deego>
;; Copyright (C) 2003 Taylor Campbell
;; Emacs Lisp Archive entry
;; Filename: erbc4.el
;; Package: erbc4
;; Author: Taylor Campbell
;; Keywords:
;; Version:
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot
;; For latest version:

(defconst erbc4-home-page
  "http://gnufans.net/~deego")


 
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
 

(defconst erbc4-version "0.0dev")


;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

;;; Real Code:

(defvar erbn-RR-empty-bets (make-hash-table))
(defvar erbn-RR-bullet-bets (make-hash-table))
(defvar erbn-money (make-hash-table))

(defun erbn-move-money (nick table1 table2 amount)
  (let ((cell1 (gethash nick table1))
        (cell2 (gethash nick table2)))
    (if cell1
        (decf (gethash nick table1) amount)
      (setf (gethash nick table1) (- amount)))
    (if cell2
        (incf (gethash nick table2) amount)
      (setf (gethash nick table2) amount))))

(defun fs-bet (&rest args)
  (let ((nick (intern nick)))
    (cond ((null args)
           (let ((empty-bet (gethash nick erbn-RR-empty-bets))
                 (bullet-bet (gethash nick erbn-RR-bullet-bets)))
             (cond (empty-bet
                    (format "%s has bet %d on there being no bullet."
                            nick empty-bet))
                   (bullet-bet
                    (format "%s has bet %d on there being a bullet."
                            nick bullet-bet))
                   (t (format "%s has not bet anything."
                              nick)))))
          ((and (consp args)
                (consp (cdr args))
                (null (cddr args))
                (cond ((symbolp (car args))
                       (numberp (cadr args)))
                      ((numberp (car args))
                       (symbolp (cadr args)))
                      (t nil)))
           (let* ((on-what (if (symbolp (car args)) (car args) (cadr args)))
                  (how-much (if (numberp (car args)) (car args) (cadr args)))
                  (_ (if (< how-much 0)
                         (error "You can't bet negative amounts, moron.")))
                  (table (case on-what
                           ((empty no-bullet click) erbn-RR-empty-bets)
                           ((bullet bang blam) erbn-RR-bullet-bets)
                           (t (error "Invalid bet type" on-what))))
                  (not-table (if (eq table erbn-RR-bullet-bets)
                                 erbn-RR-empty-bets
                                 erbn-RR-bullet-bets)))
             (cond ((gethash nick not-table)
                    (format "%s: Idiot, you can can only bet on one outcome."
                            nick on-what))
                   ((< (or (gethash nick erbn-money) 0) how-much)
                    (format
                     "%s: Fool, you can't bet more than you've got (%d)."
                     nick (or (gethash nick erbn-money) 0)))
                   (t (erbn-move-money nick erbn-money table how-much)
                      (format "%s has bet %d GEMs so far on a %s."
                              nick
                              (gethash nick table)
                              on-what)))))
          (t (error "Invalid arguments to bet" args)))))

(defun fs-lend (arg1 arg2 &rest ignored)
  (let* ((to-whom (if (symbolp arg1) arg1 arg2))
         (how-much (if (numberp arg2) arg2 arg1))
         (nick (intern nick))
         (money (gethash nick erbn-money)))
    (if (equal nick to-whom)
        (error "You can't lend money to yourself, knave!"))
    (if (> how-much money)
        (error "You can't lend more than you have" nick how-much))
    (if (< how-much 0)
        (error "You can't lend negative amounts."))
    (decf (gethash nick erbn-money) how-much)
    (if (gethash to-whom erbn-money)
        (incf (gethash to-whom erbn-money) how-much)
      (setf (gethash to-whom erbn-money) how-much))
    (format "%s has lent %d GEMs to %s; %s now has %d GEMs and %s %d."
            nick
            how-much
            to-whom

            nick
            (gethash nick erbn-money)

            to-whom
            (gethash to-whom erbn-money))))

(defun erbn-keyshash (hash-table)
  (let ((keys nil))
    (maphash (lambda (key val) (push key keys)) hash-table)
    keys))

(defun erbn-valueshash (hash-table)
  (let ((values nil))
    (maphash (lambda (key val) (push val values)) hash-table)
    values))

(defun erbn-all-money (nick)
  (let ((amount
         (apply #'+
                (mapcar (lambda (table)
                          (or (gethash nick table) 0))
                        (list erbn-money
                              erbn-RR-bullet-bets
                              erbn-RR-empty-bets)))))
    (mapc (lambda (table)
            (remhash nick table))
          (list erbn-money
                erbn-RR-bullet-bets
                erbn-RR-empty-bets))
    amount))

(defun fs-money (&optional maybe-nick)
  (let* ((local-nick (or (and maybe-nick
                              (if (symbolp maybe-nick)
                                  maybe-nick
                                (intern maybe-nick)))
                         (intern nick)))
         (amount (or (gethash local-nick erbn-money) 0)))
    (if maybe-nick
        (format "%s has %d GEMs."
                local-nick
                amount)
      (format "You've got %d GEMs, %s."
              amount
              nick))))

(defun erbn-percent (m n)
  (/ (* (float m) 100.0) (float n)))

(defun erbn-unpercent (m n)
  (/ (* (float m) (float n)) 100.0))


(defun erbn-distribute (maybe-dead-nick winning-table losing-table)
  (prog1 (cond ((and (= (hash-table-count winning-table) 0)
                     (not maybe-dead-nick))
                ;; Give the losers their money back.
                (maphash (lambda (nick amount)
                           (incf (gethash nick erbn-money) amount))
                         losing-table))
               ((and (= (hash-table-count losing-table) 0)
                     (not maybe-dead-nick))
                ;; Give the winners their money back.
                (maphash (lambda (nick amount)
                           (incf (gethash nick erbn-money) amount))
                         winning-table))
               (t (let* ((winning-bets (erbn-valueshash winning-table))
                         (total-win-bets (apply #'+ winning-bets))
                         (total-money
                          (apply #'+
                                 (if maybe-dead-nick
                                     (erbn-all-money maybe-dead-nick)
                                   0)
                                 total-win-bets
                                 (erbn-valueshash losing-table))))
                    (maphash (lambda (nick amount)
                               (let* ((percent
                                       (erbn-percent amount total-win-bets))
                                      (unpercent
                                       (erbn-unpercent percent
                                                         total-money)))
                                 (incf (gethash nick erbn-money)
                                       (round unpercent))))
                             winning-table))))
    (clrhash winning-table)
    (clrhash losing-table)))

(defvar erbn-chamber (random 6))

;; Someone tell Riastradh if this is a good way to do this... (the
;; click and bang messages)
(defvar erbn-rr-bangs
  (list (lambda ()
          (concat "/me blows " nick "'s cerebellum all over "
                  tgt "... *BANG*"))
        (lambda ()
          (concat "/me blows " nick "'s brains all over "
                  tgt "... *BANG* ...reloading."))
        (lambda ()
          (concat nick " has to pick his brains off of the walls and "
                  " floor... *BANG*"))
        (lambda ()
          (concat nick "'s luck just ran out... *BANG*"))
        (lambda ()
          (concat "/me offers " nick " a cold "
                  (fs-describe "beer-data")
                  " before giving him the fatal blow... *KABLAM*"))))
(defvar erbn-rr-clicks
  (list (lambda ()
          (concat "/me points the gnu and " nick
                  " trembles... *CLICK*"))
        (lambda ()
          (concat nick " shudders as the great and powerful fsbot aims "
                  "the all-powerful barrel of the gnu... *CLICK*"))
        (lambda ()
          (concat nick " is one lucky punk... *CLICK*"))
        (lambda ()
          (concat "/me picks up the gnu and points it at " nick
                  "'s head... *CLICK*"))
        (lambda ()
          (concat "/me raises the gnu to " nick "'s head and " nick
                  " trembles as the *CLICK* sounds."))))

(defun erbn-rr-bang ()
  (fs-kick erbn-nick "*KABLAM!*  Goop from your head dribbles.")
  (funcall (fs-random-choose erbn-rr-bangs)))

(defun erbn-rr-click ()
  (funcall (fs-random-choose erbn-rr-clicks)))

(defun fs-add-bang (&rest bangs)
  (setq erbn-rr-bangs
        (concat bangs erbn-rr-bangs)))
(defun fs-add-click (&rest clicks)
  (setq erbn-rr-clicks
        (concat clicks erbn-rr-clicks)))

(defun fs-russian-roulette (&rest ignored)
  ;; Don't let them do that, because it confuses the money distribution.
  (if (gethash (intern nick) erbn-RR-bullet-bets)
      (error (concat nick ": Idiot, don't bet on your own death."))
    (if (= erbn-chamber 5)
        (progn
          (setq erbn-chamber (random 6))
          (erbn-distribute (intern nick)
                             erbn-RR-bullet-bets
                             erbn-RR-empty-bets)
          (erbn-rr-bang))
      (incf erbn-chamber)
      (erbn-distribute nil
                         erbn-RR-empty-bets
                         erbn-RR-bullet-bets)
      (erbn-rr-click))))

(defvar erbn-auth-bankers 
  '(deego Riastradh fledermaus _sprintf))


(defun erbn-add-banker (nick &rest ignored)
  (add-to-list 'erbn-auth-bankers nick))

(defun fs-auth-bankerp ()
  (and (member (intern nick) erbn-auth-bankers) t))

(defun fs-reset-money (&rest ignored)
  (if (not (fs-auth-bankerp))
      (error (concat nick ": You can't reset the money.")))
  (clrhash erbn-money)
  (clrhash erbn-RR-empty-bets)
  (clrhash erbn-RR-bullet-bets)
  "Money cleared.")

(defun fs-init-money (init &rest nicks)
  (if (not (fs-auth-bankerp))
      (error (concat nick ": you can't initialize the money")))
  (mapc (lambda (nick)
          (setf (gethash (if (stringp nick)
                             (intern nick)
                             nick)
                         erbn-money)
                init))
        nicks)
  "Money initialized.")

;; (defvar erbn-rr-bullet (random 6))

;; (defun fs-russian-roulette (&rest ignore)
;;   (if (>= erbn-rr-bullet 5)
;;       (progn 
;; 	(setq erbn-rr-bullet (random 6)) 
;; 	(fs-describe "rr-bang-kick")) 
;;     (incf erbn-rr-bullet) (fs-describe "rr-click")))

(defalias 'fsi-RR 'fs-russian-roulette)
(defalias 'fsi-rr 'fs-russian-roulette)


(defun fsi-kick (&optional reason &rest ignore)
  (erbn-mark-dead)
  (erc-cmd-KICK erbn-nick (when reason (format "%s" reason))))

(provide 'erbc4)
(run-hooks 'erbc4-after-load-hook)



;;; erbc4.el ends here
