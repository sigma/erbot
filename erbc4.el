;;; erbc4.el --- Russian Roulette 
;; Time-stamp: <2003-06-20 12:33:30 deego>
;; Copyright (C) 2003 Taylor Campbell
;; Emacs Lisp Archive entry
;; Filename: erbc4.el
;; Package: erbc4
;; Author: Taylor Campbell
;; Keywords:
;; Version:
;; URL: http://gnufans.net/~deego
;; For latest version:

(defconst erbc4-home-page
  "http://gnufans.net/~deego")


 
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
(defconst erbc4-quick-start
  "Help..."
)

(defun erbc4-quick-start ()
  "Provides electric help from variable `erbc4-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc4-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst erbc4-introduction
  "Help..."
)

;;;###autoload
(defun erbc4-introduction ()
  "Provides electric help from variable `erbc4-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc4-introduction) nil) "*doc*"))

;;; Commentary:
(defconst erbc4-commentary
  "Help..."
)

(defun erbc4-commentary ()
  "Provides electric help from variable `erbc4-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc4-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defconst erbc4-new-features
  "Help..."
)

(defun erbc4-new-features ()
  "Provides electric help from variable `erbc4-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc4-new-features) nil) "*doc*"))

;;; TO DO:
(defconst erbc4-todo
  "Help..."
)

(defun erbc4-todo ()
  "Provides electric help from variable `erbc4-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert erbc4-todo) nil) "*doc*"))

(defconst erbc4-version "0.0-DUMMY")
(defun erbc4-version (&optional arg)
   "Display erbc4's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "erbc4 version %s" erbc4-version))
    (message "erbc4 version %s" erbc4-version)))

;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defgroup erbc4 nil
  "The group erbc4."
  :group 'applications)
(defcustom erbc4-before-load-hook nil
  "Hook to run before loading erbc4."
  :group 'erbc4)
(defcustom erbc4-after-load-hook nil
  "Hook to run after loading erbc4."
  :group 'erbc4)
(run-hooks 'erbc4-before-load-hook)

(defcustom erbc4-verbosity 0
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'erbc4)
(defcustom erbc4-interactivity 0
  "How interactive to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'erbc4)
(defcustom erbc4-y-or-n-p-function 'erbc4-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `erbc4-y-or-n-p'."
  :type 'function
  :group 'erbc4)
(defcustom erbc4-n-or-y-p-function 'erbc4-y-or-n-p
  "Function to use for interactivity-dependent `n-or-y-p'.
Format same as that of `erbc4-n-or-y-p'."
  :type 'function
  :group 'erbc4)
(defun erbc4-message (points &rest args)
  "Signal message, depending on POINTS anderbc4-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points erbc4-verbosity))
    (apply #'message args)))
(defun erbc4-y-or-n-p (add prompt)
  "Query or assume t, based on `erbc4-interactivity'.
ADD is added to `erbc4-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbc4-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun erbc4-n-or-y-p (add prompt)
  "Query or assume t, based on `erbc4-interactivity'.
ADD is added to `erbc4-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add erbc4-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:

(defvar erbnoc-RR-empty-bets (make-hash-table))
(defvar erbnoc-RR-bullet-bets (make-hash-table))
(defvar erbnoc-money (make-hash-table))

(defun erbnoc-move-money (nick table1 table2 amount)
  (let ((cell1 (gethash nick table1))
        (cell2 (gethash nick table2)))
    (if cell1
        (decf (gethash nick table1) amount)
      (setf (gethash nick table1) (- amount)))
    (if cell2
        (incf (gethash nick table2) amount)
      (setf (gethash nick table2) amount))))

(defun fs-bet (on-what how-much)
  (let* ((nick (intern nick))
         (table (case on-what
                  ((empty no-bullet click) erbnoc-RR-empty-bets)
                  ((bullet bang blam) erbnoc-RR-bullet-bets)
                  (else (error "invalid bet type" on-what))))
         (not-table (if (eq table erbnoc-RR-bullet-bets)
                        erbnoc-RR-empty-bets
                      erbnoc-RR-bullet-bets)))
    (if (gethash nick not-table)
        (format "%s: Idiot, you can can only bet on one outcome (%s)."
                nick on-what)
      (erbnoc-move-money nick erbnoc-money table how-much)
      (format "%s has bet %d GEMs so far on a %s."
              nick
              (gethash nick table)
              on-what))))

(defun fs-lend (to-whom how-much)
  (let* ((nick (intern nick))
         (money (gethash nick erbnoc-money)))
    (if (> how-much money)
        (error "You can't lend more than you have" nick how-much))
    (decf (gethash nick erbnoc-money) how-much)
    (if (gethash to-whom erbnoc-money)
        (incf (gethash to-whom erbnoc-money) how-much)
      (setf (gethash to-whom erbnoc-money) how-much))
    (format "%s has lent %d GEMs to %s; %s now has %d GEMs and %s %d."
            nick
            how-much
            to-whom

            nick
            (gethash nick erbnoc-money)

            to-whom
            (gethash to-whom erbnoc-money))))

(defun erbnoc-keyshash (hash-table)
  (let ((keys '()))
    (maphash (lambda (key val) (push key keys)) hash-table)
    keys))

(defun erbnoc-valueshash (hash-table)
  (let ((values '()))
    (maphash (lambda (key val) (push val values)) hash-table)
    values))

(defun erbnoc-all-money (nick)
  (let ((amount
         (apply #'+
                (mapcar (lambda (table)
                          (or (gethash nick table) 0))
                        (list erbnoc-money
                              erbnoc-RR-bullet-bets
                              erbnoc-RR-empty-bets)))))
    (mapc (lambda (table)
            (remhash nick table))
          (list erbnoc-money
                erbnoc-RR-bullet-bets
                erbnoc-RR-empty-bets))
    amount))

(defun fs-money (&optional maybe-nick)
  (let ((amount (or (gethash (or maybe-nick (intern nick)) erbnoc-money)
                    0)))
    (if maybe-nick
        (format "%s has %d GEMs."
                maybe-nick
                amount)
      (format "You've got %d GEMs, %s."
              amount
              nick))))

(defun erbnoc-distribute (maybe-dead-nick winning-table losing-table)
  (let* ((vals (erbnoc-valueshash losing-table))
         (total-losing-money
          (apply #'+
                 (if maybe-dead-nick
                     (cons (erbnoc-all-money maybe-dead-nick) vals)
                   vals)))
         (each-amount
          (if (or (= total-losing-money 0)
                  (= (hash-table-count winning-table) 0))
              0
            (/ total-losing-money
               (hash-table-count winning-table)))))
    (maphash (lambda (nick amount)
               (setf (gethash nick winning-table)
                     (+ (gethash nick winning-table) each-amount)))
             winning-table)
    (maphash
     (lambda (nick amount)
       (erbnoc-move-money nick winning-table erbnoc-money amount))
     winning-table)

    (clrhash winning-table)
    (clrhash losing-table)))

(defvar erbnoc-chamber (random 6))

;; Someone tell Riastradh if this is a good way to do this... (the
;; click and bang messages)
(defvar erbnoc-rr-bangs
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
(defvar erbnoc-rr-clicks
  (list (lambda ()
          (concat "/me points the gnu and " nick
                  " trembles... *CLICK*"))
        (lambda ()
          (concat nick "shudders as the great and powerful fsbot aims "
                  "the all-powerful barrel of the gnu... *CLICK"))
        (lambda ()
          (concat nick " is one lucky punk... *CLICK*"))
        (lambda ()
          (concat "/me picks up the gnu and points it at " nick
                  "'s head... *CLICK*"))
        (lambda ()
          (concat "/me raises the gnu to " nick "'s head and " nick
                  " trembles as the *CLICK* sounds."))))

(defun erbnoc-rr-bang ()
  (funcall (fs-random-choose erbnoc-rr-bangs)))
(defun erbnoc-rr-click ()
  (funcall (fs-random-choose erbnoc-rr-clicks)))

(defun fs-add-bang (bang)
  (setq erbnoc-rr-bangs
        (cons bang erbnoc-rr-bangs)))
(defun fs-add-click (click)
  (setq erbnoc-rr-clicks
        (cons click erbnoc-rr-clicks)))

(defun fs-russian-roulette ()
  (if (= erbnoc-chamber 5)
      (progn
        (setq erbnoc-chamber (random 6))
        (erbnoc-distribute (intern nick)
                           erbnoc-RR-bullet-bets
                           erbnoc-RR-empty-bets)
        (erbnoc-rr-bang))
    (incf erbnoc-chamber)
    (erbnoc-distribute nil
                       erbnoc-RR-empty-bets
                       erbnoc-RR-bullet-bets)
    (erbnoc-rr-click)))

(defvar erbnoc-auth-bankers '())

(defun erbnoc-add-banker (nick)
  (add-to-list 'erbnoc-auth-bankers nick))

(defun fs-auth-bankerp ()
  (member (intern nick) erbnoc-auth-bankers))

(defun fs-reset-money ()
  (if (not (fs-auth-bankerp))
      (error (concat nick "You can't reset the money.")))
  (clrhash erbnoc-money)
  (clrhash erbnoc-RR-empty-bets)
  (clrhash erbnoc-RR-bullet-bets)
  "Money cleared.")

(defun fs-init-money (init &rest nicks)
  (if (not (fs-auth-bankerp))
      (error (concat nick ": you can't initialize the money")))
  (mapc (lambda (nick)
          (setf (gethash nick erbnoc-money) init))
        nicks)
  "Money initialized.")

;; (defvar erbnoc-rr-bullet (random 6))

;; (defun fs-russian-roulette (&rest ignore)
;;   (if (>= erbnoc-rr-bullet 5)
;;       (progn 
;; 	(setq erbnoc-rr-bullet (random 6)) 
;; 	(fs-describe "rr-bang-kick")) 
;;     (incf erbnoc-rr-bullet) (fs-describe "rr-click")))

(defalias 'fs-RR 'fs-russian-roulette)
(defalias 'fs-rr 'fs-russian-roulette)

(provide 'erbc4)
(run-hooks 'erbc4-after-load-hook)



;;; erbc4.el ends here
