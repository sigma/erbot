;;; flame.el --- automatic generation of flamage, as if we needed more

;;; Author: Ian G. Batten <batten@uk.ac.bham.multics>
;;; Maintainer: Noah Friedman <friedman@splode.com>
;;; Keywords: games

;;; $Id: flame.el,v 1.1 2004/12/16 01:44:34 mwolson Exp $

;;; Commentary:

;;; "Flame" program.  This has a chequered past.
;;;
;;; The original was on a Motorola 286 running Vanilla V.1,
;;; about 2 years ago.  It was couched in terms of a yacc (I think)
;;; script.  I pulled the data out of it and rewrote it as a piece
;;; of PL/1 on Multics.  Now I've moved it into an emacs-lisp
;;; form.  If the original author cares to contact me, I'd
;;; be very happy to credit you!
;;;
;;; Ian G. Batten, Batten@uk.ac.bham.multics

;;; On 1994/01/09, I discovered that rms dropped this file from the Emacs
;;; 19 distribution sometime before 19.7 was released.  He made no
;;; ChangeLog entry and didn't keep the source file around (by convention,
;;; we usually renamed files we wanted to keep but not go into official
;;; distributions so that they started with `=', e.g. `=flame.el').  This
;;; is all he had to say about it when I asked:
;;;
;;;       I think I decided I was unhappy with the legal papers for it.
;;;       Removing it took less time than trying to deal with it
;;;       any other way.
;;;
;;; I eventually found it on a backup tape, and I am now independently
;;; maintaining it.
;;;
;;; --Noah

;;; Code:

(random t)

(defvar flame-sentence
  '((how can you say that (flame-statement) \?)
    (I can\'t believe how (flame-adjective) you are\.)
    (only a (flame-der-term) like you would say that (flame-statement) \.)
    ((flame-statement) \, huh\?) (so\, (flame-statement) \?)
    ((flame-statement) \, right\?) (I mean\, (flame-sentence))
    (don\'t you realise that (flame-statement) \?)
    (I firmly believe that (flame-statement) \.)
    (let me tell you something\, you (flame-der-term) \, (flame-statement) \.)
    (furthermore\, you (flame-der-term) \, (flame-statement) \.)
    (I couldn\'t care less about your (flame-thing) \.)
    (How can you be so (flame-adjective) \?)
    (you make me sick\.)
    (it\'s well known that (flame-statement) \.)
    ((flame-statement) \.)
    (it takes a (flame-group-adj) (flame-der-term) like you to say that (flame-statement) \.)
    (I don\'t want to hear about your (flame-thing) \.)
    (you\'re always totally wrong\.)
    (I\'ve never heard anything as ridiculous as the idea that (flame-statement) \.)
    (you must be a real (flame-der-term) to think that (flame-statement) \.)
    (you (flame-adjective) (flame-group-adj) (flame-der-term) \!)
    (you\'re probably (flame-group-adj) yourself\.)
    (you sound like a real (flame-der-term) \.)
    (why\, (flame-statement) \!)
    (I have many (flame-group-adj) friends\.)
    (save the (flame-thing) s\!) (no nukes\!) (ban (flame-thing) s\!)
    (I\'ll bet you think that (flame-thing) s are (flame-adjective) \.)
    (you know\, (flame-statement) \.)
    (your (flame-quality) reminds me of a (flame-thing) \.)
    (you have the (flame-quality) of a (flame-der-term) \.)
    ((flame-der-term) \!)
    ((flame-adjective) (flame-group-adj) (flame-der-term) \!)
    (you\'re a typical (flame-group-adj) person\, totally (flame-adjective) \.)
    (man\, (flame-sentence))))

(defvar flame-sentence-loop (nconc flame-sentence flame-sentence))

(defvar flame-quality
  '((ignorance) (stupidity) (worthlessness)
    (prejudice) (lack of intelligence) (lousiness)
    (bad grammar) (lousy spelling)
    (lack of common decency) (ugliness) (nastiness)
    (subtlety) (dishonesty) ((flame-adjective) (flame-quality))))

(defvar flame-quality-loop (nconc flame-quality flame-quality))

(defvar flame-adjective
  '((ignorant) (crass) (pathetic) (sick)
    (bloated) (malignant) (perverted) (sadistic)
    (stupid) (unpleasant) (lousy) (abusive) (bad)
    (braindamaged) (selfish) (improper) (nasty)
    (disgusting) (foul) (intolerable) (primitive)
    (depressing) (dumb) (phoney) (boring)
    (gratuitous) ((flame-adjective) and (flame-adjective))
    (as (flame-adjective) as a (flame-thing))))

(defvar flame-adjective-loop (nconc flame-adjective flame-adjective))

(defvar flame-der-term
  '(((flame-adjective) (flame-der-term)) (sexist) (fascist)
    (weakling) (coward) (beast) (peasant) (racist)
    (cretin) (fool) (jerk) (ignoramus) (idiot)
    (wanker) (rat) (slimebag) (DAF driver) (quiche-eater)
    (Neanderthal) (sadist) (drunk) (capitalist)
    (wimp) (dogmatist) (wally) (maniac) (luser)
    (whimpering scumbag) (pea brain) (arsehole)
    (moron) (goof) (incompetent) (lunkhead) (Nazi)
    (SysThug) ((flame-der-term) (flame-der-term))))

(defvar flame-der-term-loop (nconc flame-der-term flame-der-term))

(defvar flame-thing
  '(((flame-adjective) (flame-thing)) (computer)
    (Honeywell dps8) (whale) (operation)
    (sexist joke) (ten-incher) (dog) (MicroVAX II)
    (source license) (real-time clock)
    (mental problem) (sexual fantasy)
    (venereal disease) (Jewish grandmother)
    (cardboard cut-out) (punk haircut) (surfboard)
    (system call) (wood-burning stove)
    (standard text editor) (processed lunch meat)
    (graphics editor) (right wing death squad)
    (disease) (vegetable) (religion) (random frob)
    (cruise missile) (bug fix) (lawyer) (copyright)
    (PAD)))

(defvar flame-thing-loop (nconc flame-thing flame-thing))


(defvar flame-group-adj
  '((gay) (old) (lesbian) (young) (black)
    (Polish) ((flame-adjective)) (white)
    (mentally retarded) (Nicaraguan) (homosexual)
    (dead) (underpriviledged) (religious)
    ((flame-thing) \-loving) (feminist) (foreign)
    (intellectual) (crazy) (working) (unborn)
    (Chinese) (short) ((flame-adjective)) (poor) (rich)
    (funny-looking) (Puerto Rican) (Mexican)
    (Italian) (communist) (fascist) (Iranian)
    (Moonie)))

(defvar flame-group-adj-loop (nconc flame-group-adj flame-group-adj))

(defvar flame-statement
  '((your (flame-thing) is great) ((flame-thing) s are fun)
    ((flame-person) is a (flame-der-term))
    ((flame-group-adj) people are (flame-adjective))
    (every (flame-group-adj) person is a (flame-der-term))
    (most (flame-group-adj) people have (flame-thing) s)
    (all (flame-group-adj) dudes should get (flame-thing) s)
    ((flame-person) is (flame-group-adj)) (trees are (flame-adjective))
    (if you\'ve seen one (flame-thing) \, you\'ve seen them all)
    (you\'re (flame-group-adj)) (you have a (flame-thing))
    (my (flame-thing) is pretty good)
    (the Martians are coming)
    (the (flame-paper) is always right)
    (just because you read it in the (flame-paper) that doesn\'t mean it\'s true)
    ((flame-person) was (flame-group-adj))
    ((flame-person) \'s ghost is living in your (flame-thing))
    (you look like a (flame-thing))
    (the oceans are full of dirty fish)
    (people are dying every day)
    (a (flame-group-adj) man ain\'t got nothing in the world these days)
    (women are inherently superior to men)
    (the system staff is fascist)
    (there is life after death)
    (the world is full of (flame-der-term) s)
    (you remind me of (flame-person)) (technology is evil)
    ((flame-person) killed (flame-person))
    (the Russians are tapping your phone)
    (the Earth is flat)
    (it\'s OK to run down (flame-group-adj) people)
    (Multics is a really (flame-adjective) operating system)
    (the CIA killed (flame-person))
    (the sexual revolution is over)
    (Lassie was (flame-group-adj))
    (the (flame-group-adj) people have really got it all together)
    (I was (flame-person) in a previous life)
    (breathing causes cancer)
    (it\'s fun to be really (flame-adjective))
    ((flame-quality) is pretty fun) (you\'re a (flame-der-term))
    (the (flame-group-adj) culture is fascinating)
    (when ya gotta go ya gotta go)
    ((flame-person) is (flame-adjective))
    ((flame-person) \'s (flame-quality) is (flame-adjective))
    (it\'s a wonderful day)
    (everything is really a (flame-thing))
    (there\'s a (flame-thing) in (flame-person) \'s brain)
    ((flame-person) is a cool dude)
    ((flame-person) is just a figment of your imagination)
    (the more (flame-thing) s you have, the better)
    (life is a (flame-thing)) (life is (flame-quality))
    ((flame-person) is (flame-adjective))
    ((flame-group-adj) people are all (flame-adjective) (flame-der-term) s)
    ((flame-statement) \, and (flame-statement))
    ((flame-statement) \, but (flame-statement))
    (I wish I had a (flame-thing))
    (you should have a (flame-thing))
    (you hope that (flame-statement))
    ((flame-person) is secretly (flame-group-adj))
    (you wish you were (flame-group-adj))
    (you wish you were a (flame-thing))
    (I wish I were a (flame-thing))
    (you think that (flame-statement))
    ((flame-statement) \, because (flame-statement))
    ((flame-group-adj) people don\'t get married to (flame-group-adj) people because (flame-reason))
    ((flame-group-adj) people are all (flame-adjective) because (flame-reason))
    ((flame-group-adj) people are (flame-adjective) \, and (flame-reason))
    (you must be a (flame-adjective) (flame-der-term) to think that (flame-person) said (flame-statement))
    ((flame-group-adj) people are inherently superior to (flame-group-adj) people)
    (God is Dead)))

(defvar flame-statement-loop (nconc flame-statement flame-statement))


(defvar flame-paper
  '((Daily Mail) (Daily Express) (Boston Glob)
    (Centre Bulletin) (Sun) (Daily Mirror) (Pravda)
    (Daily Telegraph) (Beano) (Multics Manual)))

(defvar flame-paper-loop (nconc flame-paper flame-paper))


(defvar flame-person
  '((Reagan) (Ken Thompson) (Dennis Ritchie)
    (JFK) (the Pope) (Gadaffi) (Napoleon)
    (Karl Marx) (Groucho) (Michael Jackson)
    (Caesar) (Nietzsche) (Heidegger) (\"Head-for-the-mountains\" Bush)
    (Henry Kissinger) (Nixon) (Castro) (Thatcher)
    (Attilla the Hun) (Alaric the Visigoth) (Hitler)))

(defvar flame-person-loop (nconc flame-person flame-person))

(defvar flame-reason
  '((they don\'t want their children to grow up to be too lazy to steal)
    (they can\'t tell them apart from (flame-group-adj) dudes)
    (they\'re too (flame-adjective))
    ((flame-person) wouldn\'t have done it)
    (they can\'t spray paint that small)
    (they don\'t have (flame-thing) s) (they don\'t know how)
    (they can\'t afford (flame-thing) s)))

(defvar flame-reason-loop (nconc flame-reason flame-reason))


(defmacro flame-define-element (name)
  (let ((loop-to-use (intern (concat name "-loop"))))
    (` (defun (, (intern name)) nil
         (let ((step-forward (% (random) 10)))
           (if (< step-forward 0) (setq step-forward (- step-forward)))
           (prog1
               (nth step-forward (, loop-to-use))
             (setq (, loop-to-use) (nthcdr (1+ step-forward) (, loop-to-use)))))))))

(flame-define-element "flame-sentence")
(flame-define-element "flame-quality")
(flame-define-element "flame-adjective")
(flame-define-element "flame-der-term")
(flame-define-element "flame-group-adj")
(flame-define-element "flame-statement")
(flame-define-element "flame-thing")
(flame-define-element "flame-paper")
(flame-define-element "flame-person")
(flame-define-element "flame-reason")

(defun *flame nil
  (flame-expand '(flame-sentence)))

(defun flame-expand (object)
  (cond ((atom object)
         object)
        (t (mapcar 'flame-expand (funcall (car object))))))

(defun flame-flatten (list)
  (cond ((atom list)
         (list list))
        ((null list))
        (t (apply 'append (mapcar 'flame-flatten list)))))

;;;###autoload
(defun flame (&optional arg)
  "Generate ARG (default 1) sentences of half-crazed gibberish.
If interactive, print the result in a buffer and display it.
Otherwise, just return the result as a string."
  (interactive "p")
  (or arg (setq arg 1))
  (if (interactive-p)
      (let ((w (selected-window)))
        (pop-to-buffer (get-buffer-create "*Flame*"))
        (goto-char (point-max))
        (insert ?\n)
        (flame2 arg)
        (select-window w))
    (let (result)
      (while (> arg 0)
        (setq result (concat result
                             (flame-string)
                             (if (= 1 arg) "" "\n")))
        (setq arg (1- arg)))
      result)))

(defun flame2 (arg)
  (let ((start (point)))
    (flame1 arg)
    (fill-region-as-paragraph start (point) t)))

(defun flame1 (arg)
  (cond ((zerop arg) t)
        (t (insert (flame-string))
           (flame1 (1- arg)))))

(defun flame-string ()
  (concat (flame-sentence-ify
           (flame-string-ify
            (flame-append-suffixes-hack
             (flame-flatten (*flame)))))))

(defun flame-sentence-ify (string)
  (concat (upcase (substring string 0 1))
          (substring string 1 (length string))
          "  "))

(defun flame-string-ify (list)
  (mapconcat
   '(lambda (x)
      (format "%s" x))
   list
   " "))

(defun flame-append-suffixes-hack (list)
  (cond ((null list)
         nil)
        ((memq (nth 1 list)
               '(\? \. \, s\! \! s \'s \-loving))
         (cons (intern (format "%s%s" (nth 0 list) (nth 1 list)))
               (flame-append-suffixes-hack (nthcdr 2 list))))
        (t (cons (nth 0 list)
                 (flame-append-suffixes-hack (nthcdr 1 list))))))

(defun psychoanalyze-flamer ()
  "Mr. Angry goes to the analyst."
  (interactive)
  (doctor)                              ; start the psychotherapy
  (message "")
  (switch-to-buffer "*doctor*")
  (sit-for 0)
  (while (not (input-pending-p))
    (flame2 (if (= (% (random) 2) 0) 2 1))
    (insert "\n")
    (sit-for 0)
    (doctor-ret-or-read 1)))

(provide 'flame)

;;; flame.el ends here
