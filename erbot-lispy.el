;;; erbot-lispy.el --- ErBot integration in Lispy
;; Time-stamp: <2006-04-20 14:14:33 deego>
;; Emacs Lisp Archive entry
;; Filename: erbot-lispy.el
;; Package: erbot
;; Authors:  Yann Hodique <hodique@lifl.fr>
;; Version: 0.0
;; URL:  http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot

;; Lispy can be found at http://mtpforge.melting-pot.org/projects/lispy

;; Installation
;; put an additional (require 'erbot-lispy) in you erbot's .emacs
;; *before* running (erbot-install)
;; then launch a lispy session

(require 'lispy)
(require 'erbot)

(defvar backup-buffer nil)

(defun erbot-lispy-remote (line)
   (let* ((nick nil)
          (tgt nil)
          (msg nil))

     (cond
      ((string-match "^<Mtp> \\(\\w+\\) tells you: \\(.*\\)$" line)
       (setq nick (match-string 1 line))
       (setq tgt nick)
       (setq msg (match-string 2 line)))
      ((string-match (format "^<\\(Mtp\\|%s\\)>.*$" lispy-remote-user) line)
       nil)
      ((string-match "^<\\(\\w+\\)> \\(.*\\)$" line)
       (setq nick (match-string 1 line))
       (setq tgt "#chan")
       (setq msg (match-string 2 line)))
      )
     (setq backup-buffer (current-buffer))
     (when (and lispy-connected nick)
       (progn
         (setq erbot-end-user-nick-latest nick)
         (setq fs-tgt tgt)
         (setq erbn-tgt tgt)

         (setq fs-nick nick)
         (setq erbn-nick nick)

         (let ((msgg
                (erbeng-main msg nil nick tgt nil (list nick nick nick))))

           (cond
            (erbot-quiet-p nil)
            ((and erbot-quiet-target-p-function
                  (funcall erbot-quiet-target-p-function tgt nick msg))
             nil)
            (t (erbot-lispy-reply msgg tgt)))
           ))))
   nil
   )

(defun erbot-lispy-reply (main-reply tgt)
  (unless (stringp main-reply)
    (setq main-reply (format "%S" main-reply)))
  (let ((reply (erbot-frob-with-init-string main-reply)))
    (unless
	(or
	 (null erbot-reply-p)
	 (equal main-reply 'noreply)
	 (equal main-reply "noreply"))
      ;; now we are actually gonna reply.
      (setq reply (fs-limit-lines reply))
      (set-buffer backup-buffer)
      (let ((lines (split-string reply "\n")))
        (mapc
         (lambda (line)
           (lispy-message (concat (if (string-match "^#" tgt)
				      (if (erbot-lispy-safe-p line) "" " ")
				    (format "tell %s " tgt)) line "\n")))
         lines)))))

;; Mtp does not use prefixed commands, activate the right ones via aliases
(defun erbot-lispy-safe-p (msg)
  (string-match "^/" line))

(defadvice erbot-install (after ad-erbot-install-lispy-after act)
  (add-hook 'lispy-post-insert-hook 'erbot-lispy-remote))

(provide 'erbot-lispy)
