;;; erbrss.el --- Provide an RSS feed from your erbot.
;; Time-stamp: <2005-01-01 17:30:49 forcer>
;; Copyright (C) 2004 Jorgen Schaefer
;; Emacs Lisp Archive entry
;; Filename: erbrss.el
;; Package: erbrss
;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErBot

;;; Commentary:

;; This extension to erbot will provide an RSS feed for your database
;; changes. Customize the erbrss group and run (erbrss-install) to
;; use.

;;; Code:

(defgroup erbrss nil
  "RSS feeds for the erbot."
  :group 'erbot)

(defcustom erbrss-file-name "/tmp/erbot.rss"
  "The file name for the RSS feed. This should be in your web
directory."
  :type 'file
  :group 'erbrss)

(defcustom erbrss-rc-file-name "/tmp/erbot-rc.txt"
  "The file name to store recent changes info in."
  :type 'file
  :group 'erbrss)

(defcustom erbrss-max-age 604800        ; 7 days
  "The number of seconds an entry in the recent changes should
stay."
  :type 'integer
  :group 'erbrss)

(defcustom erbrss-item-resource-prefix "prefix://"
  "The prefix for your item resources. This should be somewhere
on your webserver."
  :type 'string
  :group 'erbrss)

(defcustom erbrss-rdf:about "rss about"
  "The contents of the rdf:about attribute in your RSS feed."
  :type 'string
  :group 'erbrss)

(defcustom erbrss-title "title"
  "The title of your RSS feed."
  :type 'string
  :group 'erbrss)

(defcustom erbrss-link "link"
  "The link to your bots homepage, or the RSS feed, or wherever."
  :type 'string
  :group 'erbrss)

(defcustom erbrss-description "description"
  "The description of your RSS feed."
  :type 'string
  :group 'erbrss)

(defcustom erbrss-dc:rights "rights"
  "The copyright notice for your RSS feed."
  :type 'string
  :group 'erbrss)

(defcustom erbrss-dc:publisher "publisher"
  "The publisher of your RSS feed, i.e. you."
  :type 'string
  :group 'erbrss)

(defcustom erbrss-dc:contributor "contributor"
  "The contributors to your RSS feed. The users of the bot."
  :type 'string
  :group 'erbrss)

(defcustom erbrss-image "image"
  "A link to an image for your RSS feed."
  :type 'string
  :group 'erbrss)

(defcustom erbrss-image-title "image title"
  "A title for your RSS feed image."
  :type 'string
  :group 'erbrss)

(defcustom erbrss-image-link "image link"
  "A link for your image. This should point to your bots home page or so."
  :type 'string
  :group 'erbrss)



;;; The erbot interface

(defun erbrss-install ()
  "Initializer the RSS module of erbot."
  (add-hook 'erbot-notify-add-functions 'erbrss-add)
  (add-hook 'erbot-notify-forget-functions 'erbrss-forget)
  (add-hook 'erbot-notify-move-functions 'erbrss-move)
  (add-hook 'erbot-notify-rearrange-functions 'erbrss-rearrange)
  (add-hook 'erbot-notify-substitute-functions 'erbrss-substitute)
  (add-hook 'erbot-notify-merge-functions 'erbrss-merge))

(defun erbrss-add (nick channel term entry-num entry)
  "Note an addition to the erbot database.
This is suitable for `erbot-notify-add-functions'."
  (erbrss-rc-add term
                 (format "Added entry %i of %s: %s" entry-num term entry)
                 (format "%s in %s" nick channel)))

(defun erbrss-forget (nick channel term entry-num entry remaining-entries)
  "Note a removal from the erbot database.
This is suitable for `erbot-notify-forget-functions'."
  (erbrss-rc-add term
                 (if (not (eq entry-num 'all))
                     (format "Forgot entry %i of %s: %s\n\nRemaining:\n%s"
                             entry-num
                             term
                             entry
                             (mapconcat #'identity remaining-entries "\n"))
                   (format "Forgot %s:\n\n%s"
                           term
                           (mapconcat #'identity entry "\n")))
                 (format "%s in %s" nick channel)))

(defun erbrss-move (nick channel old-term new-term)
  "Note a move within the erbot database.
This is suitable for `erbot-notify-move-functions'."
  (erbrss-rc-add old-term
                 (format "Moved %s to %s" old-term new-term)
                 (format "%s in %s" nick channel)))

(defun erbrss-rearrange (nick channel term
                              from-num from-entry
                              to-num to-entry)
  "Note a rearrangement in the erbot database.
This is suitable for `erbot-notify-rearrange-functions'."
  (erbrss-rc-add term
                 (format "Swapped entries %i and %i in term %s. Now:\n%i: %s\n%i: %s"
                         from-num to-num term
                         to-num from-entry
                         from-num to-entry)
                 (format "%s in %s" nick channel)))

(defun erbrss-substitute (nick channel term entry-num old-entry new-entry)
  "Note a substitution in the erbot database.
This is suitable for `erbot-notify-substitue-functions'."
  (erbrss-rc-add term
                 (format "Changed entry %i of %s:\nOld: %s\nNew: %s"
                         entry-num term old-entry new-entry)
                 (format "%s in %s" nick channel)))

(defun erbrss-merge (nick channel from-term to-term
                     from-entries to-entries final-entries)
  "Note a merge in the erbot database.
This is suitable for `erbot-notify-merge-functions'."
  (erbrss-rc-add
   term
   (format (concat "Merged %s into %s. New contents:\n"
                   "(1 means from %s, 2 from %s and + from both)\n"
                   "%s")
           old-term new-term
           old-term new-term
           (erbrss-merge-description from-entries
                                     to-entries
                                     final-entries))
   (format "%s in %s" nick channel)))

(defun erbrss-merge-description (from-entries to-entries final-entries)
  "Return a string describing the merge. The string contains a
line per entry in FINAL-ENTRIES, prefixed with a 1 if that
entry is from FROM-ENTRIES, 2 if it is from TO-ENTRIES, and +
if it is from both."
  (mapconcat (lambda (entry)
               (format "%s %s"
                       (let ((fromp (member entry from-entries))
                             (top (member entry to-entries)))
                         (cond
                          ((and fromp top) "+")
                          (fromp           "1")
                          (top             "2")
                          (t               "?")))
                       entry))
             final-entries
             "\n"))


;;; Recent Changes
(defun erbrss-rc-add (term description contributor)
  "Add this item to the recent changes list.
The list is managed in `erbrss-rc-file-name'."
  (with-current-buffer (find-file-noselect erbrss-rc-file-name t)
    (goto-char (point-min))
    (when (= (point-min) (point-max))
      (insert "()"))
    (let* ((olddata (read (current-buffer)))
           (newdata (erbrss-rc-remove-old
                     (append olddata
                             (list
                              (erbrss-make-item term
                                                description
                                                (current-time)
                                                contributor))))))
      (delete-region (point-min) (point-max))
      (prin1 newdata (current-buffer))
      (let ((require-final-newline t))
        (save-buffer))
      (erbrss-regenerate-rss newdata))))

(defun erbrss-rc-remove-old (items)
  "Remove any items from ITEMS that are older then `erbrss-max-age'."
  (let ((new '()))
    (while items
      (when (< (- (float-time)
                  (float-time (erbrss-item-time (car items))))
               erbrss-max-age)
        (setq new (cons (car items)
                        new)))
      (setq items (cdr items)))
    (reverse new)))


;;; RSS
(defun erbrss-regenerate-rss (items)
  "Regenerate the RSS feed from ITEMS.
The feed is put into `erbrss-file-name'."
  (with-current-buffer (find-file-noselect erbrss-file-name t)
    (delete-region (point-min) (point-max))
    (erbrss-insert-rss items)
    (let ((require-final-newline t))
      (save-buffer))))

(defun erbrss-insert-rss (items)
  "Insert an RSS feed with ITEMS in it.
ITEMS should be a list of vectors, each vector having four elements:

- Title
- Description
- Contributor
- Timestamp in seconds since the epoch"
  (erbrss-sxml-insert
   `((rdf:RDF (@ (xmlns:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                 (xmlns "http://purl.org/rss/1.0/")
                 (xmlns:dc "http://purl.org/dc/elements/1.1/"))
              (channel (@ (rdf:about ,erbrss-rdf:about))
                       (title ,erbrss-title)
                       (link ,erbrss-link)
                       (description ,erbrss-description)
                       (dc:rights ,erbrss-dc:rights)
                       (dc:date ,(erbrss-date))
                       (dc:publisher ,erbrss-dc:publisher)
                       (dc:contributor ,erbrss-dc:contributor)
                       (items
                        (rdf:Seq
                         ,@(mapcar (lambda (item)
                                     `(rdf:li (@ (rdf:resource
                                                  ,(erbrss-item-resource item)))))
                                   items)))
                       (image (@ (rdf:resource ,erbrss-image))))

              (image (@ (rdf:resource ,erbrss-image))
                     (title ,erbrss-image-title)
                     (url ,erbrss-image)
                     (link ,erbrss-image-link))

              ,@(mapcar #'erbrss-item items)))))

(defun erbrss-item (item)
  "Insert the RSS description of ITEM."
  `(item (@ (rdf:about ,(erbrss-item-resource item)))
         (title ,(erbrss-item-title item))
                                        ;(link ,(erbrss-item-resource item))
         (description ,(erbrss-item-description item))
         (dc:date ,(erbrss-date (erbrss-item-time item)))
         (dc:contributor ,(erbrss-item-contributor item))))

(defun erbrss-make-item (title description time contributor)
  "Create a new rss item entry."
  (vector title description time contributor))

(defun erbrss-item-title (item)
  "Return the title of ITEM."
  (aref item 0))

(defun erbrss-item-description (item)
  "Return the description of ITEM."
  (aref item 1))

(defun erbrss-item-time (item)
  "Return the modification time of ITEM."
  (aref item 2))

(defun erbrss-item-contributor (item)
  "Return the contributor of ITEM."
  (aref item 3))

(defun erbrss-item-resource (item)
  "Return the resource of ITEM.
This uses `erbrss-item-resource-prefix'."
  (concat erbrss-item-resource-prefix
          (erbrss-item-title item)
          "?" (erbrss-date (erbrss-item-time item))))

(defun erbrss-date (&optional time)
  "Return a string describing TIME, or the current time if nil."
  (format-time-string "%Y-%m-%dT%H:%M:%S+00:00"
                      (or time
                          (current-time))
                      t))


;;; SXML

(defun erbrss-sxml-insert (data)
  "Insert an SXML data structure DATA."
  (set-buffer-file-coding-system 'utf-8)
  (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
  (erbrss-sxml-insert-data data))

(defun erbrss-sxml-insert-data (data)
  "Insert a list of tags DATA as SXML."
  (cond
   ((stringp data)
    (insert (erbrss-sxml-quote data)))
   ((symbolp (car data))
    (erbrss-sxml-insert-tag data))
   (t
    (mapcar #'erbrss-sxml-insert-data data))))

(defun erbrss-sxml-insert-tag (tag)
  (let ((name (symbol-name (car tag)))
        (attributes (if (and (consp (cdr tag))
                             (consp (cadr tag))
                             (eq '@ (caadr tag)))
                        (cdadr tag)
                      '()))
        (body (if (and (consp (cdr tag))
                       (consp (cadr tag))
                       (eq '@ (caadr tag)))
                  (cddr tag)
                (cdr tag))))
    (insert "<" name)
    (mapcar (lambda (entry)
              (insert " "
                      (erbrss-sxml-quote (symbol-name (car entry)))
                      "=\""
                      (erbrss-sxml-quote (cadr entry))
                      "\""))
            attributes)
    (if (null body)
        (insert "/>")
      (insert ">")
      (mapcar #'erbrss-sxml-insert-data body)
      (insert "</"
              (erbrss-sxml-quote name)
              "\n>"))))

(defun erbrss-sxml-quote (string)
  "Quote <, > and & in STRING."
  (with-temp-buffer
    (mapcar (lambda (char)
              (cond
               ((char-equal char ?&) (insert "&amp;"))
               ((char-equal char ?<) (insert "&lt;"))
               ((char-equal char ?>) (insert "&gt;"))
               (t (insert char))))
            string)
    (buffer-substring (point-min) (point-max))))

(provide 'erbrss)
;;; erbrss.el ends here
