(setq erbot-rss-file-name "~/pub//tmp/erbot.rss"
      erbot-rss-rc-file-name "~/pub/tmp/erbot-rc.txt"
      erbot-rss-max-age 604800 ; 7 days
      erbot-rss-item-resource-prefix "prefix://"
      erbot-rss-rdf:about "rss about"
      erbot-rss-title "title"
      erbot-rss-link "link"
      erbot-rss-description "description"
      erbot-rss-dc:rights "rights"
      erbot-rss-dc:publisher "publisher"
      erbot-rss-dc:contributor "contributor"
      erbot-rss-image "image"
      erbot-rss-image-title "image title"
      erbot-rss-image-link "image link")



(defun erbot-rss-add (nick channel term entry-num entry)
  "erbot-notify-add-functions"
  (erbot-rss-rc-add term
                    (format "Added entry %i of %s: %s" entry-num term entry)
                    (format "%s in %s" nick channel)))

(defun erbot-rss-forget (nick channel term entry-num entry)
  "erbot-notify-forget-functions"
  (erbot-rss-rc-add term
                    (if (not (eq entry-num 'all))
                        (format "Forgot entry %i of %s: %s" entry-num term entry)
                      (format "Forgot %s:\n\n%s"
                              term
                              (mapconcat #'identity entry "\n")))
                    (format "%s in %s" nick channel)))

(defun erbot-rss-move (nick channel old-term new-term)
  "erbot-notify-move-functions"
  (erbot-rss-rc-add old-term
                    (format "Moved %s to %s" old-term new-term)
                    (format "%s in %s" nick channel)))

(defun erbot-rss-rearrange (nick channel term
                                 from-num from-entry
                                 to-num to-entry)
  "erbot-notify-rearrange-functions"
  (erbot-rss-rc-add term
                    (format "Swapped entries %i and %i in term %s. Now:\n%i: %s\n%i: %s"
                            from-num to-num term
                            to-num from-entry
                            from-num to-entry)
                    (format "%s in %s" nick channel)))

(defun erbot-rss-substitute (nick channel term entry-num old-entry new-entry)
  "erbot-notify-substitue-functions"
  (erbot-rss-rc-add term
                    (format "Changed entry %i of %s:\nOld: %s\nNew: %s"
                            entry-num term old-entry new-entry)
                    (format "%s in %s" nick channel)))

(defun erbot-rss-merge (nick channel old-term new-term new-entries)
  "erbot-notify-merge-functions"
  (erbot-rss-rc-add term
                    (format "Merged %s into %s. New contents:\n%s"
                            old-term new-term
                            (mapconcat #'identity new-entries "\n"))
                    (format "%s in %s" nick channel)))


;;; Recent Changes
(defun erbot-rss-rc-add (term description contributor)
  "Add this item to the recent changes list.
The list is managed in `erbot-rss-rc-file-name'."
  (with-current-buffer (find-file-noselect erbot-rss-rc-file-name)
    (goto-char (point-min))
    (when (= (point-min) (point-max))
      (insert "()"))
    (let* ((olddata (read (current-buffer)))
           (newdata (erbot-rss-rc-remove-old
                     (append olddata
                             (list
                              (make-erbot-rss-item term
                                                   description
                                                   (current-time)
                                                   contributor))))))
      (delete-region (point-min) (point-max))
      (prin1 newdata (current-buffer))
      (let ((require-final-newline t))
        (save-buffer))
      (erbot-rss-regenerate-rss newdata))))

(defun erbot-rss-rc-remove-old (items)
  "Remove any items from ITEMS that are older then `erbot-rss-max-age'."
  (let ((new '()))
    (while items
      (when (< (- (float-time)
                  (float-time (erbot-rss-item-time (car items))))
               erbot-rss-max-age)
        (setq new (cons (car items)
                        new)))
      (setq items (cdr items)))
    (reverse new)))


;;; RSS
(defun erbot-rss-regenerate-rss (items)
  "Regenerate the RSS feed from ITEMS.
The feed is put into `erbot-rss-file-name'."
  (with-current-buffer (find-file-noselect erbot-rss-file-name)
    (delete-region (point-min) (point-max))
    (erbot-rss-insert-rss items)
    (let ((require-final-newline t))
      (save-buffer))))

(defun erbot-rss-insert-rss (items)
  "Insert an RSS feed with ITEMS in it.
ITEMS should be a list of vectors, each vector having four elements:

- Title
- Description
- Contributor
- Timestamp in seconds since the epoch"
  (sxml-insert
   `((rdf:RDF (@ (xmlns:rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                 (xmlns "http://purl.org/rss/1.0/")
                 (xmlns:dc "http://purl.org/dc/elements/1.1/"))
      (channel (@ (rdf:about ,erbot-rss-rdf:about))
       (title ,erbot-rss-title)
       (link ,erbot-rss-link)
       (description ,erbot-rss-description)
       (dc:rights ,erbot-rss-dc:rights)
       (dc:date ,(erbot-rss-date))
       (dc:publisher ,erbot-rss-dc:publisher)
       (dc:contributor ,erbot-rss-dc:contributor)
       (items
        (rdf:Seq
         ,@(mapcar (lambda (item)
                     `(rdf:li (@ (rdf:resource
                                  ,(erbot-rss-item-resource item)))))
                   items)))
       (image (@ (rdf:resource ,erbot-rss-image))))

      (image (@ (rdf:resource ,erbot-rss-image))
       (title ,erbot-rss-image-title)
       (url ,erbot-rss-image)
       (link ,erbot-rss-image-link))

      ,@(mapcar #'erbot-rss-item items)))))

(defun erbot-rss-item (item)
  "Insert the RSS description of ITEM."
  `(item (@ (rdf:about ,(erbot-rss-item-resource item)))
    (title ,(erbot-rss-item-title item))
    ;(link ,(erbot-rss-item-resource item))
    (description ,(erbot-rss-item-description item))
    (dc:date ,(erbot-rss-date (erbot-rss-item-time item)))
    (dc:contributor ,(erbot-rss-item-contributor item))))

(defun make-erbot-rss-item (title description time contributor)
  "Create a new rss item entry."
  (vector title description time contributor))

(defun erbot-rss-item-title (item)
  "Return the title of ITEM."
  (aref item 0))

(defun erbot-rss-item-description (item)
  "Return the description of ITEM."
  (aref item 1))

(defun erbot-rss-item-time (item)
  "Return the modification time of ITEM."
  (aref item 2))

(defun erbot-rss-item-contributor (item)
  "Return the contributor of ITEM."
  (aref item 3))

(defun erbot-rss-item-resource (item)
  "Return the resource of ITEM.
This uses `erbot-rss-item-resource-prefix'."
  (concat erbot-rss-item-resource-prefix
          (erbot-rss-item-title item)
          "?" (erbot-rss-date (erbot-rss-item-time item))))

(defun erbot-rss-date (&optional time)
  "Return a string describing TIME, or the current time if nil."
  (format-time-string "%Y-%m-%dT%H:%M:%S+00:00"
                      (or time
                          (current-time))
                      t))


;;; SXML

(defun sxml-insert (data)
  "Insert an SXML data structure DATA."
  (set-buffer-file-coding-system 'utf-8)
  (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
  (sxml-insert-data data))

(defun sxml-insert-data (data)
  "Insert a list of tags DATA as SXML."
  (cond
   ((stringp data)
    (insert (sxml-quote data)))
   ((symbolp (car data))
    (sxml-insert-tag data))
   (t
    (mapcar #'sxml-insert-data data))))

(defun sxml-insert-tag (tag)
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
                      (sxml-quote (symbol-name (car entry)))
                      "=\""
                      (sxml-quote (cadr entry))
                      "\""))
            attributes)
    (if (null body)
        (insert "/>")
      (insert ">")
      (mapcar #'sxml-insert-data body)
      (insert "</"
              (sxml-quote name)
              "\n>"))))

(defun sxml-quote (string)
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
