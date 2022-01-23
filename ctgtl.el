;;; ctgtl.el Quick track of time spent on tasks -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Pedro Abelleira Seco
;;
;; Author: Pedro Abelleira Seco <https://github.com/pedroabelleiraseco>
;; Maintainer: Pedro Abelleira Seco <coutego@gmail.com>
;; Created: December 27, 2021
;; Modified: December 27, 2021
;; Version: 0.0.1
;; Keywords: convenience outlines
;; Homepage: https://github.com/coutego/ctgtl
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;;
;; Org mode offers very advance capabilities for logging time spent on different
;; tasks and produce reports on it. Unfortunately, those are unusable for my use case,
;; so I decided to create a helper package for personal use to create prefefined
;; commands to quickly log time.
;;
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'ht)
(require 'org)
(require 'org-element)
(require 'evil-commands)
(require 'org-ml)
(require 'hydra)
(require 'ts)

(defvar ctgtl-timestamp-format "%Y-%m-%d %H:%M:%S.%2N")
(defvar ctgtl-directory (f-join org-directory "ctgtl"))

(defun ctgtl-add-todo ()
  "Add a todo subheading to the current element in the log"
  (interactive)
  (let* ((bf (ctgtl--current-filename))
         (b  (find-file bf)))
    (with-current-buffer b
      (goto-char (point-max))
      (insert "\n** TODO ")
      (evil-append 1))))

(defun ctgtl-add-note ()
  "Add a note to the current element in the log"
  (interactive)
  (let* ((bf (ctgtl--current-filename))
         (b  (find-file bf))
         (w  (get-buffer-window b)))
    (with-current-buffer b
      (goto-char (point-max))
      (insert "\n** ")
      (evil-append 1))))

(defun ctgtl-go-current()
  "Go to the current task (end of the log file)."
  (interactive)
  (let* ((bf (ctgtl--current-filename))
         (b  (find-file bf))
         (w  (get-buffer-window b)))
    (with-current-buffer b
      (goto-char (point-max)))))

(cl-defun ctgtl-add-entry (&rest entry)
  "Add a new entry in the log.

The arguments should be a plist with keys :project, :type, :title."
  (let ((b (find-file-noselect (ctgtl--current-filename))))
    (with-current-buffer b
      (goto-char (point-max))
      (insert "\n\n")
      (insert (apply #'ctgtl--create-entry entry))
      (insert "\n")
      (save-buffer)
      (goto-char (max-char)))))

(cl-defun ctgtl--create-entry-props (entry)
  "Create the properties section for a given entry."
  (->> entry
       ht<-plist
       ((lambda (x) (ht-remove x :body) x)) ; remove :body from the properties
       (ht-amap (when value
                  (format ":%s: %s"
                          (format "CTGTL-%s" (ctgtl--keyword-to-string key))
                          value)))
       (--reduce (if it (s-concat acc "\n" it) acc))))

(defun ctgtl--keyword-to-string (key)
  "Convert a key to a string (upper case, without the ':')."
  (->> key
       (format "%s")
       (s-chop-prefix ":")
       upcase))

(cl-defun ctgtl--create-entry (&rest entry)
  "Create an entry from the given list of properties."
  (let* ((timestamp (ctgtl-create-timestamp))
         (id        (ctgtl--create-id))
         (title     (or (plist-get entry :title)
                        "Time log entry"))
         (tags      (or (plist-get entry :tags) ""))
         (body      (-if-let (body (plist-get entry :body)) body ""))
         (entry     (-concat (list :id id :timestamp timestamp) entry))
         (props     (ctgtl--create-entry-props entry)))
    ;; FIXME: check how many \n do we need to add below
    (format "* %s %s\n:PROPERTIES:\n%s\n:END:\n%s" title tags props body)))

(defun ctgtl-create-timestamp ()
  "Create a timestamp to be logged."
  (format-time-string ctgtl-timestamp-format))

(defun ctgtl--create-id ()
  "Create a new (unique) entry id."
  (format "%s%s"
          (upcase (s-word-initials (s-dashed-words (system-name))))
          (format-time-string "%Y%m%d%H%M%S%3N")))

(defun ctgtl--current-filename ()
  "Return the filename for the current log file."
  (let* ((name (format "%s.org" (format-time-string "%Y-%m-%d")))
         (year-month (format-time-string "%Y-%m")))
    (f-join ctgtl-directory year-month name)))

(defun ctgtl--filter-headline-period (h period)
  "Filter function for headlines and a given period.

Return t if the heading H is inside PERIOD."
  (if period
      (-let [(start end) period]
        (let* ((start (ts-apply :hour 0 :minute 0 :second 0 start))
               (end   (ts-apply :hour 23 :minute 59 :second 59.999 end))
               (time  (if-let ((tm (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" h)))
                          (ts-parse tm)
                        nil)))
          (ts-format start);;
          (ts-format end)  ;; FIXME: remove
          (ts-format time) ;;
          (and time (ts<= start time) (ts>= end time))))
    t)) ;; else t

(cl-defun ctgtl--calculate-duration (p)
  "Calculate duration of a given period P."
  (let* ((t1 (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" (car p)))
         (t2 (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" (cadr p)))
         (td (and t2
                  (time-subtract (apply #'encode-time (parse-time-string t2))
                                 (apply #'encode-time (parse-time-string t1)))))
         (ft (format "%s" (if td (float-time td) 0))))
    (org-ml-headline-set-node-property "CTGTL-DURATION" ft (car p))))

(defun ctgtl--parse-buffer-timestamp-sorter (h1 h2)
  "Sort function for headings, based on their timestamps.

Return t if h2 is posterior to h1"
  (let ((t1 (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" h1))
        (t2 (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" h2)))
    (string< t1 t2)))

;;; Org export
(defun ctgtl-export-org (period groups &optional file)
  "Export the logged time to ORG.

PERIOD is a list of two elements corresponding to the start and end dates.
These two elements must be dates generated with the ts library.
GROUPS is a list of criteria to group the results by. Each of the elements
of GROUPS must be either a string or a cons cell with a string describing
the group as the first element and a function that, given an element, gives
the value to group by."
  (let ((file (or file (read-file-name "Select output file: " "~" "export.org" nil))))
    (if (and file period)
        (message "Wrote %s lines"
                 (or (ctgtl--export-org-impl file groups period)
                     0))
      (message "Export cancelled"))))

(defun ctgtl--export-org-impl (file groups period)
  "Export the log entries for the given PERIOD to FILE, grouped as GROUPS.
Return the number of lines written to the file"
  (->> (ctgtl--get-entries-period period)
       (ctgtl--group-entries groups)
       (ctgtl--grouped-entries-to-org-text)
       (ctgtl--write-file file)))

(defun ctgtl--get-entries-period (period)
  "Return all the log entries for the given PERIOD, as headlines."
  (->> period
       (ctgtl--find-files-period)
       (ctgtl--concatenate-file-contents)
       (ctgtl--org-string-to-headlines)
       (--filter (ctgtl--date-in-period-p
                  (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" it)
                  period))))

(defun ctgtl--get-entries-period-with-duration (period)
  "Same as ctgtl--get-entries-period, but it add duration to the header.

Thid function assumes that the last entry is a break and can be dropped,
so it does."
  (->> (ctgtl--get-entries-period period)
       (-sort #'ctgtl--parse-buffer-timestamp-sorter)
       ((lambda (xs) (-interleave xs (cdr xs))))
       (-partition 2)
       (-map #'ctgtl--calculate-duration)))

(defun ctgtl--date-in-period-p (timestamp period)
  "Checks if the string TIMESTAMP falls in the indicated PERIOD (a list)."
  (let* ((start (ctgtl--dt-or-ins-to-strdate (car period)))
         (end   (ctgtl--dt-or-ins-to-strdate (cadr period)))
         (dt    (ctgtl--dt-or-ins-to-strdate (or timestamp ""))))
    (and (or (s-less-p start dt) (s-equals-p start dt))
         (or (s-less-p dt end) (s-equals-p dt end)))))

(defun ctgtl--org-string-to-headlines (s)
  "Parse the string s and return a list of headlines."
  (with-temp-buffer
    (insert s)
    (org-ml-parse-headlines 'all)))

(defun ctgtl--find-files-period (period)
  "Find the log files for the given PERIOD (a list with two dates)."
  (-let* ((st   (ctgtl--dt-or-ins-to-strdate (car period)))
          (en   (ctgtl--dt-or-ins-to-strdate (cadr period)))
          (_err (when (not (or (s-less-p st en) (s-equals-p st en)))
                  (error (format "'%s' is not less or equal than '%s" st en))))
          (sst  (s-left 7 st))
          (sen  (s-left 7 en))
          (dis  (f-entries
                 ctgtl-directory
                 (lambda (d)
                   (let ((dir (f-filename d)))
                     (and (f-dir-p d)
                          (s-matches-p "^[0-9]\\{4\\}\\-[0-9]\\{2\\}$" dir)
                          (or (s-less-p sst dir) (s-equals-p sst dir))
                          (or (s-less-p dir sen) (s-equals-p dir sen)))))
                 nil)))
    (->> dis
         (--map (ctgtl--find-files-period-dir st en it))
         (apply #'-concat))))

(defun ctgtl--find-files-period-dir (start end dir)
  "Return the list of files containing log entries in the given period."
  (let ((dlessp  (lambda (s1 s2)
                   (let ((ss1 (s-left 10 s1))
                         (ss2 (s-left 10 s2)))
                     (or (s-equals-p ss1 ss2)
                         (s-less-p ss1 ss2))))))
    (f-files dir
             (lambda (f)
               (let ((file (f-filename f)))
                 (and (s-suffix-p ".org" file)
                      (funcall dlessp start file)
                      (funcall dlessp file end)))))))

(defun ctgtl--dt-or-ins-to-strdate (dt-or-ins)
  "Return a string representing a date from a org date of ts- instant"
  (let ((tt (if (stringp dt-or-ins)
                (s-left 10 (org-read-date nil nil dt-or-ins))
              (ts-format "%Y-%m-%d" dt-or-ins))))
    tt))

(defun ctgtl--concatenate-file-contents (files)
  "Concatenate the content of the FILES on a single string."
  (with-temp-buffer
    (--each files
      (when (f-exists-p it)
        (insert-buffer-substring (find-file-noselect it))))
    (buffer-string)))

(defun ctgtl--group-entries (_groups entries)
  "Group the entries by the given GROUPS.

FIXME: this is not currently implemented."
  entries)

(defun ctgtl--grouped-entries-to-org-text (entries)
  (org-ml-to-string entries))

(defun ctgtl--write-file (file text)
  "Write the TEXT to the given FILE, returning the number of rows written."
  (f-write-text text 'utf-8 file)
  (-> text (s-lines) (length)))

;;; CSV export
(defun ctgtl-export-csv (period fields &optional file)
  "Export the logged time to CSV.
PERIOD is a time record from the ts package
FIELDS is a list of string with the names of the fields to be
exported (don't add the 'CTGL' prefix)
If FILE is not specified, the user will be prompted for one"
  (let ((file (or file (read-file-name "Select output file: " "~" "export.csv" nil))))
    (if (and file period)
        (message "%s records written"
                 (or
                  (ctgtl--export-csv-impl file fields period)
                  0))
      (message "Export cancelled"))))

(defun ctgtl--export-csv-impl (file fields period)
  (->> (ctgtl--get-entries-period-with-duration period)
       (ctgtl--entries-to-csv fields)
       (ctgtl--write-file file)))

(defun ctgtl--entries-to-csv (fields entries)
  "Transform a list of entries to a CSV string (with headlines and newlines)"
  (->> entries
       (--map (ctgtl--entry-to-csv it fields))
       (-concat (list (s-join ", " fields)))
       (s-join "\n")))

(defun ctgtl--entry-to-csv (entry fields)
  "Convert a entry (headline) to a csv row (as a string without newline)"
  (->> fields
       (--map (format "CTGTL-%s" it))
       (--map (org-ml-headline-get-node-property it entry))
       (-map #'ctgtl--encode-csv-field)
       (--reduce (format "%s, %s" acc it))))

(defun ctgtl--encode-csv-field (s) (format "\"%s\"" (or s "")))

;;; Data access functions
(defun ctgtl-get-logs (period)
  "Return the logged entries for the given period, as a list of tables.

"
  (->> (ctgtl--get-entries-period period)
       (--filter (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" it))
       (--sort (ts<= (ts-parse (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" it))
                     (ts-parse (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" other))))
       (--map (ctgtl--entry-to-log-ht it))))

(defun ctgtl--entry-to-log-ht (entry)
  "Transform a raw org-ml entry into a hash with all the CTGTL-*
properties."
  (let ((ret (ht-create)))
    (-reduce-from #'ctgtl--entry-to-log-ht--reducer
                  ret
                  (org-ml-headline-get-node-properties entry))
    (ht-set ret :title (org-ml-to-trimmed-string (car (org-ml-get-property :title entry))))
    (ht-set ret :ctgtl--raw-prop entry)
    ret))

(defun ctgtl--entry-to-log-ht--reducer (acc p)
  (let* ((ob (cadr p))
         (k  (plist-get ob :key))
         (v  (plist-get ob :value)))
    (when (s-prefix-p "CTGTL-" k)
      (ht-set acc
              (->> (seq-drop k (length "CTGTL-"))
                   (s-downcase)
                   (s-concat ":")
                   (intern-soft))
              v))
    acc))

(defun ctgtl-util-completing-read (prompt obs &optional to-str)
  "Like completing-read, but allow the user to select arbitrary items.

OBS must be a seq of arbitrary things of any type T.
TO-STR is a function that takes a thing of type T and returns a string
to show the user."
  (let* ((ts (or to-str (lambda (x) x)))
         (ss (--map (funcall ts it) obs))
         (h  (ht<-alist (--map (cons (funcall ts it) it) obs)))
         (ip (when (boundp ivy-prescient-mode) (default-value ivy-prescient-mode))))
    (unwind-protect
        (progn
          (when ip ;; if ivy-prescient-mode is not bound ip will be nil
            (ivy-prescient-mode -1))
          (ht-get h (completing-read prompt ss)))
      (when ip
        (ivy-prescient-mode))))) ;; idem

(provide 'ctgtl)
;;; ctgtl.el ends here

