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
(require 'json)
(require 'dash)
(require 'f)
(require 'ht)
(require 'org)
(require 'org-element)
(require 'evil-commands)
(require 'org-ml)

(defvar ctgtl-timestamp-format "%Y-%m-%dT%H:%M:%S.%2N")
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

(cl-defun ctgtl-add-entry (&rest entry)
  "Add a new entry in the log.

The arguments should be a plist with keys :project, :type, :title"
  (let ((b (find-file-noselect (ctgtl--current-filename))))
    (with-current-buffer b
      (goto-char (point-max))
      (insert "\n\n")
      (insert (apply #'ctgtl--create-entry entry))
      (insert "\n")
      (save-buffer)
      (goto-char (max-char)))))

(cl-defun ctgtl--create-entry-props (entry)
  (->> entry
       ht<-plist
       (ht-amap (format ":%s: %s"
                        (format "CTGTL-%s" (ctgtl--keyword-to-string key))
                        value))
       (--reduce (s-concat acc "\n" it))))

(defun ctgtl--keyword-to-string (key)
  (->> key
       (format "%s")
       (s-chop-prefix ":")
       upcase))

(cl-defun ctgtl--create-entry (&rest entry)
  (let* ((timestamp (ctgtl-create-timestamp))
         (title     (or (plist-get entry :title)
                        "Time log entry"))
         (tags      (or (plist-get entry :tags) ""))
         (entry     (-concat (list :timestamp timestamp) entry))
         (props     (ctgtl--create-entry-props entry)))
    (format "* %s %s\n:PROPERTIES:\n%s\n:END:" title tags props)))

(defun ctgtl-create-timestamp ()
  "Creates a timestamp to be logged"
  (format-time-string ctgtl-timestamp-format))

(defun ctgtl--current-filename ()
  (let* ((name (format "%s.org" (format-time-string "%Y-%m-%d")))
         (year-month (format-time-string "%Y-%m")))
    (f-join ctgtl-directory year-month name)))

;; Parse log buffer
;;
;;

(defun ctgtl--export-buffer (b)
  (->>
   (with-current-buffer b (org-ml-parse-this-buffer))
   (org-ml-get-children)
   (--sort (ctgtl--parse-buffer-timestamp-sorter it other))
   ((lambda (xs) (-interleave xs (-concat (-drop 1 xs) '(nil)))))
   (-partition 2)
   (-map #'ctgtl--calculate-duration)
   (-map #'ctgtl--export-csv-row)
   (--reduce (and it (format "%s\n%s" acc it)))))

(cl-defun ctgtl--calculate-duration (p)
  (let* ((t1 (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" (car p)))
         (t2 (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" (cadr p)))
         (td (and t2
                  (time-subtract (parse-time-string t2)
                                 (parse-time-string t1))))
         (ft (format "%s" (float-time td))))
    (org-ml-headline-set-node-property "CTGTL-DURATION" ft (car p))))


(defun ctgtl--parse-buffer-timestamp-sorter (h1 h2)
  (let ((t1 (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" h1))
        (t2 (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" h2)))
    (string< t1 t2)))

;;; CSV export functions

(defun ctgtl-export-csv ()
  "Exports the logged time to CSV"
  (interactive)
  (let ((file   (read-file-name "Select output file: " "~" "export.csv" nil))
        (period (completing-read "Select period: " '(:today :this-week :this-month :other))))
    (if (and file period)
        (ctgtl--export-csv-impl file period)
      (message "Export cancelled"))))

(defun ctgtl--encode-csv-field (s) (format "\"%s\"" s))

(defun ctgtl--export-csv-row (r)
  (->>
    (list (org-ml-headline-get-node-property "CTGTL-TIMESTAMP" r)
          (org-ml-headline-get-node-property "CTGTL-DURATION" r)
          (org-ml-headline-get-node-property "CTGTL-PROJECT" r)
          (org-ml-headline-get-node-property "CTGTL-TYPE" r)
          (org-ml-headline-get-node-property "CTGTL-TITLE" r))
    (-map #'ctgtl--encode-csv-field)
    (--reduce (format "%s, %s" acc it))))

(defun ctgtl--export-csv-impl (file period)
  (let ((csv (ctgtl--export-csv-period-to-s period)))
    (if (and csv (< 0 (length (s-lines csv))))
        (progn
          (f-write csv 'utf-8 file)
          (message (format "Exported %d rows" (length (s-lines csv)))))
      (message "No data to be exported"))))

(defun ctgtl--export-csv-period-to-s (_period)
  (let ((b (find-file-noselect (ctgtl--current-filename))))
    (ctgtl--export-buffer b))) ;; FIXME: filter by period

(provide 'ctgtl)
;;; ctgtl.el ends here
