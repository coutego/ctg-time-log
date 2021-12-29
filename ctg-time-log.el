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

(defmacro comment (&rest _body)) ;; Clojure's comment macro

(defvar ctgtl-timestamp-format "%Y.%m.%d - %H:%M:%S:%2N")
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
      (insert (apply #'ctgtl--create-entry entry))
      (insert "\n\n")
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
         (year (format-time-string "%Y"))
         (month (format-time-string "%m")))
    (f-join ctgtl-directory year month name)))

(defun ctgtl--parse-buffer (b)
  (->>
   (with-current-buffer b (org-element-parse-buffer))
   (-drop 2)
   (-map #'cadr)
   (-map #'ht<-plist)))

;; (defun ctgtl--extract-parsed-entry (h)
;;   (ht-))

(comment
  (ctgtl--create-entry :project "EUCTP" :type "TASK" :title "Review the project plan")
  (ctgtl-add-entry '(:project "EUCTP" :type "TASK" :title "Review the project plan"))
  (ctgtl--create-entry-props :title "My title" :project "project 1"))


(seq-into (vector :a 1 :b 2) 'list)

(provide 'ctgtl)
;;; ctgtl.el ends here
;;;
