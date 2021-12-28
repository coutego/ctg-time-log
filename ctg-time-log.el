;;; ctg-time-log.el Quick track of time spent on tasks -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Pedro Abelleira Seco
;;
;; Author: Pedro Abelleira Seco <https://github.com/pedroabelleiraseco>
;; Maintainer: Pedro Abelleira Seco <coutego@gmail.com>
;; Created: December 27, 2021
;; Modified: December 27, 2021
;; Version: 0.0.1
;; Keywords: convenience outlines
;; Homepage: https://github.com/coutego/ctg-time-log
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

(defvar ctg-time-log-timestamp-format "%Y.%m.%d - %H:%M:%S:%2N")
(defvar ctg-time-log-directory (f-join org-directory "ctg-time-log"))

(defun ctg-time-log-add-note ()
  "Add a note to the current element in the log"
  (interactive)
  (let* ((bf (ctg-time-log--current-filename))
         (b  (find-file bf))
         (w  (get-buffer-window b)))
    (with-current-buffer b
      (goto-char (point-max))
      (insert "\n** ")
      (evil-append 1))))

(cl-defun ctg-time-log-add-entry (&rest entry)
  "Add a new entry in the log.

The arguments should be a plist with keys :project, :type, :title"
  (let ((b (find-file-noselect (ctg-time-log--current-filename))))
    (with-current-buffer b
      (goto-char (point-max))
      (insert (apply #'ctg-time-log--create-entry entry))
      (insert "\n\n")
      (save-buffer)
      (goto-char (max-char)))))

(cl-defun ctg-time-log--create-entry (&key project type title)
  (let* ((timestamp (format-time-string ctg-time-log-timestamp-format))
         (tags (format ":%s:" type))
         (props (format ":CTL-TIMESTAMP: %s\n:CTL-PROJECT: %s" timestamp project)))
    (format "* %s %s\n:PROPERTIES:\n%s\n:END:" title tags props)))

(defun ctg-time-log--current-filename ()
  (let* ((name (format "%s.org" (format-time-string "%Y-%m-%d")))
         (year (format-time-string "%Y"))
         (month (format-time-string "%m")))
    (f-join ctg-time-log-directory year month name)))

(defun ctg-time-log--parse-buffer (b)
  (->>
   (with-current-buffer b (org-element-parse-buffer))
   (-drop 2)
   (-map #'cadr)
   (-map #'ht<-plist)))

;; (defun ctg-time-log--extract-parsed-entry (h)
;;   (ht-))

(comment
  (ctg-time-log--create-entry :project "EUCTP" :type "TASK" :title "Review the project plan")j
  (ctg-time-log-add-entry '(:project "EUCTP" :type "TASK" :title "Review the project plan")))

(seq-into (vector :a 1 :b 2) 'list)

(provide 'ctg-time-log)
;;; ctg-time-log.el ends here
