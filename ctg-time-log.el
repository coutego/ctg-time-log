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
(require 'org)

(defmacro comment (&rest _body)) ;; Clojure's comment macro

(defvar ctg-time-log-timestamp-format "%Y.%m.%d - %H:%M:%S:%2N")
(defvar ctg-time-log-directory (f-join org-directory "ctg-time-log"))

(cl-defun ctg-time-log-add-entry (entry)
  "Add a new entry in the log.

An entry should be a plist with keys :project, :type, :title"
  (let ((b (find-file-noselect (ctg-time-log--current-filename))))
    (save-excursion
      (with-current-buffer b
        (fundamental-mode) ; avoid 'magic' inserted by mode hooks
        (goto-char (point-max))
        (insert (apply #'ctg-time-log--create-entry entry))
        (insert "\n\n")
        (save-buffer)))))

(cl-defun ctg-time-log--create-entry (&key project type title)
  (let* ((timestamp (format-time-string ctg-time-log-timestamp-format))
         (entry (list :timestamp timestamp
                      :project   project
                      :type      type
                      :title     title))
         (json-encoding-pretty-print t))
    (json-encode entry)))

(defun ctg-time-log--current-filename ()
  (let* ((name (format "%s.json" (format-time-string "%Y-%m-%d")))
         (year (format-time-string "%Y"))
         (month (format-time-string "%m")))
    (f-join ctg-time-log-directory year month name)))

(comment
  (ctg-time-log--create-entry :project "EUCTP" :type "TASK" :title "Review the project plan")
  (ctg-time-log-add-entry '(:project "EUCTP" :type "TASK" :title "Review the project plan"))
  (ctg-time-log--current-filename))


(provide 'ctg-time-log)
;;; ctg-time-log.el ends here
