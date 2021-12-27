;;; ctg-time-log.el Quick track of time spent on tasks -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Pedro Abelleira Seco
;;
;; Author: Pedro Abelleira Seco <https://github.com/pedroabelleiraseco>
;; Maintainer: Pedro Abelleira Seco <coutego@gmail.com>
;; Created: December 27, 2021
;; Modified: December 27, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/pedroabelleiraseco/ctg-time-log
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Org mode offers very advance capabilities for logging time spent on different
;; tasks and produce reports on it. Unfortunately, those are unusable for my use case,
;; so I decided to create a helper package for personal use to create prefefined
;; commands to quickly log time.
;;
;;
;;; Code:

(require 'cl-lib)
(require 'json)

(defvar ctg-time-log-timestamp-format "%Y.%m.%d - %H:%M:%S:%2N")

(cl-defun ctg-time-log-add-entry (entry)
  "Add a new entry in the log.

An entry should be a plist with keys :project, :type, :title"
  entry) ;; FIXME Implement this function


(cl-defun ctg-time-log--create-entry (&keys project type title)
  (let* ((timestamp (format-time-string ctg-time-log-timestamp-format))
         (entry (list :timestamp timestamp
                      :project   project
                      :type      type
                      :title     title)))
    (json-encode entry)))


(provide 'ctg-time-log)
;;; ctg-time-log.el ends here
