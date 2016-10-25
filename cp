#!/usr/bin/env emacs -script
;;; -*- lexical-binding: t -*-
;;> copy a file
;; NOTE: There is a better way to copy files.  Use the copy-file function!

(defun cp (from-name to-name)
  (with-temp-file to-name
    (insert (with-temp-buffer
              (insert-file-contents-literally from-name)
              (buffer-string)))))

(cp (elt command-line-args-left 0)
    (elt command-line-args-left 1))



