
;;; findr.el	-- Breadth-first file-finding facility for (X)Emacs
;;  Thursday July 30 1999

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: David Bakhash <cadet@bu.edu>
;; Maintainer: David Bakhash <cadet@bu.edu>
;; Version: 0.5
;; Created: Tue Jul 27 12:49:22 EST 1999
;; Keywords: files

;; This file is not part of emacs or XEmacs.

;; Emacs is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; Emacs program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;; Commentary:

;; This code contains a command, called `findr', which allows you to
;; search for a file breadth-first.  This works on UNIX, Windows, and
;; over the network, using efs and ange-ftp. It's pretty quick, and (at
;; times) is a better and easier alternative to other mechanisms of
;; finding nested files, when you've forgotten where they are.

;; You pass `findr' a regexp, which must match the file you're looking
;; for, and a directory, and then it just does its thing:

;; M-x findr <ENTER> ^my-lib.p[lm]$ <ENTER> c:/ <ENTER>

;; If called interactively, findr will prompt the user for opening the
;; found file(s).  Regardless, it will continue to search, until
;; either the search is complete or the user quits the search.
;; Regardless of the exit (natural or user-invoked), a findr will
;; return a list of found matches.

;; Change Log:

;; 0.1: Added prompt to open file, if uses so chooses, following
;;      request and code example from Thomas Plass. 
;; 0.2: Made `findr' not stop after the first match, based on the
;;      request by Thomas Plass.
;;      Also, fixed a minor bug where findr was finding additional
;;      files that were not correct matches, based on
;;      `file-relative-name' misuse (I had to add the 2nd arg to it).
;; 0.3: Added a `sit-for' for redisplay reasons.
;;      Modifications as suggested by RMS: e.g. docstring.
;; 0.4  Added `findr-query-replace', courtesy of Dan Nelsen.
;; 0.5  Fixed spelling and minor bug in `findr-query-replace' when
;;      non-byte-compiled.

;; Code:

(eval-when-compile
  (require 'cl)				; of course
  )

(provide 'findr)

;;;; breadth-first file finder...

(defun* findr (name dir &key (prompt-p (interactive-p)))
  "Search directory DIR breadth-first for files matching regexp NAME.
If PROMPT-P is non-nil, or if called interactively, Prompts for visiting 
search result\(s\)."
  (interactive "sfile name \(regexp\): \nDDirectory: ")
  (let ((*dirs* (findr-make-queue))
	*found-files*)
    (labels ((findr-1 (dir)
	       (message "searching %s ..." dir)
	       (let ((files (directory-files dir t "\\w")))
		 (loop
		  for file in files
		  for fname = (file-relative-name file dir)
		  when (file-directory-p file)
		  do (findr-enqueue file *dirs*)
		  when (string-match name fname)
		  do (push file *found-files*)
		  (when (and prompt-p
			     (y-or-n-p (format "Find file %s? " file)))
		    (find-file file)
		    (sit-for 0)	; redisplay hack
		    )))))
      (unwind-protect
	  (progn
	    (findr-enqueue dir *dirs*)
	    (while (findr-queue-contents *dirs*)
	      (findr-1 (findr-dequeue *dirs*)))
	    (message "searching ... done"))
	;; protected:
	(return-from findr (nreverse *found-files*))))))

(defun findr-query-replace (from to name dir)
  "Do `query-replace-regexp' of FROM with TO, on each file found by findr.
If you exit (\\[keyboard-quit] or ESC), you can resume the query replace
with the command \\[tags-loop-continue]."
  (interactive
   ;; I'd love to have the #. reader macro here, and use concat to
   ;; break up this string, but that was problematic.  Hopefully, GNU
   ;; Emacs will make `cl-read.el' a standard package.
   "sfindr query replace (regexp): \nsQuery replace %s by: \nsfile name \(regexp\): \nDDirectory: ")
  (tags-query-replace from to nil '(findr name dir)))

;;;; Queues

(defun findr-make-queue ()
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)
    q))

(defun findr-enqueue (item q)
  "Insert item at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun findr-dequeue (q)
  "Remove an item from the front of the queue."
  (prog1 (pop (cdr q))
    (when (null (cdr q))
      (setf (car q) q))))

(defsubst findr-queue-contents (q)
  (cdr q))