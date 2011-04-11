;;; vj-util.el --- Utility functions

;; Copyright (C) 2007 Vagn Johansen

;;; Commentary:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(defun vj-file-last-modified-duration (filename)
  "Return seconds from now to last modified for FILENAME."
  (let ((mtime (nth 5 (file-attributes filename))))
    (time-to-seconds (time-since mtime))))

;; VJ 2004 Oct
(defun vj-read-from-minibuffer (prompt &optional default-value hist)
  "Simple version of read-from-minibuffer that returns default value on empty input."
  (let* ((prompt-with-default (if default-value
                                (format "%s [%s]: " prompt default-value)
                                prompt))
          (result (read-from-minibuffer
                    ;; PROMPT
                    prompt-with-default
                    ;; INITIAL-CONTENTS KEYMAP READ
                    nil nil nil
                    ;; HIST DEFAULT-VALUE
                    hist default-value)))
  (if (and default-value (string= result ""))
    (format "%s" default-value)
    result)))

(defun vj-ensure-trailing-slash (dir)
  (replace-regexp-in-string "\\([^/\\]\\)$" "\\1/" dir))

;; (vj-ensure-trailing-slash "/dir\\")

(defun vj-subdirs (directory &optional full)
  "Return a list of names of directores (excl. . and ..)files in DIRECTORY.
If FULL is non-nil, return absolute file names.  Otherwise return names
that are relative to the specified directory."
  (let ((sub-dirs) (dir (vj-ensure-trailing-slash directory)))
    (dolist (maybe-dir (directory-files
                         dir
                         full            ; FULL
                         "^[^\\.]"      ; Ignore ^\.*
                         ))
      (if (file-directory-p (if full maybe-dir (concat dir maybe-dir)))
        (add-to-list 'sub-dirs maybe-dir)))
    sub-dirs))


(defun vj-subsubdirs-full (rootdir)
  (let (list)
    (dolist (dir1 (vj-subdirs rootdir t))
      (dolist (dir2 (vj-subdirs dir1 t))
        (add-to-list 'list dir2 t)))
    list))



(defun vj-chomp (str)
  "Remove trailing newline a la Perls chomp function."
  (replace-regexp-in-string "[\015\012]+\\'" "" str))


(unless (string= (vj-chomp "foo\n") "foo")
  (error "vj-chomp has bug"))



(defun vj-sub-directories (directory)
  "Return all directories recursively below DIRECTORY."
  (let ((sub-dirs) (dir (vj-ensure-trailing-slash directory)))
    (dolist (maybe-dir (directory-files
                         dir
                         t              ; FULL
                         "^[^\\.]"      ; Ignore ^\.*
                         ))
      (when (file-directory-p maybe-dir)
        (add-to-list 'sub-dirs maybe-dir)
        (setq sub-dirs (append sub-dirs (vj-sub-directories maybe-dir)))))
    sub-dirs))


(provide 'vj-util)

