;;; json-reformat --- Reformat tool for JSON

;; Author: Wataru MIYAGUNI <gonngo@gmail.com>
;; Keywords: json

;; Copyright (c) 2012 Wataru MIYAGUNI
;;
;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;;; Commentary:

;;; Code:

(require 'json)

(defun json-reformat:indent (level)
  (make-string (* level 4) ? ))

(defun json-reformat:p-of-number (val)
  (number-to-string val))

(defun json-reformat:p-of-list (val level)
  (concat "{\n" (json:list-to-string val (1+ level)) (json-reformat:indent level) "}"))

(defun json-reformat:p-of-vector (val level)
  (if (= (length val) 0) "[]"
    (concat "[\n"
            (mapconcat
             'identity
             (loop for v across val
                   collect (concat
                            (json-reformat:indent (1+ level))
                            (json-reformat:print-value v (1+ level))
                            ))
             (concat ",\n"))
            "\n" (json-reformat:indent level) "]"
            )))

(defun json-reformat:p-of-symbol (val)
  (cond ((equal 't val) "true")
        ((equal json-false val) "false")
        (t (symbol-name val))))

(defun json-reformat:print-value (val level)
  (cond ((consp val) (json-reformat:p-of-list val level))
        ((numberp val) (json-reformat:p-of-number val))
        ((vectorp val) (json-reformat:p-of-vector val level))
        ((null val) "null")
        ((symbolp val) (json-reformat:p-of-symbol val))
        (t (json-encode-string val))))

(defun json:list-to-string (root level)
  (let (key val str)
    (while root
      (setq key (car root)
            val (cadr root)
            root (cddr root))
      (setq str
            (concat str (json-reformat:indent level)
                    "\"" key "\""
                    ": "
                    (json-reformat:print-value val level)
                    (when root ",")
                    "\n"
                    )))
    str))

(defun json-reformat-region (begin end)
  (interactive "r")
  (message "gist.github.com/gongo/1789605 is moved to github.com/gongo/json-reformat.
This repository is not maintained.")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (let* ((json-key-type 'string)
             (json-object-type 'plist)
             (before (buffer-substring (point-min) (point-max)))
             (json-tree (json-read-from-string before))
             after)
        (setq after (json-reformat:p-of-list json-tree 0))
        (delete-region (point-min) (point-max))
        (insert after)))))

(provide 'json-reformat)