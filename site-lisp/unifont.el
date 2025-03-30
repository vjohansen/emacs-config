;;; unifont.el --- Render GNU Unifont                -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/unifont.el
;; Package-Requires: ((emacs "25.1"))
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'seq)

(defconst unifont-data
  (let ((filename (expand-file-name
                   "unifont-12.1.01.hex"
                   (file-name-directory (or load-file-name buffer-file-name)))))
    (unless (file-exists-p filename)
      (url-copy-file "http://unifoundry.com/pub/unifont/unifont-12.1.01/font-builds/unifont-12.1.01.hex.gz"
                     "unifont-12.1.01.hex.gz")
      (shell-command "gunzip unifont-12.1.01.hex.gz"))
    filename))

(defun unifont-get-bits (char)
  (with-temp-buffer
    (insert-file-contents-literally unifont-data)
    (re-search-forward (format "^%04x:" char))
    (let* ((bits (buffer-substring (point) (line-end-position)))
           (width (pcase-exhaustive (length bits)
                    (32 2)
                    (64 4))))
      (cons width
            (mapcar (lambda (n) (string-to-number n 16))
                    (seq-partition bits width))))))

(defvar unifont-block (or
                       "█"
                       (propertize " " 'font-lock-face '(:background "white"))
                       (propertize " " 'face '(:background "white"))
                       (propertize " " 'font-lock-face '(:background "pink"))
                       "■"))

(defun unifont-render-char (char)
  (pcase-let* ((`(,width . ,bits) (unifont-get-bits char))
               (masks
                (pcase-exhaustive width
                  (2 '(128 64 32 16 8 4 2 1))
                  (4 '(32768 16384 8192 4096 2048 1024 512 256 128 64 32 16 8 4 2 1)))))
    (mapcar (lambda (n)
              (mapconcat (lambda (mask)
                           (if (zerop (logand n mask))
                               " "
                             unifont-block))
                         masks ""))
            bits)))

(defun unifont-render-string (string)
  (let ((lines (apply #'seq-mapn #'concat (mapcar #'unifont-render-char string))))
    (mapconcat #'identity lines "\n")))

;;;###autoload
(defun unifont-test (string)
  (interactive (list (read-string "String to render: " nil nil "你好 Emacs")))
  (with-current-buffer (get-buffer-create "*Unifont Test*")
    (delete-other-windows)
    (switch-to-buffer (current-buffer))
    (erase-buffer)
    (insert (unifont-render-string string))))

;;;###autoload
(defun unifont-insert (string)
  (interactive (list (read-string "Insert: ")))
  (insert (mapconcat #'unifont-render-string (split-string string "\n") "\n.\n")))

(provide 'unifont)
;;; unifont.el ends here