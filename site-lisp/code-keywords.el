;;; code-keywords.el --- highlight code keywords

;; Copyright (C) 1997-2000 Free Software Foundation, Inc.

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: code highlight keywords
;; Version: 1.0.0

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; Software Engineers/Hackers often use words like "FIXME" or "TODO" or "HACK"
;; when developing software.  code-keywords is an attempt to highlight these
;; words so that they are more obvious.

;;; Install:

;; (require 'code-keywords) in your .emacs

;;; History:

;;; Code:

;;(defface code-keywords-serious-face '((t (:background "cyan"))) "Serious keywords.")
(defface code-keywords-serious-face '((t (:background "grey90" :foreground "red"))) "Serious keywords.")

(defface code-keywords-warning-face '((t (:background "grey90" :foreground "BlueViolet"))) "Warning keywords.")

(defface code-keywords-accent-face '((t (:italic t))) "Highlight this keyword keywords.")

(defun code-keywords-font-lock-add-keywords(strings face)

  (unless (equal major-mode 'org-mode)
    (font-lock-add-keywords nil
      (list
        (list (concat "[^a-zA-Z0-9/,-]\\(\\("
                (regexp-opt strings)
                "\\)\\)[^a-zA-Z0-9/,-]") 1 face t)))))

(defun code-keywords-font-lock()
  "Adds a font-lock for buffers that user opens."
  (interactive)

  ;;only highlight buffers that aren't special
  (if (and (not (string-match "\\*"
                              (buffer-name (current-buffer))))
           (not (equal major-mode 'records-mode))
           (not (equal major-mode 'dired-mode)))
      (progn

        ;;add "FIX ME" code...
        
        (code-keywords-font-lock-add-keywords '( "FIXME" "HACK" ) ''code-keywords-serious-face)

        (code-keywords-font-lock-add-keywords '( "WARNING" "NOTE" "TODO" ) ''code-keywords-warning-face)

        (code-keywords-font-lock-add-keywords '( "DONE_" "PENDING_" ) ''code-keywords-accent-face))))
        
  
(add-hook 'font-lock-mode-hook 'code-keywords-font-lock)

(provide 'code-keywords)

;;; code-keywords.el ends here
