;;; context-info.el --- Use header-line-format to show info depending on (point)

;; Copyright (C) 2008 Vagn Johansen

;; Author: Vagn Johansen <gonz808@hotmail.com>
;; Keywords: ui, user interface, header-line-format
;; URL: http://www.ozymandias.dk/emacs/emacs.html#context-info

;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:
;;
;; Use header-line-format to show info depending on (point)
;;
;; Installation:
;;
;;   (require 'context-info)
;;

;;; Code:

;;(require 'lang-info)

(eval-when-compile (require 'cl))

(defgroup context-info nil
  "."
  :link  '(url-link "http://www.ozymandias.dk/emacs/emacs.html")
  :group 'convenience)

(defvar context-info-hook nil "blah")
(defvar context-info-show-hook nil "blah")
(defvar context-info-last nil "blah")

(defun context-info-display (info)
  ""
  (setq context-info-last info)
  (when info
    (setq header-line-format info)
    (force-mode-line-update)
    (run-hooks 'context-info-show-hook)))

(defun context-info ()
  (interactive)
  (let ((info (run-hook-with-args-until-success 'context-info-hook)))
    (context-info-display info)))

(run-with-idle-timer 0.9 t 'context-info)



;; ------------------------------------------------------------------------

;; (load "ectags-select") ;; this corrupts colouring in org files!!!! FIXME


;; (defun vj-find-ectag (fn tag-buffer)
;;   "Scan a tag table buffer for an exact match with a tag"
;;   (save-excursion
;;     (set-buffer tag-buffer)
;;     (goto-char (point-min))
;;     (while (re-search-forward (format "^\\(%s\\)	\\([^	]+\\)	\\(.+\\);\"\\(.+\\)$" *ectags-regexp*)  nil t)
;;       (apply fn (list (match-string-no-properties 1)
;;                       (match-string-no-properties 2)
;;                       (match-string-no-properties 3)
;;                       (match-string-no-properties 4))))))

;; (defun vj-ectags-eldoc-print-current-symbol-info (sym)
;;   "Print the ectags info associated with the current eldoc symbol"
;;   (let* ((eldoc-sym sym))
;;     (seek-ectag eldoc-sym 'vj-find-ectag)
;;     (if (> (length *ectags-matches*) 0)
;;         (pp-to-string (car *ectags-matches*))
;; ;;        (ectags-match-tag-info (car *ectags-matches*))
;;       (format "Unknown %s " eldoc-sym))))



(defun context-info-c-common-setup ()
  (add-hook 'context-info-hook 'context-info-c-common t t))

;; (add-hook 'c-mode-hook 'context-info-c-common-setup t)
;; (add-hook 'c++-mode-hook 'context-info-c-common-setup t)

(defun context-info-c-common ()
  ""
  (let ((info))
    (if (thing-at-point-looking-at "\\sw\\sw*")
      (setq info (vj-ectags-eldoc-print-current-symbol-info
                   (match-string-no-properties 0))))
    info))

;; ------------------------------------------------------------------------

(add-hook 'csharp-mode-hook 'context-info-csharp-setup t)

(defun context-info-csharp-setup ()
  (add-hook 'context-info-hook 'context-info-csharp t t))

(defun context-info-csharp ()
  ""
  (let (full-type type generic generics r-type found)
    (when (thing-at-point-looking-at "\\([A-Z][a-zA-Z.]*\\)\\(<[^>]*>\\)?")
      (setq full-type (match-string-no-properties 0))
      (setq type (match-string-no-properties 1))
      (setq generic (match-string-no-properties 2))
      )
    (setq r-type type)
    (when (and r-type generic)
      (setq generics (split-string "[ ]*,[ ]*" generic))
      (setq r-type (format "%s`%d" type (length generics))))

    ;;    (csharp-types classname-re)
    ;;(member classname (csharp-types))

    (if (and type (csharp-full-type-name-p r-type))
      (concat (propertize type 'face 'font-lock-type-face)
        (propertize generic 'face 'bold))
      (setq found (csharp-types r-type))
      (cond
        ((equal (length found) 0)
          type)
        ((equal (length found) 1)
          (car found))
        (t "ambiguous")) ;; <- FIXME look at using statements
      )))


;; ------------------------------------------------------------------------

(require 'flymake)

(defcustom context-info-flymake-autoshow-max-line-difference 16
  "Max number of lines from point where flymake errors should be shown."
  :type 'integer
  :group 'context-info)

(defun context-info-flymake ()
  (interactive)
  (let ((point-line (+ (count-lines 1 (point)) (if (= (current-column) 0) 1 0)))
         tip nearest-line last-line)
    (setq context-info-flymake-last-tip-line nil)
    (when (member 'flymake-after-change-function after-change-functions)
      ;; foreach line
      (dolist (err-info flymake-err-info)
        ;; foreach error on line
        (dolist (info-and-pos (flymake-er-get-line-err-info-list err-info))
          (let ((line (flymake-ler-line info-and-pos))
                 (text (flymake-ler-text info-and-pos)))
            ;; Is it near and closer than current?
            (when (and (< (abs (- point-line line))
                         context-info-flymake-autoshow-max-line-difference)
                    (or (not nearest-line)
                      (< (abs (- point-line line))
                        (abs(- point-line nearest-line)))))

              (setq tip (if (equal line last-line)
                          (concat tip " " (propertize text 'face 'modeline-inactive))
                          (propertize text 'face 'modeline-inactive)))
              (setq last-line line))))))
    (if tip (format "!%d: %s" last-line tip))))


;;(add-hook 'context-info-hook 'context-info-flymake)

;; ------------------------------------------------------------------------

(provide 'context-info)
