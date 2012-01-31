;;; ac-source-sql.el--- Complete sql names with auto-complete

;; Copyright (C) 2011 Vagn Johansen

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

;; Installation:
;;
;; (require 'ac-source-sql)
;; (setq ac-source-sql-sqlcmd "sqlcmd -S localhost\\SQLEXPRESS -d nothwind")
;;
;; A function is installed in sql-interactive-mode-hook that sets ac-sources
;;
;; Use M-x ac-source-sql-test-connection to test the connection

;;; History:

;;; Code:

(require 'auto-complete)

(defvar ac-source-sql-sqlcmd nil "sqlcmd command line with connection info.

Example  sqlcmd -S localhost\\SQLEXPRESS -d nothwind")

(add-to-list 'ac-modes 'sql-interactive-mode)
(add-hook 'sql-interactive-mode-hook 'ac-source-sql-setup)

;; --------------------------------------------

;; For Internal use only
(defvar ac-source-sql-table-name-list nil)
(defvar ac-source-sql-hash-table (make-hash-table :test 'equal))
(defvar ac-source-sql-document-hash-table (make-hash-table :test 'equal))
(defvar ac-source-sql-last-type nil)
(defvar ac-source-sql-dd-loaded nil "Is data dictionary loaded?")

(defvar ac-source-sql-dd
  (concat ;; maybe use QUOTENAME(table_name, '"')
   "SELECT 'LINE|' + table_name + '|' + column_name + '|' + "
   "'<br/>is_nullable: ' + IS_NULLABLE + '<br/>column_default: ' + "
   "IsNULL(column_default, '') + '<br/>character_octet_length: ' + "
   "CAST(IsNULL(character_octet_length, -1) as VARCHAR) + "
   "'<br/>data_type: ' + data_type "
   "FROM INFORMATION_SCHEMA.COLUMNS ORDER BY table_name"))

(defun ac-source-sql-setup ()
  (unless ac-source-sql-dd-loaded
    (ac-source-sql-load-dd))
  (setq ac-sources '(ac-source-sql ac-source-sql-table-name)))

(defun ac-source-sql-test-connection ()
  (interactive)
  (shell-command
   (format "%s -W -s ,  -Q %s" ac-source-sql-sqlcmd
           (shell-quote-argument "select top 5 table_name, column_name FROM INFORMATION_SCHEMA.COLUMNS ORDER BY table_name")))
  (switch-to-buffer-other-window "*Shell Command Output*"))

(defun ac-source-load-dd-via-sqlcmd ()
  (unless ac-source-sql-sqlcmd
    (message "**** YOU MUST SET ac-source-sql-sqlcmd"))
  (message "Calling sqlcmd ..\nPlease wait..")
  (let ((count 0) table-name new-table-name fields result doc
        (output (shell-command-to-string
                 (format "%s -W -Q %s" ac-source-sql-sqlcmd
                         (shell-quote-argument ac-source-sql-dd)))))
    (with-temp-buffer
      (insert output)
      (write-region (point-min) (point-max) "~/dd.sql")
      (goto-char (point-min))
      ;;      (if ^Cannot open database
      (goto-char (point-min))
      (message "Parsing sqlcmd output....")
      (while (re-search-forward "^LINE|\\([^|]*\\)|\\([^|]*\\)|\\(.*\\)" nil t)
        (setq new-table-name (match-string 1))
        (setq doc (replace-regexp-in-string
                   "<br/>" "\n"
                   (replace-regexp-in-string " *\\'" "" (match-string 3))))
        (unless table-name (setq table-name new-table-name))
        (if (equal table-name new-table-name) ;same table name
            (add-to-list 'fields
                         (list (match-string 2)
                               doc))
          ;; else: different table-name => store current fields
          (when fields
            (add-to-list 'result (list table-name fields))
            ;;            (message "Store table %s %d" table-name (length fields))
            (setq fields (list (list (match-string 2) doc))
                  table-name new-table-name)
            (setq count (1+ count))))
        ))
    (when fields
      (add-to-list 'result (list table-name fields)))
    (message "Found %d table definitions" count)
    result))

(defun ac-source-sql-load-dd ()
  ""
  (interactive)
  (ac-source-sql-clear-caches)

  ;; (funcall (intern (format "ac-source-load-dd-via-%s" 'sqlcmd)))
  ;; FIXME (if (equal sql-product 'ms))
  (setq ac-source-sql-dd-loaded 'loading-error)
  (dolist (table (ac-source-load-dd-via-sqlcmd))
    (ac-source-sql-add-entry (car table)  (cadr table)))
  (setq ac-source-sql-dd-loaded t))     ;success


(defun ac-source-sql-clear-caches ()
  (interactive)
  (setq ac-source-sql-dd-loaded nil)
  (setq ac-source-sql-table-name-list nil)
  (clrhash ac-source-sql-hash-table)
  (clrhash ac-source-sql-document-hash-table))

(defun ac-source-sql-add-entry (tablename fields)
  (add-to-list 'ac-source-sql-table-name-list tablename)
  (puthash tablename (mapcar 'car fields) ac-source-sql-hash-table)
  (dolist (e fields)
    (puthash (concat tablename "." (nth 0 e))
             (nth 1 e) ac-source-sql-document-hash-table)))

(defvar ac-source-sql
  '((prefix . "\\_<[a-zA-Z][a-zA-Z_0-9]*\\.\\([a-zA-Z_0-9]*\\)")
     (document . ac-source-sql-document)
     (requires . 0)
     (candidates . ac-source-sql-candidates)))

(defvar ac-source-sql-table-name
  '((candidates . ac-source-sql-table-name-list)
     (prefix . symbol)))

(defun ac-source-sql-document (name)
  (let ((entry
         (gethash (format "%s.%s"
                          (or ac-source-sql-last-type "?") name)
                  ac-source-sql-document-hash-table)))
    (if entry (format "%s" entry))))

(defun ac-source-sql-candidates ()
  " "
  (if (and ac-source-sql-dd-loaded
           (save-excursion
             (unless ac-point
               (message "no ac-point!"))
             (goto-char ac-point)
             (thing-at-point-looking-at "\\([a-zA-Z][a-zA-Z0-9_]*\\)\\.")))
      (let* ((var (match-string-no-properties 1))
             (regex (concat "\\(from\\|join\\) *\\_<\\([a-zA-Z_0-9]+\\) " var "\\>"))
             type type-point hash-entry
             type2 type2-point)
        (save-excursion
          (when (re-search-backward regex nil t)
            (setq type (match-string-no-properties 2))
            (setq type-point (point))))
        (save-excursion
          (when (re-search-forward regex nil t)
            (setq type2 (match-string-no-properties 2))
            (setq type2-point (point))))

        (if (and type type2
                 (< (- type2-point (point)) (- (point) type-point)))
            ;; type2-point is closer to point
            (setq type type2))
        (if (not type)
            (setq type type2))
        (setq ac-source-sql-last-type type)
        (if type
            (if (setq hash-entry (gethash type ac-source-sql-hash-table))
                hash-entry
              (message "do not know about %s" type))))))

(provide 'ac-source-sql)

;;; ac-source-sql.el ends here
