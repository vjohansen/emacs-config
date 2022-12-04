;;; helm-vps.el --- helm sources for filelist and indexed DB

;; Copyright (C) 2013 Vagn Johansen

;; Sources helm-source-vps-files and helm-vps-index-db-source. Access the
;; latter via helm-vps-index-db.
;;
;; Usage:
;;
;; Create a vps project and run M-x vps-make-index RET
;;
;; (require 'helm-vps)
;; (global-set-key (kbd "C-'") 'helm-vps-index-db)
;;
;;

(require 'helm-grep)
(require 'vps)

(defun helm-source-vps-files-candidates ()
  (with-temp-buffer
    (when (and vps-project-name
            (file-exists-p (vps-filelist-filename)))
      (insert-file-contents (vps-filelist-filename))
      (split-string (buffer-string) "\n"))))

(defvar helm-source-vps-files
  (helm-build-sync-source "Project"
    :candidates 'helm-source-vps-files-candidates
    :filtered-candidate-transformer 'helm-highlight-files
    :action 'helm-find-files-actions))
;; (helm :sources 'helm-source-vps-files :buffer "*VPS*")

;; Supports spaces in the pattern. First word is prefix and the remaining are
;; substrings
(defvar helm-source-vps-index-db
  '((name . "Word Prefix Search")
     (candidates-process .
       (lambda ()
         (start-process "perl--vj-search-index-complete" nil
           vps-perl-program (or (locate-library "vj-search-index-complete.pl")
                              "-S vj-search-index-complete.pl")
           helm-pattern
           (vps-index-db-filename))))
     (action ("View" .
               (lambda (line)
                 (let ((fields (helm-grep-split-line line)))
                   (find-file-other-window (nth 0 fields))
 (goto-line (string-to-number (nth 1 fields)))
                   (when (fboundp 'etags-select-highlight)
                     (etags-select-highlight (point-at-bol) (point-at-eol)))))))
     (type . file)
     (requires-pattern . 5)
     (delayed))
  "Source for vps indexed DB")


(defun helm-vps-index-db ()
  (interactive)
  (helm-other-buffer 'helm-source-vps-index-db "*helm-vps-index-db*"))

(provide 'helm-vps)
