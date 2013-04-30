
;; Usage:
;;
;; Create a vps project and run M-x vps-make-index RET
;;
;; (require 'helm-vps)
;; (global-set-key (kbd "C-'") 'helm-vps-index-db)


(require 'helm)
(require 'vps)

(defvar helm-vps-index-db-source
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
                 (let ((fields (split-string line ":")))
                   (find-file-other-window (nth 0 fields))
                   (goto-line (string-to-int (nth 1 fields)))
                   (when (fboundp 'etags-select-highlight)
                     (etags-select-highlight (point-at-bol) (point-at-eol)))))))
     (type . file)
     (requires-pattern . 5)
     (delayed))
  "Source for vps indexed DB")


(defun helm-vps-index-db ()
  (interactive)
  (helm-other-buffer 'helm-vps-index-db-source "*helm-vps-index-db*"))

(provide 'helm-vps)