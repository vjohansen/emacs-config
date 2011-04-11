
(defvar anything-source-vps-files
  '((name . "Project Files")
    (candidates
     . ;; cons
     (lambda ()
       (when (and vps-project-name
		  (file-exists-p (vps-filelist-filename)))
	 (start-process "anything-source-vps-files" nil
			vj-grep-like-program "-i"
			(replace-regexp-in-string "^-" "."  anything-pattern)
			(convert-standard-filename (vps-filelist-filename))))))
    (type . file)
    (requires-pattern . 4))
  "Source for retrieving files in the vps filelist.")

(provide 'anything-vps)