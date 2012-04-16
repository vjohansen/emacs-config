
;; Uses candidates-in-buffer
(defvar anything-source-vps-files
  '((name . "Project Files")
    (init . (lambda ()
              (with-current-buffer (anything-candidate-buffer 'global)
                (when (and vps-project-name
                        (file-exists-p (vps-filelist-filename)))
                  (insert-file-contents (vps-filelist-filename))))))
    (requires-pattern . 3)
    (candidates-in-buffer)
    (type . file)))


(provide 'anything-vps)
