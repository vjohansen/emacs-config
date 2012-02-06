(require 'w32-utl)

;; Example: (vj-get-cached-data "~/x" (lambda () '("test")) 10)
;; - Load data from x file if newer than 10 days otherwise build it via the
;;   lambda and store in the filename given
(defun vj-get-cached-data (filename builder-func rebuild-days)
  (let* ((exists (file-exists-p (expand-file-name filename)))
          (mtime (if exists
                   (time-to-seconds (time-since
                                      (nth 5 (file-attributes filename))))))
          (age (if exists
                 (/ (vj-file-last-modified-duration filename) (* 24 60 60))))
          result)
    (if (and exists (< age rebuild-days))
      (load-file filename)              ;sets result
      ;; else: we need to build the data
      (require 'recentf)
      (setq result (funcall builder-func))
      (with-temp-file filename
        (insert (format ";; Emacs was here. Generated %s\n"
                  (current-time-string)))
        (recentf-dump-variable 'result)))
    result))

(defun vj-w32-apps-build ()
  (interactive)
  "Build the command list by recursively look for .lnk files"
  (let ((files
          (append
            (findr "\.lnk$" (w32-utl-special-folder "StartMenu"))
            (findr "\.lnk$" (w32-utl-special-folder "AllUsersStartMenu"))))
        link-name)
    (message "Please be patient. Scanning disk...")
    (setq w32-apps-list
          (mapcar
           (lambda (file)
             (setq link-name (file-name-nondirectory file))
             (setq link-name (substring
                              link-name 0 (- (length link-name) 4)))
             (cons link-name (w32-utl-lnk-get-target-and-args file)))
           files))))


(defvar w32-apps-list
  (get-cached-data "~/.w32-apps.el" 'vj-w32-apps-build 30))

(defvar anything-source-w32-launch
  '((name . "Launch Program")
     (candidates . (lambda () w32-apps-list))
;;     (type . program)
      (action . (("Launch" .
                   (lambda (filename)
                     (let ((f (car filename)))
                       (message "Launch %s" f)
                       (if (equal  (file-name-extension f) "url")
                         (vj-os-open f)
                         (vj-w32-launch filename))
                         )))
                  ("Dired" .
                    (lambda (filename)
                      (dired (file-name-directory (car filename)))
                      (dired-goto-file (car filename))))))
     (requires-pattern . 2))
  "Source for launching windows apps.")

(defun vj-w32-launch (app-and-parms)
  ""
  (apply 'call-process
    ; maybe (cdr app-and-parms) should be split on space?
    (append (list (car app-and-parms) nil 0 nil) (cdr app-and-parms)))) 

(defun anything-for-apps ()
  "Preconfigured `anything' for opening files.
ffap -> recentf -> buffer -> bookmark -> file-cache -> files-in-current-dir -> locate"
  (interactive)
  (anything-other-buffer '(anything-source-w32-launch)
    "*anything for apps*"))

(global-set-key (kbd "C-æ") 'anything-for-apps)

;; TEST
;; (setq w32-apps-list '(("Notepad-aaa" "C:\\Windows\\system32\\notepad.exe" "aaa.txt")("Notepad-bbb" "C:\\Windows\\system32\\notepad.exe" "bbb.txt")))
