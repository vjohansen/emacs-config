(require 'w32-utl)
(require 'findr)

;; Example: (vj-get-cached-data "~/x" (lambda () '("test")) 10)
;; - Load data from x file if newer than 10 days otherwise build it via the
;;   lambda and store in the filename given.
;; Always return data either cached or built via the function.
(defun vj-get-cached-data (filename builder-func rebuild-days)
  (let* ((exists (file-exists-p (expand-file-name filename)))
          (mtime (if exists
                   (time-to-seconds (time-since
                                      (nth 5 (file-attributes filename))))))
          (age (if exists (/ mtime (* 24 60 60))))
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
  (delq nil (mapcar
              ;; remove those where exe-path is ""
              (lambda (x) (if (equal (nth 1 x) "") nil x))
              ;; input list
              (vj-get-cached-data "~/.w32-apps.el" 'vj-w32-apps-build 60))))




(defvar anything-source-w32-launch
  '((name . "Launch Program")
     (candidates . (lambda ()
                     (delq nil
                       (mapcar
                         (lambda (f)
                           (if (string-match "\\.\\(exe\\|msc\\)\\'"
                                 (nth 1 f))
                             f))
                         w32-apps-list))))
     (action . (("Launch" .
                  (lambda (filename)
                    (let* ((f (car filename))
                            (ext (file-name-extension f)))
                      (message "Launch %s" f)
                      (if (equal ext "url")
                        (vj-os-open f)
                        (if (string-match (format "%s\\'"
                                            (regexp-opt '("msc"))) ext )
                          (shell-command (message "start %s" f))
                          (if (string-match
                                (format "%s\\'" (regexp-opt '("txt"))) ext )
                            (find-file (car filename))

                            (vj-w32-launch filename)))
                        ))))
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
  "Preconfigured `anything' for apps."
  (interactive)
  (anything-other-buffer '(anything-source-w32-launch)
    "*anything for apps*"))

(global-set-key (kbd "C-æ") 'anything-for-apps)
