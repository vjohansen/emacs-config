
;; GPL blah blah

;; A work-around for the feature loss in Emacs 24
;; grep-mode no longer inherits the feature from compilation-mode where it
;; looks for Entering Directory lines. :-(

(require 'compile)

(defvar compilation-find-file-hook nil)

(add-hook 'compilation-find-file-hook 'compilation-find-file-via-entering)

;; Look for "Entering directory `c:/xx/hestehoved'"
(defun compilation-find-file-via-entering (filename)
  (let ((result
	 (save-excursion
	   (if (re-search-backward
                 (concat "Ent" "ering directory `\\([^']*\\)'") nil t)
	       (concat (match-string-no-properties 1) "/"
		       filename)))))
    (if (and result (file-exists-p result))
	(progn
	  (message "vj: found \"%s\"" result)
	  (find-file-noselect result))  ;return a buffer
      nil)))

(defun compilation-find-file (marker filename directory &rest formats)
  "Find a buffer for file FILENAME (***OVERRIDDEN***).
If FILENAME is not found at all, ask the user where to find it.
Pop up the buffer containing MARKER and scroll to MARKER if we ask
the user where to find the file.
Search the directories in `compilation-search-path'.
A nil in `compilation-search-path' means to try the
\"current\" directory, which is passed in DIRECTORY.
If DIRECTORY is relative, it is combined with `default-directory'.
If DIRECTORY is nil, that means use `default-directory'.
FORMATS, if given, is a list of formats to reformat FILENAME when
looking for it: for each element FMT in FORMATS, this function
attempts to find a file whose name is produced by (format FMT FILENAME)."
  (or formats (setq formats '("%s")))
  (let ((dirs compilation-search-path)
        (spec-dir (if directory
                      (expand-file-name directory)
                    default-directory))
        buffer thisdir fmts name)
    (if (file-name-absolute-p filename)
        ;; The file name is absolute.  Use its explicit directory as
        ;; the first in the search path, and strip it from FILENAME.
        (setq filename (abbreviate-file-name (expand-file-name filename))
              dirs (cons (file-name-directory filename) dirs)
              filename (file-name-nondirectory filename)))
    ;; Now search the path.
    (while (and dirs (null buffer))
      (setq thisdir (or (car dirs) spec-dir)
            fmts formats)
      ;; For each directory, try each format string.
      (while (and fmts (null buffer))
        (setq name (expand-file-name (format (car fmts) filename) thisdir)
              buffer (and (file-exists-p name)
                          (find-file-noselect name))
              fmts (cdr fmts)))
      (setq dirs (cdr dirs)))

    (when (and (null buffer) compilation-find-file-hook)
      (message "Look for %s" filename)
      (setq buffer (run-hook-with-args-until-success
		    'compilation-find-file-hook filename)))

    (while (null buffer)    ;Repeat until the user selects an existing file.
      ;; The file doesn't exist.  Ask the user where to find it.
      (save-excursion            ;This save-excursion is probably not right.
        (let ((pop-up-windows t))
          (compilation-set-window (display-buffer (marker-buffer marker))
                                  marker)
          (let* ((name (read-file-name
                        (format "Find this %s in (default %s): "
                                compilation-error filename)
                        spec-dir filename t nil
                        ;; The predicate below is fine when called from
                        ;; minibuffer-complete-and-exit, but it's too
                        ;; restrictive otherwise, since it also prevents the
                        ;; user from completing "fo" to "foo/" when she
                        ;; wants to enter "foo/bar".
                        ;;
                        ;; Try to make sure the user can only select
                        ;; a valid answer.  This predicate may be ignored,
                        ;; tho, so we still have to double-check afterwards.
                        ;; TODO: We should probably fix read-file-name so
                        ;; that it never ignores this predicate, even when
                        ;; using popup dialog boxes.
                        ;; (lambda (name)
                        ;;   (if (file-directory-p name)
                        ;;       (setq name (expand-file-name filename name)))
                        ;;   (file-exists-p name))
                        ))
                 (origname name))
            (cond
             ((not (file-exists-p name))
              (message "Cannot find file `%s'" name)
              (ding) (sit-for 2))
             ((and (file-directory-p name)
                   (not (file-exists-p
                         (setq name (expand-file-name filename name)))))
              (message "No `%s' in directory %s" filename origname)
              (ding) (sit-for 2))
             (t
              (setq buffer (find-file-noselect name))))))))
    ;; Make intangible overlays tangible.
    ;; This is weird: it's not even clear which is the current buffer,
    ;; so the code below can't be expected to DTRT here.  -- Stef
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'intangible)
        (overlay-put ov 'intangible nil)))
    buffer))

(message "Override compilation-find-file!")
