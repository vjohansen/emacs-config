;;; bin-detect.el --- Search for program locations in paths

;;; Commentary:
;;
;; Search for programs in paths
;;
;; (bin-detect-find "git" '("c:/temp" "c:/tools/git/bin"))
;; (bin-detect-find "git.exe" '("c:/temp" "c:/tools/git/bin"))
;; (bin-detect-find "git/bin/git" (bin-detect-windows-paths))
;;
;;; Code:

(defvar bin-detect-tools-dir "c:/tools")
(defvar bin-detect-git nil)

(defun bin-detect-windows-paths ()
  "DOCSTRING"
  (interactive)
  (let (result try)
    (setq try bin-detect-tools-dir)
    (if (file-directory-p try)
      (add-to-list 'result try))

    (setq try "c:\\Program Files")
    (if (file-directory-p try)
      (add-to-list 'result try t))

    (setq try "c:\\Program Files (x86)")
    (if (file-directory-p try)
      (add-to-list 'result try t))
    result))

(defun bin-detect--find (program paths)
  "Search for PROGRAM in PATHS.

Returns first match."
  (interactive)
  (let (result candidate)
    (dolist (path paths)
      (setq candidate (concat
                        (replace-regexp-in-string "\\([^/\\]\\)$" "\\1/" path)
                        program))
      (if (and (file-exists-p candidate) (not result)
            (not (file-directory-p candidate)))
        (setq result candidate)))
    result
    ))

(defun bin-detect-find (program paths)
  "Search for PROGRAM in PATHS.

Will try to append .exe.

Returns first match."
  (if (not (string-match "\\.exe$" (downcase program)))
    (or
      (bin-detect--find (concat program ".exe") paths)
      (bin-detect--find program paths))
      ;; else
      (bin-detect--find program paths)))

(provide 'bin-detect)

;;; bin-detect.el ends here
