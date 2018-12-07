;;; vj-helm-simple-git-grep.el --- helm source that calls git grep

;;; Commentary:
;; A helm source for calling git grep. There are no bells and whistles.

;;; Code:

(defvar vj-helm-simple-git-grep-colors t "Disabling colors may improve speed")

(defvar vj-helm-simple-git-grep-root nil "Last git repo found. Reused when
running helm outside a git repo")

(defun vj-helm-simple-git-grep-candidates ()
  "Find the candidates.

Colorize result if vj-helm-simple-git-grep-colors is t.

If not in a git repo use the root of the previously used git repo"
  (let ((root
          (replace-regexp-in-string "[\015\012]+\\'" ""
            (shell-command-to-string "git rev-parse --show-toplevel")))
         fields)
    (if (string-match "^fatal:" root)
      (setq root nil))
    (unless root
      ;; reuse previous root
      (setq root vj-helm-simple-git-grep-root))
    (setq vj-helm-simple-git-grep-root root)

    (with-temp-buffer
      (shell-command (format "cd \"%s\" && git grep -n -i %s" root
                       helm-pattern) t)
      (if vj-helm-simple-git-grep-colors
        (mapcar
          (lambda (line)
            (setq fields (helm-grep-split-line line))
            (format "%s:%s:%s"
              (propertize (format "%s" (nth 0 fields)) 'face 'compilation-info)
              (propertize (format "%s" (nth 1 fields))
                'face 'compilation-line-number)
              (nth 2 fields)))
          (split-string (buffer-string) "\n"))
        (split-string (buffer-string) "\n")))))

(defvar vj-helm-simple-git-grep
  '((name . "Git grep")
     (candidates . vj-helm-simple-git-grep-candidates)
     (action ("View" .
               (lambda (line)
                 (let ((fields (helm-grep-split-line line)))
                   ;; (message "View file: \"%s\" \"%s\""
                   ;;   vj-helm-simple-git-grep-root (nth 0 fields))
                   (find-file-other-window (format "%s/%s"
                                             vj-helm-simple-git-grep-root
                                             (nth 0 fields)))
                   (goto-line (string-to-number (nth 1 fields)))
                   (when (fboundp 'etags-select-highlight)
                     (etags-select-highlight (point-at-bol) (point-at-eol)))))))

     (requires-pattern . 3))
  "Source for git.")

(defun vj-helm-git-grep ()
    (interactive)
    (helm-other-buffer '(vj-helm-simple-git-grep) "*helm git grep*"))

(provide 'vj-helm-simple-git-grep)

;;; vj-helm-simple-git-grep.el ends here
