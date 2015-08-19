
(require 'magit)

(setq magit-last-seen-setup-instructions "1.4.0")

;; (when (equal magit-log-format-graph-function 'magit-log-format-unicode-graph)
;;   (setcdr (assoc ?* magit-log-format-unicode-graph-alist) ?●))


(define-key magit-log-mode-map (kbd "TAB") 'vj-git-diff)

(defvar vj-git-diff-last-commit nil)

(defun vj-git-diff ()
  (interactive)
  (if (thing-at-point-looking-at "\\b[0-9a-z]\\{7\\}\\b")
    (if (equal (match-string-no-properties 0) vj-git-diff-last-commit)
      ;; same commit id
      (scroll-other-window)
      ;; show diff
      (setq vj-git-diff-last-commit (match-string-no-properties 0))
      (shell-command
        (format "git show %s" vj-git-diff-last-commit)
        "*vj-git-diff*")
      (with-current-buffer "*vj-git-diff*"
        (diff-mode)))
    (setq vj-git-diff-last-commit nil)))
