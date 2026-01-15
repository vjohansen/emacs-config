
(require 'magit)

(setq magit-last-seen-setup-instructions "1.4.0")

(setopt magit-diff-visit-prefer-worktree t)

;; (when (equal magit-log-format-graph-function 'magit-log-format-unicode-graph)
;;   (setcdr (assoc ?* magit-log-format-unicode-graph-alist) ?●))


;; (define-key magit-log-mode-map (kbd "TAB") 'vj-git-diff)
;; (define-key magit-log-mode-map (kbd "<f2>") 'vj-git-diff-gitk-gui)
;; (define-key magit-mode-map (kbd "<f2>") 'vj-git-diff-gitk-gui)

(defvar vj-git-diff-last-commit nil)

(defun vj-git-diff ()
  (interactive)
  (if (thing-at-point-looking-at "\\b[0-9a-z]\\{7,40\\}\\b")
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

(defun vj-git-diff-gitk-gui ()
  (interactive)
  (if (thing-at-point-looking-at "\\b[0-9a-z]\\{7,40\\}\\b")
    (shell-command
      ;; show_commit.bat contains:
      ;;   "C:\Program Files (x86)\Git\cmd\gitk" --select-commit=%1
      ;;
      (format "c:/temp/show_commit.bat %s &" (match-string-no-properties 0))
        "*vj-git-diff-gui*")))

(defun vj-magit-bun-check ()
  (interactive)
  (save-buffer)
  (compile (concat "bun x --bun commitlint --edit " (buffer-file-name)))
  )