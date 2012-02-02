
(defvar vj-load-site-lisp-prefix "vj-")
(defvar vj-load-site-lisp-path "~/site-lisp")

(defun vj-load-site-lisp-code (dir file)
  (add-to-list 'load-path dir nil)
  (message "Add %s and load %s" dir
    (concat vj-load-site-lisp-prefix (match-string 1 dir)))
  (load (concat vj-load-site-lisp-prefix (match-string 1 dir)) t))

(defun vj-load-site-lisp (packages)
  (dolist (dir (directory-files vj-load-site-lisp-path t))
    (when (string-match
	   (concat "/" (regexp-opt packages t) "-?\\([^/]*\\)\\'")
	   dir)
      (vj-load-site-lisp-code dir (match-string 1 dir)))))
