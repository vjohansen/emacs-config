
;; This will add magit and auto-complete directories to load-path if they have
;; been downloaded and put in ~/site-lisp (change via
;; vj-load-site-lisp-path). Also "vj-" + package-name will be loaded. Set
;; another prefix via vj-load-site-lisp-prefix.
;;

(defvar vj-load-site-lisp-prefix "vj-")
(defvar vj-load-site-lisp-path "~/site-lisp")

(defun vj-load-site-lisp (dir file)
  (add-to-list 'load-path dir nil)
  (message "load-library %s via %s" dir
    (concat vj-load-site-lisp-prefix (match-string 1 dir)))
  (load (concat vj-load-site-lisp-prefix (match-string 1 dir)) t))

(dolist (dir (directory-files vj-load-site-lisp-path t))
  (when (string-match
          (concat "/" (regexp-opt '("magit" "auto-complete") t)
            "\\([^/]+\\)\\'")
          dir)
    (vj-load-site-lisp dir (match-string 1 dir))))
