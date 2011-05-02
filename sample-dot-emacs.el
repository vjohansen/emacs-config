;; -*- mode: emacs-lisp; mode: ruler -*-

(defvar my-lisp-dir "c:/public/elisp")

;; Add path to locally installed packages. Change as needed.
(dolist (dir
	 `(
	   "h:/site-lisp/emacs-config"
	   "h:/site-lisp/yasnippet-0.6.1c"
	   "h:/site-lisp/auto-complete-1.3.1"
	   ,my-lisp-dir	      ; Most important last
	   ))
  (add-to-list 'load-path dir))

(load "vj-init")
