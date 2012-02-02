
;;
;; TO LOAD THIS FILE ADD THE FOLLOWING TO .emacs or ~.emacs.d/init.el
;;

;; MISSING defvar here

;; ;; Add path to locally installed packages. Change as needed.
;; (dolist (dir
;; 	 `(
;; 	   "~/src/gnus/lisp"
;; 	   "~/site-lisp/ess-5.13/lisp"
;;         ;; ...
;; 	   ,my-lisp-dir	      ; Most important last
;; 	   ))
;;   (add-to-list 'load-path dir))
;;
;; (load "vj-init")
;;

(set-background-color "#ffeeff")

(unless (boundp 'my-lisp-dir)
  (defvar my-lisp-dir "~/elisp"))

(setq my-lisp-dir (expand-file-name my-lisp-dir))
(unless (file-directory-p my-lisp-dir)
   (make-directory my-lisp-dir)
   (message "*** Created %s!***\nDelete dir and set my-lisp-dir in .emacs to change" 
	    my-lisp-dir)
   (sit-for 2))
(add-to-list 'load-path my-lisp-dir)

;; Load customize file
(setq custom-file (format "%s/custom-%s.el" my-lisp-dir (downcase system-name)))
(load custom-file)
(set-register ?c `(file . ,custom-file))

(load "vj-load")

(set-background-color "#ffeedd")

(defvar vj-system-type-specific-elisp-file
  (format "%s/system-type-%s.el"
    my-lisp-dir
    (downcase (replace-regexp-in-string "[^a-zA-Z0-9_]+" "-"
                (symbol-name system-type)))))
(safe-load vj-system-type-specific-elisp-file)
(set-register ?s `(file . ,vj-system-type-specific-elisp-file)) ;; C-x r j s

(defvar vj-machine-specific-elisp-file
  (format "%s/machine-%s.el" my-lisp-dir (downcase system-name)))
(safe-load vj-machine-specific-elisp-file)
(set-register ?e `(file . ,vj-machine-specific-elisp-file)) ;; C-x r j e


