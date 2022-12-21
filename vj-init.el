;;
;; TO LOAD THIS FILE ADD THE FOLLOWING TO .emacs or ~.emacs.d/init.el
;;
;;   (add-to-list 'load-path "~/site-lisp/emacs-config/")
;;   (setq my-lisp-dir "~/elisp")
;;   (load "vj-init")
;;

(setq debug-on-error t
      debug-on-quit t)

(defvar my-lisp-dir "~/elisp")

(setq my-lisp-dir (expand-file-name my-lisp-dir))

(unless (file-directory-p my-lisp-dir)
   (make-directory my-lisp-dir)
   (message "*** Created %s!***\nDelete dir and set my-lisp-dir in .emacs to change"
	    my-lisp-dir)
   (sit-for 2))
(add-to-list 'load-path my-lisp-dir)

;; Load customize file
(setq custom-file (format "%s/custom-%s.el" my-lisp-dir (downcase system-name)))
(set-register ?c `(file . ,custom-file))
(load custom-file t)


(message "vj-load: system-type=%s system-name=%s LOGONSERVER=%s"
  system-type (system-name) (getenv "LOGONSERVER"))

(defvar vj-emacs-config-dir
  (file-name-directory (locate-file "vj-load.el" load-path)))

(add-to-list 'load-path (concat vj-emacs-config-dir "site-lisp"))

(defvar vj-load-site-lisp-path
  (expand-file-name (concat vj-emacs-config-dir "/..")))

(set-register ?l `(file . ,vj-load-site-lisp-path))
(set-register ?L `(file . ,vj-emacs-config-dir))
(set-register ?U `(file . ,user-emacs-directory))

(load "vj-load")
(defvar vj-system-type-specific-elisp-file
  (format "%s/system-type-%s.el"
    my-lisp-dir
    (downcase (replace-regexp-in-string "[^a-zA-Z0-9_]+" "-"
                (symbol-name system-type)))))
(set-register ?s `(file . ,vj-system-type-specific-elisp-file)) ;; C-x r j s
(if (file-exists-p vj-system-type-specific-elisp-file)
    (load vj-system-type-specific-elisp-file)
  (message "*** Please create the file: %s ***"
    vj-system-type-specific-elisp-file))


(defvar vj-machine-specific-elisp-file
  (format "%s/machine-%s.el" my-lisp-dir (downcase system-name)))

(set-register ?e `(file . ,vj-machine-specific-elisp-file)) ;; C-x r j e

(unless (file-exists-p vj-machine-specific-elisp-file)
  (message "*** CREATING THE FILE: %s ***" vj-machine-specific-elisp-file)
  (sit-for 2)
  (with-temp-file vj-machine-specific-elisp-file
    (insert ";; add stuff here\n\n;; footer\n")))

(load vj-machine-specific-elisp-file)

(setq debug-on-error nil
      debug-on-quit nil)
