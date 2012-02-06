
;;
;; TO LOAD THIS FILE ADD THE FOLLOWING TO .emacs or ~.emacs.d/init.el
;;
;;   (add-to-list 'load-path "~/site-lisp/emacs-config/")
;;   (setq my-lisp-dir "~/elisp")
;;   (load "vj-init")
;;

(setq debug-on-error t
      debug-on-quit t)
(set-background-color "#d8f0f0")

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
(load custom-file t)
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
(if (file-exists-p vj-machine-specific-elisp-file)
    (load vj-machine-specific-elisp-file)
  (message "*** Create the file: %s ***" vj-machine-specific-elisp-file))
(set-register ?e `(file . ,vj-machine-specific-elisp-file)) ;; C-x r j e

(setq debug-on-error nil
      debug-on-quit nil)
