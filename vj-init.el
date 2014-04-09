
;;
;; TO LOAD THIS FILE ADD THE FOLLOWING TO .emacs or ~.emacs.d/init.el
;;
;;   (add-to-list 'load-path "~/site-lisp/emacs-config/")
;;   (setq my-lisp-dir "~/elisp")
;;   (load "vj-init")
;;

(setq debug-on-error t
      debug-on-quit t)


(defvar vj-background-color-old nil)
(when (< emacs-major-version 24)
  (setq vj-background-color-old (frame-parameter nil 'background-color))
  (set-background-color "#d8f0f0"))

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

(load "vj-load")

(when (< emacs-major-version 24)
  (set-background-color "#ffeedd"))

(defvar vj-system-type-specific-elisp-file
  (format "%s/system-type-%s.el"
    my-lisp-dir
    (downcase (replace-regexp-in-string "[^a-zA-Z0-9_]+" "-"
                (symbol-name system-type)))))
(set-register ?s `(file . ,vj-system-type-specific-elisp-file)) ;; C-x r j s
(if (file-exists-p vj-system-type-specific-elisp-file)
    (load vj-system-type-specific-elisp-file)
  (message "*** Create the file: %s ***" vj-system-type-specific-elisp-file))


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
