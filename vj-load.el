
(message "vj-load: system-type=%s system-name=%s LOGONSERVER=%s"
  system-type (system-name) (getenv "LOGONSERVER"))

;; --- vj-load-std ---

(require 'vj-std-essentials) ; <- Tip: Use M-q RET on package name to call
(require 'vj-std-extras)     ;         `find-file-at-point'
(require 'vj-set-key)
(load "vj-functions")
(load "vj-prog")

;; --- vj-load-packages ---

(defvar vj-emacs-config-dir (file-name-directory (locate-file "vj-load.el" load-path)))
(set-register ?d `(file . vj-emacs-config-dir))

(add-to-list 'load-path (concat vj-emacs-config-dir "site-lisp"))

(defvar vj-load-site-lisp-path
  (expand-file-name (concat vj-emacs-config-dir "/..")))



(require 'auto-complete-config)
(ac-config-default)
(require 'auto-complete-etags)


(defvar vj-load-site-lisp-prefix "vj-")

(defun vj-load-site-lisp-code (dir file)
  (add-to-list 'load-path dir nil)
  (message "Add %s and load %s" dir
    (concat vj-load-site-lisp-prefix (match-string 1 dir)))
  (load (concat vj-load-site-lisp-prefix (match-string 1 dir)) t))

(defun vj-load-site-lisp (base-package-names)
  "Load base-package-names if in vj-load-site-lisp-path.
This will add directories to load-path if they have been
downloaded and put in ~/site-lisp (change via
vj-load-site-lisp-path) if the prefix name exist in
`base-package-names' (name sans version number). Also 'vj-' +
base-package-name will be loaded. Set another prefix via
vj-load-site-lisp-prefix."
  (dolist (dir (directory-files vj-load-site-lisp-path t))
    (when (string-match
            (concat "/" (regexp-opt base-package-names t) "-?\\([^/]*\\)\\'")
            dir)
      (vj-load-site-lisp-code dir (match-string 1 dir)))))


(require 'anything)
(require 'anything-config)
(setq anything-enable-digit-shortcuts t)
(global-set-key (kbd "C-å") 'anything)

(add-to-list 'load-path (format "%s/org-mode/lisp" vj-load-site-lisp-path))
(add-to-list 'load-path (format "%s/org-mode/contrib/lisp" vj-load-site-lisp-path))
(eval-after-load "org"
  '(progn
     (when (fboundp 'org-babel-do-load-languages)
       (org-babel-do-load-languages
         'org-babel-load-languages
         '((perl . t)
            (ditaa . t)
            (sql . t)
            (shell . nil)
            (emacs-lisp . nil)
            )))))

(require 'bm)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

(when (file-directory-p (concat vj-emacs-config-dir "vj-complete"))
  (add-to-list (concat vj-emacs-config-dir "vj-complete"))
  (require 'vj-complete)
  ;; add eval-after-load here FIXME
  )

(defvar vj-grep-like-program
  ;; check system-type
  (if (eq system-type 'windows-nt)
    (progn
      (if (locate-file "grep.exe" exec-path) "grep.exe"
        (if (locate-file "findstr.exe" exec-path)
          "findstr.exe")))
    "grep")
  "Name of grep-like program that also kinda works on windows.
")

(require 'vps)
(vps-init "\M-i")


(when (>= emacs-major-version 24)
  ;; Redefine compilation-find-file!
  (load "vj-compilation-find-file-not-found-hook"))

(require 'anything-vps)

(add-to-list 'anything-sources 'anything-source-vps-files)

;; Generate a vps project based on all elisp files in `load-path'

(vps-add-project "load-path" `((dirs ,load-path) (ext ("el"))))

(vps-add-project "elisp" `((rdirs (,my-lisp-dir))
                           (dirs (,vj-emacs-config-dir)) (ext ("el"))))

(vps-change-project "elisp")

;; FIXME
(vps-add-project "vj-elisp" `((rdirs (,my-lisp-dir))
			      (dirs (,vj-emacs-config-dir))
			      (ext ("el"))))

(message "server-start")
(server-start)

