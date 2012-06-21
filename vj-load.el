
(message "vj-load: system-type=%s system-name=%s LOGONSERVER=%s"
  system-type (system-name) (getenv "LOGONSERVER"))

(defvar vj-emacs-config-dir (file-name-directory (locate-file "vj-load.el" load-path)))

(add-to-list 'load-path (concat vj-emacs-config-dir "site-lisp"))

(defvar vj-load-site-lisp-path
  (expand-file-name (concat vj-emacs-config-dir "/..")))


;; FIXME org-mode needs special handling to override the built-in version
(setq vj-var (expand-file-name (concat vj-load-site-lisp-path "/org-mode")))
(when (file-directory-p vj-var)
  (add-to-list 'load-path (concat vj-var "/lisp"))
  (add-to-list 'load-path (concat vj-var "/contrib/lisp"))
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
)


;; --- vj-load-std ---

(require 'vj-std-essentials) ; <- Tip: Use M-q RET on package name to call
(require 'vj-std-extras)     ;         `find-file-at-point'
(require 'vj-set-key)
(load "vj-functions")
(load "vj-prog")

;; --- vj-load-packages ---

;; (load "help-mode") ; help-xref-following issue
(require 'auto-complete-config)
(ac-config-default)
(require 'auto-complete-etags)

(require 'anything)
(require 'anything-config)
(setq anything-enable-digit-shortcuts t)
(global-set-key (kbd "C-å") 'anything)

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

(require 'vj-grep)

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

(when window-system
  (message "server-start")
  (server-start))

