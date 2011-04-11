(message "vj-load: system-type=%s system-name=%s LOGONSERVER=%s"
  system-type (system-name) (getenv "LOGONSERVER"))

;; --- vj-load-std ---

(require 'vj-std-essentials) ; <- Tip: Use M-q M-p on package name to call
(require 'vj-std-extras)     ;         `find-file-at-point'
(require 'vj-set-key)
(load "vj-functions")

;; --- vj-load-packages ---

(defvar vj-emacs-config-dir (file-name-directory (locate-file "vj-load.el" load-path)))

(add-to-list 'load-path (concat vj-emacs-config-dir "site-lisp"))

(add-to-list 'load-path "~/site-lisp/auto-complete-1.3.1")
(safe-load "auto-complete-config")
(ac-config-default)

(require 'anything)
(require 'anything-config)
(setq anything-enable-digit-shortcuts t)
(global-set-key (kbd "C-å") 'anything)

(when (file-directory-p "~/site-lisp/yasnippet")
  (add-to-list 'load-path "~/site-lisp/yasnippet")
  (defvar vj-yasnippet-install-dir  "~/site-lisp/yasnippet")
  (add-to-list 'load-path vj-yasnippet-install-dir)
  (require 'yasnippet)
  (yas/initialize)
  (yas/load-directory vj-yasnippet-install-dir)
  (yas/load-directory (concat vj-emacs-config-dir "yasnippet")))

(add-to-list 'load-path "~/site-lisp/org-mode/lisp")
(add-to-list 'load-path "~/site-lisp/org-mode/contrib/lisp")
(eval-after-load "org"
  '(progn
     (when (fboundp 'org-babel-do-load-languages)
       (org-babel-do-load-languages
	'org-babel-load-languages
	'((perl . t)
	  (ditaa . t)
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

(require 'anything-vps)

(add-to-list 'anything-sources 'anything-source-vps-files)

;; Generate a vps project based on all elisp files in `load-path'
(vps-add-project "elisp" `((dirs ,load-path) (ext ("el"))))

(server-start)

