;; --- vj-load-std ---

(require 'vj-std-essentials) ; <- Tip: Use M-q RET on package name to call
(require 'vj-std-extras)     ;         `find-file-at-point'
(require 'vj-set-key)
(load "vj-functions")
(load "vj-prog")

;; --- vj-load-packages ---

(require 'vps)
(vps-init "\M-i")

(require 'vj-grep)

(when (>= emacs-major-version 24)
  ;; Redefine compilation-find-file! Needed by vps
  (load "vj-compilation-find-file-not-found-hook"))

;; Generate a vps project based on all elisp files in `load-path'

(vps-add-project "load-path" `((dirs ,load-path) (ext ("el"))))

(vps-add-project "elisp" `((rdirs (,my-lisp-dir))
                           (dirs (,vj-emacs-config-dir)) (ext ("el"))))

(vps-change-project "elisp")

(when window-system
  (message "server-start")
  (server-start))

