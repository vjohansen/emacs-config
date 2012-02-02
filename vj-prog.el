

; ------------------------------------------------------------

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist (cons '("\\.cs\\'" . csharp-mode) auto-mode-alist))

(eval-after-load "csharp-mode"
  '(progn
     (add-hook 'csharp-mode-hook 'csharp-mode-hook)))

; ------------------------------------------------------------


(setq auto-mode-alist (cons '("\\.p[lm]$" . cperl-mode) auto-mode-alist))

;; (setq cperl-array-face (make-face 'cperl-array-face))
;; (set-face-foreground 'cperl-array-face "DarkMagenta")
;;                      ;(set-face-background 'cperl-array-face "gray90")
;; (setq cperl-hash-face (make-face 'cperl-hash-face))
;; (set-face-foreground 'cperl-hash-face "MediumVioletRed")
;;                       ;(set-face-background 'cperl-hash-face "gray90")

;;(setq font-lock-constant-face (make-face 'font-lock-constant-face))
;;(set-face-background 'font-lock-constant-face "Thistle")


(add-hook 'cperl-mode-hook 'vj-cperl-mode-hook)

(defun vj-cperl-mode-hook ()
  "VJOs Perl mode hook for cperl-mode"

 (make-local-variable 'compile-command)
  (setq compile-command
    (concat
      (if (equal vj-env 'home-mac) "/opt/local/bin/perl" "perl")
      " -w " (file-name-nondirectory (buffer-file-name))))
  (message "vj-perl: set compile-command to \"%s\"" compile-command)


  (setq cperl-indent-level 2
    cperl-continued-statement-offset 2
    cperl-continued-brace-offset -2
    cperl-brace-offset 0
    cperl-brace-imaginary-offset 0
    cperl-label-offset 0		; was -2
    ;;    cperl-electric-parens 'null
    ;;    cperl-electric-parens 't
    cperl-indent-parens-as-block t
    comment-column 40)
  )



;; make perldocs work on windows
(defun win-cperl-perldoc (cmds)
  ;; quick hack by Mike Slass <mikesl@wrq.com>
  "Hack to make perldocs sorta work under windows.

Works using the perldoc.bat that ships with ActiveState Perl;
that file will need to be in your path."
  (interactive
   (list
    (read-from-minibuffer
     "perldoc: "
     (cperl-word-at-point))))
  (shell-command
   (concat
    "perldoc "
    (shell-quote-argument cmds))
   (let ((buf (get-buffer-create "*perldoc*")))
     (set-buffer buf)
     (delete-region (point-min) (point-max))
     buf))
;;      (outline-mode)
;;      (setq outline-regexp "^[A-Z]")
    )




;; (eval-after-load
;;  "compile"
;;  ;; Append regexp for Perl errors to global list (20.2 misses the "." case)
;;  '(setq compilation-error-regexp-alist  ; Perl
;; 	(append compilation-error-regexp-alist
;; 		'(;; <Good Perl advice> at foo.cgi line 13.
;; 		  ;; syntax error at bar.pl line 270, near ...
;; 		  ;; NB: Emacs 20.2 needs the `.*' (implicitly anchored w/ `^')
;; 		  (".* at \\([^ ]+\\) line \\([0-9]+\\)[,.]" 1 2)))))

;; (eval-after-load
;;  "compile"
;;  '(setq compilation-error-regexp-alist  ; "assertion .."
;; 	(append compilation-error-regexp-alist
;; 		'(;; <Good Perl advice> at foo.cgi line 13.
;; 		  ;; syntax error at bar.pl line 270, near ...
;; 		  ;; NB: Emacs 20.2 needs the `.*' (implicitly anchored w/ `^')
;; 		  ("assertion \\([^ ]+\\), line \\([0-9]+\\)" 1 2)))))
;; ;;		  ("failed: file \"\\([^ ]+\\), line \\([0-9]+\\)" 1 2)))))

;; ;;		  ("assertion .* failed: file \\([^ ]+\\), line \\([0-9]+\\)[,.]" 1 2)))))


(defun perl-region (command)
  "Run perl command on selected region"
  (interactive "sWhat command: perl ")
  (shell-command-on-region (mark) (point) (concat "perl " command) t t))


(defun perl-autoformat-on-region ()
  "Run perl command on selected region"
  (interactive)
  (shell-command-on-region (mark) (point)
    "perl -MText::Autoformat -e 'autoformat'" t t))

; ------------------------------------------------------------


;;; XML

(require 'flyspell)
(add-to-list 'flyspell-prog-text-faces 'nxml-text-face)

(eval-after-load "nxml-mode"
  '(progn
     (define-key nxml-mode-map "\C-c\C-r" 'rng-reload-schema)

     (defun rng-reload-schema ()
       (interactive)
       (let ((schema-filename rng-current-schema-file-name))
         (when schema-filename
           (setq rng-current-schema (rng-load-schema schema-filename))
           (run-hooks 'rng-schema-change-hook)
           (message "Reloaded schema %s" schema-filename))
         (unless schema-filename
           (rng-set-schema-file-and-validate schema-filename)))) ;; VJ fixed

     (defun rng-apply-find-schema-file (fn)
       (let ((schema-filename rng-current-schema-file-name))
         (unless schema-filename
           (error "This file is not associated with a schema file."))
         (funcall fn schema-filename)))

     (defun rng-find-schema-file ()
       (interactive)
       (rng-apply-find-schema-file 'find-file))

     (defun rng-find-schema-file-other-window ()
       (interactive)
       (rng-apply-find-schema-file 'find-file-other-window))))


(autoload 'rnc-mode "rnc-mode")
(setq auto-mode-alist (cons '("\\.rnc\\'" . rnc-mode) auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.x[ms]l\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.vcproj\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.proj\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.snippet\\'" . nxml-mode))


; ------------------------------------------------------------
