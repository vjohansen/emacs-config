(setq auto-mode-alist (cons '("\\.p[lm]$" . cperl-mode) auto-mode-alist))

;; (setq cperl-array-face (make-face 'cperl-array-face))
;; (set-face-foreground 'cperl-array-face "DarkMagenta")
;;                      ;(set-face-background 'cperl-array-face "gray90")
;; (setq cperl-hash-face (make-face 'cperl-hash-face))
;; (set-face-foreground 'cperl-hash-face "MediumVioletRed")
;;                       ;(set-face-background 'cperl-hash-face "gray90")

;;(setq font-lock-constant-face (make-face 'font-lock-constant-face))
;;(set-face-background 'font-lock-constant-face "Thistle")


(add-hook 'cperl-mode-hook 'vjo-cperl-mode-hook)

(defun vjo-cperl-mode-hook ()
  "VJOs Perl mode hook for cperl-mode"

;;  (make-local-variable 'compile-command)
  ;; (setq compile-command
  ;;   (concat
  ;;     (if (equal vj-env 'home-mac) "/opt/local/bin/perl" "perl")
  ;;     " -w " (file-name-nondirectory (buffer-file-name))))
  ;; (message "vj-perl: set compile-command to \"%s\"" compile-command)


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


;; (defun perl-region (command)
;;   "Run perl command on selected region"
;;   (interactive "sWhat command: perl ")
;;   (shell-command-on-region (mark) (point) (concat "perl " command) t t))

;; (defun perl-buffer (command)
;;   "Run perl command on entire buffer"
;;   (interactive "sWhat command: perl ")
;;   (shell-command-on-region (point-min) (point-max) (concat "perl " command) t t))

;; (defun perl-point (command)
;;   "Run perl command at point"
;;   (interactive "sWhat command: perl ")
;;   (shell-command (concat "perl " command) t))


;; (defun perl-autoformat-on-region ()
;;   "Run perl command on selected region"
;;   (interactive)
;;   (shell-command-on-region (mark) (point)
;;     "perl -MText::Autoformat -e 'autoformat'" t t))

;; TODO
;; http://search.cpan.org/~perler/Devel-IntelliPerl-0.02/lib/Devel/IntelliPerl.pm
