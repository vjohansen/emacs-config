

(setq gdb-many-windows t)
(setq dired-dwim-target            t
      dired-auto-revert-buffer     t
      display-time-24hr-format     t
      tags-revert-without-query    t
      large-file-warning-threshold 100000000
      comint-input-ignoredups      t
      make-pointer-invisible       nil)

(setq inhibit-splash-screen t
      max-lisp-eval-depth 1000
      garbage-collection-messages nil
      gc-cons-threshold (* 8 400000))


(setq default-major-mode 'text-mode)
(setq initial-major-mode 'emacs-lisp-mode)

(if (file-directory-p "~/temp/emacs")
  (setq backup-directory-alist '(("." . "~/temp/emacs")))
  ;; else
  (message "please create ~/temp/emacs (for backup files)"))

(setq eshell-ask-to-save-history 'always)

(setq
  ls-lisp-dirs-first t
  ls-lisp-ignore-case t
  mark-even-if-inactive t
  x-stretch-cursor t)

(setq calendar-week-start-day 1)	;monday
;;(minibuffer-electric-default-mode 1)

;; ------------------------------------------------------------

;; calendar and diary stuff

(require 'appt)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
;;(add-hook 'diary-display-hook 'fancy-diary-display)

(display-time)
;; (add-hook 'diary-hook 'appt-make-list)
;; (diary 0)
(if (< emacs-major-version 23)
   (european-calendar)
  (calendar-set-date-style 'iso))
(setq display-time-24hr-format t)

;; ------------------------------------------------------------

(require 'generic-x)

(require 'dired-x)

(autoload 'wdired-change-to-wdired-mode "wdired")
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
     (define-key dired-mode-map
       [menu-bar immediate wdired-change-to-wdired-mode]
       '("Edit File Names" . wdired-change-to-wdired-mode))))

;; Add different directory sorting keys to dired
(eval-after-load "dired"
  '(mapc (lambda (elt)
           (define-key dired-mode-map (car elt)
             `(lambda ()
                (interactive)
                (dired-sort-other (concat dired-listing-switches
                                    ,(cadr elt))))))
     '(([(control f3)]       ""     "by name")
        ([(control f4)]       " -X"  "by extension")
        ([(control f5)]       " -t"  "by date")
        ([(control f6)]       " -S"  "by size")
        ([(control shift f3)] " -r"  "by reverse name")
        ([(control shift f4)] " -rX" "by reverse extension")
        ([(control shift f5)] " -rt" "by reverse date")
        ([(control shift f6)] " -rS" "by reverse size"))))


;; ------------------------------------------------------------

;; Extra C-h .. keybindings
(find-function-setup-keys)



;; ------------------------------------------------------------
(defun isearch-yank-symbol-or-char ()
  "Pull next character or symbol from buffer into search string."
  (interactive)
  (isearch-yank-internal
   (lambda ()
     (if (or (memq (char-syntax (or (char-after) 0)) '(?w ?_))
             (memq (char-syntax (or (char-after (1+ (point))) 0)) '(?w ?_)))
         (forward-symbol 1)
       (forward-char 1))
     (point))))

(define-key isearch-mode-map (kbd "C-S-w") 'isearch-yank-symbol-or-char)

;; ------------------------------------------------------------
(defvar safe-load-error-list ""
        "*List of files that reported errors when loaded via safe-load")

(defun safe-load (file)
  "Load a file.  If error when loading, report back and continue on"
  (interactive "f")
  (condition-case nil (load file nil t)
    (error
      (progn
       (setq safe-load-error-list  (concat safe-load-error-list  " " file))
       (message (concat "****** error loading " safe-load-error-list)) nil))))

;; ------------------------------------------------------------

;;; Kill Scratch Buffer
(let ((b (get-buffer "*scratch*")))
  (when b
    (kill-buffer b)))

(provide 'vj-std-extras)
