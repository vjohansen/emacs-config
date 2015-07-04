
(defun my-scroll-up-hook ()
   "My scroll-up."
   (interactive)
   (scroll-up 2)
)

(defun my-scroll-down-hook ()
   "My scroll-down."
   (interactive)
   (scroll-down 2)
)


(defun vj-os-open (filename)
  "Let the OS open the file with the appropriate program."
  (cond ((equal system-type 'darwin)
          (shell-command (concat "Open " (shell-quote-argument
                                           (expand-file-name filename)))))
    ((equal system-type 'windows-nt)
      (w32-shell-execute "open" (substitute ?\\ ?/ (expand-file-name filename))))
    (t
      (start-process "vj-os-open" nil "/usr/bin/open" (expand-file-name filename)))))


(defun eshell/lt () "vj 2009" (eshell/ls "-lrt"))

(defun eshell/open (FILE)
  "Let the OS determine how open a file."
  (vj-os-open FILE))

(defun eshell/e (filename)
  "alias e to find-file"
  (find-file filename))

(global-set-key "\M-:" 'vj-find-tag)

(defun vj-find-tag ()
    "My find-tag wrapper for easy repetition (VJO 2003).
 Call `find-tag' with current word first time and after that call
 find-tag with NEXT-P set to t (if called repeatedly)"
    (interactive)
    (vps-auto-change-project t)         ;can change last-command
;;    (message "vj-find-tag tag %s, cmd %s" last-tag last-command)
    (if (eq last-command 'vj-find-tag)
        (find-tag nil t)
        (find-tag (current-word) current-prefix-arg)))


(defun kill-whole-line ()
  "Kills the entire current line.
With prefix argument, kill that many lines from point."
  (interactive)
  (beginning-of-line)
  (let ((beg (point)))
    (end-of-line)
    (if (eq current-prefix-arg nil)
        (forward-line 1)
      (forward-line current-prefix-arg))
    (kill-region beg (point)))
  )

(defun copy-line-as-kill ()
  "Save the entire line as if killed, but keep it in buffer."
  (interactive)
  (let ((org (point)))
    (beginning-of-line)
    (let ((beg (point)))
      (end-of-line)
      (forward-line 1)
      (copy-region-as-kill beg (point)))
    (goto-char org))
  )





(defun vj-insert-local-variables-section ()
  ""
  (interactive)
  (goto-char (point-max))
  (insert (concat comment-start " Local " "Variables: ***\n"))
  (insert (concat comment-start " compile-command: " (format "\"%s\"" compile-command) "  ***\n"))
  (insert (concat comment-start " End: ***\n"))
  (previous-line 2)
  (end-of-line)
  (backward-word 1))



(defun list-buffers-change-window ( )
  "Display a list of names of existing buffers and
   jumps to that window."
  (interactive)
  (list-buffers)
  (other-window 1)
  (next-line 2)
  )




(defun vjo-forward-current-word-keep-offset ()
  " (VJO 1999)"
  (interactive)
  (let ((re-curword) (curword) (offset (point))
        (old-case-fold-search case-fold-search) )
    (setq curword (thing-at-point 'symbol))
    (setq re-curword (concat "\\<" (thing-at-point 'symbol) "\\>") )
    (beginning-of-thing 'symbol)
    (setq offset (- offset (point)))    ; offset from start of symbol/word
    (setq offset (- (length curword) offset)) ; offset from end
    ;;    (message "VJO-SEARCH-FWD: %s %d" curword  offset)
    (forward-char)
    (setq case-fold-search nil)
    (if (re-search-forward re-curword nil t)
        (backward-char offset)
      ;; else
      (progn (goto-char (point-min))
             (if (re-search-forward re-curword nil t)
                 (progn (message "Searching from top. %s" (what-line))
                        (backward-char offset))
               ;; else
               (message "Searching from top: Not found"))
             ))
    (setq case-fold-search old-case-fold-search)
    ))


(defun vjo-backward-current-word-keep-offset ()
  " (VJO 2002)"
  (interactive)
  (let ((re-curword) (curword) (offset (point))
        (old-case-fold-search case-fold-search) )
    (setq curword (thing-at-point 'symbol))
    (setq re-curword (concat "\\<" curword "\\>") )
    (beginning-of-thing 'symbol)
    (setq offset (- offset (point)))    ; offset from start of symbol/word
;;    (message "VJO-SEARCH-BACK: %s %d" curword  offset)
    (forward-char)
    (setq case-fold-search nil)
    (if (re-search-backward re-curword nil t)
        (forward-char offset)
      ;; else
      (progn (goto-char (point-max))
             (if (re-search-backward re-curword nil t)
                 (progn (message "Searching from bottom. %s" (what-line))
                        (forward-char offset))
               ;; else
               (message "Searching from bottom: Not found"))
             ))
    (setq case-fold-search old-case-fold-search)
    ))



;show ascii table
; From http://www.dotemacs.de/dotfiles/BenjaminRutt.emacs.html
(defun ascii-table ()
    "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
    (interactive)
    (switch-to-buffer "*ASCII*")
    (erase-buffer)
    (insert (format "ASCII characters up to number %d.\n" 254))
    (let ((i 0))
        (while (< i 254)
            (setq i (+ i 1))
            (insert (format "0x%02x %3d %c\n" i i i))))
    (beginning-of-buffer))



;;; Original author: ttn@netcom.com, 28-Jan-1996
(defun copy-and-inc-line-no-arg ()
    "Copies line, preserving cursor column, and increments any numbers found.
This should probably be generalized in the future."
    (interactive)
    (let* ((col (current-column))
              (bol (progn (beginning-of-line) (point)))
              (eol (progn (end-of-line) (point)))
              (line (buffer-substring bol eol)))
        (beginning-of-line)
        (while (re-search-forward "[0-9]+" eol 1)
            (let ((num (string-to-number (buffer-substring
                                          (match-beginning 0) (match-end 0)))))
                (replace-match (number-to-string (1+ num)))))
        (beginning-of-line)
        (insert line "\n")
        (move-to-column col)))

(defun copy-and-inc-line ()
    "Copy and yank current line with support for prearg (VJO 2002)"
    (interactive)
    (if current-prefix-arg
        (while (not (= current-prefix-arg 0))
            (progn
                (setq current-prefix-arg (- current-prefix-arg 1))
                (copy-and-inc-line-no-arg)
                ))
        (copy-and-inc-line-no-arg)))


(defun my-increment-number-at-point (&optional amount)
  "Increment the number under point by `amount'"
  (interactive "p")
  (let ((num (number-at-point)))
    (when (numberp num)
      (let ((newnum (+ num amount))
         (p (point)))
    (save-excursion
      (skip-chars-backward "-.0123456789")
      (delete-region (point) (+ (point) (length (number-to-string num))))
      (insert (number-to-string newnum)))
    (goto-char p)))))


(defun backslash-replace-on-region (beg end)
   "(VJO 2002)"
   (interactive "r")
   (replace-string "\\" "/" nil beg end))



;; ------------------------------------------------------------


(defun ext-unfill-paragraph ()
  "Do the opposite of `fill-paragraph'; stuff all lines in the current paragraph into a single long line."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))



(defun vj-tv-mode ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(45 70))
  (tool-bar-mode -1)
  (menu-bar-mode -1)
)

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))


;;(trace-function-background 'query-replace)
;; trick -- trace ignore -- (ignore "interesting" "stuff")


;; ------------------------------------------------------------




(defun vjo-forward-current-word-keep-offset ()
  " (VJO 1999)"
  (interactive)
  (let ((re-curword) (curword) (offset (point))
        (old-case-fold-search case-fold-search) )
    (setq curword (thing-at-point 'symbol))
    (setq re-curword (concat "\\<" (thing-at-point 'symbol) "\\>") )
    (beginning-of-thing 'symbol)
    (setq offset (- offset (point)))    ; offset from start of symbol/word
    (setq offset (- (length curword) offset)) ; offset from end
    ;;    (message "VJO-SEARCH-FWD: %s %d" curword  offset)
    (forward-char)
    (setq case-fold-search nil)
    (if (re-search-forward re-curword nil t)
        (backward-char offset)
      ;; else
      (progn (goto-char (point-min))
             (if (re-search-forward re-curword nil t)
                 (progn (message "Searching from top. %s" (what-line))
                        (backward-char offset))
               ;; else
               (message "Searching from top: Not found"))
             ))
    (setq case-fold-search old-case-fold-search)
    ))


(defun vjo-backward-current-word-keep-offset ()
  " (VJO 2002)"
  (interactive)
  (let ((re-curword) (curword) (offset (point))
        (old-case-fold-search case-fold-search) )
    (setq curword (thing-at-point 'symbol))
    (setq re-curword (concat "\\<" curword "\\>") )
    (beginning-of-thing 'symbol)
    (setq offset (- offset (point)))    ; offset from start of symbol/word
;;    (message "VJO-SEARCH-BACK: %s %d" curword  offset)
    (forward-char)
    (setq case-fold-search nil)
    (if (re-search-backward re-curword nil t)
        (forward-char offset)
      ;; else
      (progn (goto-char (point-max))
             (if (re-search-backward re-curword nil t)
                 (progn (message "Searching from bottom. %s" (what-line))
                        (forward-char offset))
               ;; else
               (message "Searching from bottom: Not found"))
             ))
    (setq case-fold-search old-case-fold-search)
    ))


(defun insert-char-above (&optional n)
  "Insert the character above point.
With a prefix arg, insert the N characters above point.
(Kevin Rodgers)"
  (interactive "p")
  (insert (save-excursion
            (next-line -1)
            (buffer-substring (point)
                              (progn
                                (forward-char n)
                                (point))))))

;; ------------------------------------------------------------


;;; TODO: only print "/* str */" after endif if more than 10 lines apart
;;; Should check if mark is active (`mark-active')
(defun vj-ifdef-insert(str)
  "my ifdef insertion (VJO 1997)"
  (interactive "*sEnter define: ")
  (if mark-active
    (save-excursion
      (if (> (mark) (point))
        (exchange-point-and-mark))
      (if (eq current-prefix-arg nil)
        (insert "#endif /* " str " */\n")
        (insert "#endif /* not " str " */\n"))
      (exchange-point-and-mark)
      (if (eq current-prefix-arg nil)
        (insert "#ifdef " str "\n")
        (insert "#ifndef " str "\n"))
      )))

(defun vj-ifndef-insert ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^\\( \\|$\\)" nil t)
    (forward-line 1)              ;skip file header
    (insert (concat "#ifndef " (tempo-c-cpp-hfilename) "\n"))
    (insert (concat "#define " (tempo-c-cpp-hfilename) "\n"))
    (goto-char (point-max))
    (insert (concat "\n#endif /* not " (tempo-c-cpp-hfilename) " */"))))


;; ------------------------------------------------------------


(defun slash-replace-on-region (beg end)
  "(VJO 2012)"
  (interactive "r")
  (let (from to)
    (save-excursion
      (goto-char beg)
      (when (search-forward-regexp "[/\\]+")
        (setq from (match-string-no-properties 0))
        (cond
          ((equal from "/") (setq to "\\"))
          ((equal from "\\") (setq to "\\\\"))
          ((equal from "\\\\") (setq to "/"))
          (t (message "No slash found")))))      ;never happens
    (if to
      (save-excursion
        (replace-string from to nil beg end)))))

(defvar vj-perl-history
  '("perl -MText::Autoformat -e 'autoformat'"
     "$S=','; @F=split(qr($S)); $F[1]=  ; $_=join($S,@F);" ; field manip
     "$_ = \"$. - $_\""                 ; number lines
     "s///g"))

(defun perl-on-region (command)
  (interactive
    (let ((string (read-string "perl -pe " nil 'vj-perl-history)))
      (list string)))
  ;; (kill-ring-save (mark) (point))
  (let ((coding-system-for-write 'raw-text))
    (shell-command-on-region (mark) (point)
      (format "perl -pe %s" (shell-quote-argument command)) nil t)))

(defun vj-git-grep ()
  (interactive)
  (let ((grep-use-null-device)
         (dir (vj-chomp (shell-command-to-string "git rev-parse --show-toplevel"))))
    (grep (format "cd %s && git --no-pager grep -n %s" dir (current-word)))))


;; ------------------------------------------------------------

(defun vj-line-at-point ()
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun vj-mark-paragraph ()
  (interactive)
  (if (string-match "^[ \\t]*$" (vj-line-at-point))
    (re-search-forward "[^ \t\n]" nil t)
    ;;
    (end-of-line)
    (re-search-backward "^[ \\t]*$" nil t)
    (next-line 1))
  (beginning-of-line)
  (set-mark-command nil)
  (re-search-forward "^[ \\t]*$" nil t)
  (exchange-point-and-mark))

;; ------------------------------------------------------------

(setq vj-package-list '( json-reformat
                         company
                         company-statistics
                         helm
                         guide-key
                         gitconfig-mode
                         gitignore-mode))

(defun vj-install-missing-packages ()
  (interactive)
  ;; Refresh
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  ;; Install the missing packages
  (dolist (package vj-package-list)
    (unless (package-installed-p package)
      (package-install package))))
