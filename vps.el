;;; vps.el --- Define a project (set of dirs) and handle tags, grep etc.

;; Copyright (C) 2003-2012 Vagn Johansen

;;; Commentary:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Installation:
;;
;; (require 'vps)
;; (vps-init "\M-i") ;; \M-i is default key-prefix
;;
;; ;; Define projects
;; (vps-add-project "vjo_elisp"
;;     '((dirs ("c:/vjo/setup/emacs/" "c:/tools/emacs-21.3/site-lisp"))
;;          (ext ("el"))
;;          (file-cache-add t)))
;;
;; (vps-add-project "Emacs_load-path" `((dirs ,load-path) (ext ("el"))))
;; (vps-add-project "C++" '((rdirs ("/src/project-root" "/usr/include"))))
;;
;;

;;; History:
;;
;; Added python version of vgrep22.pl
;;   Use (setq vps-vgrep-call (concat "python " (locate-library "vgrep.py")))
;;       (setq null-device nil) ;; Do not append NUL to grep command calls


;;; Code:

(require 'compile)
(require 'etags)
(require 'dired-x)
(require 'grep)

(defun vps-ensure-trailing-slash (dir)
  (replace-regexp-in-string "\\([^/\\]\\)$" "\\1/" dir))

(defun vps-chomp (str)
  "Remove trailing newline a la Perls chomp function."
  (replace-regexp-in-string "[\015\012]+\\'" "" str))

(defun vps-file-last-modified-duration (filename)
  "Return seconds from now to last modified for FILENAME."
  (let ((mtime (nth 5 (file-attributes filename))))
    (time-to-seconds (time-since mtime))))

;;(eval-when-compile (require 'c-includes))

(defgroup vps nil
  "Project settings (grepping, tags building, etc) via directory sets."
  :link  '(url-link "http://ozymandias.dk/emacs/emacs.html#vps")
  :group 'convenience)


(defcustom vps-alist nil
  "Contains alls vps project definitions. Use vps-add-project to add projects.

 (vps-add-project \"name\"
  '((dirs  (\"~/dir1\" \"~/dir2\"))
    (rdirs (\"~/topdir3\")
    (ext   (\"c\" \"h\")))))

Don't use customize."
  :type '(alist :key-type string
                :value-type (alist :key-type symbol :value-type sexp))
  :group 'vps)


(defcustom vps-change-project-hook nil
  "Hook that is run when the project is changed."
  :type 'hook
  :group 'vps)

(defcustom vps-write-file-list-hook nil
  "Hook that is run when all filenames have been inserted.

Useful for removing build filenames from the list:

  (add-hook 'vps-write-file-list-hook
    (lambda ()
      (goto-char (point-min))
      (flush-lines \"/build/\")))
"
  :type 'hook
  :group 'vps)

(defcustom vps-max-tags-age-in-days 3
  "Max allowed age of tags file for current or selected project before rebuilding."
  :type 'integer
  :group 'vps)

(defcustom vps-max-rdirs-cache-age-in-days 6
  "Max allowed age of directory cache file for current project or selected before rebuilding."
  :type 'integer
  :group 'vps)

(defcustom vps-skip-dir-regexp "\\(node_modules\\)$"
  "When generating directory lists from rdir setting then prune directory tree
if they match this setting."
  :type 'string
  :group 'vps)


(defvar vps-project-dir "~/.emacs.d/vps")
(defvar vps-perl-program "perl")
(defvar vps-etags-program "etags")

(defvar vps-default-setting-alist
  '(
     (ext ("c" "cpp" "cxx" "cc" "h" "hpp" "el"))
     (auto-build-tags t)
     (file-cache-add nil)
     (setup-function nil)
     (filename-pregexp nil)
     )
  "Fallback values for projects. Update with vps-add-default-setting")

(defvar vps-vgrep-call
  (concat vps-perl-program " "
          (or (locate-library "vgrep22.pl") "-S vgrep22.pl"))
  "Command line for calling vgrep22.pl program.")

;; Internal globals
(defvar vps-project-name nil "Current project.")
(defvar vps-last-tags-filename "")
(defvar vps-rdir-cache nil)
(defvar vps-rdir-cache-timestamp nil)
(defvar vps-vj-search-index-program nil "Cached location of search program")


(defun vps-init (&optional key-prefix)
  "Set default keybindings and hooks.
Optional argument KEY-PREFIX overrides builtin M-i."
  (interactive)
  ;; (setq imenu-scanning-message nil)
  (unless key-prefix (setq key-prefix "\M-i"))
  (if (not (keymapp (lookup-key global-map key-prefix)))
    (global-unset-key key-prefix))
  (global-set-key (concat key-prefix "\M-i") 'vps-auto-change-project)
  (global-set-key (concat key-prefix "i") 'vps-auto-change-project)
  (global-set-key (concat key-prefix "\M-g") 'vps-grep)
  (global-set-key (concat key-prefix "\M-o") 'vps-compile)
  (global-set-key (concat key-prefix "g") 'vps-grep-ignore-case)
  (global-set-key (concat key-prefix "s") 'vps-grep-via-index)
  (global-set-key (concat key-prefix "l") 'vps-list-dirs)
  (global-set-key (concat key-prefix "u") 'vps-add-to-c-includes-path)
  (global-set-key (concat key-prefix "c") 'vps-change-project)
  (global-set-key (concat key-prefix "d") 'vps-virtual-dired)
;;  (add-hook 'find-file-hooks ' vps-detect-project-change t)
;;  (remove-hook 'find-file-hooks 'vps-detect-project-change)

  ;; Create project directory if necessary
  (unless (file-directory-p (vps-project-dir))
    (if (y-or-n-p (format "vps: Create *required* project directory: %s "
                    vps-project-dir))
      (make-directory (vps-project-dir) t)))
  )

(defun vps-add-default-setting (setting settingdef)
  "Define a setting called SETTING with a setting definition SETTINGDEF."
  (let ((entry (assoc setting vps-default-setting-alist)))
    (if (not entry)
        (setq vps-alist (cons (cons setting settingdef)
                              vps-default-setting-alist))
      ;;else: modifying existing definition
      (setcdr entry settingdef)
      )))

(defun vps-completing-read (&rest args)
  "Like `completing-read' but send TAB so candidates are listed immediately.
Optional argument ARGS is the list of possible completions."
  (let ((unread-command-events (cons ?\t unread-command-events)))
    (apply 'completing-read args)))

(defun vps-icompleting-read (prompt choices)
  (let ((iswitchb-make-buflist-hook
          (lambda ()
            (setq iswitchb-temp-buflist choices))))
    (iswitchb-read-buffer prompt)))



(defun vps-all-sub-directories (directory)
  "Return all directories recursively below DIRECTORY."
  (let ((sub-dirs) (dir (vps-ensure-trailing-slash directory)))
    (dolist (maybe-dir (directory-files
                         dir
                         t              ; FULL
                         "^[^\\.]"      ; Ignore ^\.*
                         ))

      (when (and (file-directory-p maybe-dir)
              (not (string-match vps-skip-dir-regexp maybe-dir)))
        (add-to-list 'sub-dirs maybe-dir)
        (setq sub-dirs (append sub-dirs (vps-all-sub-directories maybe-dir)))))
    sub-dirs))


(defun vps-detect-project-change ()
    (let* ((proj-list (vps-get-project-list (buffer-file-name)))
              (proj-count (length proj-list)))
        (if proj-list
            (if (member vps-project-name proj-list)
                (message "%s belongs to current project: %s"
                  (file-name-nondirectory buffer-file-name)
                  vps-project-name)
                (message "Suggest project change to %s"
                    (mapconcat 'identity proj-list " or "))))))

(defun vps-add-project (name projdef)
    "Define a project called NAME with a project definition PROJDEF."
    (interactive)
    (unless (listp (cadr (assoc 'dirs projdef)))
      (error "dirs setting must be a list"))
    (unless (listp (cadr (assoc 'rdirs projdef)))
      (error "rdirs setting must be a list"))
    (let ((entry (assoc name vps-alist)))
        (if (not entry)
            (setq vps-alist (cons (cons name projdef) vps-alist))
            ;;else: Modifying existing definition
          (let ((vps-project-name name))
            (if (file-exists-p (vps-tags-filename))
              (delete-file (vps-tags-filename)))
            (if (file-exists-p (vps-dirlist-filename))
              (delete-file (vps-dirlist-filename)))
            (if (file-exists-p (vps-filelist-filename))
              (delete-file (vps-filelist-filename)))
            (if (file-exists-p (vps-alldirslist-filename))
              (delete-file (vps-alldirslist-filename)))
            (if (file-exists-p (vps-index-db-filename))
              (delete-file (vps-index-db-filename))))
            (setq vps-rdir-cache-timestamp nil)
            (setcdr entry projdef)
            )))

(defun vps-strip-slash (dirname)
    "Remove trailing slash from DIRNAME."
    (replace-regexp-in-string "[/\\\\]$" "" dirname)
 )

(defun vps-same-dir (dir1 dir2)
  "Are DIR1 and DIR2 the same dir regardless of case and trailing slashes?"
  (string=
    (downcase (vps-strip-slash dir1))
    (downcase (vps-strip-slash dir2))))

(defun vps-project-dir ()
    (expand-file-name vps-project-dir))

(defun vps-tags-filename ()
    (concat (vps-project-dir) "/" vps-project-name ".tags"))

(defun vps-ctags-filename ()
    (concat (vps-project-dir) "/" vps-project-name ".ctags"))

(defun vps-filelist-filename ()
    (concat (vps-project-dir) "/" vps-project-name ".txt"))

(defun vps-dirlist-filename ()
    (concat (vps-project-dir) "/" vps-project-name ".dirs"))

(defun vps-alldirslist-filename ()
    (concat (vps-project-dir) "/" vps-project-name ".alldirs"))

(defun vps-index-db-filename (&optional proj-name)
  (concat (vps-project-dir) "/" (or proj-name) vps-project-name ".db"))

(defun vps-project (&optional proj-name)
    "Return a project given a project name.
If no project name is supplied, use the current project name."
    (if (not proj-name) (setq proj-name vps-project-name))
    (let ((pair (assoc proj-name vps-alist)))
        (if (null pair)
            (error "No such project defined: %s" proj-name)
            (cdr pair))))


(defun vps-ext-regexp (proj-name)
    (concat
        "\\.\\("
        (regexp-opt (cadr (vps-get-setting 'ext proj-name)))
        "\\)\\'"))


(defun vps-file-to-string (file)
  "Read the content of FILE and return it as a string."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-substring (point-min) (point-max)))
    (file-error nil)))

(defun vps-rebuild-rdir-cache (rdirs)
  (if (file-exists-p (vps-dirlist-filename))
    (message "vps: Rebuilding directory cache for %s" vps-project-name)
    (message "vps: Building directory cache for %s" vps-project-name))
  (let ((collected-dirs))
    (dolist (rdir (cadr rdirs))
      (add-to-list 'collected-dirs (expand-file-name rdir) t)
      (dolist (dir (vps-all-sub-directories rdir))
        (if (> (length (vps-directory-files dir)) 0)
            (add-to-list 'collected-dirs dir t))))

    (with-temp-file (vps-dirlist-filename)
      (set-buffer-file-coding-system 'raw-text)
      (dolist (dir collected-dirs)
        (insert dir "\n")))

    (setq vps-rdir-cache collected-dirs)
    (setq vps-rdir-cache-timestamp (nth 5 (file-attributes (vps-dirlist-filename))))

    collected-dirs))


(defun vps-rdirs-file-cache ()
  "Return 'rdirs settings file cache if any."
  ;; return vps-rdir-cache
  (setq vps-rdir-cache
        (or
         (if (not (file-exists-p (vps-dirlist-filename)))
             ;; rebuild cache file
             (vps-rebuild-rdir-cache (vps-get-setting-or-default 'rdirs nil)))
         ;; load cache file from disk
         (prog1 (split-string (vps-chomp (vps-file-to-string (vps-dirlist-filename))) "\n")
                (setq vps-rdir-cache-timestamp
                      (nth 5 (file-attributes (vps-dirlist-filename)))))
         )))



(defun vps-rdirs ()
  "Return actual (sub)directories for the 'rdir setting."
  (let* ((rdirs (vps-get-setting-or-default 'rdirs nil))
         (max-rdirs-cache-age (vps-get-setting-or-default
                               'max-rdirs-cache-age
                               vps-max-rdirs-cache-age-in-days)))

    (if (not vps-rdir-cache-timestamp)
        (vps-rdirs-file-cache)
      (if (> (time-to-seconds (time-since vps-rdir-cache-timestamp))
             (* max-rdirs-cache-age 24 60 60)) ;; in days
          ;; Rebuild too old file if allowed by user
          (if (not (file-exists-p (vps-dirlist-filename)))
              (vps-rebuild-rdir-cache rdirs)
            ;; Ask before rebuilding cache files larger than 10000 bytes
            (if (> (nth 7 (file-attributes (vps-dirlist-filename))) 10000)
                (if (y-or-n-p "vps: Rebuild directory cache? ")
                    (vps-rebuild-rdir-cache rdirs)
                  ;; refresh both timestamps (memory and file) - fake rebuild
                  (shell-command (format "touch %s" (shell-quote-argument (vps-dirlist-filename))))
                  (setq vps-rdir-cache-timestamp
                       (nth 5 (file-attributes (vps-dirlist-filename))))
                  vps-rdir-cache)
              (vps-rebuild-rdir-cache rdirs)))
        ;; not time for rebuild yet
        (or vps-rdir-cache (vps-rdirs-file-cache))
        ))))



(defun vps-dirs (&optional proj-name)
  (let ((dirs (vps-get-setting-or-default 'dirs nil proj-name))
         (rdirs (vps-get-setting-or-default 'rdirs nil proj-name)))
    ;; return
    (append
      (if dirs (cadr dirs))
      (if rdirs (vps-rdirs))
      )))

(defun vps-valid-dirs (&optional proj-name)
    "todo vps-valid-dirs file-directory-p and expand-file-name removes duplicates,."
    (let ((valid-list nil))
        (dolist (userdir (vps-dirs vps-project-name))
            (let ((dir (vps-strip-slash (expand-file-name userdir)))
                     seen-list)
                (unless (or (member-ignore-case dir seen-list)
                            (not (file-directory-p dir)))
                    (add-to-list 'valid-list dir))
                (add-to-list 'seen-list dir)))
        valid-list))

(defun vps-dirs-and-ext-as-string (proj-name)
    (concat
        (mapconcat 'identity (cadr (vps-get-setting 'ext proj-name)) ",") " "
        (mapconcat 'shell-quote-argument (vps-dirs) " ")))

(defun vps-get-setting (setting &optional proj-name)
    "Get setting from project setting or vps-default-setting-alist (returns return value from assoc)."
    (if (not proj-name) (setq proj-name vps-project-name))
    (or
        (assoc setting (vps-project proj-name))
        (assoc setting vps-default-setting-alist)
        (error "No such setting: %s" (symbol-name setting))))

(defun vps-get-setting-or-default (setting default &optional proj-name)
    "Get setting from project setting or vps-default-setting-alist (returns return value from assoc)."
    (if (not proj-name) (setq proj-name vps-project-name))
    (or
        (assoc setting (vps-project proj-name))
        (assoc setting vps-default-setting-alist)
        default))

;; (defun vps-get-project-list-old (filename)
;;     "Return the project list that FILENAME belongs to."
;;     (let ((file-list nil))
;;         (dolist (proj vps-alist)
;;             (let ((dir1 (file-name-directory filename))
;;                      (proj-name (car proj)))
;;                 (dolist (dir2 (vps-dirs proj-name))
;;                     (when (vps-same-dir dir1 (expand-file-name dir2))
;; ;;                        (message "DIRHIT %s %s" dir1 (expand-file-name dir2))
;;                         ;; check extension FIXME
;;                         (add-to-list 'file-list proj-name t)
;;                         ))))
;; ;;        (message "file-list: %s" (mapconcat 'identity file-list ", "))
;;         file-list))


;; (vps-add-project "vtest" '((dirs ("~/site-lisp")) (rdirs ("~/elisp")) (ext ("el"))))

(defun vps-get-project-list (filename)
  "Return a list of the projects that FILENAME belongs to."
  (let ((file-list nil)
  (dir1 (file-name-directory filename)))
    (dolist (proj vps-alist)
      (let* ((proj-name (car proj))
       (dirs (vps-get-setting-or-default 'dirs nil proj-name))
       (rdirs (vps-get-setting-or-default 'rdirs nil proj-name))
       add-dir )

  (if dirs
      (dolist (dir2 (cadr dirs))
        (when (vps-same-dir dir1 (expand-file-name dir2))
    (setq add-dir t))))

  (if rdirs
      (dolist (rdir (cadr rdirs))
        (when (string-match
         (concat "^" (regexp-quote (expand-file-name rdir)))
         (expand-file-name dir1))
    (setq add-dir t))))

  (if add-dir
      (add-to-list 'file-list proj-name t))

  ))
    ;;    (message "file-list: %s" (mapconcat 'identity file-list ", "))
    file-list))


(defun vps-change-project (&optional proj-name)
  "Change current project interactively or programmatically via PROJ-NAME."
  (interactive)
  (if (interactive-p)
    (setq proj-name
      (vps-completing-read "Select project: "
        (mapcar (lambda (pair)(cons (car pair) (car pair)))
          vps-alist)
        nil                             ; PREDICATE
        t)))                            ; REQUIRE-MATCH
  (setq vps-project-name proj-name)

  (setq vps-rdir-cache nil)
  (setq vps-rdir-cache-timestamp nil)
  (let ((tags-age)
         (tags-filename (vps-tags-filename))
         (auto-build-tags (cadr (vps-get-setting 'auto-build-tags))))

    (when auto-build-tags
      (if (not (file-exists-p tags-filename))
        (vps-make-tags)
        ;; else: tags exists
        (progn
          (setq tags-age
            (/ (vps-file-last-modified-duration (vps-tags-filename))
              (* 24 60 60))) ;; in days
          (when (and (> tags-age
                        (vps-get-setting-or-default 'max-tags-age
                                                    vps-max-tags-age-in-days))
                  (< (length (vps-dirs)) 30))
            (message "vps: tags file was created %.1f days ago" tags-age)
            (sit-for 1)
            (vps-make-tags))
          )
        )
      (vps-visit-tags))

    (message "Project %s" proj-name))

  (when (and (featurep 'filecache) (cadr (vps-get-setting 'file-cache-add)))
    (sit-for 1)
    (message "vps: file-cache-add")
    (file-cache-add-directory-list (vps-dirs)))
  ;;         (setq c-includes-path (vps-dirs))

  (run-hooks 'vps-change-project-hook)
  (let ((setup-function (cadr (vps-get-setting 'setup-function))))
    (when setup-function
      (funcall setup-function)))
  )


(defun vps-auto-change-project (&optional nomessage)
  (interactive)
  (if (and buffer-file-name
        (not (string-match "tags$" buffer-file-name)))
    (let*
      ((proj-list (vps-get-project-list (buffer-file-name)))
        (proj-count (length proj-list)))
      (if proj-list
        (if (member vps-project-name proj-list)
          ;; file belongs to a project
          (unless nomessage
            (message "File belongs to current project: %s %s"
              vps-project-name
              (if (> proj-count 1) (format "(total %s)" proj-count ) "")))
          (if (> proj-count 1)
            ;; file belongs to multiple projects
            (let ((default (car proj-list)))
              (vps-change-project

                (vps-completing-read "Ambiguous.\nSelect project: " proj-list)
                ))
            ;; else: only belongs to one
            (vps-change-project (car proj-list))
            ))
        (unless nomessage
          (message "\"%s\" does not belong to a project"
            buffer-file-name))
        ))
    (unless nomessage
      (message "No filename for this buffer"))
    ))



(defun vps-grep-invisible (command args)
    "Call grep with visible COMMAND string and invisible ARGS string."
    (let* ((cmd (progn
                  (set-text-properties 0 (length args) '(invisible t) args)
                  (format "%s %s" command args))))
        (grep cmd)))

(defun vps-read-from-minibuffer (prompt &optional default-value hist)
  "Simple version of read-from-minibuffer that returns default value on empty input."
  (let* ((prompt-with-default (if default-value
                                (format "%s [%s]: " prompt default-value)
                                (format "%s: " prompt)))
          (result (read-from-minibuffer
                    ;; PROMPT
                    prompt-with-default
                    ;; INITIAL-CONTENTS KEYMAP READ
                    nil nil nil
                    ;; HIST DEFAULT-VALUE
                    hist default-value)))
  (if (and default-value (string= result ""))
    (format "%s" default-value)
    result)))

(defun vps-grep-ignore-case (word)
  "Run grep -i WORD on files associated with current project."
  (interactive (list (vps-read-from-minibuffer "-i vgrep"
                       (current-word 'grep-history))))
  (vps-grep word t))

(defun vps-grep (word &optional grep-ignore-case proj-name)
  "Run grep WORD on files associated with current project.
Optional argument GREP-IGNORE-CASE when non-nil ignore case in search."
  (interactive (list (vps-read-from-minibuffer "vgrep" (current-word 'grep-history))))
  (if (string= word "")
    (setq word (current-word)))
  (unless (< (length word) 1)
    (let*
      ((pregexp (cadr (vps-get-setting 'filename-pregexp proj-name)))
        (ext     (cadr (vps-get-setting 'ext proj-name)))
        (dirs    (cadr (vps-get-setting-or-default 'dirs nil proj-name)))
        (rdirs   (cadr (vps-get-setting-or-default 'rdirs nil proj-name)))
        (visible-part
          (concat vps-vgrep-call " "
                  (if grep-ignore-case "-i " "")
                  "-m " (format "%s" major-mode) " "
                  "-e "
                  (mapconcat 'identity ext ",") " "
                  (if pregexp
                      (concat "-f " (shell-quote-argument pregexp) " ")
                    "")
                  (shell-quote-argument word) " ")))

      (if (get-buffer "*grep*")
        (kill-buffer "*grep*"))         ; kill old hi-lock vars

      (vps-grep-invisible
        (if current-prefix-arg
          (read-from-minibuffer "vps Command: " visible-part)
          visible-part)

        (concat
         (mapconcat (lambda (x)
                      (shell-quote-argument  x)) dirs " ")
         " "
         (mapconcat (lambda (x)
                      (concat "-r " (shell-quote-argument  x))) rdirs " "))))))


(defun vps-directory-files (dir)
  "Project files for a directory in same order as extension list."
  (let
    (file-list
      (ext-list (cadr (vps-get-setting 'ext))))
    (dolist (ext ext-list)
      (setq file-list
        (append file-list (directory-files dir t
                            (concat "\\.\\(" ext "\\)\\'") t))))
    file-list))

(defun vps-make-ctags ()
  "Delete old ctags file and make a new one (for current project)."
  (interactive)
  (message "%s: Make ctags file..." vps-project-name)
  (let (command (tagsfile (vps-ctags-filename))
         (progress 0) (max-progress (length (vps-dirs))))
    (if (file-exists-p tagsfile)
      (delete-file tagsfile))
    (vps-write-filelist)
    (eshell-command (concat "ctags -L " (shell-quote-argument (vps-filelist-filename)) " -f " tagsfile))
;;    (setq vtags-tagfile tagsfile)
    (message "%s: Make ctags file...done" vps-project-name)
    ))


;; ?? FIXME why is (vps-valid-dirs) not used (it has a seen list)

(defun vps-make-tags-slow ()
    "Delete old tags file and make a new one (for current project)."
    (interactive)
    (let (command seen-list (tagsfile (vps-tags-filename))
             (progress 0) (max-progress (length (vps-dirs)))
           (error-msg ""))
        (if (file-exists-p tagsfile)
            (delete-file tagsfile))
        ;; Append to tags file for each dir
        (dolist (userdir (vps-dirs))
            (let ((dir (vps-strip-slash (expand-file-name userdir))) file-list)
                (setq progress (1+ progress))
                (unless (or (member-ignore-case dir seen-list)
                            (not (file-directory-p dir)))
                    (setq file-list (vps-directory-files dir))
                    (if (null file-list)
                        (setq error-msg (format "Empty dir: %s" dir))
                        ;; else
                        (setq command
                            (format
                                "etags --members -a --output=%s %s"
                                (shell-quote-argument tagsfile)
                                (mapconcat
                                    'shell-quote-argument
                                    file-list
                                    " ")))
                        (add-to-list 'seen-list dir)
                        (shell-command command)
                        (message "%s\n%s: Slowly make tags file... %d/%d"
                            error-msg
                            vps-project-name
                            progress max-progress)
                        ))))
      (message "%s: Slowly make tags file...done" vps-project-name)
      ))

(defun vps-make-tags ()
  (interactive)
  (vps-write-filelist)
  (with-temp-buffer
    (insert-file-contents (vps-filelist-filename))
    (mark-whole-buffer)
    (shell-command-on-region (point-min) (point-max)
      (format "%s --members --output=%s -" vps-etags-program
        (shell-quote-argument (vps-tags-filename))))))

(defun vps-make-ectags ()
  (interactive)
  (vps-write-filelist)
  (shell-command-on-region (point-min) (point-max)
    (format "%s -L %s -e -f %s" "ectags"
      (shell-quote-argument (vps-filelist-filename))
      (shell-quote-argument (vps-ctags-filename)))))



(defun vps-make-ebrowse ()
  "make ebrowse."
  (interactive)
  (let ((filelist (vps-filelist-filename))
         (ebrowse-filename
           (concat (vps-project-dir) "/" vps-project-name ".ebrowse")))
    (vps-write-filelist)
    (shell-command
      (format "ebrowse --output=%s --files=%s"
        (shell-quote-argument ebrowse-filename)
        (shell-quote-argument (vps-filelist-filename))))
    (set-register ?B `(file . ,ebrowse-filename))))



(define-derived-mode vps-list-dirs-mode outline-mode "VPS List Dirs")

(font-lock-add-keywords 'vps-list-dirs-mode
  '(("^  \\(.* s\\'\\)" 1 'shadow append)))

(defun vps-list-dirs-find-file ()
  (interactive)
  (let ((default-directory ""))
    (find-file (replace-regexp-in-string "^ *" ""
                 (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(defun vps-list-dirs ()
  "Display directories for current project in a buffer. Press space to
call \\[find-file-at-point]"
  (interactive)
  (unless vps-project-name
    (error "No current project"))
  (switch-to-buffer (get-buffer-create "*vps dirs*"))
  (delete-region (point-min) (point-max))
  (insert (concat "Project: " vps-project-name "\n\n"))
  (let ((dirs (vps-dirs)) dir len)
    (setq len (length dirs))
    (dotimes-with-progress-reporter (i len) "Reading directories.."
      (progn
        (setq dir (nth i dirs))
        ;;    (dolist (dir dirs)
        (if (not (file-directory-p dir))
          (insert "* " dir " (n/a)\n")
          ;; else:
          (insert "* " dir "\n")
          (dolist (file (directory-files dir t nil  t))
            (if (string-match
                  (concat "\\(" (mapconcat 'regexp-quote
                                  completion-ignored-extensions
                                  "\\|") "\\)\\'")
                  file)
              (insert "  " (propertize file 'face 'dired-ignored) " s\n")
              (insert "  " file "\n")))
          (insert "    \n"))))
    )
  (insert "\n")
  (insert "* Settings keys\n")
  (mapcar (lambda (x) (insert (symbol-name (car x)) " ")) (vps-project))
  (goto-line 3)
  (move-to-column 3)
  (vps-list-dirs-mode)
  (hide-body)
  (local-set-key (kbd "SPC") 'show-subtree)
  (local-set-key (kbd "RET") 'vps-list-dirs-find-file)
  (message (propertize " RET find-file-at-point   SPACE show-subtree"
             'face 'header-line)))



(defun vps-write-filelist ()
    "write file list."
    (interactive)
    (with-temp-file (vps-filelist-filename)
      (set-buffer-file-coding-system 'raw-text)
      (dolist (dir (vps-dirs))
        (if (file-directory-p dir)
          (dolist (file (directory-files dir t
                          (vps-ext-regexp vps-project-name) t))
            (insert file "\n"))))
      (run-hooks 'vps-write-file-list-hook)))


(defun vps-write-alldirslist ()
  (with-temp-file (vps-alldirslist-filename)
    (set-buffer-file-coding-system 'raw-text)
    (dolist (dir (vps-valid-dirs))
      (insert dir "\n"))))

(defun vps-virtual-dired ()
    "??"
    (interactive)
    (unless vps-project-name
        (error "No current project"))
    (unless (featurep 'dired-x)
        (require 'dired-x))
    (message "vps-virtual-dired: wait...")
    (switch-to-buffer (get-buffer-create "*vps virtual dired*"))
    (delete-region (point-min) (point-max))
    (insert (concat "Project: " vps-project-name "\n\n"))
    (shell-command
        (concat "ls " dired-listing-switches " "
            (mapconcat (lambda (dir)
                         (shell-quote-argument (vps-strip-slash (expand-file-name dir))))
              (vps-dirs) " "))
        t)
    (dired-virtual-mode))


(defun vps-visit-tags ()
  (interactive)
  ;; Delete old tags file loaded via vps-visit-tags
  (catch 'loop
    (dolist (buf (buffer-list))
      (let ((filename (buffer-file-name buf)))
        (when (and filename (string= filename vps-last-tags-filename)
                (message "Killed tags buffer: %s" filename)
                (kill-buffer buf)
                (throw 'loop nil)
                )))))
  (setq vps-last-tags-filename (vps-tags-filename))
  (tags-reset-tags-tables)
  (setq last-tag
    (propertize "<Project changed. Try again>" 'face 'font-lock-warning-face))
  (visit-tags-table (vps-tags-filename)))


(defun vps-quick-add-recursive-project (name ext-string)
  (interactive "sEnter project name: \nsExtensions (eg \"c,h\"): ")
  (if (equal ext-string "")
    (setq ext-string "c,cpp,h,hpp,hxx,cxx,cc,pl,pm,py,cs,rb,fsx,fs"))
  (let ((dir (file-name-directory (read-file-name "Directory: "))))
    (vps-add-project name `((rdirs (,dir))
                            (ext ,(split-string ext-string ","))))))

(defun vps-add-to-c-includes-path ()
  (interactive)
  (setq c-includes-path (vps-valid-dirs))
  (if (y-or-n-p "Add MSVC:VC98/Include ")
    (setq c-includes-path
      (append c-includes-path
        '("c:/Program Files/Microsoft Visual Studio/VC98/Include")))))


(defvar vps-c-includes-path nil "Project specific c-includes-path. Set in hook")

;; (defun vps-c-includes (filename &optional regexp)
;;   "Wrap c-includes to search vps-c-includes-path"
;;   (interactive)
;;   (let ((c-includes-path vps-c-includes-path))
;;     (c-includes filename regexp)))


(defun vps-shell-command-in-dirs (command)
  "Run the same command in all vps dirs"
  (interactive "sEnter command: ")
  (switch-to-buffer (get-buffer-create "*vps-shcmd-in-dirs*"))
  (delete-region (point-min) (point-max))
  (insert "  Hi-lock: ((\"[Ee]rror.*\" (0 (quote hi-pink) t)))\n")
  (insert "  Hi-lock: ((\"[Ww]arning.*\" (0 (quote hi-blue) t)))\n\n")
  (dolist (dir (vps-dirs))
    (insert
      (format "Entering Directory: `%s'\n" (propertize dir 'face 'dired-directory))
      (if (eq window-system 'w32)
        (shell-command-to-string (format "cd %s && %s" (expand-file-name dir)
                                   command))
        (shell-command-to-string (format "zsh -c \"cd %s && %s\"" dir command)))
      (format "Leaving Directory: `%s'\n" (propertize dir 'face 'dired-ignored)))
    ;;    (hi-lock-find-patterns)
    (sit-for 0))
  (compilation-minor-mode)
  )


(defun vps-make-index (&optional proj-name)
  "Make inverse index for PROJ-NAME."
  (interactive)
  (let ((filename (vps-index-db-filename proj-name)))
    (vps-write-filelist)
    (if (file-exists-p filename)
      (delete-file filename))
    (message "Making index: %s" filename)
    (redisplay)
    (if (> (shell-command
             (concat "perl -w " (or (locate-library "vj-make-index.pl")
                                  "-S vj-make-index.pl") " " filename)
             nil "*vps-make-index: Error*")
          0)
      (switch-to-buffer "*vps-make-index: Error*")
      ;; else
      (message "Making index: done"))))


(defun vps-make-index-async (&optional proj-name)
  "Make inverse index for PROJ-NAME asynchronously."
  (interactive)
  (let ((filename (vps-index-db-filename proj-name)))
    (vps-write-filelist)
    (if (file-exists-p filename)
      (delete-file filename))
    (message "Making index: %s" filename)
    (eshell-command
      (concat vps-perl-program " -w " (or (locate-library "vj-make-index.pl")
                           "-S vj-make-index.pl") " " filename " &") t)))


(defun vps-grep-via-index (word)
  ;; FIXME use thing-at-point-looking-at to exclude dots
  (interactive
    (list
      (read-from-minibuffer
        (if (> (length (current-word)) 0)
          (format "vps-grep-via-index [%s]: " (current-word))
          "vgrep Expression: ")
        nil nil nil 'grep-history (current-word))))
  (unless vps-project-name
    (error "No current project!"))
  (if (string= word "")
    (setq word (current-word)))
  (unless vps-vj-search-index-program
    (setq vps-vj-search-index-program
      (concat "perl -w " (or (locate-library "vj-search-index.pl")
                           "-S vj-search-index.pl"))))
  (if (and
        (not (file-exists-p (vps-index-db-filename)))
        (y-or-n-p (format "Make index for %s? " vps-project-name)))
    (vps-make-index-async)
    ;; else
    (if (string= word "")
      (setq word (current-word)))
    (grep (concat vps-vj-search-index-program " "
            word
            " "
            (vps-index-db-filename)))))

(defun vps-compile ()
  "Call compile with the project setting in 'compile-command in first 'rdirs directory."
  (interactive)
  (compile (read-from-minibuffer "Compile command: "
    (format "cd %s && %s"
      (caadr (vps-get-setting 'rdirs))
      (cadr (vps-get-setting 'compile-command))))))


(defun vps-midnight-install ()
  "Rebuild directory cache for current project at midnight.

Note that (require 'midnight) may install clean-buffer-list in midnight-hook"
  (interactive)
  (require 'midnight)
  (unless midnight-mode
    (message "vps-midnight-install: you must enable option midnight-mode!"))
  (add-hook 'midnight-hook 'vps-midnight))

(defun vps-midnight ()
  "Rebuild directory cache for current project."
  (message "vps-midnight %s" (current-time-string))
  (vps-rebuild-rdir-cache (vps-get-setting-or-default 'rdirs nil))
  (vps-write-filelist))

(provide 'vps)

;;; vps.el ends here
