
(require 'compile)
(require 'vj-util)

(defgroup vj-grep nil
  "Extension based Grep with external script. Displays directories separately
from filenames for nicer display."
  :group 'convenience
  :group 'vj)


(defcustom vj-grep-source-extensions
  ":code,:text"

  "Extensions to include in searches."
  :type 'string
  :group 'vj-grep)

(defvar vj-perl-program "perl")

(defvar vj-vgrep-call (concat "node " (locate-library "vgrep.mjs"))
  "Command line for calling vgrep.mjs.")

(defun vj-grep-current-dir (word)
  (interactive (list (vps-read-from-minibuffer "Search Term" (current-word))))
  (grep (format "grep -n -i -I -s \"%s\" -- *" word)))

(defun vgrep (word &optional ext-string)
  "Run grep WORD on files with vj-grep-source-extensions in current directory.

With prefix argument do a recursive grep, prompting for a directory."
  (interactive
   (list
    (read-from-minibuffer
     (if (> (length (current-word)) 0)
         (format "vgrep [%s]: " (current-word))
       "vgrep Expression: ")
     nil nil nil 'grep-history (current-word))))
  (when (string-empty-p word)
    (setq word (current-word)))
  (unless (string-empty-p word)
    (grep
     (concat vj-vgrep-call
       " -i -e " (or ext-string vj-grep-source-extensions) " "
       (shell-quote-argument word) " "
       (if current-prefix-arg
         (concat "-r " (shell-quote-argument
                         (expand-file-name (read-file-name "Directory: " default-directory))))
         ".")))))


;; VJ april 2005
(defun vj-grep-includes (word)
  "grep in iclude files for current buffer"
  (interactive (list (read-string "include grep: " (thing-at-point 'symbol))))
  (grep (concat vj-perl-program " -w "
          (or (locate-library "incgrep.pl") "-S incgrep.pl")
          " " (buffer-file-name) " " (shell-quote-argument word))))

(provide 'vj-grep)
