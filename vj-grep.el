
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

(defface vj-grep-match '((t (:inherit match :background "bisque")))
  "Face for highlighting failed part in Isearch echo-area message."
  :group 'vj-grep)

(defvar vj-perl-program "perl")

(defvar vj-vgrep-call
  (concat vj-perl-program " "
          (or (locate-library "vgrep22.pl") "-S vgrep22.pl"))
  "Command line for calling vgrep22.pl program.")


(defun vgrep (word &optional ext-string)
  "Run grep WORD on files with vj-grep-source-extensions in current directory.

With prefix argument do a recursive grep."
  (interactive
   (list
    (read-from-minibuffer
     (if (> (length (current-word)) 0)
         (format "vgrep [%s]: " (current-word))
       "vgrep Expression: ")
     nil nil nil 'grep-history (current-word))))
  (if (string= word "")
      (setq word (current-word)))
  (unless (< (length word) 1)
    (grep
     (concat vj-vgrep-call
       " -i -e " (or ext-string vj-grep-source-extensions) " "
       (shell-quote-argument word) " "
       (if current-prefix-arg
         (concat "-r " (shell-quote-argument
                         (file-name-directory
                           (expand-file-name (read-file-name "Directory(-r): " default-directory)))))
         ".")
       ))))



(defun vj-rgrep ()
  "Recursive grep (VJ June 2003)"
  (interactive)
  (let
    ((compilation-enter-directory-regexp-alist '(("Dir: \\(.*\\)" 1)))
      (compilation-scroll-output nil)
      (command
        (concat vj-vgrep-call " -i "
          " -e " vj-grep-source-extensions
          " " (shell-quote-argument
                (vj-read-from-minibuffer "rgrep" (current-word) 'grep-history))
          " -r " (replace-regexp-in-string " " "%20"
                   (expand-file-name (read-file-name "Dir: " nil default-directory nil))))))
    (grep (if current-prefix-arg
            (read-from-minibuffer "rgrep command: " command)
            command ))))


;; VJ april 2005
(defun vj-grep-includes (word)
  "grep in iclude files for current buffer"
  (interactive (list (read-string "include grep: " (thing-at-point 'symbol))))
  (grep (concat vj-perl-program " -w "
          (or (locate-library "incgrep.pl") "-S incgrep.pl")
          " " (buffer-file-name) " " word)))


(provide 'vj-grep)
