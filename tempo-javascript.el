(require 'tempo)

(defvar javascript-tempo-tags nil)

(add-hook 'javascript-mode-hook 'javascript-tempo)


(defun javascript-tempo ()
   "Set up javascript mode to use tempo.el"
   (local-set-key "\C-c\C-e" 'tempo-complete-tag)
   (local-set-key "\C-c\C-f" 'tempo-forward-mark)
   (tempo-use-tag-list 'javascript-tempo-tags))

(tempo-define-template
 "javascript-for"
 (list "for (" '(p "initial: ") "; " '(p "condition: ") "; " '(p "increment: ") ") {" 'n> 'p 'n "}" '>)
 "for" "insert a for loop" 'javascript-tempo-tags)


;;; You can use 'P' to force interactive query.
;;; This is useful here because "var" is used more than once.
(tempo-define-template "javascript-for-i"
		       '(> "for (var " (P "variable: " var) " = 0; " (s var)
			   " < "(p "upper bound: " ub)"; " (s var) "++)" >  n>
			   "{" > n> r n
			   "}" > n>
			   )
		       "fori" "Insert a indexed for loop" 'javascript-tempo-tags)

(tempo-define-template
 "javascript-for-in"
 (list "for (" '(p "variable: ") " in " '(p "object: ") ") {" '> 'n> 'p 'n "}" '>)
 "infor" "insert a for loop" 'javascript-tempo-tags)

(tempo-define-template
 "javascript-if"
 (list "if (" '(p "condition: ") ") {" 'n> 'p 'n "}" '>)
 "if" "insert an if statement" 'javascript-tempo-tags)

(tempo-define-template
 "javascript-while"
 (list "while (" '(p "condition: ") ") {" 'n> 'p 'n "}" '>)
 "while" "insert a while statement" 'javascript-tempo-tags)

(tempo-define-template
 "javascript-do"
 (list "do {" '> 'n> 'p 'n "} while(" '(p "condition: ") ");" '>)
 "do" "insert a do-while statement" 'javascript-tempo-tags)

(tempo-define-template
 "javascript-with"
 (list "with (" '(p "with what? ") ") {" 'n> 'p 'n "}" '>)
 "with" "insert a with statement" 'javascript-tempo-tags)

(tempo-define-template
 "javascript-defun"
 (list "function " '(p "function name: ") "(" '(p "arguments: ") ") {" 'n> 'p 'n "}" '>)
 "function" "insert a function definition" 'javascript-tempo-tags)

(tempo-define-template
 "javascript-switch"
 (list "switch (" '(p "variable: ") ") {" '> 'n> "case '" 'p "' :" '> 'n> "break;" '> 'n> "default :" '> 'n> "}" '>)
 "switch" "insert a switch statement" 'javascript-tempo-tags)

(tempo-define-template
 "javascript-case"
 (list "case '" 'p "' :" '> 'n> "break;" '>)
 "case" "insert a case" 'javascript-tempo-tags)


(defun vj-js-proto-name ()
  ""
  (save-excursion
      (if (re-search-backward "^function \\([A-Za-z_][A-Za-z0-9_]*\\)" nil t)
        (match-string-no-properties 1) (file-name-sans-extension (buffer-name)))))

(tempo-define-template
  "javascript-prototype"
  '((vj-js-proto-name) ".prototype."  'p " = function(" 'p "){" '> 'n> 'p '> 'n> "}" '> )
  "prototype" "insert a method" 'javascript-tempo-tags)

(tempo-define-template
 "javascript-log"
 (list "console.log(\"" 'p "\");")
 "console.log" "insert a log" 'javascript-tempo-tags)
