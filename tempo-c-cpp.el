                                               ;; -*- auto-recompile: t -*-
;; This is a way to hook tempo into cc-mode

(defvar c-tempo-tags nil
  "Tempo tags for C mode")

(defvar c++-tempo-tags nil
  "Tempo tags for C++ mode")

(defvar csharp-tempo-tags nil
  "Tempo tags for C# mode")

(require 'tempo)
;;; C-Mode Templates and C++-Mode Templates (uses C-Mode Templates also)
(add-hook 'c-mode-hook '(lambda ()
			  (tempo-use-tag-list 'c-tempo-tags)
			  ))

(add-hook 'c++-mode-hook '(lambda ()
			    (tempo-use-tag-list 'c-tempo-tags)
			    (tempo-use-tag-list 'c++-tempo-tags)))

(add-hook 'csharp-mode-hook '(lambda ()
                               (tempo-use-tag-list 'c-tempo-tags)
                               (tempo-use-tag-list 'csharp-tempo-tags)
                               ))

;;; C-Mode Templates

(tempo-define-template "c-if"
		       '(> "if (" (p "if-clause: " clause) ")"  n>
			   "{" > n> r n
			   "}" > n>
			   )
		       "if"
		       "Insert a C if statement"
		       'c-tempo-tags)

(tempo-define-template "c-else"
		       '(> "else" n>
			   "{" > n> r n
			   "}" > n>
			   )
		       "else"
		       "Insert a C else statement"
		       'c-tempo-tags)

(tempo-define-template "c-if-else"
		       '(> "if (" (p "if-clause: " clause) ")"  n>
			   "{" > n> r n
			   "} /* end of if(" (s clause) ") */" > n>
			   > "else" n>
			   "{" > n> r n
			   "} /* end of if(" (s clause) ")else */" > n>
			   )
		       "ifelse"
		       "Insert a C if else statement"
		       'c-tempo-tags)

(tempo-define-template "c-while"
		       '(> "while (" (p "while-clause: " clause) ")" >  n>
			   "{" > n> r n
			   "}" > n>
			   )
		       "while"
		       "Insert a C while statement"
		       'c-tempo-tags)


;;; You can use 'P' to force interactive query.
;;; This is useful here because "var" is used more than once.
(tempo-define-template "c-for-i"
		       '(> "for (" (P "variable: " var) " = 0; " (s var)
			   " < "(p "upper bound: " ub)"; " (s var) "++)" >  n>
			   "{" > n> r n
			   "}" > n>
			   )
		       "fori"
		       "Insert a C for loop: for(x = 0; x < ..; x++)"
		       'c-tempo-tags)

(tempo-define-template "c-for"
		       '(> "for (" (p "for-clause: " clause) ")" >  n>
			   "{" > n> r n
			   "}" > n>
			   )
		       "for"
		       "Insert a C for statement"
		       'c-tempo-tags)

(tempo-define-template "c-main"
		       '(> "int main(int argc, char *argv[])" >  n>
			   "{" > n> r n
			   "return 0;" > n>
			   "}" > n>
			   )
		       "main"
		       "Insert a C main statement"
		       'c-tempo-tags)

(tempo-define-template "c-voidfunc"
  '(> "void " p "()" >  n>
     "{" > n>
     p n
     "}" > n>
     )
  "void"
  "Insert a void function"
  'c-tempo-tags)

(tempo-define-template "c-intfunc"
  '(> "int " p "()" >  n>
     "{" > n>
     p n
     "return 0;" > n>
     "}" > n>
     )
  "int"
  "Insert an int function"
  'c-tempo-tags)

(tempo-define-template "c-switch"
		       '(> "switch(" (p "switch-condition: " clause) ")" >  n>
			   "{" > n
			   "case " (p "first value: ") ":" > n> p n
			   "break;" > n> p n
			   "default:" > n> p n
			   "break;" > n
			   "}" > n>
			   )
		       "switch"
		       "Insert a C switch statement"
		       'c-tempo-tags)

(tempo-define-template "c-case"
		       '(n "case " (p "value: ") ":" > n> p n
			   "break;" > n> p
			   )
		       "case"
		       "Insert a C case statement"
		       'c-tempo-tags)


(tempo-define-template "c-printf"
		       '("printf(\"" p "\\n\"" p ");")
		       "printf"
		       "Insert a printf statement"
		       'c-tempo-tags)


;;; Preprocessor Templates (appended to c-tempo-tags)

(tempo-define-template "c-include"
		       '("include <" r ".h>" > n
			 )
		       "include"
		       "Insert a #include <> statement"
		       'c-tempo-tags)

(tempo-define-template "c-ifdef"
		       '("ifdef " (P "ifdef-clause: " clause) > n> p n
			 "#else /* not " (s clause) " */" n> p n
			 "#endif /* " (s clause)" */" n>
			 )
		       "ifdef"
		       "Insert a #ifdef #else #endif statement"
		       'c-tempo-tags)

;; (tempo-define-template "c-ifndef"
;; 		       '("#ifndef " (P "ifndef-clause: " clause) > n
;; 			 "#define " (s clause) n> p n
;; 			 "#endif /* " (s clause)" */" n>
;; 			 )
;; 		       "ifndef"
;; 		       "Insert a #ifndef #define #endif statement"
;; 		       'c-tempo-tags)

(defun tempo-c-cpp-hfilename ()
  "Generate preprocessor define name based on buffer name. eg x.h becomes X__"
  (upcase
   (concat
     (or
       (replace-regexp-in-string "\\..*$" "" (file-name-nondirectory
                                               (buffer-file-name)))
       (buffer-name))
     "_H_")
   ))

(defun tempo-c-cpp-filename ()
  "???_"
  (or (file-name-sans-extension (buffer-name)) (buffer-name)))

;; naughty use: (defun tempo-save-named (name data) ..
;; (tempo-save-named clause (tempo-c-cpp-hfilename))

(tempo-define-template "c-ifndef"
		       '("ifndef " (tempo-c-cpp-hfilename) > n
			 "#define " (tempo-c-cpp-hfilename) n> n p
			 "#endif /* not " (tempo-c-cpp-hfilename) " */" n>
			 )
		       "ifndef"
		       "Insert a #ifndef #define #endif statement"
		       'c-tempo-tags)

;;; problem with # in default tempo-match-finder
;;; (set (make-local-variable 'tempo-match-finder) "\([0-9a-zA-Z_#]+\)\\=") <-sucks




(tempo-define-template "c-TESTOUT_EXT"
  '(>
     "TESTOUT_EXT(STD_COMP, _T(\"" p "\"), T(\""
	 (save-excursion
	   (if (re-search-backward "\\<\\([A-Za-z_][A-Za-z0-9_]*\\)::[A-Za-z_][A-Za-z0-9_]*(")
		 (match-string-no-properties 1) ("")))
	 "\"), _T(\""
	 (save-excursion
	   (if (re-search-backward "\\<[A-Za-z_][A-Za-z0-9_]*::\\([A-Za-z_][A-Za-z0-9_]*\\)(")
		 (match-string-no-properties 1) ("")))
	 "\"), _T(\"" p "\");" n>
     )
  "TESTOUT_EXT"
  "TESTOUT_EXT"
  'c-tempo-tags)


;;;C++-Mode Templates

(tempo-define-template "c++-class"
  '(> "class " (P "class " var) n>
     "{" > n> 
     "public:" > n> 
     > (s var) "(" p  ");" n> 
     > "virtual ~" (s var) "();" n> 
     "private:" > p n>
     "};" > n> p
     )
  "class"
  "New C++ class"
  'c++-tempo-tags)

(tempo-define-template "c++-interface-class"
  '(> "class " (P "class " var) n>
     "{" > n>
     "public:" > n>
     "virtual ~" (s var) "() {};" n> 
     p n>
     "}:" > n>
     )
  "iclass"
  "New C++ interface (pure abstract class)"
  'c++-tempo-tags)

(tempo-define-template "c++-class-noncopyable"
  '(> "class " (P "class " var) n>
     "{" > n> 
     "public:" > n> 
     > (s var) "(" p  ");" n> 
     > "virtual ~" (s var) "();" n> 
     "private:" > p n>
     > (s var) "(const " (s var) " &);" n>
     > "void operator=(const " (s var) " &);"  n>
     "};" > n> p
     )
  "klass"
  "New C++ class with private copy and assi."
  'c++-tempo-tags)


;; class MyClass {
;;    private:
;;      MyClass(const MyClass &);
;;      void operator = (const MyClass &);
;;    ...
;;    };



(tempo-define-template "c++-doxygen-class"
		       '("/** @brief "  p n> n>
                         "@author " (getenv "USERNAME") n>
                         "*/" n>
			   )
		       "dclass"
		       "New doxygen C++ class header"
		       'c++-tempo-tags)

;;; C++ STL

(tempo-define-template "c++-for-iter"
		       '(> "for (" (P "Iterator: " iter)  "="
                           (P "Container: " cont) ".begin(); "
                           (s iter) "!=" (s cont) ".end(); " 
                           "++" (s iter) ")" >  n>
			   "{" > n> r n
			   "}" > n>
			   )
		       "ifor"
		       "C++ for loop with STL iterator"
		       'c++-tempo-tags)

(tempo-define-template "c++-for-reverse-iter"
		       '(> "for (" (P "iterator: " iter) ".rbegin(); "
                           (s iter) "!=" (s iter) ".rend(); " 
                           (s iter) "++)" >  n>
			   "{" > n> r n
			   "}" > n>
			   )
		       "rfor"
		       "C++ for loop with reverse STL iterator"
		       'c++-tempo-tags)


(tempo-define-template "c++-for_each"
  '(> "for_each(" 
     (P "Container: " cont) ".begin(), "
     (s cont) ".end(), &" p
     (P "Method: " method) ");" >
     )
  "for_each"
  "C++ STL for_each with method call no args)"
  'c++-tempo-tags)

(tempo-define-template "c++-try"
  '(> "try" n>
     "{" > n> r n
     "} " p >
     )
  "try"
  "Insert C++ try statement"
  'c++-tempo-tags)

(tempo-define-template "c++-catch"
  '(> "catch (std::exception& ex)" n>
     "{" > n> r n
     p
     "}" >
     n>
     ) 
  "catch"
  "Insert C++ catch statement"
  'c++-tempo-tags)


;;; Make special function for this?
;;;          copy(x.be<COMPLETE>   <-  change "x.be" to "x.begin(),x.end()"

(tempo-define-template "c++-copy-iter"
		       '(> "copy(" (P "iterator: " iter) ".begin(), "
                           (s iter) ".end(), " p ");"
			   )
		       "icopy"
		       "C++ STL copy"
		       'c++-tempo-tags)

(tempo-define-template "c++-cout"
		       '(> "cout << \"" p  "\" << endl;"
			   )
		       "cout"
		       "cout with endl"
		       'c++-tempo-tags)

(tempo-define-template "c++-cout-format"
		       '(> "cout << format(\"" p  "\\n\");"
			   )
		       "cformat"
		       "cout with boost::format"
		       'c++-tempo-tags)

(tempo-define-template "c++-using"
		       '(> "using namespace std" p ";"
			   )
		       "using"
		       "using namespace"
		       'c++-tempo-tags)



(tempo-define-template "c++-functor-class"
  '(>
     "struct " (P "Type: " type) n>
     "{" n>
     "void operator()(" p ")" n>
     "{" > n>
     p n>
     "}" > n>
     "};" > n>
     )
  "fclass"
  "functor class"
  'c++-tempo-tags)










;;;C#-Mode Templates


(tempo-define-template "csharp-class"
  '(> "class " (P "class " var) n>
     "{" > n> 
     "public " (s var) "(" p  ") {}" n> 
     "}" > n> p
     )
  "class"
  "New csharp class"
  'csharp-tempo-tags)


(tempo-define-template "csharp-interface"
  '(> "interface " (P "interface " var) n>
     "{" > n> 
     p n> 
     "}" > n> p
     )
  "interface"
  "New csharp interface"
  'csharp-tempo-tags)

(defvar vj-tempo-csharp-property-varname nil)

(defun vj-tempo-csharp-property-read ()
  (let ((name (read-from-minibuffer "private variable name: ")))
    (setq vj-tempo-csharp-property-varname name)
    (capitalize (replace-regexp-in-string "^m_" "" name))))


(tempo-define-template "csharp-property"
  '(> "public " (P "Type: " type) " " (vj-tempo-csharp-property-read) p n>
     "{" > n> 
     "get { return " 'vj-tempo-csharp-property-varname "; }" n> 
     "set { " 'vj-tempo-csharp-property-varname " = value; }" n> 
     "}" > n>
     "private " (s type) " " 'vj-tempo-csharp-property-varname ";" n>
     )
  "property"
  "New csharp property"
  'csharp-tempo-tags)


(tempo-define-template "csharp-console-writeline"
  '(> "Console.WriteLine(\"" p "\");" n>
     )
  "Console"
  "New csharp WriteLine"
  'csharp-tempo-tags)


(tempo-define-template "csharp-staticvoidfunc"
  '(> "static public void " p "()" >  n>
     "{" > n>
     p n
     "}" > n>
     )
  "static"
  "Insert a static void function"
  'csharp-tempo-tags)

(tempo-define-template "csharp-publicmethod"
  '(> "public void " p "()" >  n>
     "{" > n>
     p n
     "}" > n>
     )
  "public"
  "Insert a public void method"
  'csharp-tempo-tags)


(tempo-define-template "csharp-privatemethod"
  '(> "private void " p "()" >  n>
     "{" > n>
     p n
     "}" > n>
     )
  "private"
  "Insert a private void method"
  'csharp-tempo-tags)


(tempo-define-template "csharp-main-class"
  '(
     "class "
     (capitalize 
       (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
     p  > n>
     "{" > n>
     "  static void Main() " > n>
     "  {" > n>
     p n> 
     "    Console.WriteLine(\"done\");" > n> ;
     "    return; " > n>
     "  }" > n>
     "}" > n>)
  "main"
  "Insert a C# main class"
  'csharp-tempo-tags)





(tempo-define-template "csharp-doc-summary"
  '(>
     "/// <summary>" > n>
     "/// " p > n>
     "/// </summary> " > 
     )
  "summary"
  "Insert summary XML"
  'csharp-tempo-tags)

(tempo-define-template "csharp-doc-summary-ex"
  '(>
     "/// <summary>" > n>
     "/// " p > n>
     "/// <exception cref=\"" p "\"" n> > 
     "/// " p > n>
     "/// </exception> " > n>
     "/// </summary> " > n>
     )
  "esummary"
  "Insert summary XML with exception"
  'csharp-tempo-tags)
