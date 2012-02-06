(defconst mono-csharp-attrib-key
  (concat "\[" c-symbol-key "\\(([^)]*)\\)?\]"))
(defconst mono-csharp-class-protection-key
  "\\(new\\|public\\|protected\\|internal\\|private\\|abstract\\|sealed\\)")
(defconst mono-csharp-class-parent-key
  (concat
   "\\(\\s +:\\s *" c-symbol-key
      "\\(\\s *,\\s *" c-symbol-key "\\)*"
   "\\)*"))

(defconst mono-csharp-class-key
  (concat
   "\\(" mono-csharp-attrib-key "\\)?"
   "\\(" mono-csharp-class-protection-key "\\s +\\)*"
   "\\(struct\\|class\\)\\s +"
   c-symbol-key
   "\\(" mono-csharp-class-parent-key "\\)?"))

(c-add-style "MonoC#Style"
	     '("C#"
	       (c-basic-offset . 8)
	       (c-comment-only-line-offset . 0)
	       (c-hanging-braces-alist . ((brace-list-open)
					  (brace-entry-open)
					  (substatement-open after)
					  (block-close . c-snug-do-while)))
	       (c-cleanup-list . (brace-else-brace))
	       (c-offsets-alist . (
				   (c                     . c-lineup-C-comments)
				   (inclass		  . 0)
				   (namespace-open	  . 0)
				   (namespace-close	  . 0)
				   (innamespace           . +)
				   (class-open		  . 0)
				   (class-close	          . 0)
				   (inclass		  . +)
				   (block-open            . 0)
				   (block-close           . 0)
				   (defun-open		  . 0)
				   (defun-block-intro     . +)
				   (defun-close           . 0)
				   (inline-open	          . 0)
				   (inline-close          . 0)
				   (statement-block-intro . +)
				   (brace-list-intro      . +)
				   ))
	       ))

(defun mono-csharp-mode-hook ()
  (cond (window-system
	 (turn-on-font-lock)
	 (c-set-style "MonoC#Style")
	 (setq c-class-key mono-csharp-class-key)
	 )))
(add-hook 'csharp-mode-hook 'mono-csharp-mode-hook)
(setq auto-mode-alist
      (append '(
		("\\.cs$" . csharp-mode)
		) auto-mode-alist ))