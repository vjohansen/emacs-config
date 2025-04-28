
(require 'tempo)
(setq auto-mode-alist (cons '("\\.zsh\\'" . sh-mode) auto-mode-alist))

(autoload 'smart-compile "smart-compile" "Smart-Compile*" t)

; ------------------------------------------------------------

;; Autodetect tab-infested files

;; http://lists.gnu.org/archive/html/emacs-devel/2005-09/msg00860.html

(defun sm-find-file-tab-setup ()
  (when (and (null indent-tabs-mode)
          (local-variable-p 'indent-tabs-mode) ; Trust the major mode.
          (save-excursion
            (goto-char (point-min))
            ;; If there are at least 10 lines with a leading TAB, use TABs.
            (re-search-forward "^	" (+ (point) 100000) t 10)))
    (set (make-local-variable 'indent-tabs-mode) t)
    (setq tab-width 4)))

;;(add-hook 'find-file-hook 'sm-find-file-tab-setup)

; ------------------------------------------------------------

(add-hook 'emacs-lisp-mode-hook 'vj-emacs-lisp-mode-hook)
;;(add-hook 'emacs-lisp-mode-hook 'pretty-lambdas)

(defun vj-emacs-lisp-mode-hook ()
  "Installs my preferred emacs-lisp mode."
  (interactive)
  (font-lock-add-keywords nil
    '(("\\(`\\)(" (1 'fringe append))))
  (local-set-key "\C-c\C-q" '(lambda ()
                               (interactive)
                               (save-excursion
                                 (beginning-of-defun)
                                 (indent-sexp))))

  (local-set-key "\M-q\M-q" (if (fboundp 'prog-fill-reindent-defun)
                              'prog-fill-reindent-defun
                              'lisp-fill-paragraph))
  (setq lisp-indent-offset 2)
  (setq fill-column 78)
  ;; Templates
  (auto-fill-mode)
  (eldoc-mode t)
  )

(defun pretty-lambdas ()
    (font-lock-add-keywords
     nil `(("(\\(lambda\\>\\)"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(make-char 'greek-iso8859-7 107))
                      nil))))))


(defun pretty-js-lambdas ()
  (font-lock-add-keywords
   nil `(("\\(function\\) *("
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

; ------------------------------------------------------------

;; See also "Perl Development Environment in emacs"
;; http://cpansearch.perl.org/src/YEWENBIN/Emacs-PDE-0.2.16/lisp/doc/QuickStartEn.html

(setq auto-mode-alist (cons '("\\.p[lm]$" . cperl-mode) auto-mode-alist))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

(add-hook 'cperl-mode-hook 'vj-cperl-mode-hook)

(eval-after-load "cperl"
  '(progn
    (setq cperl-lazy-help-time 2)
    (cperl-lazy-install)))

(defun vj-cperl-mode-hook ()
  "VJOs Perl mode hook for cperl-mode"

  (make-local-variable 'compile-command)
  (setq compile-command
    (concat "perl -w " (file-name-nondirectory (buffer-file-name))))

  (setq cperl-indent-level 2
    cperl-continued-statement-offset 2
    cperl-continued-brace-offset -2
    cperl-brace-offset 0
    cperl-brace-imaginary-offset 0
    cperl-label-offset 0                ; was -2
    ;;    cperl-electric-parens 'null
    ;;    cperl-electric-parens 't
    cperl-indent-parens-as-block t
    comment-column 40)
  )


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
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . nxml-mode))


; ------------------------------------------------------------

(load "tempo-c-cpp")

(add-hook 'c-mode-common-hook 'vjo-c-mode-common-hook)
;;(add-hook 'c-mode-common-hook 'hs-hide-initial-comment-block t)


(defun vjo-c-mode-common-hook ()

  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (setq c-auto-newline nil)
  (setq c-tab-always-indent nil)
  (setq c-block-comment-prefix "")      ; was "* "
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)

;;  (define-key c-mode-map "\C-m" 'my-cc-mode-return)
;;  (define-key c++-mode-map "\C-m" 'my-cc-mode-return)

  (local-set-key "\C-c\C-f" 'tempo-forward-mark)
  (local-set-key "\C-c\C-e" 'tempo-complete-tag)
  (local-set-key "\C-c\C-c" 'comment-region)
  (if (equal major-mode 'csharp-mode)
    (local-set-key "\C-t" 'vjo-toggle-csharp-file)
    (local-set-key "\C-t" 'vjo-toggle-h-cpp-file));; use M-o?
  (local-set-key "\C-ch" 'hs-hide-block)
  (local-set-key "\C-cs" 'hs-show-block)
  (local-set-key "\M-q\M-w" 'vjo-cout-watch-current-word)
  (local-set-key "\C-c\C-i" 'ewd-insert-new-method)
  (local-set-key "\C-ci" 'vjo-using-fix)

  (require 'if-jump)
  (local-set-key [C-home]'(lambda() (interactive) (if-jump-jump 'backward)))
  (local-set-key [C-end] '(lambda() (interactive) (if-jump-jump 'forward)))

  (hs-minor-mode 1)
  (define-key hs-minor-mode-map
    [?\C-z]
    (lookup-key hs-minor-mode-map [?\C-c ?@]))

  ;; ac does not work if ac-source-yasnippet is the first element
  (setq ac-sources '(ac-source-gtags ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))  (setq ac-sources '(ac-source-words-in-same-mode-buffers))

  (auto-fill-mode)
;;  (filladapt-mode)
  (setq fill-column 78)
;;  (c-setup-filladapt)

  )

(defun vjo-current-symbol ()
  "(VJO Dec 2005)"
  (interactive)
  (and
    (if (memq major-mode '(c++-mode))
      (thing-at-point-looking-at "[a-zA-Z][a-zA-Z0-9_.-:]*")
      (thing-at-point-looking-at "[a-zA-Z][a-zA-Z0-9_.-]*"))
    (match-string-no-properties 0)))

(defun vjo-cout-watch-current-word ()
  "(VJO Jan 2002)"
  (interactive)
  (let ( (cw (vjo-current-symbol)) cs-prefix)
    (when cw
      (end-of-line)
      (newline-and-indent)
      (if (memq major-mode '(csharp-mode))
        (progn
          (setq cs-prefix
            (or (save-excursion
                  (if (re-search-backward "\\b\\(\\sw+\\)\\.WriteLine" nil t)
                    (match-string-no-properties 1)))
              "Console"))
          (insert (concat cs-prefix ".WriteLine" "(\"" cw "={0}\"," cw ");")))
        ;; not c#

        (if (memq major-mode '(c-mode))
          (insert (concat "printf(\"" cw "= %s\\n\"," cw ");"))
          (if (memq major-mode '(python-mode))
            (insert (concat "print('" cw "', " cw ")"))
            (if (memq major-mode '(c++-mode))
              (insert (concat "cout << \"" cw "=\" << " cw " << endl;"))
              (if (memq major-mode '(tsx-mode js2-mode js-mode rjsx-mode))
                (insert (concat "console.log(\"" cw "\", " cw ");")))))))
    )))

(add-to-list 'auto-mode-alist '("\\.cuh?\\'" . c-or-c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-or-c++-mode))
(defun c-or-c++-mode ()
  (if (save-excursion
        (goto-char (point-min))
        (or (re-search-forward "[^:]//" 10000 t) (< (buffer-size) 5) ))
      (c++-mode)
    (c-mode)))

(defun vjo-toggle-h-cpp-file ()
  "Switch buffer from cc to h and vice versa depending on current buffer"
  (interactive)
  (let* (
        (name (file-name-sans-extension (buffer-file-name)))
        (hname (concat name ".h"))
        (hhname (concat name ".hh"))
        (hppname (concat name ".hpp"))
        (cname (concat name ".c"))
        (cppname (concat name ".cpp"))
        (cxxname (concat name ".cxx"))
        (ccname (concat name ".cc"))
        )
    (if (string-match "\\.h.?.?$" (buffer-file-name))
        (or
         (and (file-exists-p ccname) (find-file ccname))
         (and (file-exists-p cppname) (find-file cppname))
         (and (file-exists-p cxxname) (find-file cxxname))
         (and (file-exists-p cname) (find-file cname))
         (if (y-or-n-p "Create cpp file ")
           (find-file cppname))
         )
        (or
         (and (file-exists-p hhname) (find-file hhname))
         (and (file-exists-p hname) (find-file hname))
         (and (file-exists-p hppname) (find-file hppname))
         (if (y-or-n-p "Create hpp-file ")
             (find-file hppname)))
      ))
    )

(defun vj-nxml-xaml ()
  (local-set-key (kbd "C-t") 'vjo-toggle-csharp-file))

(add-hook 'nxml-mode-hook 'vj-nxml-xaml)

(defun vjo-toggle-csharp-file ()
  "Switch buffer from cc to h and vice versa depending on current buffer"
  (interactive)
 (let*
   ((fn (buffer-file-name)) basename ext)
  (when (string-match "^\\([^.]+\\)\\(\\..*\\)$" fn)
    (setq basename (match-string 1 fn))
    (setq ext (downcase (match-string 2 fn)))
    (if (equal ext ".xaml.cs")
      (find-file (concat basename ".xaml"))
      (if (equal ext ".xaml")
        (find-file (concat basename ".xaml.cs"))
;;        (message "Dunno about %s %s" basename ext)
    )))))



(defun my-cc-mode-return ()
   "Intelligent line breaking in all cc-modes. Handles strings in a smart
 way (Klaus Berndl)"
   (interactive)
   (cond ((eq (c-in-literal) 'string)
          (if (memq major-mode '(java-mode jde-mode))
              (insert-and-inherit "\"+")
            (insert-and-inherit "\""))
          (c-context-line-break)
          (insert "\" ") ;; cause of strings in c will be concatenated we
                         ;; must add a single space.
          (end-of-line))
         (t (c-context-line-break))))



;; If point is in a class definition, return the name of the
;; class. Otherwise, return nil. Thanks to Elijah Daniel for this one.

(defun ewd-classname ()
  "If the point is in a class definition, gets the name of the class.
Return nil otherwise."
  (save-excursion
    (let ((brace (assoc 'inclass (c-guess-basic-syntax))))
      (if (null brace) '()
        (goto-char (cadr brace))
        (let ((class-open (assoc 'class-open (c-guess-basic-syntax))))
          (if class-open (goto-char (cadr class-open)))
          (if (looking-at "^class[ \t]+\\([A-Za-z_][^ \t:{]*\\)")
              (buffer-substring (match-beginning 1) (match-end 1))
            (error "Error parsing class definition!")))))))

;; Insert function prototype in current header file and matching
;; function body in implementation file.
(defun ewd-insert-new-method (rettype proto)
  "Insert a function declaration into the current class header file at
point, along with matching function definition in the corresponding
implementation file, complete with class name and scope resolution
operator.  This function expects the implementation file to be named
foo.cpp and in the same directory as the current header file, foo.h."
  (interactive "sReturn type:\nsPrototype: ")
  (let ((classname (ewd-classname))
        (c-tab-always-indent t))
    (if (null classname) (message "Not in class definition!")
      (unless (string-equal rettype "") (setq rettype (concat rettype " ")))
      (insert rettype proto ";")
      (c-indent-command)
      (save-window-excursion
        (find-file (concat (file-name-sans-extension (buffer-file-name))
                           ".cc"))
        (end-of-buffer)
        (insert "\n\n")
        (end-of-buffer)
        (insert rettype classname "::" proto "\n{\n  \n}\n")
        (previous-line 2)))))


; ------------------------------------------------------------

(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)

; ------------------------------------------------------------

(setq auto-mode-alist
  (append '(("\\.md\\'" . markdown-view-mode))
    auto-mode-alist))

;;(autoload 'markdown-mode "markdown-mode" "markdown-mode*" t)


(defun markdown-mode-setup ()
  (turn-on-visual-line-mode)
  (keymap-local-set "C-c f" (lambda ()
                              (interactive)
                              (buffer-face-set :family "Verdana" :height 1.0)))
  )

(add-hook 'markdown-mode-hook 'markdown-mode-setup)


; ------------------------------------------------------------

(defun vj-python-setup ()
  (local-set-key (kbd "<f9>") 'python-shell-send-region)
  ;; (company-mode t)
  ;; (eldoc-mode t)
  (auto-fill-mode -1)
  (local-set-key "\M-q\M-w" 'vjo-cout-watch-current-word)
  )

(add-hook 'python-mode-hook 'vj-python-setup)


; ------------------------------------------------------------




(defun vjo-buffer-contains (text)
  (interactive "sFind: ")
  (save-excursion
    (goto-char (point-min))
    (re-search-forward text nil t)))

(defun vjo-maybe-insert (typename)
  (when (and (not (vjo-buffer-contains (concat "^using +" typename)))
          (vjo-buffer-contains typename))
    (insert (concat "using " typename ";"))
    (newline)))

(defun vjo-using-fix ()
  "2021 Sep"
  (interactive)
  (goto-char (point-max))
  (re-search-backward "^#include")
  (beginning-of-line)
  (forward-line 1)
  (vjo-maybe-insert "std::string")
  (vjo-maybe-insert "std::wstring")
  (vjo-maybe-insert "std::vector")
  (vjo-maybe-insert "std::map")
  (vjo-maybe-insert "std::shared_ptr")
  )