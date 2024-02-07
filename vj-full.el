(tool-bar-mode -1)
(global-auto-revert-mode t)
(setq read-file-name-completion-ignore-case t)
(setq mode-require-final-newline nil)

(global-hi-lock-mode t)
(defun fucking-yes (x) t)
(setq hi-lock-file-patterns-policy 'fucking-yes)

(if window-system
    (setq confirm-kill-emacs 'y-or-n-p)
    (setq confirm-kill-emacs nil))

(show-paren-mode t)
(recentf-mode t)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
;;(which-function-mode)
(auto-compression-mode t)

;; Change annoying yes/no RET questions to single keypress of y or n
(fset 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)

;; Also show filename and vps project in title bar
(setq frame-title-format
  '("" invocation-name "@" system-name " "  " %f [" vps-project-name "]"))

(setq visible-bell t) ;; no beeps!

(setq-default indent-tabs-mode nil)     ; Do not use tabs for indent
(setq compilation-ask-about-save nil)   ; Just save files before compile
(setq find-file-existing-other-name t)


(setq nxml-slash-auto-complete-flag t
      nxml-sexp-element-flag        t
      ediff-split-window-function   'split-window-horizontally
      ediff-window-setup-function   'ediff-setup-windows-plain
      show-trailing-whitespace      t
      tab-width                     2
      compilation-ask-about-save    nil
      compilation-scroll-output     'first-error
      use-file-dialog               nil
      eshell-ask-to-save-history    'always
      eshell-save-history-on-exit   t
      eshell-buffer-maximum-lines   8192)


(autoload 'edit-list "edit-list"
  "*Edit a list called LIST-NAME interactively." nil t)

(autoload 'browse-kill-ring "browse-kill-ring" nil t)

(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))
(global-set-key "\M-\C-y" 'kill-ring-search)


;;(autoload 'iedit-mode "iedit" nil t)
(define-key global-map (kbd "C-;") 'iedit-mode)


(require 'avy)
(define-key global-map (kbd "C-c SPC") 'avy-goto-word-1)
(setq avy-keys (nconc (number-sequence ?a ?z) ; avy-style myst be 'at-full not 'words
                      (number-sequence ?1 ?9)
                      '(?0)))
(setq avy-style 'at-full)
(avy-setup-default) ;; C-' in search mode

(autoload 'align-regexp2 "align-regexp2" "*Align region according to regular expressions..." t)

(autoload 'align-equals "align-equals" "*make the first assignment operator on each line line up vertically" t)

(global-set-key "\M-q\M-a" 'align-current)
(global-set-key "\M-qz" 'align-regexp2)
(global-set-key "\M-qa" 'align-equals)
(global-set-key (kbd "M-q RET") 'find-file-at-point)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-\M-g" 'vgrep)


(autoload 'fnexpand-complete "fnexpand" "*Expand the file name, env var or command near point" t)

;;trick: call global-set-key interactively and then do C-x ESC ESC

(load "vj-grep")
(global-set-key "\C-\M-g" 'vgrep)       ;FIXME rename to grep vj-lgrep

(global-set-key [M-down] 'vjo-forward-current-word-keep-offset)
(global-set-key [M-up] 'vjo-backward-current-word-keep-offset)

(global-set-key [S-up] 'my-scroll-down-hook)
(global-set-key [S-down] 'my-scroll-up-hook)


(global-set-key [M-f2] #'(lambda () (interactive) (vps-grep (current-word))))
(global-set-key [pause] #'(lambda() (interactive) (kill-buffer nil)))

;;  http://www.reddit.com/r/emacs/comments/y76sl/proper_way_of_overriding_mode_keys/
;;
(define-minor-mode vj-global-keys-mode
  "VJ's set-key mode"
  :init-value nil
  :lighter " ☥"
  :keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-<backspace>") 'forward-char)
    keymap)
  :group 'vj-global-keys)

(define-globalized-minor-mode vj-global-keys-global-mode vj-global-keys-mode
  #'(lambda ()
    (if (not (minibufferp (current-buffer)))
        (vj-global-keys-mode 1))))

(add-to-list 'emulation-mode-map-alists '(vj-global-keys-global-mode vj-global-keys-mode-map))




(defun google ()
  "Search the web for a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (url-hexify-string (buffer-substring (region-beginning) (region-end)))
      (read-string "Query: ")))))

(defun wsearch ()
  "Search the web for a query or region if any."
  (interactive)
  (browse-url
   (concat
    "https://duckduckgo.com/?q="
    (if mark-active
        (url-hexify-string (buffer-substring (region-beginning) (region-end)))
      (read-string "Query: ")))))

;; Type unicode numbers with C-q
(setq read-quoted-char-radix 16)


(global-set-key (kbd "M-[") 'my-expand-file-name-at-point)

(defun my-expand-file-name-at-point ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list
          '(try-complete-file-name-partially try-complete-file-name)))
    (call-interactively 'hippie-expand)))

(define-generic-mode csv-highlight-mode
  nil
  nil
  `(
     (,(concat "^\\([^,\n]*\\)[,]\\([^,\n]*\\)[,]?\\([^,\n]*\\)?,?\\([^,\n]*\\)?,?"
         "\\([^,\n]*\\)?,?\\([^,\n]*\\)?,?\\([^,\n]*\\)?,?\\([^,\n]*\\)?,?"
         "\\([^,\n]*\\)?,?\\([^,\n]*\\)?,?\\([^,\n]*\\)?,?\\([^,\n]*\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-type-face)
       (3 font-lock-reference-face)
       (4 font-lock-comment-face)
       (5 font-lock-keyword-face)
       (6 font-lock-type-face)
       (7 font-lock-reference-face)
       (8 font-lock-comment-face)
       (9 font-lock-keyword-face)
       (10 font-lock-type-face)
       (11 font-lock-reference-face)
       (12 font-lock-comment-face)
       )
     )
  '("\\.csv\\'")
  '(
     (lambda ()
       (interactive)
       (setq header-line-format
         (save-excursion
           (goto-char (point-min))
           (buffer-substring (point-at-bol) (point-at-eol))))
       ;;       (local-set-key (kbd "<f10>") 'xml-format-region)
       (local-set-key (kbd "<f2>") 'csv-highlight-plot)
       (local-set-key (kbd "<f9>") 'csv-plot-all)
       (local-set-key (kbd "C-<f2>") 'csv-format)
       (local-set-key (kbd "C-<f3>") 'csv-align)
       (setq tab-width 8)
       ))
  "CSV files.")


(define-generic-mode tsv-highlight-mode
  nil
  nil
  `(
     (,(concat "^\\([^\t\n]*\\)[\t]\\([^\t\n]*\\)[\t]?\\([^\t\n]*\\)?\t?\\([^\t\n]*\\)?\t?"
         "\\([^\t\n]*\\)?\t?\\([^\t\n]*\\)?\t?\\([^\t\n]*\\)?\t?\\([^\t\n]*\\)?\t?"
         "\\([^\t\n]*\\)?\t?\\([^\t\n]*\\)?\t?\\([^\t\n]*\\)?\t?\\([^\t\n]*\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-type-face)
       (3 font-lock-reference-face)
       (4 font-lock-comment-face)
       (5 font-lock-variable-name-face);;   (5 font-lock-keyword-face)
       (6 font-lock-type-face)
       (7 font-lock-reference-face)
       (8 font-lock-function-name-face)
       (9 font-lock-keyword-face)
       (10 font-lock-type-face)
       (11 font-lock-reference-face)
       (12 font-lock-comment-face)
       )
     )
  '("\\.tsv\\'")
  '(
     (lambda ()
       (interactive)
       (setq header-line-format
         (save-excursion
           (goto-char (point-min))
           (buffer-substring (point-at-bol) (point-at-eol))))
       ;;       (local-set-key (kbd "<f10>") 'xml-format-region)
       ;; (local-set-key (kbd "<f2>") 'csv-highlight-plot)
       ;; (local-set-key (kbd "<f9>") 'csv-plot-all)
       ;; (local-set-key (kbd "C-<f2>") 'csv-format)
       ;; (local-set-key (kbd "C-<f3>") 'csv-align)
       (setq tab-width 8)
       ))
  "TSV files.")


(defun vj-tv-mode ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(45 70))
  (tool-bar-mode -1)
  (menu-bar-mode -1))


(defun my/org-narrow-forward ()
  "Move to the next subtree at same level, and narrow to it."
  (interactive)
  (widen)
  (org-forward-heading-same-level 1)
  (org-narrow-to-subtree))

(defun my/org-narrow-backward ()
  "Move to the previous subtree at same level, and narrow to it."
  (interactive)
  (widen)
  (org-forward-heading-same-level -1)
  (org-narrow-to-subtree))

;; C-x n s  is  org-narrow-to-subtree
;;(global-set-key (kbd "C-<f12>") 'my/org-narrow-forward)
;;(global-set-key (kbd "M-<f12>") 'my/org-narrow-backward)



(defun vj-alpha-adjust (inc)
  "Increase or decrease the alpha"
  (interactive "p")
  (let ((first t)
        (step t)
        val
        (ev last-command-event)
        (echo-keystrokes nil))
    (while step
      (let ((base (event-basic-type ev)))
        (cond ((or (eq base ?+) (eq base ?=))
               (setq step inc))
              ((eq base ?-)
               (setq step (- inc)))
              ((eq base ?0)
               (setq step 0))
              (first
               (setq step inc))
              (t
               (setq step nil))))
      (when step
        (setq val (car (frame-parameter (selected-frame) 'alpha)))
        (setq val (+ val step))
        (set-frame-parameter (selected-frame) 'alpha (list val 70))
        (setq inc 1 first nil)
        (setq ev (read-event (format "+,-,0 for further adjustment: %d" val)))))
    (push ev unread-command-events)))

(global-set-key (kbd "C-c +") 'vj-alpha-adjust)
(global-set-key (kbd "C-c -") 'vj-alpha-adjust)



;; (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(setq
  recentf-exclude '("\\.tags$")
  recentf-max-saved-items 200)

;; ------------------------------------------------------------

;; Use M-n/M-p in minibuffer to complete history entries based on prefix
(mapcar
 (function
  (lambda (x)
    (define-key x (kbd "ESC p") 'previous-complete-history-element)
    (define-key x (kbd "ESC n") 'next-complete-history-element)))
 (list minibuffer-local-completion-map minibuffer-local-isearch-map
       minibuffer-local-map minibuffer-local-must-match-map
       minibuffer-local-ns-map))
