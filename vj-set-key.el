
(global-set-key [M-f1] 'recompile)
(global-set-key [f5] 'compile)
(global-set-key [C-tab] 'hippie-expand)
(global-set-key [pause]   '(lambda() (interactive) (kill-buffer nil)))

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

;; Easier to type on danish keyboard than M-/
(global-set-key (kbd "M-\'") 'dabbrev-expand)

(defun vj-switch-to-buffer-hook ()
   "Call switch-to-buffer without prompting"
   (interactive)
   (switch-to-buffer nil))

(global-set-key (kbd "C-+") 'vj-switch-to-buffer-hook)

;; Locally set tab-width to 4. Usually used when reading files made in VS
;; where the default is tabs with an indent that is 4.
(global-set-key [C-f9] (lambda ()
                         (interactive)
                         (setq tab-width 4)
                         (recenter)))

(global-set-key [S-up] 'my-scroll-down-hook)
(global-set-key [S-down] 'my-scroll-up-hook)

(global-set-key [M-down] 'vjo-forward-current-word-keep-offset)
(global-set-key [M-up] 'vjo-backward-current-word-keep-offset)


;; I want to use \M-q as a keymap
(if (not (keymapp (lookup-key global-map "\M-q")))
    (global-unset-key "\M-q"))

(global-set-key "\M-q\M-q" 'fill-paragraph)
(global-set-key "\M-q\M-l" 'vj-insert-local-variables-section)
(global-set-key "\M-qc" 'indent-region)
(global-set-key "\M-qp" 'find-file-at-point)
(global-set-key "\M-q\M-s" 'slash-replace-on-region)
(global-set-key "\M-qr" 'insert-char-above)

(global-set-key (kbd "M-q +") 'my-increment-number-at-point)
(global-set-key (kbd "M-q M-+") 'copy-and-inc-line)

;; tags-apropos current word. Use C-u for C++ members (prepends ::)
(global-set-key "\M-qt"
  (lambda (&optional member)
    (interactive "p")
    (if (equal member 4)
      (tags-apropos (concat "::" (current-word) "\\>"))
      (tags-apropos (concat "\\<" (current-word) "\\>")))))


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


(defun vj-prog-keys-setup ()
  "Bind æ -> {,  M-æ -> forward-paragraph, etc."
  (interactive)
  (local-set-key (kbd "æ") '(lambda () (interactive) (insert "[")))
  (local-set-key (kbd "ø") '(lambda () (interactive) (insert "]")))
  (local-set-key (kbd "å") '(lambda () (interactive) (insert "\\")))

  (local-set-key (kbd "Æ") '(lambda () (interactive) (insert "{")))
  (local-set-key (kbd "Ø") '(lambda () (interactive) (insert "}")))
  (local-set-key (kbd "Å") '(lambda () (interactive) (insert "\\n")))

  (setq show-trailing-whitespace t)
  )

(defun vj-prog-auto-fill ()
  (turn-on-auto-fill))

(require 'prog-mode)
(add-hook 'prog-mode-hook 'vj-prog-keys-setup)
(add-hook 'prog-mode-hook 'vj-prog-auto-fill)


(local-set-key (kbd "M-æ") 'forward-paragraph)
(local-set-key (kbd "M-ø") 'backward-paragraph)

(global-set-key (kbd "M-æ")  'backward-paragraph)
(global-set-key (kbd "M-ø")  'forward-paragraph)
(global-set-key (kbd "M-å")  'delete-horizontal-space)
(global-set-key (kbd "C-M-å")  'just-one-space)

;;trick: call global-set-key interactively and then do C-x ESC ESC
;;       to show the keybinding description

(provide 'vj-set-key)
