
(if window-system
    (setq confirm-kill-emacs 'y-or-n-p)
    (setq confirm-kill-emacs nil))

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

;; (setq solar-holidays nil
;;       calendar-holidays nil)


(autoload 'edit-list "edit-list"
  "*Edit a list called LIST-NAME interactively." nil t)

(autoload 'browse-kill-ring "browse-kill-ring" nil t)

(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))
(global-set-key "\M-\C-y" 'kill-ring-search)


;;(autoload 'iedit-mode "iedit" nil t)
(define-key global-map (kbd "C-;") 'iedit-mode)


(autoload 'etags-select-find-tag-at-point "etags-select" "*Do a find-tag-at-point, and display all exact matches." t)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


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


;; (global-set-key [M-f2] (lambda () (interactive) (vps-grep (current-word))))
(global-set-key [pause] '(lambda() (interactive) (kill-buffer nil)))

(if (equal system-type 'windows-nt)
  ;; http://blogs.msdn.com/b/dotnetinterop/archive/2008/04/10/run-powershell-as-a-shell-within-emacs.aspx
  (progn
    (autoload 'powershell "powershell"
      "*Run powershell as a shell within emacs." t)
    (autoload 'shell-toggle "eshell-toggle" "Toggles between the *shell* buffer and whatever buffer you are editing."
      t))
  (autoload 'shell-toggle "shell-toggle" "Toggles between the *shell* buffer and whatever buffer you are editing."
    t))

(global-set-key (kbd "<C-f12>") 'shell-toggle-cd)
(global-set-key (kbd "<M-f12>") 'shell-toggle)
(global-set-key (kbd "ESC <f12>") 'shell-toggle)



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
  (lambda ()
    (if (not (minibufferp (current-buffer)))
        (vj-global-keys-mode 1))))

(add-to-list 'emulation-mode-map-alists '(vj-global-keys-global-mode vj-global-keys-mode-map))




(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (url-hexify-string (buffer-substring (region-beginning) (region-end)))
      (read-string "Query: ")))))

;; Type unicode numbers with C-q
(setq read-quoted-char-radix 16)
