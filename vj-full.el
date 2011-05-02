
(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))
(global-set-key "\M-\C-y" 'kill-ring-search)

(autoload 'align-regexp2 "align-regexp2" "*Align region according to regular expressions..." t)

(autoload 'align-equals "align-equals" "*make the first assignment operator on each line line up vertically" t)

(global-set-key "\M-q/" 'backslash-replace-on-region)
(global-set-key "\M-q\M-a" 'align-current)
(global-set-key "\M-qz" 'align-regexp2)
(global-set-key "\M-qa" 'align-equals)
(global-set-key (kbd "M-q RET") 'find-file-at-point)
(global-set-key "\C-x\C-b" 'ibuffer)



(autoload 'fnexpand-complete "fnexpand" "Expand the file name, env var or command near point" t)

;;trick: call global-set-key interactively and then do C-x ESC ESC
(global-set-key "\361	" 'fnexpand-complete) ; M-q TAB


(load "vj-grep")
(global-set-key "\C-\M-g" 'vgrep)       ;FIXME rename to grep vj-lgrep

(global-set-key [M-down] 'vjo-forward-current-word-keep-offset)
(global-set-key [M-up] 'vjo-backward-current-word-keep-offset)

(global-set-key [S-up] 'my-scroll-down-hook)
(global-set-key [S-down] 'my-scroll-up-hook)


;; (global-set-key [M-f2] (lambda () (interactive) (vps-grep (current-word))))
(global-set-key [pause] '(lambda() (interactive) (kill-buffer nil)))


(defun google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "Query: ")))))
