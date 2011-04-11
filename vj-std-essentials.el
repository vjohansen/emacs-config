
;; Use C-x r j E to load .emacs
(if (file-exists-p "~/.emacs")
  (set-register ?e '(file . "~/.emacs"))
  (set-register ?e '(file . "~/.emacs.d/init.el")))
(set-register ?t '(file . "~/tempfile")) ;Use this instead of *scratch*

(show-paren-mode t)
(recentf-mode t)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(which-function-mode)
(auto-compression-mode t)

;; Change annoying yes/no RET questions to single keypress of y or n
(fset 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format
  '("" invocation-name " "  " %f [" vps-project-name "]"))

(setq visible-bell t)

(setq indent-tabs-mode nil              ; Do not use tabs for indent
      compilation-ask-about-save nil)   ; Just save files before compile
(setq find-file-existing-other-name t)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(setq
  recentf-exclude '("\\.tags$")
  recentf-max-saved-items 200)

;; ------------------------------------------------------------

(iswitchb-mode t)

;; Make it possible for comma to start subprocess (ANDing)

(defun iswitchb-my-keys ()
  "Add my keybindings for iswitchb."
    (define-key iswitchb-mode-map "," 'iswitchb-exclude-nonmatching))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-my-keys)


(defun iswitchb-exclude-nonmatching()
  "Make iswitchb work on only the currently matching names."
  (interactive)
  (setq iswitchb-buflist iswitchb-matches)
  (setq iswitchb-rescan t)
  (delete-minibuffer-contents))

;; ------------------------------------------------------------


(mapcar
 (function
  (lambda (x)
    (define-key x (kbd "ESC p") 'previous-complete-history-element)
    (define-key x (kbd "ESC n") 'next-complete-history-element)))
 (list minibuffer-local-completion-map minibuffer-local-isearch-map
       minibuffer-local-map minibuffer-local-must-match-map
       minibuffer-local-ns-map))

(provide 'vj-std-essentials)
