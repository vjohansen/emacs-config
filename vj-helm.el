
(require 'vj-helm-simple-git-grep)
(global-set-key (kbd "C-x g") 'vj-helm-git-grep)
(global-set-key (kbd "C-'") 'helm-vps-index-db)
(global-set-key (kbd "C-c b") 'helm-vps)
(global-set-key (kbd "C-x c o") 'helm-occur)
(global-set-key (kbd "M-x") 'helm-M-x)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq it "") ;; helm-aif macro sets `it´

(require 'helm-config)
(require 'helm-files)
(require 'helm-command)
(require 'helm-buffers)
(require 'helm-misc) ;; enables C-r in minibuffer
(require 'helm-grep) ;for grep to work

(require 'helm-vps) ;; NOT in helm git repo

;; Overwrite binding for helm-execute-persistent-action
;; "Perform the associated action ATTR without quitting helm."
(define-key helm-map (kbd "C-j") #'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "C-<return>") #'helm-execute-persistent-action)

;; Added to helm-locate.el
;;     (define-key map (kbd "M-]")     'helm-ff-run-toggle-basename)
(if (eq system-type 'windows-nt)
  (setq helm-locate-command "c:/tools/Locate32/Locate.exe %s %s"))
(setq helm-ff-transformer-show-only-basename nil)

(global-set-key (kbd "C-å") 'vj-helm)
(global-set-key (kbd "C-x b") 'helm-mini)


(defvar vj-fav-fn "~/.fav")
(defvar vj-fav-list
  (with-temp-buffer
    (when (and vj-fav-fn (file-exists-p vj-fav-fn))
      (insert-file-contents vj-fav-fn)
      (mapcar 'expand-file-name (split-string (buffer-string))))))

(defun vj-fav-fn2 () vj-fav-fn)
(defclass helm-test-fav (helm-source-in-file helm-type-file)
  ((candidates-file :initform (vj-fav-fn2))))

;; (helm :sources (helm-make-source "fav" 'helm-test-fav) :buffer "*helm test*")

(defvar vj-helm-source-fav (helm-make-source "Favorites" 'helm-test-fav))

(require 'helm-for-files)


(defvar vj-helm-list
  '(
     helm-source-recentf
     helm-source-vps-files
     helm-source-files-in-current-dir
     vj-helm-source-fav
     helm-source-buffers-list
     ;; 			helm-source-files-in-current-dir
     ;;                        helm-c-source-locate
     ))

(defun vj-helm ()
  (interactive)
  (if current-prefix-arg
    (if (eq system-type 'windows-nt)
      (vj-helm-wdsgrep)
      (vj-helm-local-locate))
    (helm-other-buffer vj-helm-list "*vj-helm*")))


;; Desktop search for windows
(defvar helm-source-wdsgrep
  '((name . "Desktop Search")
     (candidates . (lambda ()
                     (start-process "wdsgrep-process" nil
                       "c:\\tools\\wdsgrep\\wdsgrep.exe" helm-pattern)))
     (type . file)
     (requires-pattern . 4)
     (delayed))
  "Source for retrieving files via W. Desktop Search.")

(if (eq system-type 'windows-nt)

  ;; Windows
  (defun vj-helm-wdsgrep ()
    (interactive)
    (helm-other-buffer '(helm-source-wdsgrep) "*helm Desktop Search*"))

  ;; else
  (defun vj-helm-local-locate ()
    (interactive)
    (when (not (file-exists-p "~/.vj-locate.db"))
      (message
        (propertize "Run: updatedb -l 0 -o ~/.vj-locate.db -U ~"
          'face 'compilation-info))
      (sit-for 2.0))
    (let ((helm-c-locate-command "locate -d ~/.vj-locate.db -i -r %s"))
      (helm-other-buffer
        '(helm-c-source-locate)
        "*vj-helm-local-locate*"))))


(helm-add-action-to-source
  "kill-ring-save"
  #'(lambda (_candidate)
      (with-helm-buffer (kill-new _candidate)))
  helm-source-recentf
  1)


;; (defun my-no-helm-M-x ()
;;   (interactive)
;;   (let
;;     ((helm-completing-read-handlers-alist '((execute-extended-command . nil))))
;;     (call-interactively 'execute-extended-command)))
;; (global-set-key (kbd "ESC x") 'my-no-helm-M-x)

;; (setq helm-completion-window-scroll-margin 0)


;; (setq helm-echo-input-in-header-line t)
;; (helm-autoresize-mode -1)
;; (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)


(setq cppref-dir "c:\\temp\\src\\cppreference\\reference\\en\\cpp\\")

(defvar cppref-files
  (if (file-exists-p cppref-dir)
    (mapcar
      (lambda (f)
        (substring f (length cppref-dir )))
      (directory-files-recursively cppref-dir ""))))

(defun cppref-action (name)
  (interactive)
;;  (eww-open-file (concat cppref-dir name))
  (browse-url (concat "https://en.cppreference.com/w/cpp" name)) ;; TODO remove .html
;;  (browse-url (concat cppref-dir name))
  )

(defun vj-cppref-2019 ()
  (interactive)
  (helm :sources
    (helm-build-sync-source "cppref"
      :candidates  (lambda () cppref-files)
      ;;    :filtered-candidate-transformer #'helm-kill-ring-transformer
      :action (list (cons "Do it" #'cppref-action)))))
