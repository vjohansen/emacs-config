

(setq it "") ;; helm-aif macro sets `it´

(setq helm-move-to-line-cycle-in-source nil)

(require 'helm-files)
(require 'helm-command)
(require 'helm-buffers)
(require 'helm-misc) ;; enables C-r in minibuffer
(require 'helm-grep) ;for grep to work

(require 'helm-vps) ;; NOT in helm git repo

(require 'vj-helm-simple-git-grep)
(global-set-key (kbd "C-x g") 'vj-helm-git-grep)
(global-set-key (kbd "C-c b") 'helm-vps)
(global-set-key (kbd "M-x") 'helm-M-x)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-x c o") 'helm-occur)

;; Overwrite binding for helm-execute-persistent-action
;; "Perform the associated action ATTR without quitting helm."
(define-key helm-map (kbd "C-j") #'helm-maybe-exit-minibuffer)
(define-key helm-map (kbd "C-<return>") #'helm-execute-persistent-action)

;; Added to helm-locate.el
(keymap-global-set "M-]" 'helm-ff-run-toggle-basename)

;; (if (eq system-type 'windows-nt)
;;   (setq helm-locate-command "c:/tools/Locate32/Locate.exe %s %s"))
(setq helm-ff-transformer-show-only-basename nil)

(global-set-key (kbd "C-å") 'vj-helm)
(global-set-key (kbd "C-x b") 'helm-mini)

;; use C-c o in helm for other window
(define-key helm-map (kbd "C-+") #'helm-buffer-switch-other-window)
(define-key helm-map (kbd "C-å") #'helm-toggle-buffers-details)



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


(defun vj-helm-recentf-list () recentf-list)

(defvar vj-helm-recentf
  (helm-build-sync-source "Recent Files"
    ;; https://github.com/emacs-helm/helm/issues/2709#issuecomment-3101237986
    ;;    If you want to fix it use :coerce 'substring-no-properties, otherwise
    ;;    you can use the helm-recentf-source to build your source.
    :candidates 'vj-helm-recentf-list
    :coerce 'substring-no-properties
    :filtered-candidate-transformer 'helm-highlight-files
    :action 'helm-find-files-actions))

(defvar vj-helm-list
  '(
     helm-source-recentf
;;     vj-helm-recentf                    ; helm-source-recentf TOO SLOW
     helm-source-buffers-list
     helm-source-files-in-current-dir
     helm-source-vps-files
     vj-helm-source-fav
     ;; 			helm-source-files-in-current-dir
     ;;                        helm-c-source-locate
     ))

(setq vj-text-dir (expand-file-name  "Sync/books" (getenv "USERPROFILE")))
;; (when (file-exists-p vj-text-dir)
;;   (add-to-list 'vj-helm-list 'vj-helm-text))

(defun vj-helm ()
  (interactive)
  (if current-prefix-arg
    (if (eq system-type 'windows-nt)
      (vj-helm-wdsgrep)
      (vj-helm-local-locate))
    (helm :sources vj-helm-list :buffer "*vj-helm*" :truncate-lines t)))



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



(defvar vj-text-files
  (if (file-exists-p vj-text-dir)
    (mapcar
      (lambda (f)
        (substring f (length vj-text-dir )))
      (mapcar #'(lambda (fn) (concat vj-text-dir fn))
        (directory-files-recursively vj-text-dir
        "\\.\\(epub\\|mobi\\|pdf\\)$" )))))

(defvar vj-helm-text
  (helm-build-sync-source "Books"
    :candidates  (lambda () vj-text-files)
    :candidate-number-limit 50
    :requires-pattern 4
    :filtered-candidate-transformer 'helm-highlight-files
    :action 'helm-find-files-actions
    ))


;; (defvar vj-helm-marked-files nil)

;; (defvar vj-helm-marked-files
;;   (helm-build-sync-source "Marked"
;;     :candidates  (lambda () vj-helm-marked-files)
;;     :candidate-number-limit 50
;;     :requires-pattern 2
;;     :filtered-candidate-transformer 'helm-highlight-files
;;     :action 'helm-find-files-actions
;;     ))

;; (defun vj-helm-marked-files-ui ()
;;   (interactive)
;;   (helm :sources vj-helm-marked-files
;;     :buffer "*vj-helm-marked-files*"
;;     :truncate-lines t))

;; (defun vj-helm-ff-run-helm-on-marked-files ()
;;   "Run `vj-helm' on marked files."
;;   (interactive)
;;   (let ((files (helm-marked-candidates)))
;;     (when files
;;       (setq vj-helm-marked-files files)
;;       ;; (with-helm-alive-p
;;       ;;   (exit-minibuffer))
;;       ;; exit helm
;;       (helm-keyboard-quit)
;;       (message "Marked files: %s" (length files))
;;       (vj-helm-marked-files-ui))))
;; ;; helm-grep-ag-search-results

;; (define-key helm-map (kbd "M-,") 'vj-helm-ff-run-helm-on-marked-files)


;; (require 'helm-grep)
;; (define-key helm-map (kbd "C-c s") 'helm-grep-ag-run-search-results)
;; (add-to-list 'helm-type-file-actions
;;   '("Run search on results" . helm-grep-ag-search-results) )

;; ;; helm-locate-map

(global-set-key (kbd "M-q l") 'vj-helm-locate)

(defun vj-helm-locate ()
  (interactive)
  "Query user for locate pattern and run helm locate."
  (let ((pattern (read-string "Locate pattern: ")))
    (when (and pattern (> (length pattern) 2))
      (let ((helm-locate-command
              (concat "c:/tools/Locate32/Locate.exe -i %s " pattern " %s")))
        (helm-other-buffer '(helm-source-locate) "*vj-helm-locate*"))))
  )
