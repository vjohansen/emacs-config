(require 'helm-vps)
(global-set-key (kbd "C-'") 'helm-vps-index-db)
(global-set-key (kbd "C-x c o") 'helm-occur)
(global-set-key (kbd "M-x") 'helm-M-x)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; (defface helm-selection
;;   '((((class color) (background light))
;;       (:background "#ddddff" :bold t))
;;      (t (:background "#000040" :foreground "white")))
;;   "")

;; (defface helm-ff-executable
;;   '((((class color) (background light))
;;       (:foreground "#206020"))
;;      (t (:foreground "green")))
;;   "")

;; (defface helm-ff-directory
;;   '((((class color) (background light))
;;       (:foreground "ForestGreen" :bold t))
;;      (t (:inherit 'dired-header)))
;;   "")

;; (defface helm-selection
;;   '((((class color) (background light))
;;       (:foreground "white" :background "black" :bold t))
;;      (t (:foreground "white" :background "#224" :bold t)))
;;   "")

(setq it "") ;; helm-aif macro sets `it´

(require 'helm-config)
(require 'helm-files)
(require 'helm-command)
(require 'helm-buffers)
(require 'helm-misc) ;; enables C-r in minibuffer
(require 'helm-grep) ;for grep to work

(require 'helm-vps) ;; NOT in helm git repo

;; Added to helm-locate.el
;;     (define-key map (kbd "M-]")     'helm-ff-run-toggle-basename)
(if (eq system-type 'windows-nt)
  (setq helm-c-locate-command "c:/tools/Locate32/Locate.exe %s %s"))
(setq helm-ff-transformer-show-only-basename nil)

(global-set-key (kbd "C-å") 'vj-helm)
(global-set-key (kbd "C-x b") 'helm-mini)


(defvar vj-fav-fn "~/.fav")
(defvar vj-fav-list
  (with-temp-buffer
    (when (and vj-fav-fn (file-exists-p vj-fav-fn))
      (insert-file-contents vj-fav-fn)
      (mapcar 'expand-file-name (split-string (buffer-string))))))

(defvar vj-helm-source-fav
  '((name . "Favorites")
     (candidates . (lambda () vj-fav-list))
     (type . file)
     (requires-pattern . 3))
  "Source for favorites.")

(defvar vj-helm-list '(
                        vj-helm-source-fav
 			helm-source-vps-files
                        helm-source-recentf
                        helm-source-buffers-list
 			helm-source-files-in-current-dir
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


;; (setq helm-completion-window-scroll-margin 0)
