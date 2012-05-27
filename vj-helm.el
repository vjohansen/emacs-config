
(require 'helm-config)
(require 'helm-files)
(require 'helm-buffers)
(require 'helm-mini) ;; enables C-r in minibuffer

;; Added to helm-locate.el
;;     (define-key map (kbd "M-]")     'helm-ff-run-toggle-basename)
(setq helm-c-locate-command "c:/tools/Locate32/Locate.exe %s")
(setq helm-ff-transformer-show-only-basename nil)
(global-set-key (kbd "M-h") 'vj-helm)

(defvar vj-helm-list '(helm-c-source-buffers-list
			helm-c-source-recentf
			helm-c-source-locate))
(defun vj-helm ()
  (interactive)
  (helm-other-buffer vj-helm-list "*vj-helm*"))

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

(add-to-list 'helm-sources 'helm-source-wdsgrep)

(defun vj-helm-wdsgrep ()
  (interactive)
  (helm-other-buffer '(helm-source-wdsgrep) "*helm Desktop Search*"))


