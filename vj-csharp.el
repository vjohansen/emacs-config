;; omnisharp-server-executable-path and vj-curl-path MUST be set before
;; loading this file.

(require 'csharp-mode)
(setq csharp-want-imenu nil)
(defvar vj-curl-path "curl")

(defun my-csharp-mode-hook ()
  (c-set-style "c#")
  (setq dabbrev-case-replace nil)
  (c-set-offset 'arglist-cont-nonempty '+) ;; Imitate VS
  ;;  (c-set-offset 'arglist-intro '+) ;; Imitate VS
  (setq c-basic-offset 4))

(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(defun my-csharp-omni-mode-hook ()
  (local-set-key (kbd "M-r M-r") 'omnisharp-rename)
  (local-set-key (kbd "M-r t") 'omnisharp-unit-test-single)
  (local-set-key (kbd "M-r M-a") 'omnisharp-unit-test-fixture)
  (local-set-key (kbd "M-r a") 'omnisharp-unit-test-all)
  (local-set-key (kbd "M-r u") 'omnisharp-fix-usings)
  (local-set-key (kbd "M-r i") 'omnisharp-find-implementations) ;Find *interface* implementations
  (local-set-key (kbd "<f12>") 'omnisharp-go-to-definition)
  (local-set-key (kbd "S-<f12>") 'omnisharp-find-usages)
  (local-set-key (kbd "M-<f2>") 'omnisharp-build-in-emacs)
  ;; Only enable omnisharp-mode if server is running
  (let ((server-running nil))
    (dolist (pid (list-system-processes))
      (let ((pair (assoc 'comm (process-attributes pid))))
        (if (and pair (string-match "OmniSharp\\|mono" (cdr pair)))
          (setq server-running t))))
    (if server-running
      (omnisharp-mode)))
  (company-mode t))

(when (file-exists-p omnisharp-server-executable-path)
  (require 'omnisharp)
  (setq omnisharp--curl-executable-path
    (if (file-exists-p vj-curl-path) vj-curl-path "curl"))
  (global-set-key (kbd "C-.") 'company-complete)
  (add-hook 'csharp-mode-hook 'my-csharp-omni-mode-hook)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-omnisharp)))

(defun vj-add-omnisharp-vps-project ()
  ""
  (interactive)
  (let ((files (omnisharp--find-solution-files)) dir name)
    (when files
      (setq dir (car files))
      (setq name (format "cs-%s" (nth 1 files)))
      (vps-add-project name `((rdirs (,dir)) (ext ("cs" "xaml" "fs" "fsx"))))
      (vps-auto-change-project t)
      (vps-make-tags)
      (message "Created %s" name))))
