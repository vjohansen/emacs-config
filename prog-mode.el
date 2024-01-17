
(defvar prog-mode-hook nil)

(defun prog-mode-setup ()
  (run-hooks 'prog-mode-hook))

(add-hook 'c-mode-common-hook 'prog-mode-setup)
(add-hook 'emacs-lisp-mode-hook 'prog-mode-setup)
(add-hook 'cperl-mode-hook 'prog-mode-setup)
(add-hook 'python-mode-hook 'prog-mode-setup)
(add-hook 'php-mode-hook 'prog-mode-setup)
(add-hook 'csharp-mode-hook 'prog-mode-setup)
(add-hook 'javascript-mode-hook 'prog-mode-setup)
(add-hook 'js-mode-hook 'prog-mode-setup)
(add-hook 'ruby-mode-hook 'prog-mode-setup)
(add-hook 'fsharp-mode-hook 'prog-mode-setup)

(provide 'prog-mode)
