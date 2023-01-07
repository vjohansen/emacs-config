;;; gnome2-theme.el --- extracted from color-theme-gnome2

;;; Code:

(deftheme gnome2 "Based on color-theme-gnome2")

(custom-theme-set-faces
 'gnome2

 '(default ((t (:foreground "wheat" :background "darkslategrey"))))
 '(cursor ((t (:foreground "LightGray"))))
 '(mouse-color ((t (:foreground "Grey"))))
 '(border-color ((t (:foreground "black"))))
 '(fringe ((t (:background "grey20"))))
 '(highlight ((t (:background "#224" :foreground nil))))
 '(region ((t (:background "blue3"))))
 '(isearch ((t (:foreground "white" :background "#448"))))
 '(lazy-highlight ((t (:background "#264" :foreground "white"))))
 '(trailing-whitespace ((t (:background "#600"))))

 '(escape-glyph ((t (:foreground "grey"))))
 ;; Font lock faces
 '(font-lock-builtin-face ((t (:bold t :foreground "PaleGreen"))))
 '(font-lock-comment-face ((t (:foreground "LightBlue"))))
 '(font-lock-constant-face ((t (:foreground "Aquamarine"))))
 '(font-lock-doc-string-face ((t (:foreground "LightSalmon"))))
 '(font-lock-function-name-face ((t (:bold t :foreground "Aquamarine"
					   :height 1.0))))
 '(font-lock-keyword-face ((t (:foreground "Salmon"))))
 '(font-lock-preprocessor-face ((t (:foreground "Salmon"))))
 '(font-lock-reference-face ((t (:foreground "pale green"))))
 '(font-lock-string-face ((t (:foreground "LightSalmon"))))
 '(font-lock-type-face ((t (:bold t :foreground "YellowGreen"))))
 '(font-lock-variable-name-face ((t (:bold t :foreground "Aquamarine"))))
 ;;    '(font-lock-warning-face ((t (:bold t :foreground "red")))))))
 '(org-level-2 ((t (:bold t :foreground "#fce94f")))) ;~yellow
 '(org-level-3 ((t (:bold t :foreground "YellowGreen")))) ;; LightSalmon
 '(org-date ((t (:foreground "#8df"))))

 ;; '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 ;; '(font-lock-warning-face ((t (:foreground "Pink" :weight bold))))
 '(link ((t (:underline t :foreground "cyan2"))))
 '(link-visited ((t (:underline t :foreground "violet"))))

 ;; git-like colors
 '(diff-added ((t (:foreground "PaleGreen"))))
 '(diff-removed ((t (:foreground "tomato"))))
 '(diff-changed ((t (:foreground "magenta"))))

 '(mode-line ((t (:background "dark olive green" :foreground "wheat"))))
;;  '(mode-line-buffer-id ((t (:background "dark olive green" :foreground "beige"))))
 ;; '(modeline-mousable ((t (:background "dark olive green" :foreground "yellow green"))))
 ;; '(modeline-mousable-minor-mode ((t (:background "dark olive green" :foreground "wheat"))))

 )


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'gnome2)

;;; gnome2-theme.el ends here
