;;; gnome2-theme.el --- extracted from color-theme-gnome2

;;; Code:

(deftheme gnome2 "Based on color-theme-gnome2")

(let ((class '((class color) (min-colors 89))))

  (custom-theme-set-faces
    'gnome2

    `(default ((,class (:foreground "wheat" :background "darkslategrey"))))
    `(cursor ((,class (:foreground "LightGray"))))
    `(mouse-color ((,class (:foreground "Grey"))))
    `(border-color ((,class (:foreground "black"))))
;;        '(background-mode . dark)
    ;; Highlighting faces
    `(fringe ((,class (:background "grey20"))))

    `(highlight ((,class (:background "#224" :foreground nil))))
    `(region ((,class (:background "blue4"))))
    ;;   `(secondary-selection ((,class (:background ,blue-0))))
    `(isearch ((,class (:foreground "white" :background "#448"))))
    `(lazy-highlight ((,class (:background "#264" :foreground "white"))))
    `(trailing-whitespace ((,class (:background "#600"))))

    ;; Escape and prompt faces
    ;;   `(minibuffer-prompt ((,class (:foreground "cyan"))))
    `(escape-glyph ((,class (:foreground "grey"))))
    ;; Font lock faces
    `(font-lock-builtin-face ((,class (:bold t :foreground "PaleGreen"))))
    `(font-lock-comment-face ((,class (:foreground "LightBlue"))))
    `(font-lock-constant-face ((,class (:foreground "Aquamarine"))))
    `(font-lock-doc-string-face ((,class (:foreground "LightSalmon"))))
    `(font-lock-function-name-face ((,class (:bold t :foreground "Aquamarine"
					      :height 1.0))))
    `(font-lock-keyword-face ((,class (:foreground "Salmon"))))
    `(font-lock-preprocessor-face ((,class (:foreground "Salmon"))))
    `(font-lock-reference-face ((,class (:foreground "pale green"))))
    `(font-lock-string-face ((,class (:foreground "LightSalmon"))))
    `(font-lock-type-face ((,class (:bold t :foreground "YellowGreen"))))
    `(font-lock-variable-name-face ((,class (:bold t :foreground "Aquamarine"))))
    ;;    `(font-lock-warning-face ((,class (:bold t :foreground "red")))))))

    ;; `(font-lock-variable-name-face ((,class (:foreground "LightGoldenrod"))))
    ;; `(font-lock-warning-face ((,class (:foreground "Pink" :weight bold))))
    `(link ((,class (:underline t :foreground "cyan2"))))
    `(link-visited ((,class (:underline t :foreground "violet"))))

    `(flymake-errline ((,class (:background nil :underline "red"))))
    `(flymake-warnline ((,class (:background nil :underline "magenta3"))))

    ;; ;; git-like colors
    `(diff-added ((,class (:foreground "PaleGreen"))))
    `(diff-removed ((,class (:foreground "tomato"))))
    `(diff-changed ((,class (:foreground "magenta"))))

    ))

(provide-theme 'gnome2)
