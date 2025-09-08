;;; isl.el --- Simple incremental search in current-buffer -*- lexical-binding: t -*-

;; Author:      Thierry Volpiatto <thievol@posteo.net>
;; Copyright (C) 2021~2025 Thierry Volpiatto <thievol@posteo.net>

;; Version: 1.4
;; URL: https://github.com/thierryvolpiatto/isearch-light

;; Compatibility: GNU Emacs 29.1+
;; Package-Requires: ((emacs "29.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ISL like Incremental-Search-Light.  Search regexp(s) incrementaly
;; in current-buffer using multimatch like Helm.  By default
;; multimatch is done on 'symbols', but can be toggled to 'lines' with
;; `C-j', see README file for more infos on this and other features.
;; `C-h m' is available at any moment during session for help to see
;; common commands.  The starting point is M-x isl-search, bind it to
;; a convenient keybinding of your choice.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'oclosure))

(defvar iedit-aborting)
(defvar iedit-read-only-occurrences-overlays)
(defvar iedit-read-only-occurrences-overlays)
(defvar iedit-case-sensitive)
(defvar iedit-occurrences-overlays)
(defvar iedit-mode)
(defvar helm-occur-always-search-in-current)
(defvar hs-minor-mode)
(defvar hs-show-hook)
(declare-function iedit-make-read-only-occurrence-overlay "ext:iedit-lib.el")
(declare-function iedit-make-occurrence-overlay "ext:iedit-lib.el")
(declare-function iedit-update-index "ext:iedit-lib.el")
(declare-function iedit-lib-cleanup "ext:iedit-lib.el")
(declare-function iedit-start "ext:iedit.el")
(declare-function iedit-done "ext:iedit.el")
(declare-function outline-show-entry "outline.el")
(declare-function org-reveal "org.el")
(declare-function helm-multi-occur-1 "ext:helm-occur.el")
(declare-function hs-show-block "hideshow.el")
(declare-function markdown-show-entry "ext:markdown-mode.el")
(declare-function bm-toggle "ext:bm.el")
(declare-function pulse-momentary-highlight-region "pulse.el")

;; History
(defvar isl-history nil)


;; Help
(defvar isl-help-string
  "* ISL help\n

Incremental search in current buffer with multi match (InSymbol/InLine).
Default is 'InSymbol' which means that with a pattern like \"foo bar\",
you would match the symbol 'foo-bar' but not the words \"foo\" and
\"bar\" that are on the same line, for this you will have to switch to
'Inline'.  Each pattern is separated by one space and can be a regexp,
it can start with a negation (!) which means match everything but this
e.g. \"foo !bar\" would match any symbol containing foo but not bar.

** Commands
\\<isl-map>
\\[isl-display-or-quit-help]\t\tDisplay or quit this help buffer
\\[isl-help-quit]\t\tQuit this help buffer
\\[isl-goto-next]\t\tGoto next occurence
\\[isl-goto-prev]\t\tGoto previous occurence
\\[isl-scroll-down]\t\tScroll down
\\[isl-scroll-up]\t\tScroll up
\\[isl-exit-at-point]\t\tExit at current position
\\[abort-recursive-edit]\t\tQuit and restore initial position
\\[isl-yank-word-at-point]\t\tYank word at point
\\[isl-yank-symbol-at-point]\t\tYank symbol at point
\\[isl-recenter]\t\tRecenter current buffer
\\[isl-change-matching-style]\t\tToggle matching style (regexp/literal)
\\[isl-select-case-fold-search]\t\tChange case fold search (cycle: *=smart, 1=t, 0=nil)
\\[isl-goto-first]\t\tGoto first occurence
\\[isl-goto-last]\t\tGoto last occurence
\\[isl-goto-closest-from-start]\t\tGoto closest occurence from start
\\[isl-jump-to-helm-occur]\t\tJump to helm-occur
\\[isl-jump-to-iedit-mode]\t\tJump to iedit-mode
\\[isl-query-replace]\t\tJump to query replace
\\[isl-show-or-hide-context-lines]\t\tHide or show non matching lines (num prefarg or default)
\\[isl-toggle-multi-search-in-line]\t\tToggle multi search style (InLine/InSymbol)
\\[isl-toggle-invisible-search]\t\tToggle searching in invisible text (Sinv)
\\[isl-bm-this-pos]\t\tAdd bookmark BM to current pos
\\[isl-kill-selection]\t\tKill selected occurence")


;; Private internals
(defvar isl--pattern "")
(defvar-local isl--last-query nil)
(put 'isl--last-query 'permanent-local t)
(defvar-local isl--last-object nil)
(put 'isl--last-object 'permanent-local t)
(defvar isl--visited-buffers nil)
(defvar isl--current-buffer nil)
(defvar isl--item-overlays nil)
(defvar isl--iterator nil)
(defvar isl--last-overlay nil)
(defvar isl--initial-pos nil)
(defvar isl--number-results 0)
(defvar isl--yank-point nil)
(defvar isl--quit nil)
(defvar isl--invalid nil)
(defvar isl--window-start nil)
(defvar-local isl--buffer-invisibility-spec nil)
(defconst isl--space-regexp "\\s\\\\s-"
  "Match a quoted space in a string.")
(defconst isl--case-fold-choices '(smart nil t))
(defvar isl--case-fold-choices-iterator nil)
(defvar isl--hidding nil)
(defvar isl--point-min nil)
(defvar isl--point-max nil)
(defvar isl--narrow-to-region nil)
(defvar isl--buffer-tick nil)
(defvar isl--closest-overlay nil)

;; Others
(defvar isl-help-buffer-name "*isl help*")
(defvar isl-timer-delay 0.01)
(defvar isl-search-invisible t)


;; User vars

(defgroup isl nil
  "Search buffers with `isl-search'."
  :prefix "isl-"
  :group 'matching)

(defcustom isl-search-function #'isl--re-search-forward
  "The search function that will be used by default when starting `isl-search'.
Possible values are `isl--re-search-forward' and `search-forward', the
first uses regexp matching while the second is using literal matching.
Its value can be changed during `isl-search' session with `\\<isl-map>\\[isl-change-matching-style]'."
  :type '(choice
           (function :tag "Regexp matching" isl--re-search-forward)
           (function :tag "Literal matching" search-forward)))

(defcustom isl-case-fold-search 'smart
  "The `case-fold-search' value.
Possible value are nil, t or smart.
Value smart means use `case-fold-search' when upcase chars are detected
in pattern."
  :type 'symbol)

(defcustom isl-after-position-string ">"
  "The string used to notify in mode-line when position is above initial pos."
  :type 'string)

(defcustom isl-before-position-string "<"
  "The string used to notify in mode-line when position is below initial pos."
  :type 'string)

(defcustom isl-direction-down-string "↓"
  "The string used in mode-line to notify search direction."
  :type 'string)

(defcustom isl-direction-up-string "↑"
  "The string used in mode-line to notify search direction."
  :type 'string)

(defcustom isl-warning-char "⚠"
  "Used to signal invalid regexp in mode-line."
  :type 'string)

(defcustom isl-save-pos-to-mark-ring t
  "Save initial position to `mark-ring' on exit when non nil."
  :type 'boolean)

(defcustom isl-requires-pattern 1
  "Start updating after this number of chars."
  :type 'integer)

(defcustom isl-visible-context-lines 0
  "Number of lines to show around matched line when hiding context.
Can be a cons cell like (nlines_before . nlines_after) or an integer,
in this case it is used for before and after.
When 0, the default show no context lines."
  :type '(choice
          (cons
           :tag "Context lines number before and after"
           (integer :tag "Show num lines before")
           (integer :tag "Show num lines after"))
          (integer :tag "Context lines number")))

(defcustom isl-noresume-buffers '("*Helm Help*")
  "Prevent resuming in these buffers."
  :type '(repeat string))

(defcustom isl-multi-search-in-line nil
  "Startup with multi search in line by default when non nil.
Otherwise multi search only in symbols.

In buffers containing huge lines or sometimes only one huge line, you
should multi search only in symbols and not in whole line which is
really costly and may take ages or crash Emacs.
You can toggle this at any time with \\<isl-map>\\[isl-toggle-multi-search-in-line]."
  :type 'boolean
  :initialize 'custom-initialize-changed
  :set (lambda (var val)
         (set var val)
         (if val
             (set-face-attribute 'isl-on nil :extend t)
           (set-face-attribute 'isl-on nil :extend nil))))

(defcustom isl-no-invisible-search-in-modes nil
  "Do not search invisible text in these modes."
  :type '(repeat symbol))

(defface isl-match
  '((t :background "brown4"))
  "Face used to highlight the items matched.")

(defface isl-match-items
  '((t :background "OrangeRed3" :foreground "black"))
  "Face used to highlight the items matched inside line.")

(defface isl-on
  '((t :background "SandyBrown"
       :foreground "black"))
  "Face used to highlight the item where point is.")

(defface isl-number
  '((t :foreground "green"))
  "Face used to highlight number in mode-line.")

(defface isl-string
  '((t :foreground "LightGoldenrod1" :bold t))
  "Face used to highlight pattern in mode-line.")

(defface isl-case-fold
  '((t :inherit isl-string))
  "Face used to highlight case sensitivity string in mode-line.")

(defvar isl-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-s")     'isl-goto-next)
    (define-key map (kbd "C-r")     'isl-goto-prev)
    (define-key map (kbd "C-n")     'isl-goto-next)
    (define-key map (kbd "C-p")     'isl-goto-prev)
    (define-key map (kbd "<down>")  'isl-goto-next)
    (define-key map (kbd "<up>")    'isl-goto-prev)
    (define-key map (kbd "RET")     'isl-exit-at-point)
    (define-key map (kbd "C-w")     'isl-yank-word-at-point)
    (define-key map (kbd "C-z")     'isl-yank-symbol-at-point)
    (define-key map (kbd "M-r")     'isl-change-matching-style)
    (define-key map (kbd "M-c")     'isl-select-case-fold-search)
    (define-key map (kbd "M-<")     'isl-goto-first)
    (define-key map (kbd "M->")     'isl-goto-last)
    (define-key map (kbd "M-=")     'isl-goto-closest-from-start)
    (define-key map (kbd "M-s")     'isl-jump-to-helm-occur)
    (define-key map (kbd "M-i")     'isl-toggle-invisible-search)
    (define-key map (kbd "C-;")     'isl-jump-to-iedit-mode)
    (define-key map (kbd "M-%")     'isl-query-replace)
    (define-key map (kbd "C-h m")   'isl-display-or-quit-help)
    (define-key map (kbd "C-q")     'isl-help-quit)
    (define-key map (kbd "C-'")     'isl-show-or-hide-context-lines)
    (define-key map (kbd "C-l")     'isl-recenter)
    (define-key map (kbd "C-v")     'isl-scroll-up)
    (define-key map (kbd "M-v")     'isl-scroll-down)
    (define-key map (kbd "C-k")     'isl-delete-minibuffer-contents)
    (define-key map (kbd "C-j")     'isl-toggle-multi-search-in-line)
    (define-key map (kbd "C-!")     'isl-bm-this-pos)
    (define-key map (kbd "C-c C-k") 'isl-kill-selection)
    map)
  "The map used when `isl-search' is running.
Don't forget to modify `isl-mini-map' accordingly to fit with kmacros
when modifying keybindings here.")

;;; Actions
;;
(defun isl--goto-overlay (overlay)
  "Goto OVERLAY."
  (let ((pos (and overlay (overlay-end overlay))))
    (when (and overlay pos)
      (setq isl--last-overlay overlay)
      (overlay-put overlay 'face 'isl-on)
      (goto-char pos)
      (setq isl--yank-point pos))))

(defun isl-kill-selection ()
  "Kill selected item."
  (interactive)
  (with-selected-window (minibuffer-selected-window)
    (when isl--last-overlay
      (let ((str (buffer-substring (overlay-start isl--last-overlay)
                                   (overlay-end isl--last-overlay))))
        (kill-new str)
        (message "`%s' copied to kill-ring" str)))))
(put 'isl-kill-selection 'no-helm-mx t)

(defun isl-bm-this-pos ()
  "Toggle bookmark on current position with `bm'.
Need to have the `bm' package installed."
  (interactive)
  (cl-assert (require 'bm nil t) nil "Please install `bm' package")
  (with-selected-window (minibuffer-selected-window)
    (when isl--last-overlay
      (save-excursion
        ;; Use overlay-start otherwise with inline we are one line too far.
        (goto-char (overlay-start isl--last-overlay))
        (bm-toggle)))))

(defun isl--highlight-last-overlay (face)
  "Highlight `isl--last-overlay' with FACE."
  (when (overlayp isl--last-overlay)
    (overlay-put isl--last-overlay 'face face)))

(defun isl-goto-next-1 (arg)
  "Main function that allow moving from one to ARG overlay.
It put overlay on current position, move to next overlay using
`isl--iterator', set `isl--yank-point' and then setup mode-line."
  (with-selected-window (minibuffer-selected-window)
    (if (cdr isl--item-overlays)
        (progn
          (isl--highlight-last-overlay 'isl-match)
          (when isl--iterator
            (cl-loop with ov repeat arg do (setq ov (isl-iter-next isl--iterator))
                     finally (isl--goto-overlay ov)))
          (isl-setup-mode-line))
      (user-error "No more matches"))))

(defun isl-scroll-1 (arg)
  "Scroll up if ARG is positive, down if it is negative."
  (let (ov)
    (with-selected-window (minibuffer-selected-window)
      (when isl--iterator
        (setq ov (if (> arg 0)
                     (isl--first-ov-after-pos (window-end))
                   (isl--first-ov-before-pos (window-start))))))
    (when ov
      (isl--find-and-goto-overlay ov))))

(defun isl--first-ov-after-pos (pos)
  "Find next overlay after POS."
  (cl-loop for ov in (memql isl--last-overlay isl--item-overlays)
           when (> (overlay-start ov) pos)
           return ov))

(defun isl--first-ov-before-pos (pos)
  "Find previous overlay before POS."
  (cl-loop for ov in (memql isl--last-overlay (reverse isl--item-overlays))
           when (< (overlay-start ov) pos)
           return ov))

(defun isl-scroll-up ()
  "Scroll up to closest overlay in next screen."
  (interactive)
  (isl-scroll-1 1))
(put 'isl-scroll-up 'no-helm-mx t)

(defun isl-scroll-down ()
  "Scroll down to closest overlay in previous screen."
  (interactive)
  (isl-scroll-1 -1))
(put 'isl-scroll-down 'no-helm-mx t)

(defun isl-delete-minibuffer-contents ()
  "Delete minibuffer contents from isl prompt.
If cursor is at eol delete whole contents otherwise delete from point
to end."
  (interactive)
  (with-selected-window (minibuffer-window)
    (if (eolp)
        (delete-region (minibuffer-prompt-end) (point))
      (delete-region (point) (point-max)))
    (setq isl--yank-point nil)))
(put 'isl-delete-minibuffer-contents 'no-helm-mx t)

(defun isl--find-and-goto-overlay (overlay)
  "Consume iterator up to OVERLAY and jump to it."
  (with-selected-window (minibuffer-selected-window)
    (when isl--iterator
      (let (ov)
        (while (not (eql (setq ov (isl-iter-next isl--iterator))
                         overlay)))
        (isl--highlight-last-overlay 'isl-match)
        (and ov (isl--goto-overlay ov)))
      (isl-setup-mode-line))))

(defun isl-goto-first ()
  "Goto first match."
  (interactive)
  (let ((ov (car isl--item-overlays)))
    (if (eql ov isl--last-overlay)
        (user-error "Already at first occurence")
      (isl--find-and-goto-overlay ov))))
(put 'isl-goto-first 'no-helm-mx t)

(defun isl-goto-last ()
  "Goto last match."
  (interactive)
  (let ((ov (car (last isl--item-overlays))))
    (if (eql ov isl--last-overlay)
        (user-error "Already at last occurence")
      (isl--find-and-goto-overlay ov))))
(put 'isl-goto-last 'no-helm-mx t)

(defun isl-goto-closest-from-start ()
  "Goto closest match from start."
  (interactive)
  (if (eql isl--closest-overlay isl--last-overlay)
      (user-error "Already at closest occurence from start")
    (isl--find-and-goto-overlay isl--closest-overlay)))
(put 'isl-goto-closest-from-start 'no-helm-mx t)

(defun isl-goto-next (&optional arg)
  "Go to next ARG match."
  (interactive "p")
  (when (and isl--iterator
             (eq (isl-iterator--direction isl--iterator) 'left))
    (isl-iterator-reverse isl--iterator))
  (isl-goto-next-1 arg))
(put 'isl-goto-next 'no-helm-mx t)

(defun isl-goto-prev (&optional arg)
  "Go to previous ARG matches."
  (interactive "p")
  (when (and isl--iterator
             (eq (isl-iterator--direction isl--iterator) 'right))
    (isl-iterator-reverse isl--iterator))
  (isl-goto-next-1 arg))
(put 'isl-goto-prev 'no-helm-mx t)

(defun isl-exit-at-point ()
  "Exit minibuffer and jump at current position."
  (interactive)
  (with-selected-window (minibuffer-selected-window)
    (when isl--last-overlay
      ;; Ensure user haven't scrolled to another place.
      (let ((end (overlay-end isl--last-overlay))
            (lp (overlay-get isl--last-overlay 'line-prefix)))
        (goto-char (if (or isl-multi-search-in-line lp)
                       (1- end) end)))
      (when isl-multi-search-in-line
        (let* ((ovs     (overlays-in (pos-bol) (pos-eol)))
               (matches (cl-loop for ov in ovs
                                 when (overlay-get ov 'isl-matches)
                                 collect ov)))
          (when matches
            (goto-char (overlay-end (car matches))))))
      (when isl-save-pos-to-mark-ring
        (set-marker (mark-marker) isl--initial-pos)
        (push-mark isl--initial-pos 'nomsg))
      (pulse-momentary-highlight-region (pos-bol) (pos-eol))
      ;; Save `window-start' position before exiting minibuffer and
      ;; restore it after in `isl-cleanup' to avoid a moving screen.
      (setq isl--window-start (window-start))))
  ;; Call `exit-minibuffer' out of the `with-selected-window' block to
  ;; avoid error with the emacs-28 version.
  (exit-minibuffer))
(put 'isl-exit-at-point 'no-helm-mx t)

(defun isl-yank-word-at-point ()
  "Yank word at point in minibuffer.
The word at point is relative to the current position in buffer, not
the initial position i.e. the position before launching `isl-search'."
  (interactive)
  (let (str)
    (with-current-buffer isl--current-buffer
      (when (eolp) (error "End of line"))
      (when (or (memq (char-syntax (or (char-after) 0)) '(?w ?_ ? ))
                (memq (char-syntax (or (char-after (1+ (point))) 0))
                      '(?w ?_ ? )))
        (setq str (buffer-substring-no-properties (or isl--yank-point (point))
                                                  (save-excursion
                                                    (forward-word)
                                                    (point))))
        (when (string-match "\\` " str)
          (setq str (replace-match "\\\\ " nil nil str)))
        (with-selected-window (minibuffer-window)
          (insert str))))))
(put 'isl-yank-word-at-point 'no-helm-mx t)

(defun isl-yank-symbol-at-point ()
  "Yank symbol at point in minibuffer.
The symbol at point is relative to the current position in buffer, not
the initial position i.e. the position before launching `isl-search'."
  (interactive)
  (let (str)
    (with-current-buffer isl--current-buffer
      (when (setq str (thing-at-point 'symbol t))
        (with-selected-window (minibuffer-window)
          (delete-minibuffer-contents)
          (insert str))))))
(put 'isl-yank-symbol-at-point 'no-helm-mx t)

(defun isl-recenter ()
  "Recenter from isl."
  (interactive)
  (with-selected-window (minibuffer-selected-window)
    (recenter)))
(put 'isl-recenter 'no-helm-mx t)

(defun isl-matching-style ()
  "Return current matching style as a string."
  (cl-ecase isl-search-function
    (isl--re-search-forward "Regex")
    (search-forward "Literal")))

(defun isl-change-matching-style ()
  "Toggle style matching in `isl-search' i.e. regexp/literal."
  (interactive)
  (with-current-buffer isl--current-buffer
    (setq-local isl-search-function
                (cl-ecase isl-search-function
                  (isl--re-search-forward #'search-forward)
                  (search-forward #'isl--re-search-forward)))
    (unless executing-kbd-macro
      (when (string= isl--pattern "")
        (let* ((style (isl-matching-style))
               (mode-line-format (format " Switching to %s searching" style)))
          (force-mode-line-update)
          (sit-for 1)))
      (isl-update))))
(put 'isl-change-matching-style 'no-helm-mx t)

(defun isl-toggle-invisible-search ()
  "Toggle searching in invisible text."
  (interactive)
  (with-current-buffer isl--current-buffer
    (setq-local isl-search-invisible (not isl-search-invisible))
    (unless isl-search-invisible
      (setq buffer-invisibility-spec isl--buffer-invisibility-spec))
    (message "Invisible search turned %s" (if isl-search-invisible "on" "off"))
    (isl-update)))
(put 'isl-toggle-invisible-search 'no-helm-mx t)

(defun isl-jump-to-helm-occur ()
  "Invoke `helm-occur' from `isl-search'."
  (interactive)
  (cl-assert (require 'helm-occur nil t) nil "Please install Helm package")
  (let ((input isl--pattern)
        (bufs (list isl--current-buffer)))
    (run-at-time 0.1 nil
                 (lambda ()
                   ;; Use `helm-occur-always-search-in-current' as a
                   ;; flag for `helm-occur--select-closest-candidate'.
                   (let ((helm-occur-always-search-in-current t))
                     (helm-multi-occur-1 bufs input))))
    (abort-recursive-edit)))
(put 'isl-jump-to-helm-occur 'no-helm-mx t)

(defun isl-query-replace (&optional arg)
  "Launch `query-replace' from isl.
Argument ARG have same meaning as in `query-replace'.
When it is launched from the last occurence, replacement is done
automatically backward."
  (interactive "P")
  (cl-assert isl--item-overlays nil "Nothing yet to replace")
  (let* ((style    (isl-matching-style))
         (regexp   isl--pattern)
         (start    (overlay-start isl--last-overlay))
         (end      (overlay-end isl--last-overlay))
         (bottom   (overlay-start (car (last isl--item-overlays))))
         (pnv      (prefix-numeric-value arg))
         (backward (or (and arg (< pnv 0))
                       (and start bottom (= start bottom))))
         (wr       (and arg (> pnv 0))))
    (run-at-time
     0.1 nil
     (lambda ()
       (let* ((regexp-flag (string= style "Regex"))
              (prompt1 (if backward "Query replace backward" "Query replace"))
              (prompt (concat prompt1 (if regexp-flag "%s regexp" "%s")))
              (args (list
                     regexp
                     (query-replace-read-to
                      regexp
                      (format prompt (if wr " word" ""))
                      regexp-flag)
                     (and wr arg))))
         (with-current-buffer isl--current-buffer
           (save-excursion
             (let ((case-fold-search t))
               (goto-char (if backward end start))
               (apply #'perform-replace
                      (list (nth 0 args) (nth 1 args)
                            t regexp-flag (nth 2 args) nil
                            multi-query-replace-map nil nil backward))))))))
    (abort-recursive-edit)))
(put 'isl-query-replace 'no-helm-mx t)

;; Iedit
;;
(defun isl--advice-iedit-start (old--fn &rest args)
  "Allow iedit matching multi pattern.
Use `isl--iedit-make-occurrences-overlays' instead of
`iedit-make-occurrences-overlays' then apply OLD--FN on ARGS.
This is an around advice."
  (cl-letf (((symbol-function 'iedit-make-occurrences-overlays)
             #'isl--iedit-make-occurrences-overlays))
    (apply old--fn args)))

(defun isl--iedit-make-occurrences-overlays (occurrence-regexp beg end)
  "Same as `iedit-make-occurrences-overlays' but handle multiple regexps.
Arguments OCCURRENCE-REGEXP, BEG and END have same meaning as in
`iedit-make-occurrences-overlays'."
  (setq iedit-aborting nil)
  (setq iedit-occurrences-overlays nil)
  (setq iedit-read-only-occurrences-overlays nil)
  ;; Find and record each occurrence's markers and add the overlay to the occurrences
  (let ((counter 0)
        (case-fold-search (not iedit-case-sensitive))
	(length 0)
        bounds)
    (save-excursion
      (save-selected-window
        (goto-char beg)
        (while (setq bounds (isl-multi-search-fwd occurrence-regexp end t))
          (let ((beginning (car bounds))
                (ending (cdr bounds)))
	    (if (and (> length 0) (/= (- ending beginning) length))
		(throw 'not-same-length 'not-same-length)
	      (setq length (- ending beginning)))
            (if (text-property-not-all beginning ending 'read-only nil)
                (push (iedit-make-read-only-occurrence-overlay beginning ending)
                      iedit-read-only-occurrences-overlays)
              (push (iedit-make-occurrence-overlay beginning ending)
                    iedit-occurrences-overlays))
            (setq counter (1+ counter))))))
    (iedit-update-index)
    counter))

(defun isl-jump-to-iedit-mode ()
  "Start Iedit mode from `isl' using last search string as the regexp."
  (interactive)
  (cl-assert (require 'iedit nil t))
  (cl-assert isl--item-overlays nil "Nothing yet to replace")
  (let ((regexp (if (eq isl-search-function 'search-forward)
                    (regexp-quote isl--pattern)
                  isl--pattern))
        (pos (with-current-buffer isl--current-buffer
               (overlay-end isl--last-overlay))))
    (run-at-time
     0.1 nil
     (lambda ()
       (save-restriction
         (when (and isl--point-min isl--point-max)
           (narrow-to-region isl--point-min isl--point-max))
         (let ((case-fold-search (isl-set-case-fold-search regexp))
	       result)
           (setq mark-active nil)
           (run-hooks 'deactivate-mark-hook)
           (when iedit-mode
             (iedit-lib-cleanup))
           (advice-add 'iedit-start :around #'isl--advice-iedit-start)
           (unwind-protect
                (progn
                  (setq result
	                (catch 'not-same-length
	                  (iedit-start regexp (point-min) (point-max))))
                  (cond ((not iedit-occurrences-overlays)
                         (message "No matches found for %s" regexp)
                         (iedit-done))
                        ((equal result 'not-same-length)
                         (message "Matches are not the same length.")
                         (iedit-done)))
                  (goto-char pos))
             (advice-remove 'iedit-start #'isl--advice-iedit-start))))))
    (abort-recursive-edit)))
(put 'isl-jump-to-iedit-mode 'no-helm-mx t)

(defun isl-display-or-quit-help ()
  "Display or quit isl help buffer."
  (interactive)
  (if (get-buffer-window isl-help-buffer-name 'visible)
      (progn
        (switch-to-buffer isl-help-buffer-name)
        (quit-window t))
    (with-current-buffer-window
        (get-buffer-create isl-help-buffer-name)
        (cons 'display-buffer-below-selected
	      '((window-height . fit-window-to-buffer)
	        (preserve-size . (nil . t))))
        nil
      (with-current-buffer standard-output
        (insert
         (substitute-command-keys
          isl-help-string)))
      (outline-mode)
      (setq cursor-type nil)
      (setq buffer-read-only t)
      (local-set-key (kbd "q") 'quit-window))))
(put 'isl-display-or-quit-help 'no-helm-mx t)

(defun isl-help-quit ()
  "Quit isl help buffer."
  (interactive)
  (let ((win (get-buffer-window isl-help-buffer-name 'visible)))
    (if win
        (with-selected-window win
          (quit-window))
      (user-error "No help buffer found"))))
(put 'isl-help-quit 'no-helm-mx t)

(defun isl-show-or-hide-context-lines (&optional arg)
  "Hide or show non matching lines.
Show only numeric prefix ARG or `isl-visible-context-lines'
number of lines around match.  Note that prefix arg allows specifying
only the same number before and after matched lines whereas
`isl-visible-context-lines' allows specifying a different value for
before and after."
  (interactive "p")
  (when isl--item-overlays
    (with-selected-window (minibuffer-selected-window)
      (if (setq isl--hidding (not isl--hidding))
          (let ((start 1) ; start at point-min.
                ov-end bol)
            (save-excursion
              (goto-char (overlay-end (car isl--item-overlays)))
              (setq ov-end (point))
              (set (make-local-variable 'line-move-ignore-invisible) t)
              (add-to-invisibility-spec '(isl-invisible . t))
              (while (not (eobp))
                (forward-line
                 (- (or (and current-prefix-arg (max arg 0))
                        (pcase isl-visible-context-lines
                          (`(,before . ,after) before)
                          ((and same) same)))))
                ;; Store position from n lines before
                ;; this overlay and bol and move to next overlay.
                (when (> (setq bol (pos-bol)) start)
                  (isl--put-invisible-overlay start (1- bol)))
                (goto-char ov-end)
                ;; Go to n lines after last overlay found and jump to
                ;; next overlay from there.
                (forward-line
                 (or (and current-prefix-arg (max arg 0))
                     (pcase isl-visible-context-lines
                       (`(,before . ,after) after)
                       ((and same) same))))
                (setq start (1+ (pos-eol)))
                (goto-char (next-single-char-property-change ov-end 'isl))
                (setq ov-end (point)))
              ;; Store maybe remaining lines up to eob.
              (when (< start (point-max))
                (isl--put-invisible-overlay start (point-max)))))
        (remove-overlays nil nil 'isl-invisible t)
        (remove-from-invisibility-spec '(isl-invisible . t))))))
(put 'isl-show-or-hide-context-lines 'no-helm-mx t)

(defun isl--put-invisible-overlay (beg end)
  "Make an invisible overlay from BEG to END."
  (let ((ol (make-overlay beg end)))
    (overlay-put ol 'isl-invisible t)
    (overlay-put ol 'invisible 'isl-invisible)))

;;; Iterators
;;
(oclosure-define isl-iterator
  "Provide iterator for isl navigation.
SEQ is the main list where the iterator is bound to, it is modified
only when changing direction.
TMP-SEQ is bound to SEQ and modified each time the iterator is called.
CHANGING-DIRECTION tells the iterator we want to change direction.
ELEMENT refers to the last element yielded.
DIRECTION refers to the direction we are currently using, left or right.
All slots are mutables."
  (seq :type 'list :mutable t)
  (tmp-seq :type 'list :mutable t)
  (changing-direction :type 'boolean :mutable t)
  (element :mutable t)
  (direction :type 'symbol :mutable t))

(defun isl-iter-circular (seq)
  "Return an infinite iterator from SEQ.
The iterator is mutable, its direction can be changed at anytime with
the function `isl-iterator-reverse', when doing so the sequence
handled by the iterator is reversed from the last element yielded.
Elements of the iterator can be accessed via the isl-iterator--* fns.
All the oclosure slots are mutables."
  (oclosure-lambda (isl-iterator (seq seq)
                                 (tmp-seq seq)
                                 (element (car seq))
                                 (direction 'right)
                                 (changing-direction nil))
      ()
    (let ((elm (car tmp-seq))
          revlist queue index)
      (if changing-direction
          (setq revlist (reverse seq)
                queue (memql element revlist)
                direction (pcase direction
                            ('left 'right)
                            ('right 'left))
                index (- (length seq) (length queue))
                seq (append queue (take index revlist))
                tmp-seq (or (cddr seq) seq) ; Fix seq with only two elements.
                elm (cadr seq)
                changing-direction nil)
        (setq tmp-seq (or (cdr tmp-seq) seq)))
      (setq element elm))))

(defun isl-iterator-reverse (iterator)
  "Change the direction of ITERATOR."
  (setf (isl-iterator--changing-direction iterator) t))

(defun isl-iter-next (iterator)
  "Return next elm of ITERATOR."
  (and iterator (funcall iterator)))

(defun isl-make-or-update-iterator ()
  "Build `isl--iterator' against `isl--item-overlays'.
If `isl--iterator' is non nil update it otherwise create it."
  (let* ((queue (memql isl--last-overlay isl--item-overlays))
         (index (- (length isl--item-overlays) (length queue)))
         (ovs   (append queue (take index isl--item-overlays))))
    (if isl--iterator
        (setf (isl-iterator--seq isl--iterator) ovs
              (isl-iterator--tmp-seq isl--iterator) ovs
              (isl-iterator--element isl--iterator) isl--last-overlay
              (isl-iterator--direction isl--iterator) 'right)
      (setq isl--iterator (isl-iter-circular ovs)))))

(defun isl-delete-overlays ()
  "Cleanup ovelays."
  (when isl--item-overlays
    (remove-overlays nil nil 'isl t)
    (setq isl--item-overlays nil)))

(defun isl-set-case-fold-search (&optional pattern)
  "Return a suitable value for `case-fold-search'.
This is done according to `isl-case-fold-search'.
Optional argument PATTERN default to `isl--pattern'."
  (unless pattern (setq pattern isl--pattern))
  (cl-case isl-case-fold-search
    (smart (let ((case-fold-search nil))
             (if (string-match "[[:upper:]]" pattern) nil t)))
    (t isl-case-fold-search)))

(defun isl-select-case-fold-search ()
  "Set `case-fold-search' from `isl-search' session."
  (interactive)
  (with-current-buffer isl--current-buffer
    (if (eq last-command 'isl-select-case-fold-search)
        (setq-local isl-case-fold-search
                    (isl-iter-next isl--case-fold-choices-iterator))
      (setq isl--case-fold-choices-iterator
            (isl-iter-circular
             (append (remove isl-case-fold-search isl--case-fold-choices)
                     (list isl-case-fold-search))))
      (setq-local isl-case-fold-search
                  (isl-iter-next isl--case-fold-choices-iterator)))
    (unless executing-kbd-macro
      (isl-update))))
(put 'isl-select-case-fold-search 'no-helm-mx t)

(defun isl-split-string (str)
  "Split string STR at non quoted spaces."
  (split-string
   (replace-regexp-in-string
    isl--space-regexp "\\s-" str nil t)))

(defun isl-patterns (str)
  "Return an alist of (pred . regexp) elements from STR."
  (cl-loop for s in (isl-split-string str)
           collect (if (char-equal ?! (aref s 0))
                       (cons 'not (substring s 1))
                     (cons 'identity s))))

(defsubst isl--re-search-forward (regexp &optional bound noerror count)
  "Same as `re-search-forward' but return nil when point doesn't move.
This avoid possible infloop when a wrong regexp is entered in
minibuffer.  Arguments REGEXP, BOUND, NOERROR and COUNT have same
meaning as in `re-search-forward'."
  (let ((pos (point)))
    (pcase (re-search-forward regexp bound noerror count)
      ((pred (eql pos)) nil)
      (it it))))

(defun isl-multi-search-fwd (str &optional _bound _noerror _count)
  "Return position of symbol or line matched by STR.
When arg STR contains spaces, it is converted in patterns with
`isl-patterns' , when first pattern of list match a symbol
subsequent patterns are used to check if all patterns match this
symbol.  The return value is a cons cell (beg . end) denoting
symbol or line position according to `isl-multi-search-in-line'."
  ;; Prevent infloop crashing Emacs with incorrect configuration.
  (cl-assert (memq isl-search-function '(isl--re-search-forward search-forward)))
  (let* ((pattern (isl-patterns str))
         (initial (or (assq 'identity pattern)
                      '(identity . "")))
         ;; In REST we search for all patterns but INITIAL we already
         ;; initially searched.  We were previously using (cdr
         ;; pattern) which was fine as long as first pattern was not a
         ;; negation.
         (rest    (remove initial pattern))
         (case-fold-search (isl-set-case-fold-search)))
    (cl-loop while (condition-case _err
                       (funcall isl-search-function (cdr initial) nil t)
                     (invalid-regexp
                      (setq isl--invalid t) nil))
             for bounds = (cond ((and rest isl-multi-search-in-line)
                                 (cons (pos-bol) (1+ (pos-eol))))
                                (rest
                                 (bounds-of-thing-at-point
                                  (if (and (derived-mode-p 'prog-mode)
                                           (not (nth 3 (syntax-ppss))))
                                      'symbol 'filename)))
                                (t (cons (match-beginning 0) (match-end 0))))
             unless bounds return nil
             if (or (not rest)
                    (cl-loop for (pred . re) in rest
                             always (funcall pred
                                             (progn
                                               (goto-char (car bounds))
                                               (condition-case _err
                                                   (funcall isl-search-function
                                                            re (cdr bounds) t)
                                                 (invalid-regexp
                                                  (setq isl--invalid t) nil))))))
             ;; When executing kbd macros, behave as the interactive
             ;; isl-search and leave point on last match in line
             ;; instead of jumping on eol.
             do (unless executing-kbd-macro (goto-char (cdr bounds)))
             and return bounds
             else do (goto-char (cdr bounds))
             finally return nil)))

(defun isl-toggle-multi-search-in-line ()
  "Toggle multi-search in lines/symbols."
  (interactive)
  (with-current-buffer isl--current-buffer
    (setq-local isl-multi-search-in-line (not isl-multi-search-in-line))
    (unless executing-kbd-macro
      (if isl-multi-search-in-line
          (set-face-attribute 'isl-on nil :extend t)
        (set-face-attribute 'isl-on nil :extend nil))
      (isl-update))))

(defun isl-maybe-update (str)
  "Decide starting to update according to STR length.
See `isl-requires-pattern'."
  (> (length str) isl-requires-pattern))

(defun isl-update ()
  "Update `current-buffer' when `isl--pattern' change."
  (with-selected-window (minibuffer-selected-window)
    (while-no-input
      (when isl--hidding
        (remove-overlays nil nil 'isl-invisible t)
        (remove-from-invisibility-spec '(isl-invisible . t))
        (setq isl--hidding nil))
      (isl-delete-overlays)
      (setq isl--invalid nil)
      ;; We don't use the isearch-invisible mechanism which is heavy
      ;; and don't behave as we want, instead remove invisibility in
      ;; all buffers and on exit restore it and unhide only the place
      ;; where point is with appropriate functions belonging to
      ;; major-mode e.g. org => org-reveal etc...
      (when (and isl-search-invisible
                 buffer-invisibility-spec
                 (listp buffer-invisibility-spec))
        (mapc 'remove-from-invisibility-spec buffer-invisibility-spec))
      (let ((count 1)
            (npos (point-max)) ; start comparing at highest pos [1].
            ov bounds go nearest)
        (unless (string= isl--pattern "")
          (save-excursion
            (goto-char (point-min))
            (condition-case-unless-debug nil
                (while (and (setq bounds (or go (isl-multi-search-fwd
                                                 isl--pattern nil t)))
                            (not (eobp)))
                  (setq go nil)
                  (unless (and (not isl-search-invisible)
                               (invisible-p (cdr bounds)))
                    (setq ov (make-overlay (car bounds)
                                           (if (= (car bounds) (cdr bounds))
                                               (1+ (cdr bounds))
                                             (cdr bounds))))
                    (push ov isl--item-overlays)
                    (when (= (car bounds) (cdr bounds))
                      (overlay-put ov 'line-prefix
                                   (propertize "$" 'face 'isl-match)))
                    (overlay-put ov 'isl t)
                    (overlay-put ov 'pos count)
                    ;; We could highlight later from window-start to
                    ;; window-end (lazy highlight) after this loop and
                    ;; also in isl--goto-overlay but the gain is
                    ;; insignifiant.
                    (overlay-put ov 'face 'isl-match)
                    (let* ((pos   (or isl--initial-pos 0))
                           (ovpos (car bounds))
                           (diff  (if (> pos ovpos)
                                      (- pos ovpos) (- ovpos pos))))
                      ;; [1] At first run, npos is (point-max) so diff is
                      ;; always smaller at this time.
                      (when (< diff npos) (setq nearest ov))
                      (setq npos (min diff npos)))
                    (when isl-multi-search-in-line
                      (isl--highlight-items-in-line (car bounds) (cdr bounds)))
                    (cl-incf count))
                  (when (= (car bounds) (cdr bounds))
                    (forward-line 1)
                    ;; If the line we are on is again an empty line
                    ;; `isl--re-search-forward' will be stuck on next
                    ;; turn of the loop so match it with looking-at
                    ;; and use GO value in next turn of the loop.
                    (when (looking-at isl--pattern)
                      (setq go (cons (pos-bol) (pos-eol))))))
              (invalid-regexp (setq isl--invalid t) nil))
            (setq isl--item-overlays (nreverse isl--item-overlays)))
          (if (null isl--item-overlays)
              (progn (setq isl--number-results 0)
                     (and isl--initial-pos (goto-char isl--initial-pos)))
            (setq isl--last-overlay    nearest ; Will change while moving.
                  isl--closest-overlay nearest ; Will change only on next update.
                  isl--number-results  (max (length isl--item-overlays) 0))
            (isl--highlight-last-overlay 'isl-on)
            (isl-make-or-update-iterator)
            (goto-char (overlay-end (isl-iter-next isl--iterator)))
            (setq isl--yank-point (point)))))
      (isl-setup-mode-line))))

(defun isl--highlight-items-in-line (beg end)
  "Highlight items inside a matched line from BEG to END."
  (let ((split (isl-split-string isl--pattern)))
    ;; Do nothing until we have a multi pattern.
    (when (cdr split)
      ;; When this is called we are at eol.
      (save-excursion
        (goto-char beg)
        (cl-loop with ov2
                 for p in split
                 unless (string-match "\\`!" p)
                 do (save-excursion
                      (while (funcall isl-search-function p end t)
                        (setq ov2 (make-overlay (match-beginning 0) (match-end 0)))
                        (overlay-put ov2 'face 'isl-match-items)
                        (overlay-put ov2 'isl t)
                        (overlay-put ov2 'isl-matches t)
                        (overlay-put ov2 'priority 1))))))))

(defun isl-setup-mode-line ()
  "Setup `mode-line-format' for `isl-search'."
  (let ((style (isl-matching-style))
        (search (if isl-multi-search-in-line
                    (propertize "Inline" 'help-echo "Search in line")
                  (propertize "Insym" 'help-echo "Search in symbol")))
        (position (with-current-buffer isl--current-buffer
                     (if (and isl--initial-pos
                              (> (point) isl--initial-pos))
                         isl-after-position-string
                       isl-before-position-string)))
        (direction (when isl--iterator
                     (if (eq (isl-iterator--direction isl--iterator) 'right)
                         isl-direction-down-string
                       isl-direction-up-string))))
    (when (numberp isl--number-results)
      (setq mode-line-format
            (cond ((or (string= isl--pattern "")
                       (<= (length isl--pattern)
                           isl-requires-pattern))
                   (default-value 'mode-line-format))
                  ((zerop isl--number-results)
                   `(" " mode-line-buffer-identification " "
                         (:eval ,(format "%s `%s' [%s %s %s %s]"
                                         (if isl--invalid
                                             (propertize
                                              (format "%s Invalid regexp:"
                                                      isl-warning-char)
                                              'face 'font-lock-warning-face)
                                           "No results found for pattern")
                                         (propertize isl--pattern
                                                     'face 'isl-string)
                                         style
                                         search
                                         (if isl-search-invisible
                                             (propertize
                                              " Sinv" 'help-echo
                                              "Search in invisible text")
                                           "")
                                         direction))
                         " " mode-line-position))
                  (t `(" " mode-line-buffer-identification " "
                           (:eval ,(format
                                    "[%s/%s] result(s) found [%s %s%s %s %s %s]"
                                    (propertize
                                     (number-to-string
                                      (overlay-get isl--last-overlay 'pos))
                                     'face 'isl-number)
                                    (propertize (number-to-string
                                                 isl--number-results)
                                                'face 'isl-number)
                                    style
                                    search
                                    (if isl-search-invisible
                                        (propertize
                                         " Sinv" 'help-echo
                                         "Search in invisible text")
                                      "")
                                    direction
                                    position
                                    (propertize (pcase isl-case-fold-search
                                                  ('smart "*")
                                                  ('t     "1")
                                                  ('nil   "0"))
                                                'face 'isl-case-fold
                                                'help-echo "case-fold-search")))
                           " " mode-line-position)))))))

(defun isl-check-input ()
  "Check minibuffer input."
  (with-selected-window (minibuffer-window)
    (let ((input (minibuffer-contents)))
      (when (not (string= input isl--pattern))
        (setq isl--pattern input)
        (if (and (stringp isl--pattern)
                 (isl-maybe-update isl--pattern))
            (isl-update)
          (with-selected-window (minibuffer-selected-window)
            (isl-delete-overlays)
            (isl-setup-mode-line)
            (goto-char isl--initial-pos)))))))

(defun isl-read-from-minibuffer (prompt &optional initial-input default)
  "Read input from minibuffer with prompt PROMPT.
Arguments INITIAL-INPUT and DEFAULT are same as in `read-from-minibuffer'."
  (let (timer)
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (setq timer (run-with-idle-timer
                           isl-timer-delay 'repeat #'isl-check-input)))
          (read-from-minibuffer
           prompt initial-input isl-map nil 'isl-history
           default))
      (cancel-timer timer))))

(defun isl-cleanup ()
  "Cleanup various things when `isl-search' exit."
  (with-current-buffer isl--current-buffer
    (let* ((pos (and isl--last-overlay ; nil when quitting with no results.
                     (overlay-end isl--last-overlay)))
           (hs-show-hook (list (lambda () (and pos (goto-char pos))))))
      (when (buffer-live-p (get-buffer isl-help-buffer-name))
        (kill-buffer isl-help-buffer-name))
      (setq isl--last-object
            `(lambda ()
               (setq-local mode-line-format ',mode-line-format
                           isl--last-query ,isl--pattern
                           isl--initial-pos ,pos
                           isl--point-min ,isl--point-min
                           isl--point-max ,isl--point-max
                           isl--narrow-to-region ,isl--narrow-to-region
                           isl--yank-point ,isl--yank-point
                           isl--number-results ,isl--number-results
                           isl-case-fold-search ',isl-case-fold-search
                           isl-search-function ',isl-search-function
                           buffer-invisibility-spec ',buffer-invisibility-spec
                           isl--hidding ,isl--hidding
                           isl-search-invisible ,isl-search-invisible
                           isl-multi-search-in-line ,isl-multi-search-in-line
                           cursor-in-non-selected-windows ',cursor-in-non-selected-windows)))
      (isl-delete-overlays)
      (setq mode-line-format (default-value 'mode-line-format)
            isl--yank-point nil
            isl--iterator nil
            isl--item-overlays nil
            isl--closest-overlay nil
            isl--last-overlay nil
            isl--number-results nil
            isl-case-fold-search (default-value 'isl-case-fold-search)
            isl-search-function (default-value 'isl-search-function)
            buffer-invisibility-spec isl--buffer-invisibility-spec
            isl--hidding nil
            isl-search-invisible (default-value 'isl-search-invisible)
            isl-multi-search-in-line (default-value 'isl-multi-search-in-line)
            cursor-in-non-selected-windows
            (default-value 'cursor-in-non-selected-windows))
      (remove-overlays nil nil 'isl-invisible t)
      (if isl--quit
          (setq isl--quit nil)
        (condition-case-unless-debug _err
            (cond ((eq major-mode 'org-mode)
                   (org-reveal))
                  ((eq major-mode 'outline-mode)
                   (outline-show-entry))
                  ((and (boundp 'hs-minor-mode)
                        hs-minor-mode)
                   (hs-show-block))
                  ((and (boundp 'markdown-mode-map)
                        (derived-mode-p 'markdown-mode))
                   (markdown-show-entry)))
          (error nil)))
      (when isl--window-start
        (set-window-start (selected-window) isl--window-start t)))))


(defun isl-search-1 (&optional resume input)
  "Start an isl session in `current-buffer'.
When RESUME is specified, resume the previous session in this buffer
and ignore INPUT argument if specified.
When INPUT is specified start a new isl session with input as initial
input argument in minibuffer.
Note that INPUT cannot be used with a non nil value for RESUME."
  (cl-assert (or (and resume (not input))
                 (and input (not resume))
                 (and (not input) (not resume)))
             nil "(isl-search-1): RESUME and INPUT can't be used together")
  (unless resume
    (setq isl--initial-pos (point)
          isl--pattern ""
          isl--current-buffer (current-buffer)
          isl--buffer-tick (buffer-modified-tick)
          isl--buffer-invisibility-spec buffer-invisibility-spec
          cursor-in-non-selected-windows 'bar))
  (setq isl--window-start nil)
  (when (and (buffer-live-p isl--current-buffer)
             (not (member (buffer-name isl--current-buffer)
                          isl-noresume-buffers)))
    (setq isl--visited-buffers
          (cons isl--current-buffer
                (delete isl--current-buffer isl--visited-buffers))))
  (when (memq major-mode isl-no-invisible-search-in-modes)
    (setq-local isl-search-invisible nil))
  (let* ((format-fn (if (eq isl-search-function 'search-forward)
                        #'identity #'regexp-quote))
         (default (if (region-active-p)
                      (buffer-substring-no-properties
                       (region-beginning)
                       (region-end))
                    (isl--thing-at-point)))
         blink-matching-paren)
    (when (stringp default)
      (setq default (funcall format-fn
                             ;; Prevent inserting multiline string in
                             ;; minibuf.
                             (if (string-match "\n" default)
                                 (or (isl--thing-at-point) "")
                               default))))
    (deactivate-mark)
    (unwind-protect
         (condition-case-unless-debug nil
             (isl-read-from-minibuffer
              "Search: "
              (if resume
                  (buffer-local-value
                   'isl--last-query isl--current-buffer)
                input)
              default)
           (quit
            (setq isl--quit t)
            (when isl--initial-pos
              (goto-char isl--initial-pos))))
      (isl-cleanup)
      ;; Avoid loosing focus in helm help buffer.
      (unless (eq (window-buffer (selected-window))
                  isl--current-buffer)
        (switch-to-buffer isl--current-buffer)))))

(defun isl--thing-at-point ()
  "Return symbol or filename at point."
  (thing-at-point
   (if (and (derived-mode-p 'prog-mode)
            (not (nth 3 (syntax-ppss))))
       'symbol 'filename)
   t))

(defvar isl-mini-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "M-r") 'isl-change-matching-style)
    (define-key map (kbd "M-c") 'isl-select-case-fold-search)
    (define-key map (kbd "C-j") 'isl-toggle-multi-search-in-line)
    map)
  "A minimal map used only in `isl--search-string'.
Use here only commands able to run inside kmacros.
Don't forget to modify `isl-map' accordingly to fit with kmacros when
modifying keybindings here.")

(defun isl--search-string ()
  "Search next match forward from point and stop.
This function is intended to be used in kmacros."
  (unwind-protect
       ;; Needed when toggling inline search.
       (let* ((isl--current-buffer (current-buffer))
              (str (read-from-minibuffer "Search: " nil isl-mini-map)))
         (isl-multi-search-fwd str nil t))
    ;; Reset local var to their default value for next iteration.
    (setq isl-multi-search-in-line (default-value 'isl-multi-search-in-line))
    (setq isl-search-function (default-value 'isl-search-function))
    (setq isl-case-fold-search (default-value 'isl-case-fold-search)
          isl--case-fold-choices-iterator nil)))

;;;###autoload
(defun isl-search ()
  "Start incremental searching in current buffer.
When used in kbd macros, search next match forward from point and
stop, assuming user starts its macro above the text to edit."
  (interactive)
  (if executing-kbd-macro
      (isl--search-string)
    (setq isl--point-min nil
          isl--point-max nil
          isl--narrow-to-region nil)
    (isl-search-1)))

;;;###autoload
(defun isl-resume (arg)
  "Resume isl session in current buffer.
With a prefix ARG choose one of the last buffers isl had visited."
  (interactive "P")
  (setq isl--current-buffer
        (cond ((and arg isl--visited-buffers)
               (get-buffer
                (completing-read
                 "Resume from buffer: "
                 (delq nil (mapcar 'buffer-name isl--visited-buffers))
                 nil t)))
              (isl--visited-buffers
               (car (memql (current-buffer)
                           isl--visited-buffers)))))
  (cl-assert isl--current-buffer
             nil "No previous Isl session yet recorded here")
  (switch-to-buffer isl--current-buffer)
  (let (beg end)
    (with-current-buffer isl--current-buffer
      (funcall isl--last-object)
      (setq isl--pattern "")
      ;; Last session was isl-narrow-to-defun.
      (setq beg isl--point-min end isl--point-max))
    (save-restriction
      (when (and beg end)
        (narrow-to-region beg end))
      (isl-search-1 'resume))))

;;;###autoload
(defun isl-narrow-to-region-or-defun (beg end)
  "Start incremental searching in region from BEG to END or current defun."
  (interactive "r")
  (setq isl--point-min nil
        isl--point-max nil
        isl--narrow-to-region nil)
  (save-restriction
    (if (and beg end (region-active-p))
        (progn
          (narrow-to-region beg end)
          (setq isl--narrow-to-region
                (buffer-substring (point-min) (point-max))))
      (narrow-to-defun))
    (setq isl--point-min (point-min)
          isl--point-max (point-max))
    (isl-search-1)))

;;;###autoload
(defalias 'isl-narrow-to-defun 'isl-narrow-to-region-or-defun)
;;;###autoload
(defalias 'isl-narrow-to-region 'isl-narrow-to-region-or-defun)


(provide 'isl)

;;; isl.el ends here