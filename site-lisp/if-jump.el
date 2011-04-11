;;; if-jump.el -- Jump to matching #if/#else/#endif

;;; Copyright (C) 1995 Alexander Rezinsky <alexr@msil.sps.mot.com>
;;; $Id: if-jump.el,v 1.3 1995/06/06 12:38:23 alexr Exp $
;;;
;;; Author:   Alex Rezinsky <alexr@msil.sps.mot.com>
;;; Created:  2 Jule 1995
;;; Version:  1.0
;;; Keywords: c c++ preprocessor
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; COMMENTARY
;;; ----------
;;; This package gets to you ability to walk beetwen matching #if/#else/#endif
;;; C/C++ preprocessor directives (on same nesting level).
;;;
;;; In details may be following situation:
;;;
;;;     1. If  you stay on line contains  #if directive and want jump
;;; forward, you will jump to  line with #else/#elif/#endif directive
;;; which is matched to your #if.
;;;
;;;     2. If  you  stay on line  contains  #endif directive and want
;;; jump   backward, you will  jump to  line with #if/#elif directive
;;; which is matched to your #endif.
;;;
;;;     3. If you  stay  on line  contains #else/#elif directive  and
;;; want jump backward,  you will jump  to line with  #if/#else/#elif
;;; directive which is matched to your #else/#elif.
;;;
;;;     4.  If  you stay  on  line contains #else/#elif directive and
;;; want jump forward, you will  jump to line with #else/#elif/#endif
;;; directive which is matched to your #else/#elif.
;;;
;;;     5. If you stay on line which doesn't contain any #if/#else/#endif
;;; directive you will jump to closest such directive in your direction.
;;;
;;;     6. Otherwise you won't change current buffer position.
;;;

;;; INSTALLATION
;;; ------------
;;;   Put this file in your load-path and insert the following in .emacs
;;;
;;; (require 'if-jump)
;;;
;;;   and bind two functions (jump  forward and  jump backward) to your
;;;   favorite keys, something like this:
;;;
;;; (global-set-key [M-up]   '(lambda() (interactive) (if-jump-jump 'backward)))
;;; (global-set-key [M-down] '(lambda() (interactive) (if-jump-jump 'forward)))

;;; KNOWN BUGS
;;; ----------
;;; This package doesn't recognize very smart or tricky C/C++ syntax.
;;; For example it may be confused by something like this:
;;; 
;;; /* ha-ha */ # /* My comment */ ifdef MY_STYLE_MUST_BE_IMPROVED
;;;     or
;;; char *this_is_real_case = "String \
;;; #ifdef SOMETHING_STRANGE";
;;; 
;;; I hope that such style it isn't usual practice ...

;;; LCD Archive Entry:
;;; if-jump|Alex Rezinsky|alexr@msil.sps.mot.com|
;;; Jump to matching #if/#else/#endif C-preprocessor directive|
;;; 02-Jul-1992|1.0|~/misc/if-jump.el.gz|

;;; HISTORY
;;; -------
;;; v1.0 June 2 1995 Alex Rezinsky
;;;   First release.

;;; Variables for customization
;;; ---------------------------
;;;
(defvar if-jump-if-regexp    "^[ \t]*#[ \t]*if"
  "Regular expression matching #if C/C++ preprocessor directive.")
(defvar if-jump-else-regexp  "^[ \t]*#[ \t]*el\\(se\\|if\\)"
  "Regular expression matching #else/#elif C/C++ preprocessor directive.")
(defvar if-jump-endif-regexp "^[ \t]*#[ \t]*endif"
  "Regular expression matching #endif C/C++ preprocessor directive.")

;;; Code, nothing to customize below here
;;; -------------------------------------
;;;
(defvar if-jump-exps
  (list
    (list if-jump-if-regexp    'if)    ; #if/#ifdef/#ifndef
    (list if-jump-else-regexp  'else)  ; #else/#elif
    (list if-jump-endif-regexp 'endif) ; #endif
  )
)
(defvar if-jump-something
  (concat
   "\\(" if-jump-if-regexp    "\\)\\|"
   "\\(" if-jump-else-regexp  "\\)\\|"
   "\\(" if-jump-endif-regexp "\\)"
  )
  "Regular expression matching any (interesting for if-jump
package) C/C++ preprocessor directive."
)

(defun if-jump-jump (direction)
  "Jump to next/previous #if/#else/#endif on same nesting level.
DIRECTION may be  'forward or 'backward."
  (interactive nil)

  ; Argument checking
  (if (and (not (equal direction 'forward)) (not (equal direction 'backward)))
      (error "if-jump-jump: direction must be or 'forward or 'backward")
  )

  (let ((current-line (if-jump-what-is)) (saved-pnt (point)))
    (if current-line
      (progn            ; Current line is a CPP directive - find match
        (if (and (equal current-line 'if) (equal direction 'backward))
          (error "From #if you can jump only forward.")
        )
        (if (and (equal current-line 'endif) (equal direction 'forward))
          (error "From #endif you can jump only backward.")
        )
        (let ((new (if-jump-find direction)))
          (if new
            (progn
              (push-mark saved-pnt)
              (goto-char new)
              (beginning-of-line)
            )
            (goto-char saved-pnt)
          )
        )
      )
      (progn            ; Current line isn't a CPP directive - find closest
        (if (setq current-line (if-jump-find-closest direction))
          (progn
            (push-mark saved-pnt)
            (beginning-of-line)
          )
          (goto-char saved-pnt)
        )
      )
    )
  )
)

(defun if-jump-find (direction)
  "Find next/previous preprocessor directive (on same nesting level).
Returns point if found or nil if not found."
  (let ((nest 0) (type (if-jump-find-closest direction)) (found nil))
    (while (and type (not found))
      (cond
       ((equal direction 'forward)
        (if (and (or (equal type 'else) (equal type 'endif)) (= nest 0))
            (setq found t)
          (cond
           ((equal type 'if)
            (setq nest (1+ nest))
           )
           ((equal type 'endif)
            (setq nest (1- nest))
           )
          )
        )
       )
       ((equal direction 'backward)
        (if (and (or (equal type 'else) (equal type 'if)) (= nest 0))
            (setq found t)
          (cond
           ((equal type 'if)
            (setq nest (1- nest))
           )
           ((equal type 'endif)
            (setq nest (1+ nest))
           )
          )
        )
       )
      )
      (if (not found) (setq type (if-jump-find-closest direction)))
    )
    (if found
        (point)
      (error "Mathing directive not found")
      nil
    )
  )
)

(defun if-jump-find-closest (direction)
  "Find closest preprocessor directive (regardless nesting).
Returns its type ('if, 'else or 'endif) if found and nil if not found."
  (cond
   ((equal direction 'forward)
    (end-of-line)
    (if (re-search-forward if-jump-something nil t)
        (if-jump-what-is)
      nil
    )
   )
   ((equal direction 'backward)
    (beginning-of-line)
    (if (re-search-backward if-jump-something nil t)
        (if-jump-what-is)
      nil
    )
   )
  )
)

(defun if-jump-what-is ()
  "Determinate what is in the current line. If current line is a
preprocessor directive returns its type ('if, 'else or 'endif). If
current line isn't a preprocessor directive returns nil."
  (save-excursion
    (beginning-of-line)
    (let ((exps if-jump-exps) (cur-exp (car if-jump-exps)) (ret nil))
      (while cur-exp
        (if (looking-at (car cur-exp))
          (progn
            (setq ret (car (cdr cur-exp)))
            (setq cur-exp nil)
          )
          (progn
            (setq exps (cdr exps))
            (setq cur-exp (car exps))
          )
        )
      )
      ret
    )
  )
)

(provide 'if-jump)

;; if-jump ends here
