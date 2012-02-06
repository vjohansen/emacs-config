;;; bintris.el --- implementation of Bintris for Emacs
;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Author: Janne Hellsten <jjhellst@gmail.com>
;; Version 1.0
;; Created 2007-06-07
;; Keywords: games

;; Bintris game concept by Juha Lainema.  He implemented the original
;; version as a Win32 application.  See bintris doc string for a
;; playing instructions.
;;
;; The Emacs Lisp implementation of Bintris is based on tetris.el by
;; Glynn Clements.  This file is included in the Emacs distribution.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'gamegrid)

;; ;;;;;;;;;;;;; customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup bintris nil
  "Play a game of bintris."
  :prefix "bintris-"
  :group 'games)

(defcustom bintris-use-glyphs t
  "*Non-nil means use glyphs when available."
  :group 'bintris
  :type 'boolean)

(defcustom bintris-use-color t
  "*Non-nil means use color when available."
  :group 'bintris
  :type 'boolean)

(defcustom bintris-draw-border-with-glyphs t
  "*Non-nil means draw a border even when using glyphs."
  :group 'bintris
  :type 'boolean)

(defcustom bintris-default-tick-period 0.4
  "*The default time taken for a shape to drop one row."
  :group 'bintris
  :type 'number)

(defcustom bintris-mode-hook nil
  "Hook run upon starting Bintris."
  :group 'bintris
  :type 'hook)

(defcustom bintris-tty-colors
  [nil "black" "white"]
  "Vector of colors of the various shapes in text mode
Element 0 is ignored."
  :group 'bintris
  :type (let ((names `("Zero" "One"))
              (result `(vector (const nil))))
          (while names
            (add-to-list 'result
                         (cons 'choice
                               (cons :tag
                                     (cons (car names)
                                           (mapcar (lambda (color)
                                                     (list 'const color))
                                                   (defined-colors)))))
                         t)
            (setq names (cdr names)))
          result))

(defcustom bintris-x-colors
  [nil [0 0 0] [1 1 1]]
  "Vector of colors of the zero and one blocks.
Element 0 is ignored."
  :group 'bintris
  :type 'sexp)

(defcustom bintris-buffer-name "*Bintris*"
  "Name used for Bintris buffer."
  :group 'bintris
  :type 'string)

(defcustom bintris-start-level 3
  "Bit width to use as the starting game level."
  :group 'bintris
  :type 'number)

(defcustom bintris-num-visible-targets 3
  "How many decimal targets to show."
  :group 'bintris
  :type 'number)

(defcustom bintris-buffer-width 40
  "Width of used portion of buffer."
  :group 'bintris
  :type 'number)

(defcustom bintris-buffer-height 22
  "Height of used portion of buffer."
  :group 'bintris
  :type 'number)

(defcustom bintris-height 16
  "Height of playing area."
  :group 'bintris
  :type 'number)

(defcustom bintris-top-left-x 3
  "X position of top left of playing area."
  :group 'bintris
  :type 'number)

(defcustom bintris-top-left-y 1
  "Y position of top left of playing area."
  :group 'bintris
  :type 'number)

(defvar bintris-next-x (+ (* 2 bintris-top-left-x) bintris-start-level)
  "X position of next shape.")

(defvar bintris-next-y bintris-top-left-y
  "Y position of next shape.")

(defvar bintris-score-x bintris-next-x
  "X position of score.")

(defvar bintris-score-y (+ bintris-next-y 6)
  "Y position of score.")

;; It is not safe to put this in /tmp.
;; Someone could make a symlink in /tmp
;; pointing to a file you don't want to clobber.
(defvar bintris-score-file "bintris-scores"
  "File for holding high scores.")

;; ;;;;;;;;;;;;; display options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bintris-blank-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0.6 0.7 0.6])
     (color-tty "gray"))))

(defvar bintris-cell-options
  '(((glyph colorize)
     (emacs-tty ?O)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    ;; color information is taken from bintris-x-colors and bintris-tty-colors
    ))

(defvar bintris-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty "white"))))

(defvar bintris-space-options
  '(((t ?\040))
    nil
    nil))

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst bintris-blank 0)

(defconst bintris-border 8)

(defconst bintris-space 9)

;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar bintris-bit-width bintris-start-level)
(defvar bintris-num-extra-targets 0)
;; Current decimal number(s) that we try to match our row against
(defvar bintris-cur-decimal-targets '())

(defvar bintris-shape 0)
(defvar bintris-next-shape 0)
(defvar bintris-n-shapes 0)
(defvar bintris-n-rows 0)
(defvar bintris-score 0)
(defvar bintris-pos-x 0)
(defvar bintris-pos-y 0)
(defvar bintris-paused nil)

(make-variable-buffer-local 'bintris-bit-width)
(make-variable-buffer-local 'bintris-num-extra-targets)
(make-variable-buffer-local 'bintris-cur-decimal-targets)
(make-variable-buffer-local 'bintris-shape)
(make-variable-buffer-local 'bintris-next-shape)
(make-variable-buffer-local 'bintris-n-shapes)
(make-variable-buffer-local 'bintris-n-rows)
(make-variable-buffer-local 'bintris-score)
(make-variable-buffer-local 'bintris-pos-x)
(make-variable-buffer-local 'bintris-pos-y)
(make-variable-buffer-local 'bintris-paused)

;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bintris-mode-map
  (make-sparse-keymap 'bintris-mode-map))

(define-key bintris-mode-map "n" 'bintris-start-game)
(define-key bintris-mode-map "q" 'bintris-end-game)
(define-key bintris-mode-map "p"  'bintris-pause-game)

;; TODO remove before release, testing only!
(define-key bintris-mode-map "l"  'bintris-level-up)

(define-key bintris-mode-map " " 'bintris-move-bottom)
(define-key bintris-mode-map [left]     'bintris-move-left)
(define-key bintris-mode-map [right]    'bintris-move-right)

(defvar bintris-null-map
  (make-sparse-keymap 'bintris-null-map))

(define-key bintris-null-map "n" 'bintris-start-game)

;; ;;;;;;;;;;;;;;;; game functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ++ (var)
   (list 'setq var (list '1+ var)))

(defmacro -- (var)
   (list 'setq var (list '1- var)))

(defun bintris-new-decimal-targets ()
  (setq bintris-cur-decimal-targets nil)
  (dotimes (n (+ bintris-bit-width 3))
    (setq bintris-cur-decimal-targets
          (cons (random (lsh 1 bintris-bit-width))
                bintris-cur-decimal-targets)))
  (setq bintris-num-extra-targets (logxor bintris-num-extra-targets 1)))

;; Does a number match the current decimal target?
(defun bintris-in-targets-p (n)
  (let ((r nil))
    (if bintris-cur-decimal-targets
        (= (car bintris-cur-decimal-targets) n)
      nil)))
    
(defun bintris-display-options ()
  (let ((options (make-vector 256 nil)))
    (loop for c from 0 to 255 do
      (aset options c
            (cond ((= c bintris-blank)
                    bintris-blank-options)
                  ((and (>= c 1) (<= c 2))
                   (append
                    bintris-cell-options
                    `((((glyph color-x) ,(aref bintris-x-colors c))
                       (color-tty ,(aref bintris-tty-colors c))
                       (t nil)))))
                   ((= c bintris-border)
                    bintris-border-options)
                   ((= c bintris-space)
                    bintris-space-options)
                  (t
                   '(nil nil nil)))))
    options))

(defun bintris-get-tick-period ()
  (if (boundp 'bintris-update-speed-function)
      (let ((period (apply bintris-update-speed-function
                           bintris-n-shapes
                           bintris-n-rows nil)))
        (and (numberp period) period))))

(defun bintris-get-shape-cell ()
  (1+ bintris-shape))

(defun bintris-string-decimal-target ()
  (if bintris-cur-decimal-targets
      (let ((r (format "%d - " (car bintris-cur-decimal-targets)))
            (targets (cdr bintris-cur-decimal-targets)))
        (if (> (length targets) bintris-num-visible-targets)
            (progn
              (dotimes (n bintris-num-visible-targets)
                (setq r (concat r (format "%d " (car targets))))
                (setq targets (cdr targets)))
              (setq r (concat r " ...")))
          (progn 
            (setq r (concat r (mapconcat '(lambda (x) (format "%d" x)) targets " ")))))
        r)
    ""))

(defun bintris-draw-score ()
  ;; Note: space after %s is for clearing the screen
  (let ((strings (vector (format "Target: %s   " 
                                 (bintris-string-decimal-target))
                         (format "Bits:   %d" bintris-n-shapes)
                         (format "Rows:   %d" bintris-n-rows)
                         (format "Score:  %d" bintris-score))))
    (loop for y from 0 to 3 do
          (let* ((string (aref strings y))
                 (len (length string)))
            ;; Clear background first
            (loop for x from 0 to (- bintris-buffer-width 1 bintris-score-x) do
                  (gamegrid-set-cell (+ bintris-score-x x)
                                     (+ bintris-score-y y)
                                     bintris-space))
            (loop for x from 0 to (1- len) do
                  (let ((xx (+ bintris-score-x x)))
                  (if (< xx bintris-buffer-width)
                      (gamegrid-set-cell xx
                                         (+ bintris-score-y y)
                                         (aref string x)))))))))

(defun bintris-update-score ()
  (bintris-draw-score)
  (let ((period (bintris-get-tick-period)))
    (if period (gamegrid-set-timer period))))

(defun bintris-new-shape ()
  (setq bintris-shape bintris-next-shape)
  (setq bintris-next-shape (random 2))
  (setq bintris-pos-x (/ (- bintris-bit-width 1) 2))
  (setq bintris-pos-y 0)
  (if (bintris-test-shape)
      (bintris-end-game)
    (bintris-draw-shape)
    (bintris-draw-next-shape)
    (bintris-update-score)))

(defun bintris-draw-next-shape ()
  (gamegrid-set-cell bintris-next-x
                     bintris-next-y
                     (let ((bintris-shape bintris-next-shape))
                       (bintris-get-shape-cell))))

(defun bintris-draw-shape ()
  (let ((c (bintris-get-shape-cell)))
    (gamegrid-set-cell (+ bintris-top-left-x bintris-pos-x)
                       (+ bintris-top-left-y bintris-pos-y)
                       c)))

(defun bintris-erase-shape ()
  (let ((px (+ bintris-top-left-x bintris-pos-x))
        (py (+ bintris-top-left-y bintris-pos-y)))
    (gamegrid-set-cell px py bintris-blank)))

;; TODO simplify for 1x1 blocks
(defun bintris-test-shape ()
  (let* ((px (+ bintris-top-left-x bintris-pos-x))
         (py (+ bintris-top-left-y bintris-pos-y)))
    (or (>= bintris-pos-x bintris-bit-width)
        (>= bintris-pos-y bintris-height)
        (/= (gamegrid-get-cell px py) bintris-blank))))

;; Turn row of bits into a number
(defun bintris-row-to-number (y)
  (let ((full t)
        (v 0))
    (loop for x from 0 to (1- bintris-bit-width) do
          (let ((grid-val (gamegrid-get-cell (+ bintris-top-left-x x)
                                             (+ bintris-top-left-y y))))
            (if (= grid-val bintris-blank)
                (setq full nil)
              (setq v (logior (lsh v 1) (1- grid-val))))))
    (list full v)))

;; A row is full if it matches any of the current "decimal targets"
(defun bintris-full-row (y)
  (let ((row-status (bintris-row-to-number y)))
    (if (car row-status)
        (if (bintris-in-targets-p (cadr row-status))
            (cadr row-status)
          nil)
      nil)))

(defun bintris-shift-row (y)
  (if (= y 0)
      (loop for x from 0 to (1- bintris-bit-width) do
            (gamegrid-set-cell (+ bintris-top-left-x x)
                               (+ bintris-top-left-y y)
                               bintris-blank))
    (loop for x from 0 to (1- bintris-bit-width) do
          (let ((c (gamegrid-get-cell (+ bintris-top-left-x x)
                                      (+ bintris-top-left-y y -1))))
            (gamegrid-set-cell (+ bintris-top-left-x x)
                               (+ bintris-top-left-y y)
                               c)))))

;; Collapse rows
(defun bintris-shift-down ()
  ;; If rows get collapsed and bintris-new-decimal-targets gets
  ;; called, the board may now contain numbers that were introduced by
  ;; bintris-new-decimal-targets.  In this case those rows don't get
  ;; collapsed, but they will be collapsed once the current shape is
  ;; done.  We needed to iterate collapse/new-targets until we reach
  ;; fixpoint.
  (let ((changed t))
    (while changed
      (let ((collapsed nil))
        (loop for y0 from 0 to (1- bintris-height) do
              (let ((full-row-num (bintris-full-row y0)))
                (if full-row-num
                    (progn 
                      ;; TODO scoring rules -- must get higher score in higher
                      ;; bit widths
                      (setq collapsed t)
                      (++ bintris-score)
                      (++ bintris-n-rows)
                      (loop for y from y0 downto 0 do
                            (bintris-shift-row y))))))
        (setq changed nil)
        (if collapsed
            (progn
              (setq changed t)
              (setq bintris-cur-decimal-targets (cdr bintris-cur-decimal-targets))))))))

(defun bintris-draw-border-p ()
  (or (not (eq gamegrid-display-mode 'glyph))
      bintris-draw-border-with-glyphs))

(defun bintris-draw-buffer ()
  (let ((buffer-read-only nil))
    (if (bintris-draw-border-p)
        (loop for y from -1 to bintris-height do
              (loop for x from -1 to bintris-bit-width do
                    (gamegrid-set-cell (+ bintris-top-left-x x)
                                       (+ bintris-top-left-y y)
                                       bintris-border))))
    (loop for y from 0 to (1- bintris-height) do
          (loop for x from 0 to (1- bintris-bit-width) do
                (gamegrid-set-cell (+ bintris-top-left-x x)
                                   (+ bintris-top-left-y y)
                                   bintris-blank)))
    (if (bintris-draw-border-p)
        (loop for y from -1 to 1 do
              (loop for x from -1 to 1 do
                    (gamegrid-set-cell (+ bintris-next-x x)
                                       (+ bintris-next-y y)
                                       bintris-border))))))

(defun bintris-reset-level ()
  ;; Clear the board completely
  (loop for y from 0 to (1- bintris-buffer-height) do
        (loop for x from 0 to (1- bintris-buffer-width) do
              (gamegrid-set-cell x y bintris-space)))

  (setq bintris-next-x (+ (* 2 bintris-top-left-x) bintris-bit-width))
  (setq bintris-score-x bintris-next-x)
  (setq bintris-shape 0
        bintris-pos-x 0
        bintris-pos-y 0)
  (bintris-new-decimal-targets)
  (bintris-draw-buffer)
  (bintris-new-shape))

(defun bintris-reset-game ()
  (setq bintris-score 0
        bintris-n-rows 0
        bintris-paused nil
        bintris-n-shapes 0)

  (gamegrid-init-buffer bintris-buffer-width
                        bintris-buffer-height
                        bintris-space)

  (gamegrid-kill-timer)

  (bintris-reset-level)

  (let ((period (or (bintris-get-tick-period)
                    bintris-default-tick-period)))
    (gamegrid-start-timer period 'bintris-update-game)))

(defun bintris-level-up ()
  (interactive)
  (++ bintris-bit-width)
  (bintris-reset-level))
  

(defun bintris-shape-done ()
  (bintris-shift-down)
  (++ bintris-n-shapes)
  (bintris-update-score)
  (bintris-new-shape)
  ;; Time to level up?
  (unless bintris-cur-decimal-targets
    (++ bintris-bit-width)
    (bintris-reset-level)))

(defun bintris-update-game (bintris-buffer)
  "Called on each clock tick.
Drops the shape one square, testing for collision."
  (if (and (not bintris-paused)
           (eq (current-buffer) bintris-buffer))
      (let (hit)
        (bintris-erase-shape)
        (++ bintris-pos-y)
        (setq hit (bintris-test-shape))
        (if hit
            (-- bintris-pos-y))
        (bintris-draw-shape)
        (if hit
            (bintris-shape-done)))))

(defun bintris-move-bottom ()
  "Drops the shape to the bottom of the playing area"
  (interactive)
  (unless bintris-paused
    (let ((hit nil))
      (bintris-erase-shape)
      (while (not hit)
        (++ bintris-pos-y)
        (setq hit (bintris-test-shape)))
      (-- bintris-pos-y)
      (bintris-draw-shape)
      (bintris-shape-done))))

(defun bintris-move-left ()
  "Moves the shape one square to the left"
  (interactive)
  (unless (or (= bintris-pos-x 0)
              bintris-paused)
    (bintris-erase-shape)
    (-- bintris-pos-x)
    (if (bintris-test-shape)
        (++ bintris-pos-x))
    (bintris-draw-shape)))

(defun bintris-move-right ()
  "Moves the shape one square to the right"
  (interactive)
  (unless (or (= (+ bintris-pos-x 1)
                 bintris-bit-width)
              bintris-paused)
    (bintris-erase-shape)
    (setq bintris-pos-x (1+ bintris-pos-x))
    (if (bintris-test-shape)
        (-- bintris-pos-x))
    (bintris-draw-shape)))

(defun bintris-end-game ()
  "Terminates the current game"
  (interactive)
  (gamegrid-kill-timer)
  (use-local-map bintris-null-map)
  (gamegrid-add-score bintris-score-file bintris-score))

(defun bintris-start-game ()
  "Starts a new game of Bintris"
  (interactive)
  (bintris-reset-game)
  (use-local-map bintris-mode-map))

(defun bintris-pause-game ()
  "Pauses (or resumes) the current game"
  (interactive)
  (setq bintris-paused (not bintris-paused))
  (message (and bintris-paused "Game paused (press p to resume)")))

(defun bintris-active-p ()
  (eq (current-local-map) bintris-mode-map))

(put 'bintris-mode 'mode-class 'special)

(defun bintris-mode ()
  "A mode for playing Bintris.

bintris-mode keybindings:
   \\{bintris-mode-map}
"
  (kill-all-local-variables)

  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)

  (use-local-map bintris-null-map)

  (setq major-mode 'bintris-mode)
  (setq mode-name "Bintris")

  (unless (featurep 'emacs)
    (setq mode-popup-menu
          '("Bintris Commands"
            ["Start new game"   bintris-start-game]
            ["End game"         bintris-end-game
             (bintris-active-p)]
            ["Pause"            bintris-pause-game
             (and (bintris-active-p) (not bintris-paused))]
            ["Resume"           bintris-pause-game
             (and (bintris-active-p) bintris-paused)])))

  (setq gamegrid-use-glyphs bintris-use-glyphs)
  (setq gamegrid-use-color bintris-use-color)

  (gamegrid-init (bintris-display-options)))
;;  (run-mode-hooks 'bintris-mode-hook))

;;;###autoload
(defun bintris ()
  "Play the Bintris game.
Bits (0=black or 1=white) drop from the top of the screen and the
user has to move the bits to form rows that when interpreted as a
bit string evaluate to the current target number.  The target
number is represented in decimal base and is displayed on the
right side of the board.

bintris-mode keybindings:
   \\<bintris-mode-map>
\\[bintris-start-game]  Starts a new game of Bintris
\\[bintris-end-game]    Terminates the current game
\\[bintris-pause-game]  Pauses (or resumes) the current game
\\[bintris-move-left]   Moves the shape one square to the left
\\[bintris-move-right]  Moves the shape one square to the right
\\[bintris-move-bottom] Drops the shape to the bottom of the playing area

"
  (interactive)

  ;; TODO remove before release
  (setq debug-on-error t)

  (switch-to-buffer bintris-buffer-name)
  (gamegrid-kill-timer)
  (bintris-mode)
  (bintris-start-game))

(random t)

(provide 'bintris)
