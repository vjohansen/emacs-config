;To: unix-emacs@bbn.com
;Date: 18 Nov 88 10:55:11 GMT
;From: MIT.EDU!@EDDIE
;Subject: Fn. to line up assigment ops.
;
;The following little bit of lisp will ensure the first assignment operators
;on each of the lines line up. This is part of our local formatting style
;'cos it looks nice ;-)
;
;The style of the lisp however, is atrocious. All the problems come from ==,
;which looks too much like 'op='.
;
;Enjoy.
;
;Paul Hudson 
;
;Snail mail: Monotype ADG	Email:	...!ukc!acorn!moncam!paul
;	    Science Park,		paul@moncam.co.uk
;	    Milton Road,	"Sun Microsysytems:
;	    Cambridge,		 The Company is Arrogant (TM)"
;	    CB4 4FQ

(defun align-equals (start end)
 "make the first assignment operator on each line line up vertically"
 (interactive "*r")
 (save-excursion
   (let ((indent 0))
     (narrow-to-region start end)
     (beginning-of-buffer)
     (while (not (eobp))
       (if (find-assignment)
	   (progn
	     (exchange-point-and-mark)
	     (setq indent (max indent (current-column)))
	     (delete-horizontal-space)
	     (insert " ")))
       (forward-line 1))
     (beginning-of-buffer)
     (while (not (eobp))
       (if (find-assignment)
	   (indent-to-column (1+ (- indent  (- (mark) (point))))))
       (forward-line 1)))
   (widen)))


(defun find-assignment ()
  (if (re-search-forward
	     "[^<>=!]=\\|\\+=\\|-=\\|\\*=\\|/=\\|&=\\||=\\|\\^=\\|<<=\\|>>="
	     (save-excursion (end-of-line) (point)) t)
      (progn
	(goto-char (match-beginning 0))
	(if (looking-at ".==")
	    nil
	  (if (looking-at "\\+=\\|-=\\|\\*=\\|/=\\|&=\\||=\\|\\^=\\|<<=\\|>>=")
	      (set-mark (match-end 0))
	    (forward-char 1)
	    (set-mark (1+ (point))))
	  (delete-horizontal-space)
	  t))
    nil))



       
;     
;Paul Hudson 
;
;Snail mail: Monotype ADG	Email:	...!ukc!acorn!moncam!paul
;	    Science Park,		paul@moncam.co.uk
;	    Milton Road,	"Sun Microsysytems:
;	    Cambridge,		 The Company is Arrogant (TM)"
;	    CB4 4FQ
  
