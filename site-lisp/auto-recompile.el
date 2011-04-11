;; -*- auto-recompile: t -*-
;;; auto-recompile.el --- Automatically recompile Emacs Lisp files

;; Copyright (C) 1999 by Michael Abraham Shulman

;; Emacs Lisp Archive Entry
;; Filename: auto-recomp.el
;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>
;; Version: 
;; Keywords: extensions, tools

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;; This file allows you to set up Emacs Lisp files to be automatically
;; byte-compiled whenever they are saved.  Mark such files by giving
;; them a local variable named `auto-recompile' set to a non-nil
;; value.  See the Emacs Manual for how to make file local variables.
;; Then, if this file is loaded, whenever such a file is saved it will
;; be byte-compiled.

;; Add this to your .emacs:
;; (require 'auto-recompile)
;; (add-hook 'emacs-lisp-mode-hook 'auto-recompile-add-after-save-hook)

;;; Code:

;;;###autoload
(defvar auto-recompile nil
  "Automatically byte-recompile this file whenever it is saved.")

(defun auto-recompile-file-maybe ()
  (when auto-recompile
    (byte-compile-file buffer-file-name)))

;;;###autoload
(defun auto-recompile-add-after-save-hook ()
  "Configure and add-after-save-hook"
  ;; not needed any more because add-hook makes it local now..
  ;;(make-local-hook 'after-save-hook)
  (make-variable-buffer-local 'auto-recompile)
  (add-hook 'after-save-hook 'auto-recompile-file-maybe nil t))


(provide 'auto-recompile)

;;; auto-recompile.el ends here
