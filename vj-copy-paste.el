


;; Fluxbox on unix
;; ---------------
;;
;; Mod4 is left windows on my setup
;;
;;   Mod4 c :ExecCommand ssh hostname "xclip -o > /home/vj/tempfile"


;; Windows with AutoHotKey
;; -----------------------
;;
;; file: c:/Documents and Settings/username/My Documents/AutoHotkey.ahk
;;
;; #c = RightWindowsKey-c
;;
;;   #c::
;;     FileDelete, h:\tempfile
;;     FileAppend, %clipboard%, h:\tempfile
;;   return
;;
;; Note: h: in windows is mapped via samba a server to the unix homedir so
;; that ~/ on unix is the same file system as h: on windows

;; Shell - ZSH
;;
;; In .zshrc (M-i t) will insert $(< ~/tempfile). To expand immediately press C-x *
;;
;;   bindkey -s "^[ic" ' > ~/tempfile'
;;   bindkey -s "^[it" '\$(< ~/tempfile)'


;; override this in the machine or sytem specific file as needed
(defvar vj-tempfile-name (if (equal system-type 'windows-nt)
                             "h:/tempfile" "~/tempfile"))


(defun my-insert-specific-file ()
   "My insert ~/tempfile in buffer (VJO 1997)."
   (interactive "")
   (insert-file-contents vj-tempfile-name)
)


(defun my-copy-region-to-file (beg end)
  "My copy region to a file called tempfile in my home-dir (VJO 1997)."
  (interactive "r")
  (if (file-exists-p vj-tempfile-name)
      (rename-file vj-tempfile-name (concat vj-tempfile-name "_") t))
  (write-region beg end vj-tempfile-name))

(global-set-key [S-f6] 'my-copy-region-to-file)
(global-set-key [C-S-f6] 'my-insert-specific-file)

(provide 'vj-copy-paste)
