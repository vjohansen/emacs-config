
(add-to-list 'exec-path "c:/hunspell/bin/")

(setq ispell-program-name (locate-file "hunspell"
                            exec-path exec-suffixes 'file-executable-p))

;; Create a text file with a word per line: c:/hunspell/personal.en

(setq ispell-local-dictionary-alist
  '(("en_US" ;; was nil
       "[[:alpha:]]"
       "[^[:alpha:]]"
       "[']"
       t
       ("-d" "en_US" "-p" "c:\\hunspell\\personal.en")
       nil
       utf-8)
     ("da_DK"
       "[[:alpha:]ÆØÅæåø]"
       "[^[:alpha:]ÆØÅæåø]"
       "[']"
       t
       ("-d" "da_DK" "-p" "c:\\hunspell\\personal.da")
       nil
       utf-8)
     ("american"
       "[[:alpha:]]"
       "[^[:alpha:]]"
       "[']"
       t
       ("-d" "en_US" "-p" "c:\\hunspell\\personal.en")
       nil
       utf-8)
     ("deutsch"
       "[[:alpha:]ÄÖÜéäöüß]"
       "[^[:alpha:]ÄÖÜéäöüß]"
       "[']"
       t
       ("-d" "de_DE_frami" "-p"
         "c:\\hunspell\\personal.de")
       nil
       utf-8)
     ("francais"
       "[[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]"
       "[^[:alpha:]ÀÂÇÈÉÊËÎÏÔÙÛÜàâçèéêëîïôùûü]"
       "[-']"
       t
       ("-d" "fr-classique" "-p"
         "c:\\hunspell\\personal.fr")
       nil
       utf-8)
     ))

 (require 'ispell)

;; M-x ispell-change-dictionary
(setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist
  ispell-silently-savep t)


(provide 'vj-hunspell)
;; hting

;; (ispell-change-dictionary "da_DK" t)
;; (ispell-change-dictionary "en_US" t)


;; http://emacs.1067599.n8.nabble.com/bug-19229-ispell-phaf-No-matching-entry-for-td340912.html

;; (ispell-find-hunspell-dictionaries)