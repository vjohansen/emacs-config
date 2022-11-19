;;; da-kalender.el --- Danish calendar for Emacs -*-coding:utf-8-*-

;; Copyright (c) 2018 Søren Lund <soren@lund.org>

;; Author: Søren Lund <soren@lund.org>
;; Version: 1.2
;; Keywords: calendar danish localization

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Danish calendar localization. This is my version of the swedish
;; localization, sv-kalender.el, by Daniel Jensen - see
;; http://bigwalter.net/daniel/elisp/sv-kalender.el

;; This file will modify how Emacs displays its calendar. The names of
;; months, days, etc. have been replaced with danish
;; translations. Furthermore the default american holidays are
;; replaced by danish holidays.

;; To use it, save this file somewhere in your load-path and add (load
;; "da-kalender") to your ~/.emacs file.

;;; History

;; 1.0 - Initial release (Mar 25th, 2010)
;; 1.1 - Added week numbers (Feb 13th, 2014)
;; 1.2 - Fixed to work with newer version of Emacs (Sep 24th, 2018)

;;; Code:

;; Week starts on monday
(setq calendar-week-start-day 1)

;; Use european date style, i.e. date/month
(setq european-calendar-style 'european)

;; Date format
(setq calendar-date-display-form
      '((if dayname
            (concat dayname ", "))
        day " " monthname " " year))

;; 24 hour clock format
(setq calendar-time-display-form
      '(24-hours ":" minutes))

;; Weekday names
(setq calendar-day-name-array
      ["søndag" "mandag" "tirsdag" "onsdag" "torsdag" "fredag" "lørdag"])
(setq calendar-day-abbrev-array
      ["søn" "man" "tirs" "ons" "tors" "fre" "lør"])
(setq calendar-day-header-array
      ["sø" "ma" "ti" "on" "to" "fr" "lø"])

;; Month names
(setq calendar-month-name-array
      ["januar" "februar" "marts" "april" "maj" "juni"
       "juli" "august" "september" "oktober" "november" "december"])
(setq calendar-month-abbrev-array
      ["jan" "feb" "mar" "apr" "maj" "jun" "jul" "aug" "sep" "okt" "nov" "dec"])

:; Equinoxes/solstices names
;; http://www.dmi.dk/dmi/nyd_aarets_laengste_dag_-_sommersolhverv
(eval-after-load "solar"
  '(progn
     (setq solar-n-hemi-seasons
	   '("Forårsjævndøgn" "Sommersolhverv"
	     "Efterårsjævndøgn" "Vintersolhverv"))
     (setq solar-s-hemi-seasons
	   '("Efterårsjævndøgn" "Vintersolhverv"
	     "Forårsjævndøgn" "Sommersolhverv"))))

;; Moon phace names
(defadvice lunar-phase-name (around da-lunar-phase-name activate)
  "Phases of the moon in danish."
  (setq ad-return-value
	(let ((phase (ad-get-arg 0)))
	  (cond ((= 0 phase) "Nymåne")
		((= 1 phase) "Tiltagende Halvmåne")
		((= 2 phase) "Fuldmåne")
		((= 3 phase) "Aftagende Halvmåne")))))


;; Sunrise and sunset
(defadvice solar-sunrise-sunset-string (around da-solar-sunrise-sunset-string
                                               activate)
  "Sunrise and sunset in danish."
  (setq ad-return-value
        (let ((l (solar-sunrise-sunset date)))
          (format
           "%s, %s i %s (%s timers dagslys)"
           (if (car l)
               (concat "Sol op " (apply 'solar-time-string (car l)))
             "Ingen solopgang")
           (if (car (cdr l))
               (concat "ned " (apply 'solar-time-string (car (cdr l))))
       "ingen solnedgang")
           (eval calendar-location-name)
           (car (cdr (cdr l)))))))


;; Show week number
;; Adapted from http://stackoverflow.com/questions/21364948/how-to-align-the-calendar-with-week-number-as-the-intermonth-text
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-doc-face))

;; Week numbers are very common and there's no two-letter abreviation for it,
;; Thus I'm not added a header. It could be added using this (ug=ugenummer):
;;
;; (setq calendar-intermonth-header nil)
;;       (propertize "un" 'font-lock-face 'font-lock-keyword-face))


;; Calculation of easter, the fix point for many holidays (taken from
;; sv-kalender.el, originally from holiday-easter-etc)
(defun da-easter (year)
  "Calculate the date for Easter in YEAR."
  (let* ((century (1+ (/ year 100)))
         (shifted-epact (% (+ 14 (* 11 (% year 19))
                              (- (/ (* 3 century) 4))
                              (/ (+ 5 (* 8 century)) 25)
                              (* 30 century))
                           30))
         (adjusted-epact (if (or (= shifted-epact 0)
                                 (and (= shifted-epact 1)
                                      (< 10 (% year 19))))
                             (1+ shifted-epact)
                           shifted-epact))
         (paschal-moon (- (calendar-absolute-from-gregorian
                           (list 4 19 year))
                          adjusted-epact)))
    (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))

(setq general-holidays
      '((holiday-fixed 1 1 "Nytårsdag")
	(holiday-fixed 1 6 "Hellige 3 konger")

	;; Easter and Pentecost
	(holiday-filter-visible-calendar
	 (mapcar
	  (lambda (dag)
	    (list (calendar-gregorian-from-absolute
		   (+ (da-easter displayed-year) (car dag)))
		  (cadr dag)))
	  '(( -49 "Fastelavn")
	    (  -7 "Palmesøndag")
	    (  -3 "Skærtorsdag")
	    (  -2 "Langfredag")
	    (   0 "Påskedag")
	    (  +1 "Anden påskedag")
	    ( +26 "Store bededag")
	    ( +39 "Kristi himmelfartsdag")
	    ( +49 "Pinsedag")
	    ( +50 "Anden pinsedag"))))

	(holiday-fixed 12 24 "Juleaften")
	(holiday-fixed 12 25 "Juledag")
	(holiday-fixed 12 26 "Anden juledag")
	(holiday-fixed 12 31 "Nytårsaften")))

(setq other-holidays
  '(
     ;;(holiday-fixed 3 8 "Kvindernes internationale kampdag")
	(holiday-fixed 5 1 "Arbejdernes internationale kampdag")
	(holiday-fixed 5 4 "Danmarks befrielse")
	(holiday-float 5 0 2 "Mors dag")
	(holiday-fixed 6 5 "Grundlovsdag")
	(holiday-fixed 6 5 "Fars dag")
	(holiday-fixed 6 15 "Valdemarsdag (Dannebrog)")
	(holiday-fixed 6 24 "Skt. Hans dag")))

(setq calendar-holidays
      (append general-holidays other-holidays))

;;; da-kalender.el ends here