;;; lilypond-keywords.el --- Initialize recognized keywords, identifiers, reserved words  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author: ;; -*- lexical-binding: t; -*- <n@nbook>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(defun lilypond-words-filename ()
  "The file containing LilyPond \keywords \Identifiers and ReservedWords.
Finds file lilypond-words.el from `load-path'."
  (let ((fn nil)
        (lp load-path)
        (words-file "lilypond-words.el"))
    (while (and (> (length lp) 0) (not fn))
      (setq fn (concat (car lp) "/" words-file))
      (if (not (file-readable-p fn))
          (progn (setq fn nil) (setq lp (cdr lp)))))
    (if (not fn)
        (progn (message "Warning: `lilypond-words.el' not found in `load-path'. See `lilypond-init.el'.")
               (sit-for 5 0)))
    fn))

(defun lilypond-words ()
  "Return a list of Lilypond keywords, identifiers, and reserved words."
  (let ((file (or (lilypond-words-filename)
                  (error "Cannot find `lilypond-words.el' in `load-path'")))
        words
        word)
    (with-current-buffer (find-file-noselect file 'nowarn 'raw)
      (goto-char (point-min))
      (while (setq word (ignore-errors (read (current-buffer))))
        (push (symbol-name word) words))
      (setq words (reverse words))
      (kill-buffer)
      (append words (list (length words))))))

(defvar lilypond-words (lilypond-words)
  "List of Lilypond keywords, identifiers, and reserved words.")
(defvar lilypond-keywords       '("\\score")      "Lilypond \\Keywords.")
(defvar lilypond-identifiers    '("\\voiceOne")   "Lilypond \\Identifiers.")
(defvar lilypond-reserved-words '("StaffContext") "LilyPond reserved words.")
(defvar lilypond-note-names     '("cessess" "a" "b" "c" "d" "e" "f" "g")
  "LilyPond note names.")
(defvar lilypond-uncategorized-words nil
  "List of words which uncategorized words from `lilypond-words.el'.
Useful for debugging.")

(defun lilypond-keyword-p (string)
  "Return t if STRING is a lilypond keyword."
  (and (> (length string) 1)
       (string-prefix-p "\\" string)
       (string-match "[a-z-]+" string)
       (= (match-beginning 0) 1)
       (= (match-end 0) (length string))
       (not (member string (list "\\longa" "\\breve" "\\maxima")))
       (string-equal (downcase string) string)))

(defun lilypond-identifier-p (string)
  "Return t if STRING is a \\Identifier."
  (and (> (length string) 1)
       (string-prefix-p  "\\" string)
       (string-match "[a-zA-Z-]+" string)
       (= (match-beginning 0) 1)
       (= (match-end 0) (length string))
       (not (string-equal (downcase string) string))))

(defun lilypond-notename-p (string)
  "Return t if STRING is a note name."
  (and (> (length string) 0)
       (string-match "[a-z]+" string)
       (= (match-beginning 0) 0)
       (= (match-end 0) (length string))
       (string-equal (downcase string) string)))

(defun lilypond-capitalized-reserved-word-p (string)
  "Return t if STRING is a capitalized reserved word."
  (and (> (length string) 0)
       (string-match "[a-zA-Z_]+" string)
       (= (match-beginning 0) 0)
       (= (match-end 0) (length string))
       (not (string-equal (downcase string) string))))

(defun lilypond-populate-keyword-lists ()
  "Populate various keyword list variables."
  (let ((words (butlast lilypond-words))
        current
        ignored)
    (while (setq current (pop words))
      (eval `(cl-pushnew ,current
                         ,(pcase current
                            ((pred lilypond-keyword-p) 'lilypond-keywords)
                            ((pred lilypond-identifier-p) 'lilypond-identifiers)
                            ((pred lilypond-notename-p) 'lilypond-note-names)
                            ((pred lilypond-capitalized-reserved-word-p) 'lilypond-reserved-words)
                            (_ 'lilypond-uncategorized-words))
                         :test #'string=)
            t)
      (when (string-equal "-" (car words))
        (pop words)
        (while (not (string-equal "-" (pop words)))))))
  (setq lilypond-identifiers    (cl-sort (reverse lilypond-identifiers) #'string>)
        lilypond-note-names     (cl-sort (reverse lilypond-note-names) #'string>)
        lilypond-reserved-words (cl-sort (reverse lilypond-reserved-words) #'string>)
        lilypond-keywords       (cl-sort (reverse lilypond-keywords) #'string>))
  nil)

(lilypond-populate-keyword-lists)

(provide 'lilypond-keywords)
;;; lilypond-keywords.el ends here
