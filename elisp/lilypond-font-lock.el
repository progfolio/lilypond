;;;; lilypond-font-lock.el --- syntax coloring for LilyPond mode  -*- lexical-binding: t; -*-
;;;;
;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1997--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>,
;;;;               2001--2006 Heikki Junes <hjunes@cc.hut.fi>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Author: 2001-2006: Heikki Junes
;;  * Emacs-mode: new keywords, reserved words, identifiers, notenames,
;;    some dynamics and brackets are font-lock-keywords
;;  * context-dependent syntax-tables
;; Author: 1997: Han-Wen Nienhuys
;; Author: 1995-1996 Barry A. Warsaw
;;         1992-1994 Tim Peters
;; Created:       Feb 1992
;; Version:       2.9.29
;; Last Modified: 11NOV2006
;; Keywords: lilypond languages music notation

;; This started out as a cannabalised version of python-mode.el, by hwn
;; For changes see the LilyPond ChangeLog
;;

;; TODO:
;;   - handle lexer modes (\header, \melodic) etc.

;;; Commentary:
;;

(require 'lilypond-keywords)

;;; Code:

(defconst lilypond-font-lock-keywords
  (let*
      ((keyword-regexp (mapconcat (lambda (x) (concat "\\" x))  lilypond-keywords "\\|"))
       (identifier-regexp (mapconcat (lambda (x) (concat "\\" x))  lilypond-identifiers "\\|"))
       (note-name-regexp (mapconcat (lambda (x) (concat "" x))  lilypond-note-names "\\|"))
       (reserved-word-regexp (mapconcat (lambda (x) (concat "" x))  lilypond-reserved-words "\\|"))
       (duration "\\([ \t]*\\(128\\|6?4\\|3?2\\|16?\\|8\\)[.]*\\([ \t]*[*][ \t]*[0-9]+\\(/[1-9][0-9]*\\)?\\)?\\)")
       (longduration "\\([ \t]*\\(\\\\\\(longa\\|breve\\|maxima\\)\\)[.]*\\([ \t]*[*][ \t]*[0-9]+\\(/[1-9][0-9]*\\)?\\)?\\)"))

    (list
     ;; Fonts in use (from GNU Emacs Lisp Reference Manual, elisp.ps):
     ;; font-lock- (c)omment / (s)tring / (k)eyword / (b)uiltin / (f)unction-name /
     ;;            (v)ariable-name / (t)ype / co(n)stant / (w)arning -face

     ;; The order below is designed so that proofreading would be possible.

     ;; Fontify...
     ;; ... (f) identifiers and (k) keywords.
     ;; ... (n) user defined indetifiers
     ;; ... (v) the right and the left side of '='-marks.
     ;; ... (v) reserved words, e.g., FiguredBass.
     ;; ... (t) notes and rests
     ;; "on top", ... (s) lyrics-mode
     ;; "on top", ... (w) horizontal grouping
     ;; "on top", ... (f) vertical grouping
     ;; "on top", ... (b) expressional grouping
     ;; "on top", ... (s) (multiline-)scheme; urgh. one should count the slurs
     ;; "on top", ... (s) strings
     ;; "on top", ... (c) (multiline-)comments

     ;; ... identifiers (defined above, see identifier-regexp)
     (cons (concat "\\(\\([_^-]?\\(" identifier-regexp "\\)\\)+\\)\\($\\|[] \t(~{}>\\\\_()^*-]\\)") '(1 font-lock-function-name-face))

     ;; ... keywords (defined above, see keyword-regexp)
     (cons (concat "\\(\\([_^-]?\\(" keyword-regexp "\\)\\)+\\)\\($\\|[] \t(~{}>\\\\_()^*-]\\)") '(1 font-lock-keyword-face))

     ;; ... user defined identifiers, roughly  \[a-zA-Z]+ with single - or _ in between.
     '("\\([_^-]?\\\\\\([a-zA-Z[:nonascii:]]\\(?:[-_]?[a-zA-Z[:nonascii:]]\\)*\\)\\)" 1 font-lock-constant-face)

     ;; ... the left side of '=' -mark
     '("\\([_a-zA-Z.0-9-]+\\)[ \t]*=[ \t]*" 1 font-lock-variable-name-face)

     ;; ... the right side of '=' -mark
     '("[ \t]*=[ \t]*\\([_a-zA-Z.0-9-]+\\)" 1 font-lock-variable-name-face)

     ;; ... reserved words (defined above, see reserved-word-regexp)
     (cons (concat "\\(" reserved-word-regexp "\\)") 'font-lock-variable-name-face)

     ;; ... note or rest with (an accidental and) a duration, e.g., b,?16.*3/4
     (cons (concat "\\(^\\|[ <\{[/~(!)\t\\\|]\\)\\(\\(\\(" note-name-regexp "\\)[,']*[?!]?\\|[srR]\\)" duration "?\\)") '(2 font-lock-type-face))

     ;; "on top", ... notes and rests with a long duration
     (cons (concat "\\(^\\|[ <\{[/~(!)\t\\\|]\\)\\(\\(\\(" note-name-regexp "\\)[,']*[?!]?\\|[srR]\\)" longduration "\\)") '(2 font-lock-type-face t))

     ;; "on top", ... lyrics-mode: fontify everything between '<'...'>' or '{'...'}'
                                        ;            URGH, does not know anything about inner brackets.
                                        ;            Multiple lines may need refontifying (C-c f).
     '("\\(\\\\lyrics[^{<]*\\)\\({[^}]*\\|<[^>]*\\)" 2 font-lock-string-face t)

     ;; "on top", ... horizontal grouping, also as postfix syntax '-*':
     ;;               - brackets '{[]}'
     ;;               - ties '~'
     ;;               - ligatures \[, \]
     '("\\(-?[][~}{]\\|\\\\[][]\\)" 0 font-lock-reference-face t)

     ;; "on top", ... vertical grouping:
     ;;               - '<>'-chord brackets with '\\'-voice sep., not marcato '->'
     ;;               - '<< a b >>8' -chords
     (cons (concat "\\(\\(-.\\)+\\|[^-^_]\\)\\([<>]+\\(" duration "\\|" longduration "\\)?\\|\\\\\\\\\\)") '(3 font-lock-function-name-face t))

     ;; "on top", ... expressional grouping, also as postfix syntax '-*':
     ;;               - slurs ( ), \( \), [-^_][()]
     ;;               - hairpins \<, \>, \!
     '("\\(-?\\\\[(<!>)]\\|[-^_]?[()]\\)" 0 font-lock-builtin-face t)

     ;; "on top", ... (multiline-)scheme: try find slurs up to 7th
     '("[_^-]?#\\(#[ft]\\|-?[0-9.]+\\|\"[^\"]*\"\\|['`]?[a-zA-Z:-]+\\|['`]?([^()]*\\(([^()]*\\(([^()]*\\(([^()]*\\(([^()]*\\(([^()]*\\(([^)]*)[^()]*\\)*)[^()]*\\)*)[^()]*\\)*)[^()]*\\)*)[^()]*\\)*)[^()]*\\)*[^)]*)\\)" 0 font-lock-string-face t)

     ;; "on top", ... strings, match also unending strings at eof:
     ;;               if '\n' was not found, it must be '$' which is eof (?).
     '("\\([_^-]?\"\\([^\"\\\\]\\|\\\\.\\|\\\\\n\\)*\\(\"\\|$\\)\\)" 0 font-lock-string-face t)

     ;; "on top", ... (multiline-)comments
     '("\\(%\\({[^%]*%\\(}\\|\\([^}][^%]*%\\)+}\\)\\|.*\\)\\)" 0 font-lock-comment-face t)))
  "Additional expressions to fontify in LilyPond mode.")

;; define a mode-specific abbrev table for those who use such things
(defvar lilypond-mode-abbrev-table nil
  "Abbrev table in use in `lilypond-mode' buffers.")

(define-abbrev-table 'lilypond-mode-abbrev-table nil)

(defvar lilypond-mode-syntax-table (make-syntax-table)
  "Syntax table used in `lilypond-mode' buffers.")

(defun lilypond-mode-set-syntax-table (&optional not-punct)
  "Change syntax table according to NOT-PUNCT.
NOT-PUNCT characters are given a context dependent non-punctuation syntax.
Parens may be set to paren syntax.
Characters `-', `^' and `_' may be set to escape syntax."
  (if (not not-punct) (setq not-punct '()))
  (setq lilypond-mode-syntax-table (make-syntax-table))
  (let ((defaults
         '(
           ;; NOTE: Emacs knows only "13"-style (used), XEmacs knows also "1b3b", etc.
           ( ?\% . "< 13" )   ; comment starter, 1st char in block-comments
           ( ?\n . ">")       ; newline: comment ender
           ( ?\r . ">")       ; formfeed: comment ender
           ( ?\\ . "\\" )     ; escape characters (as '\n' in strings)
           ( ?\" . "\"" )     ; string quote characters
           ;; word constituents (e.g., belonging to a note)
           ( ?\' . "w") ( ?\, . "w") ; transposing octaves
           ;; punctuation characters (separate symbols from another)
           ( ?\$ . "." ) ( ?\& . "." )
           ( ?\* . "." ) ( ?\+ . "." ) ( ?\/ . "." )  ( ?\= . "." )
           ( ?\| . "." )      ; bar line
           )))
    ;; all the paren characters are now handled by lily-specific indenting/matching code in lilypond-indent.el
    (if (or (memq ?\{ not-punct) (memq ?\} not-punct))
        (setq defaults (cons '( ?\{ . "(} 2" ) (cons '( ?\} . "){ 4" ) defaults))) ; begin and end of a block-comment
      (setq defaults (cons '( ?\{ . ". 2" ) (cons '( ?\} . ". 4" ) defaults))))    ; begin and end of a block-comment
    (if (or (memq ?\[ not-punct) (memq ?\] not-punct))
        (setq defaults (cons '( ?\[ . "(]" ) (cons '( ?\] . ")[" ) defaults)))
      (setq defaults (cons '( ?\[ . "." ) (cons '( ?\] . "." ) defaults))))
    (if (or (memq ?\< not-punct) (memq ?\> not-punct))
        (setq defaults (cons '( ?\< . "(>" ) (cons '( ?\> . ")<" ) defaults)))
      (setq defaults (cons '( ?\< . "." ) (cons '( ?\> . "." ) defaults))))
    (if (or (memq ?\( not-punct) (memq ?\) not-punct))
        (setq defaults (cons '( ?\( . "()" ) (cons '( ?\) . ")(" ) defaults)))
      (setq defaults (cons '( ?\( . "." ) (cons '( ?\) . "." ) defaults))))
    ;; In LilyPond the following chars serve as escape chars, e.g., c^> d-) e_( ,
    ;; but they may be set to punctuation chars, since inside strings they should not act as escape chars
    (setq defaults (cons (if (memq ?- not-punct) '( ?\- . "\\" ) '( ?\- . "." ) ) defaults))
    (setq defaults (cons (if (memq ?^ not-punct) '( ?^ . "\\" ) '( ?^ . "." ) ) defaults))
    (setq defaults (cons (if (memq ?\_ not-punct) '( ?\_ . "\\" ) '( ?\_ . "." ) ) defaults))
    (mapc (lambda (x) (modify-syntax-entry (car x) (cdr x) lilypond-mode-syntax-table))
          defaults)
    (set-syntax-table lilypond-mode-syntax-table)))

(defun lilypond-mode-context-set-syntax-table ()
  "Change syntax table according to current context."
  (interactive)
  ;; default syntax table sets parentheses to punctuation characters
  (lilypond-mode-set-syntax-table)
  ;; find current context
  (let ((context (parse-partial-sexp (point-min) (point))))
    (cond ((nth 3 context)) ; inside string
          ((nth 4 context)) ; inside a comment
          ((eq (char-syntax (or (char-before (point)) 0)) ?\\)) ; found escape-char
          ((and (eq (char-syntax (or (char-before (- (point) 1)) 0)) ?\\)
                (memq (char-before (point)) '( ?\) ?\] )))) ; found escape-char
          ((memq (char-before (point)) '( ?\) ))
           (lilypond-mode-set-syntax-table '( ?\( ?\) )))
          ((memq (char-before (point)) '( ?\] ))
           (lilypond-mode-set-syntax-table '( ?\[ ?\] )))
          ((memq (char-before (point)) '( ?\> ?\} ))
           (lilypond-mode-set-syntax-table '( ?\< ?\> ?\{ ?\} ?\^ ?\- ?\_ )))
          ((memq (char-after (point)) '( ?\( ))
           (lilypond-mode-set-syntax-table '( ?\( ?\) )))
          ((memq (char-after (point)) '( ?\[ ))
           (lilypond-mode-set-syntax-table '( ?\[ ?\] )))
          ((memq (char-after (point)) '( ?\< ?\{ ))
           (lilypond-mode-set-syntax-table '( ?\< ?\> ?\{ ?\} ?\^ ?\- ?\_ ))))))

(provide 'lilypond-font-lock)

;;; lilypond-font-lock.el ends here
