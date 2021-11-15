;;; lilypond-indent.el --- Auto-indentation for lilypond code  -*- lexical-binding: t; -*-
;;;;
;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2002--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>,
;;;;               2003--2004 Heikki Junes <hjunes@cc.hut.fi>
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
;;;
;;; Heikki Junes <hjunes@cc.hut.fi>
;;; * ond-char paren matching is handled by context dependent syntax tables
;;; * match two-char slurs '\( ... \)' and '\[ ... \]' separately.
;;; * adopt Emacs' f90-comment-region

;;; Chris Jackson <chris@fluffhouse.org.uk>
;;; some code is taken from ESS (Emacs Speaks Statistics) S-mode by A.J.Rossini <rossini@biostat.washington.edu>

;;; Variables for customising indentation style

;;; TODO:
;;;    * currently, in bracket matching one may need a non-bracket
;;;      chararacter between the bracket characters, like ( ( ) )


;;; Commentary:
;;

;;; Code:

(defcustom lilypond-indent-level 2
  "*Indentation of lilypond statements with respect to containing block."
  :group 'LilyPond
  :type 'integer)

(defcustom lilypond-brace-offset 0
  "*Extra indentation for open braces.
Compares with other text in same context."
  :group 'LilyPond
  :type 'integer)

(defcustom lilypond-angle-offset 0
  "*Extra indentation for open angled brackets.
Compares with other text in same context."
  :group 'LilyPond
  :type 'integer)

(defcustom lilypond-square-offset 0
  "*Extra indentation for open square brackets.
Compares with other text in same context."
  :group 'LilyPond
  :type 'integer)

(defcustom lilypond-scheme-paren-offset 0
  "*Extra indentation for open scheme parens.
Compares with other text in same context."
  :group 'LilyPond
  :type 'integer)

(defcustom lilypond-close-brace-offset 0
  "*Extra indentation for closing braces."
  :group 'LilyPond
  :type 'integer)

(defcustom lilypond-close-angle-offset 0
  "*Extra indentation for closing angle brackets."
  :group 'LilyPond
  :type 'integer)

(defcustom lilypond-close-square-offset 0
  "*Extra indentation for closing square brackets."
  :group 'LilyPond
  :type 'integer)

(defcustom lilypond-close-scheme-paren-offset 0
  "*Extra indentation for closing scheme parens."
  :group 'LilyPond
  :type 'integer)

(defcustom lilypond-fancy-comments t
  "*Non-nil means distiguish between %, %%, and %%% for indentation."
  :group 'LilyPond
  :type 'boolean)

(defcustom lilypond-comment-region "%%%"
  "*String inserted by \\[lilypond-comment-region]\
at start of each line in region."
  :group 'LilyPond
  :type 'string)

(defun lilypond-comment-region (beg end)
  "Comment/uncomment lines between BEG and END."
  (interactive "*r")
  (let ((m (make-marker)))
    (set-marker m end)
    (goto-char beg)
    (beginning-of-line)
    (if (looking-at (regexp-quote lilypond-comment-region))
        (delete-region (point) (match-end 0))
      (insert lilypond-comment-region))
    (while (and  (zerop (forward-line 1))
                 (< (point) (marker-position end)))
      (if (looking-at (regexp-quote lilypond-comment-region))
          (delete-region (point) (match-end 0))
        (insert lilypond-comment-region)))
    (set-marker m nil)))

(defun lilypond-calculate-indent ()
  "Return appropriate indentation for current line as lilypond code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string"
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          (case-fold-search nil)
          (containing-sexp (save-excursion (lilypond-scan-containing-sexp)))
          state)
      (beginning-of-defun)
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      ;; (setq containing-sexp (car (cdr state))) is the traditional way for languages
      ;; with simpler parenthesis delimiters
      (cond ((nth 3 state)
             ;; point is in the middle of a string
             nil)
            ((nth 4 state)
             ;; point is in the middle of a block comment
             (lilypond-calculate-indent-within-blockcomment))
            ((null containing-sexp)
             ;; Line is at top level - no indent
             (beginning-of-line)
             0)
            (t
             ;; Find previous non-comment character.
             (goto-char indent-point)
             (lilypond-backward-to-noncomment containing-sexp)
             ;; Now we get the answer.
             ;; Position following last unclosed open.
             (goto-char containing-sexp)
             (or
              ;; Is line first statement after an open brace or bracket?
              ;; If no, find that first statement and indent like it.
              (save-excursion
                (forward-char 1)
                ;; Skip over comments following open brace.
                (skip-chars-forward " \t\n")
                (cond ((looking-at "%{")
                       (while  (progn
                                 (and (not (looking-at "%}"))
                                      (< (point) (point-max))))
                         (forward-line 1)
                         (skip-chars-forward " \t\n"))
                       (forward-line 1)
                       (skip-chars-forward " \t\n"))
                      ((looking-at "%")
                       (while (progn (skip-chars-forward " \t\n")
                                     (looking-at "%"))
                         (forward-line 1))))
                ;; The first following code counts
                ;; if it is before the line we want to indent.
                (and (< (point) indent-point)
                     (current-column)))
              ;; If no previous statement,
              ;; indent it relative to line brace is on.
              ;; For open brace in column zero, don't let statement
              ;; start there too.  If lilypond-indent-level is zero, use
              ;; lilypond-brace-offset instead
              (+ (if (and (bolp) (zerop lilypond-indent-level))
                     (cond ((= (following-char) ?{)
                            lilypond-brace-offset)
                           ((= (following-char) ?<)
                            lilypond-angle-offset)
                           ((= (following-char) ?\[)
                            lilypond-square-offset)
                           ((= (following-char) ?\))
                            lilypond-scheme-paren-offset)
                           (t
                            0))
                   lilypond-indent-level)
                 (progn
                   (skip-chars-backward " \t")
                   (current-indentation)))))))))


(defun lilypond-indent-line ()
  "Indent current line as lilypond code.
Return the amount the indentation changed by."
  (let ((indent (lilypond-calculate-indent))
        beg shift-amt
        (case-fold-search nil)
        (pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
           (setq indent (current-indentation)))
          (t
           (skip-chars-forward " \t")
           (if (and lilypond-fancy-comments (looking-at "%%%\\|%{\\|%}"))
               (setq indent 0))
           (if (and lilypond-fancy-comments
                    (looking-at "%")
                    (not (looking-at "%%\\|%{\\|%}")))
               (setq indent comment-column)
             (if (eq indent t) (setq indent 0))
             (if (listp indent) (setq indent (car indent)))
             (cond
              ((= (following-char) ?})
               (setq indent  (+ indent (- lilypond-close-brace-offset lilypond-indent-level))))
              ((= (following-char) ?>)
               (setq indent  (+ indent (- lilypond-close-angle-offset lilypond-indent-level))))
              ((= (following-char) ?\])
               (setq indent  (+ indent (- lilypond-close-square-offset lilypond-indent-level))))
              ((and (= (following-char) ?\)) (lilypond-inside-scheme-p))
               (setq indent  (+ indent (- lilypond-close-scheme-paren-offset lilypond-indent-level))))
              ((= (following-char) ?{)
               (setq indent  (+ indent lilypond-brace-offset)))
              ((= (following-char) ?<)
               (setq indent  (+ indent lilypond-angle-offset)))
              ((= (following-char) ?\[)
               (setq indent  (+ indent lilypond-square-offset)))
              ((and (= (following-char) ?\() (lilypond-inside-scheme-p))
               (setq indent  (+ indent lilypond-scheme-paren-offset)))
              ))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
        (if (> (- (point-max) pos) (point))
            (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.
      ;; Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos))))
    shift-amt))


(defun lilypond-inside-comment-p ()
  "Return non-nil if point is inside a line or block comment."
  (or (save-excursion (beginning-of-line)
                      (skip-chars-forward " \t")
                      (looking-at "%"))
      (save-excursion
        ;; point is in the middle of a block comment
        (let ((lastopen  (save-excursion (re-search-backward "%{[ \\t]*" (point-min) t)))
              (lastclose (save-excursion (re-search-backward "%}[ \\t]*" (point-min) t))))
          (if (or (and (= (char-before) ?%) (= (char-after) ?{))
                  (and (= (char-after)  ?%) (= (char-after (1+ (point))) ?{)))
              (setq lastopen (save-excursion (backward-char) (point))))
          (and lastopen
               (or (not lastclose)
                   (<= lastclose lastopen)))))))

(defun lilypond-inside-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (let ((this-point (point))
        state)
    (or (save-excursion (beginning-of-line)
                        (skip-chars-forward " \t")
                        (looking-at "%"))
        (save-excursion
          (beginning-of-defun)
          (while (< (point) this-point)
            (setq state (parse-partial-sexp (point) this-point 0)))
          (cond
           ;; point is in the middle of a string
           ((nth 3 state) t)
           ;; point is in the middle of a block comment
           ((nth 4 state) t)
           (t nil))))))

(defun lilypond-backward-over-blockcomments (lim)
  "Move point back to closest non-whitespace character not part of a block comment.
LIM sets the limit to search."
  (let ((lastopen  (save-excursion (re-search-backward "%{[ \\t]*" lim t)))
        (lastclose (save-excursion (re-search-backward "%}[ \\t]*" lim t))))
    (if lastopen
        (if lastclose
            (if (<= lastclose lastopen) (goto-char lastopen))
          (goto-char lastopen)))
    (skip-chars-backward " %\t\n\f")))

(defun lilypond-backward-over-linecomments (lim)
  "Move point back to closest non-whitespace character not part of a line comment.
LIM sets the limit to search."
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (setq opoint (point))
      (beginning-of-line)
      (search-forward "%" opoint 'move)
      (skip-chars-backward " \t%")
      (setq stop (or (/= (preceding-char) ?\n) (<= (point) lim)))
      (if stop (point)
        (beginning-of-line)))))

(defun lilypond-backward-to-noncomment (lim)
  "Move point back to closest non-whitespace character not part of a comment.
LIM sets the limit to search."
  (lilypond-backward-over-linecomments lim)
  (lilypond-backward-over-blockcomments lim))

(defun lilypond-calculate-indent-within-blockcomment ()
  "Return the indentation amount for line inside a block comment."
  (let (end)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (skip-chars-backward " \t\n")
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (and (re-search-forward "%{[ \t]*" end t)
           (goto-char (1+ (match-beginning 0))))
      (if (and (looking-at "[ \t]*$") (= (preceding-char) ?\%))
          (1+ (current-column))
        (current-column)))))

;; Key:   Type of bracket (character).
;; Value: Pair of regexps representing the corresponding open and close bracket
;; () are treated specially (need to indent in Scheme but not in music)
(defconst lilypond-parens-regexp-alist
  `( ( ?>  .  ("\\([^\\]\\|^\\)<" . "\\([^ \\n\\t_^-]\\|[_^-][-^]\\|\\s-\\)\\s-*>"))
     ;; a b c->, a b c^> and a b c_> are not close-angle-brackets, they're accents
     ;; but a b c^-> and a b c^^> are close brackets with tenuto/marcato before them
     ;; also \> and \< are hairpins
     ;; duh .. a single '>', as in chords '<< ... >>', was not matched here
     ( ?}  .  ("{" . "}"))
     ;; ligatures  '\[ ... \]' are skipped in the following expression
     ( ?\]  .  ("\\([^\\]\\([\\][\\]\\)*\\|^\\)[[]" . "\\([^\\]\\([\\][\\]\\)*\\|^\\)[]]"))
     ( "\\]" . ("\\([^\\]\\|^\\)\\([\\][\\]\\)*[\\][[]" . "\\([^\\]\\|^\\)\\([\\][\\]\\)*[\\][]]"))
     ( "\\)" . ("\\([^\\]\\|^\\)\\([\\][\\]\\)*[\\][(]" . "\\([^\\]\\|^\\)\\([\\][\\]\\)*[\\][)]"))))

(defconst lilypond-parens-alist
  `( ( ?<  .  ?> )
     ( ?{  .  ?} )
     ( ?\[  .  ?\] )
     ( "\\["  .  "\\]" )
     ( ?\(  .  ?\) )
     ( "\\("  .  "\\)" )))


(defun lilypond-matching-paren (bracket-type)
  "Return the other character for BRACKET-TYPE."
  (cond
   ((member bracket-type (mapcar 'car lilypond-parens-alist))
    (cdr (assoc bracket-type lilypond-parens-alist)))
   ((member bracket-type (mapcar 'cdr lilypond-parens-alist))
    (car (rassoc bracket-type lilypond-parens-alist)))
   (t nil)))


(defun lilypond-scan-containing-sexp (&optional bracket-type slur-paren-p direction)
  "Move point to the beginning of the deepest parenthesis pair enclosing point.

If the optional argument BRACKET-TYPE, a character representing a
close bracket such as ) or }, is specified, then the parenthesis pairs
searched are limited to this type.

If the optional argument SLUR-PAREN-P is non-nil, then slur
parentheses () are considered as matching pairs. Otherwise Scheme
parentheses are considered to be matching pairs, but slurs are not.
slur-paren-p defaults to nil.

DIRECTION indicates which direction to search.
1 is forward, -1 is backward."
  (let ((level (if (not (eq direction 1)) 1 -1))
        (regexp-alist lilypond-parens-regexp-alist)
        (oldpos (point))
        (assoc-bracket-type (if (not (eq direction 1)) bracket-type (lilypond-matching-paren bracket-type)))
        paren-regexp
        match)
    (if (lilypond-inside-scheme-p)
        (setq paren-regexp "(\\|)")
      (if slur-paren-p
          ;; expressional slurs  '\( ... \)' are not taken into account
          (setq regexp-alist (cons '( ?\) . ("\\([^\\]\\([\\][\\]\\)*\\|^\\)(" . "\\([^\\]\\([\\][\\]\\)*\\|^\\))")) regexp-alist)))
      (if (member assoc-bracket-type (mapcar 'car regexp-alist))
          (progn (setq paren-regexp (cdr (assoc assoc-bracket-type regexp-alist)))
                 (setq paren-regexp (concat (car paren-regexp) "\\|" (cdr paren-regexp))))
        (setq paren-regexp (concat (mapconcat 'car (mapcar 'cdr regexp-alist) "\\|") "\\|"
                                   (mapconcat 'cdr (mapcar 'cdr regexp-alist) "\\|")))))
    ;; match concurrent one-char opening and closing slurs
    (if (and (eq direction 1)
             (not (sequencep bracket-type))
             (eq (char-syntax (or (char-after oldpos) 0)) ?\()
             (not (eq (char-after oldpos) ?<)))
        ;; anyway do not count open slur, since already level = -1
        (progn (forward-char 1)
               (if (eq (following-char)
                       (lilypond-matching-paren (char-after oldpos)))
                   ;; matching char found, go after it and set level = 0
                   (progn (forward-char 1)
                          (setq level 0)))))
    ;; browse the code until matching slur is found, or report mismatch
    (while (and (if (not (eq direction 1))
                    (> level 0)
                  (< level 0))
                ;; direction tells whether to search backward or forward
                (if (not (eq direction 1))
                    (re-search-backward paren-regexp nil t)
                  (re-search-forward paren-regexp nil t))
                ;; note: in case of two-char bracket only latter is compared
                (setq match (char-before (match-end 0))))
;;;      (message "%d" level) (sit-for 0 300)
      (if (not (save-excursion (goto-char (match-end 0))
                               ;; skip over strings and comments
                               (lilypond-inside-string-or-comment-p)))
          (if (memq match '(?} ?> ?\] ?\)))
              ;; count closing brackets
              (progn (setq level (1+ level))
                     ;; slurs may be close to each other, e.g.,
                     ;; a single '>' was not matched .. need to be corrected
                     (if (and (eq direction 1) (eq (char-after (match-end 0)) match))
                         (if (/= level 0)
                             (progn
                               (setq level (1+ level))
                               (forward-char 1))))
;;;        (message "%d %c" level match) (sit-for 0 300)
                     ;; hmm..
                     (if (and (= match ?>)
                              (looking-at ".\\s-+>\\|\\({\\|}\\|<\\|>\\|(\\|)\\|[][]\\)>"))
                         (forward-char 1)))
            ;; count opening brackets
            (progn (setq level (1- level))
;;;      (message "%d %c" level match) (sit-for 0 300)
                   ;; hmm..
                   (if (and (= match ?<)
                            (looking-at ".\\s-+<\\|\\({\\|}\\|<\\|>\\|(\\|)\\|[][]\\)<"))
                       (forward-char 1))))))
    ;; jump to the matching slur
    (if (not (eq direction 1))
        (progn
          (if (sequencep bracket-type)
              ;; match the latter char in two-char brackets
              (if (looking-at "..[][)(]") (forward-char 1)))
          ;; if the following char is not already a slur
          (if (and (not (looking-at "[)(]"))
                   ;; match the slur which follows
                   (looking-at ".[][><)(]")) (forward-char 1)))
      (backward-char 1))
    (if (= level 0)
        (point)
      (progn (goto-char oldpos)
             nil))))


(defun lilypond-inside-scheme-p ()
  "Return t if point is inside embedded Scheme code."
;;; An user does not call this function directly, or by a key sequence.
  ;;  (interactive)
  (let ((test-point (point))
        (level 0)
        match)
    (save-excursion
      (if (or (and (eq (char-after (point)) ?\()
                   (save-excursion
                     (skip-chars-backward "'`")
                     (memq (char-before) '(?# ?$))))
              (and (re-search-backward "[#$][`']?(" nil t)
                   (progn
                     (search-forward "(")
                     (setq level 1)
                     (while (and (> level 0)
                                 (re-search-forward "[()]" test-point t)
                                 (setq match (char-after (match-beginning 0)))
                                 (<= (point) test-point))
                       (if (= match ?\()
                           (setq level (1+ level))
                         (setq level (1- level))))
                     (> level 0))))
          t
        nil))))


;;; Largely taken from the 'blink-matching-open' in lisp/simple.el in
;;; the Emacs distribution.

(defun lilypond-blink-matching-paren (&optional dir)
  "Temporarily move cursor to the beginning of the sexp before point.
In lilypond files this is used for closing ), ], } and >, whereas the
builtin 'blink-matching-open' is not used. In syntax table, see
`lilypond-font-lock.el', all brackets are punctuation characters.

DIR indicates direction."
  (let ((oldpos (point))
         (mismatch)
         np
         bracket-type
         char-before-bracket-type
         blinkpos)
    (if (not (or (equal this-command 'lilypond-electric-close-paren)
                 (eq dir 1)))
        (goto-char (setq oldpos (- oldpos 1))))
    ;; Test if a ligature \] or expressional slur \) was encountered
    (setq bracket-type (char-after (point)))
    (setq char-before-bracket-type nil)
    (if (memq bracket-type '(?\] ?\) ?\[ ?\())
        (progn
          (setq np -1)
          (while (eq (char-before (- (point) (setq np (+ np 1)))) ?\\)
            (setq char-before-bracket-type (if char-before-bracket-type nil ?\\)))
          (if (eq char-before-bracket-type ?\\)
              (setq bracket-type (string char-before-bracket-type bracket-type)))))
    (when blink-matching-paren-distance
      (narrow-to-region
       (max (point-min) (- (point) blink-matching-paren-distance))
       (min (point-max) (+ (point) blink-matching-paren-distance))))
    (if (and (equal this-command 'lilypond-electric-close-paren)
             (memq bracket-type '(?> ?} ?< ?{)))
        ;; < { need to be mutually balanced and nested, so search backwards for both of these bracket types
        (lilypond-scan-containing-sexp nil nil dir)
      ;; whereas ( ) slurs within music don't, so only need to search for ( )
      ;; use same mechanism for [ ] slurs
      (lilypond-scan-containing-sexp bracket-type t dir))
    (setq blinkpos (point))
    (setq mismatch
          (or (null (lilypond-matching-paren (char-after blinkpos)))
              (/= (char-after oldpos)
                  (lilypond-matching-paren (char-after blinkpos)))))
    (if mismatch (progn (setq blinkpos nil)
                        (message "Mismatched parentheses")))
    (if (and blinkpos
             (equal this-command 'lilypond-electric-close-paren))
        (if (pos-visible-in-window-p)
            (and blink-matching-paren-on-screen
                 (sit-for blink-matching-delay))
          (message
           "Matches %s"
           ;; Show what precedes the open in its line, if anything.
           (if (save-excursion
                 (skip-chars-backward " \t")
                 (not (bolp)))
               (buffer-substring (progn (beginning-of-line) (point))
                                 (1+ blinkpos))
             ;; Show what follows the open in its line, if anything.
             (if (save-excursion
                   (forward-char 1)
                   (skip-chars-forward " \t")
                   (not (eolp)))
                 (buffer-substring blinkpos
                                   (progn (end-of-line) (point)))
               ;; Otherwise show the previous nonblank line,
               ;; if there is one.
               (if (save-excursion
                     (skip-chars-backward "\n \t")
                     (not (bobp)))
                   (concat
                    (buffer-substring (progn
                                        (skip-chars-backward "\n \t")
                                        (beginning-of-line)
                                        (point))
                                      (progn (end-of-line)
                                             (skip-chars-backward " \t")
                                             (point)))
                    ;; Replace the newline and other whitespace with `...'.
                    "..."
                    (buffer-substring blinkpos (1+ blinkpos)))
                 ;; There is nothing to show except the char itself.
                 (buffer-substring blinkpos (1+ blinkpos))))))))
    (if (not (equal this-command 'lilypond-electric-close-paren))
        (goto-char (setq oldpos (+ oldpos 1)))
      (goto-char oldpos))
    (if (not (eq dir 1))
        blinkpos
      (+ blinkpos 1))))

(defun lilypond-electric-close-paren ()
  "Blink on the matching open paren when a >, ), } or ] is inserted."
  (interactive)
  (let ((oldpos (point)))
    (self-insert-command 1)
    ;; Refontify buffer if a block-comment-ender '%}' is inserted
    (if (and (eq (char-before (point)) ?})
             (eq (char-before (- (point) 1)) ?%))
        (font-lock-ensure)
      ;; Match paren if the cursor is not inside string or comment.
      (if (and blink-matching-paren
               (not (lilypond-inside-string-or-comment-p))
               (save-excursion (re-search-backward
                                (concat (mapconcat 'cdr (mapcar 'cdr lilypond-parens-regexp-alist) "\\|") "\\|)") nil t)
                               (eq oldpos (1- (match-end 0)))))
          (progn (backward-char 1)
                 (lilypond-blink-matching-paren)
                 (forward-char 1))))))

;; (defun lilypond-scan-sexps (pos dir)
;;   "This function is redefined to be used in Emacs' show-paren-function and
;; in XEmacs' paren-highlight."
;;   (lilypond-blink-matching-paren dir))

(provide 'lilypond-indent)

;;; lilypond-indent.el ends here
