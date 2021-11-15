;;;; lilypond-mode.el -- Major mode for editing GNU LilyPond music scores  -*- lexical-binding: t; -*-
;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1999--2021 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Changed 2001--2003 Heikki Junes <heikki.junes@hut.fi>
;;;;    * Add PS-compilation, PS-viewing and MIDI-play (29th Aug 2001)
;;;;    * Keyboard shortcuts (12th Sep 2001)
;;;;    * Inserting tags, inspired on sgml-mode (11th Oct 2001)
;;;;    * Autocompletion & Info (23rd Nov 2002)
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


;;; Inspired on auctex

;;; Look lilypond-init.el or Documentation/topdocs/INSTALL.texi
;;; for installing instructions.


;;; Commentary:
;;

(require 'compile)
(require 'cl-lib)
(require 'lilypond-font-lock)
(require 'lilypond-indent)
;;(require 'lilypond-what-beat)

;;; Code:

(defconst lilypond-version "2.5.20"
  "`lilypond-mode' version number.")

(defconst lilypond-help-address "bug-lilypond@gnu.org"
  "Address accepting submission of bug reports.")

(defvar lilypond-mode-hook nil
  "*Hook called by `lilypond-mode'.")

(defvar lilypond-region-file-prefix "emacs-lily"
  "File prefix for commands on buffer or region.")

(defvar lilypond-master-file nil
  "Master file that LilyPond will be run on.")

;; FIXME: find ``\score'' in buffers / make settable?
(defun lilypond-get-master-file ()
  "The master file."
  (or lilypond-master-file (buffer-file-name)))

(defvar lilypond-kick-xdvi nil
  "If true, no simultaneous xdvi's are started, but reload signal is sent.")

(defvar lilypond-command-history nil
  "Command history list.")

(defvar lilypond-regexp-alist
  '(("\\([a-zA-Z]?:?[^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2))
  "Regexp used to match LilyPond errors.  See `compilation-error-regexp-alist'.")

(defvar lilypond-imenu nil
  "A flag to tell whether `lilypond-imenu' is turned on.")
(make-variable-buffer-local 'lilypond-imenu)

(defcustom lilypond-include-path ".:/tmp"
  "* LilyPond include path."
  :type 'string
  :group 'LilyPond)

(defcustom lilypond-ly-command "lilypond"
  "Command used to compile LY files."
  :group 'LilyPond
  :type 'string)

(defun lilypond--compile-command (file)
  "Return the shell command to compile LY FILE."
  (concat lilypond-ly-command " " (shell-quote-argument file)))

(defun lilypond-compile-file (file)
  "Compile FILE using `lilypond-ly-command'.
When called interactively, assume current buffer's FILE.
If the current buffer is not backed by a FILE, prompt for FILE."
  (interactive (list
                (or
                 (when-let ((name (buffer-file-name)))
                   (and (string-suffix-p "\.ly" name) name))
                 (read-file-name
                  "compile: "
                  nil nil 'mustmatch
                  nil ))))
  (let ((default-directory (file-name-directory (expand-file-name file))))
    (compile (lilypond--compile-command file))))

(defcustom lilypond-ps-command "gv --watch"
  "Command used to display PS files."
  :group 'LilyPond
  :type 'string)

(defcustom lilypond-pdf-command "xpdf"
  "Command used to display PDF files."
  :group 'LilyPond
  :type 'string)

(defcustom lilypond-midi-command "timidity"
  "Command used to play MIDI files."
  :group 'LilyPond
  :type 'string)

(defvar lilypond-window-conf nil "Used to store the pre-compilation window configuration.")
(defun lilypond-play (as-is)
  "Play the midi file corresponding to the current buffer.
If AS-IS is non-nil, do not compile current file first."
  (interactive "P")
  (setq lilypond-window-conf (current-window-configuration))
  (let* ((source (buffer-file-name))
         (midi (file-name-with-extension source ".midi")))
    (compile (concat
              (unless (or as-is (file-newer-than-file-p midi source))
                (concat (lilypond--compile-command source) " && "))
              lilypond-midi-command " " midi))))

(defun count-rexp (beg end regexp)
  "Return number of REGEXP matches between BEG and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (count-matches regexp))))

(defun count-midi-words ()
  "Check number of midi-scores before the curser."
  (if (region-active-p)
      (count-rexp (mark t) (point) "\\\\midi")
    (count-rexp (point-min) (point-max) "\\\\midi")))

(defun count-midi-words-backwards ()
  "Check number of midi-scores before the curser."
  (if (region-active-p)
      (count-rexp (mark t) (point) "\\\\midi")
    (count-rexp (point-min) (point) "\\\\midi")))

;; (defun lilypond-region-file (_begin _end)
;;   "Return temp file path for command on region."
;;   (let ((dir (temporary-file-directory))
;;         (base lilypond-region-file-prefix)
;;         (ext lilypond-file-extension))
;;     (concat dir base ext)))

;;; Commands on Region work if there is an appropriate '\score'.
;; (defun lilypond-command-region (begin end)
;;   "Run LilyPond on the region between BEGIN and END."
;;   (interactive "r")
;;   (if (or (> begin (point-min)) (< end (point-max)))
;;       ;;(lilypond-command-select-region))
;;       nil
;;     (write-region begin end (lilypond-region-file begin end) nil 'nomsg)
;;     (lilypond-command (lilypond-command-query
;;                        (lilypond-region-file begin end))
;;                       '(lambda () (lilypond-region-file begin end)))
;;     ;; Region may deactivate even if buffer was intact, reactivate?
;;     ;; Currently, also deactived regions are used.
;;     )

(defconst lilypond-imenu-generic-re "^\\([a-zA-Z]+\\) *="
  "Regexp matching Identifier definitions.")

(defvar lilypond-imenu-generic-expression
  (list (list nil lilypond-imenu-generic-re 1))
  "Expression for imenu.")

;;;###autoload
(define-derived-mode lilypond-mode prog-mode "lilypond-mode"
  "Major mode for editing LilyPond music files.

This mode knows about LilyPond keywords and line comments, not about
indentation or block comments.  It features easy compilation, error
finding and viewing of a LilyPond source buffer or region.

COMMANDS
\\{lilypond-mode-map}
VARIABLES

lilypond-command-alist\t\talist from name to command"
  ;; set up local variables
  (setq-local font-lock-defaults '(lilypond-font-lock-keywords))
  ;; string and comments are fontified explicitly
  (setq-local font-lock-keywords-only t)
  ;; Multi-line font-locking needs Emacs 21.1 or newer.
  ;; For older versions there is hotkey "C-c f".
  (setq-local font-lock-multiline t)
  (setq-local paragraph-separate "^[ \t]*$")
  (setq-local paragraph-start "^[ \t]*$")
  (setq-local comment-start "%")
  (setq-local comment-start-skip "%{? *")
  (setq-local comment-end "")
  (setq-local block-comment-start "%{")
  (setq-local block-comment-end   "%}")
  (setq-local indent-line-function 'lilypond-indent-line)
  (setq-local imenu-generic-expression lilypond-imenu-generic-expression)
  (lilypond-mode-set-syntax-table '(?\< ?\> ?\{ ?\}))
  (setq local-abbrev-table lilypond-mode-abbrev-table)
  (add-hook 'post-command-hook 'lilypond-mode-context-set-syntax-table nil t))

(define-key lilypond-mode-map ")" 'lilypond-electric-close-paren)
(define-key lilypond-mode-map ">" 'lilypond-electric-close-paren)
(define-key lilypond-mode-map "}" 'lilypond-electric-close-paren)
(define-key lilypond-mode-map "]" 'lilypond-electric-close-paren)
(define-key lilypond-mode-map "|" 'lilypond-electric-bar)

(defun lilypond-version ()
  "Echo the current version of `lilypond-mode' in the minibuffer."
  (interactive)
  (message "Using `lilypond-mode' version %s" lilypond-version))

;; (defun lilypond-guile ()
;;   "Excute guile."
;;   (interactive)
;;   (require 'ilisp)
;;   (guile "lilyguile" (lilypond-command-expand (cadr (assoc "LilyPond" lilypond-command-alist))
;;                                               (funcall 'lilypond-get-master-file)))
;;   (comint-default-send (ilisp-process) "(define-module (*anonymous-ly-0*))")
;;   (comint-default-send (ilisp-process) "(set! %load-path (cons \"/usr/share/ilisp/\" %load-path))")
;;   (comint-default-send (ilisp-process) "(use-modules (guile-user) (guile-ilisp))")
;;   (comint-default-send (ilisp-process) "(newline)"))

(provide 'lilypond-mode)
;;; lilypond-mode.el ends here
