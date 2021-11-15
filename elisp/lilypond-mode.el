;;; lilypond-mode.el --- Major mode for editing GNU lilypond music scores  -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:
;; Keywords: languages, tools, multimedia
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/progfolio/lilypond
;; Version: 0.0.0

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

;; This is a rewrite.

;;; Code:

(require 'compile)
(require 'cl-lib)
(require 'lilypond-font-lock)
(require 'lilypond-indent)
;;(require 'lilypond-what-beat)

;;; Code:
(defgroup lilypond nil
  "Major mode for editing GNU LilyPond files."
  :group 'lilypond
  :prefix "lilypond-")

(defcustom lilypond-ly-command "lilypond"
  "Command used to compile LY files."
  :group 'lilypond
  :type 'string)

(defcustom lilypond-midi-command "timidity"
  "Command used to play MIDI files."
  :group 'lilypond
  :type 'string)

(defconst lilypond-version "2.5.20" "`lilypond-mode' version number.")

(defvar lilypond-regexp-alist
  '(("\\([a-zA-Z]?:?[^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2))
  "Regexp used to match LilyPond errors.  See `compilation-error-regexp-alist'.")
(defvar lilypond-window-conf nil "Used to store the pre-compilation window configuration.")

(defun lilypond--compile-command (file)
  "Return the shell command to compile LY FILE."
  (concat lilypond-ly-command " " (shell-quote-argument file)))

;;;###autoload
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

(defun lilypond-region-context (beg end)
  "Return a context string for text between BEG and END."
  (interactive "r")
  (let ((open-bracket (save-excursion (re-search-backward " ?{" nil 'noerror)))
        (_close-bracket (save-excursion (re-search-forward " ?}" nil 'noerror))))
    (if open-bracket
        (if-let ((context (save-excursion (re-search-backward  "= \\(.*\\)" nil 'noerror))))
            (concat (string-trim (match-string-no-properties 1))
                    " "
                    (string-trim (buffer-substring-no-properties beg end))
                    " }")
          (user-error "Could not determine playback context")))))

(defun lilypond-play-region (beg end)
  "Play region between BEG and END."
  (interactive "r")
  (let ((file (make-temp-file "lilypond-"))
        (context (string-trim (lilypond-region-context beg end)))
        (lang (save-excursion (goto-char (point-min))
                              (re-search-forward "\\(?:\\\\language [^z-a]*?$\\)"
                                                 nil 'noerror)
                              (match-string 0)))
        (tempo (save-excursion (goto-char (point-min))
                               (let ((tempo-re  "\\(?:\\\\tempo .*?$\\)"))
                                 (if (re-search-backward tempo-re nil 'noerror)
                                     (match-string 0)
                                   (when (re-search-forward tempo-re nil 'noerror)
                                     (match-string 0))))))
        (midi-context ""))
    (with-current-buffer (find-file-noselect file 'nowarn 'raw)
      (with-silent-modifications
        (insert (format "\\version %S\n"
                        (nth 2 (split-string
                                (shell-command-to-string "lilypond --version")))))
        (when lang (insert lang "\n"))
        (when tempo (setq midi-context (concat tempo "\n" midi-context)))
        (insert (format "\\score {\n %s \n\\midi {\n %s \n}\n}" context midi-context))
        (write-file file nil)
        (lilypond-play nil)
        (kill-buffer)))))

(defun lilypond-play (as-is)
  "Play the midi file corresponding to the current buffer.
If AS-IS is non-nil, do not compile current file first.
If region is active, play that region."
  (interactive "P")
  (setq lilypond-window-conf (current-window-configuration))
  (if (region-active-p)
      (lilypond-play-region (region-beginning) (region-end))
    (let* ((source (buffer-file-name))
           (midi (file-name-with-extension source ".midi")))
      (compile (concat
                (unless (or as-is (file-newer-than-file-p midi source))
                  (concat (lilypond--compile-command source) " && "))
                lilypond-midi-command " " midi)))))

;;;###autoload
(defun lilypond-version ()
  "Echo the current version of `lilypond-mode' in the minibuffer."
  (interactive)
  (message "Using `lilypond-mode' version %s" lilypond-version))

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
  (setq font-lock-defaults '(lilypond-font-lock-keywords
                             keywords-only)
        font-lock-multiline t
        local-abbrev-table lilypond-mode-abbrev-table)
  (setq-local paragraph-separate "^[ \t]*$")
  (setq-local paragraph-start "^[ \t]*$")
  (setq-local comment-start "%")
  (setq-local comment-start-skip "%{? *")
  (setq-local comment-end "")
  (setq-local block-comment-start "%{")
  (setq-local block-comment-end   "%}")
  (setq-local indent-line-function 'lilypond-indent-line)
  (lilypond-mode-set-syntax-table '(?\< ?\> ?\{ ?\}))
  (add-hook 'post-command-hook 'lilypond-mode-context-set-syntax-table nil t))

(provide 'lilypond-mode)
;;; lilypond-mode.el ends here
