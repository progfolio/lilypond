;;; lyre-mode.el --- Rewrite of lilypond mode.        -*- lexical-binding: t; -*-

;; Copyright (C) 2021

;; Author:  <n@nbook>
;; Keywords: tools

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
(declare-function lilypond-mode "lilypond-mode")

(define-derived-mode lyre-mode lilypond-mode "lyre"
  "A new major mode for editing lilypond files."
  (lyre-hide-comp-mode 1))

(defvar lyre-window-conf nil "Used to store window conf.")

(defun lyre-hide-successful-compile (buffer string)
  "Hide successful compilation BUFFER.
STRING indicates compilation status."
  (with-current-buffer buffer
    (when (and
           (string-match "compilation" (buffer-name buffer))
           (string-match "finished" string)
           (not (save-excursion (goto-char (point-min))
                                (re-search-forward "warning" nil 'noerror))))
      (bury-buffer buffer)
      (when lyre-window-conf
        (set-window-configuration lyre-window-conf)))))

(define-minor-mode lyre-hide-comp-mode "Auto hide successful compilation buffers."
  :lighter " lyhc"
  (if  lyre-hide-comp-mode
      (add-hook 'compilation-finish-functions #'lyre-hide-successful-compile)
    (remove-hook 'compilation-finish-functions #'lyre-hide-successful-compile)))

(defvar lyre-compile-command "lilypond" "Command used to copmile lilypond files.")
(defun lyre--compile-command ()
  "Return the lilypond compile command."
  (concat lyre-compile-command " " (shell-quote-argument (buffer-file-name))))

(defun lyre-compile ()
  "Compile the current lilypod file."
  (interactive)
  (setq lyre-window-conf (current-window-configuration))
  (compile (lyre--compile-command)))

(defvar lyre-midi-command "timidity" "Command to play a midi file.")

(defun lyre-play (as-is)
  "Play the current midi file.
If AS-IS is non-nil, do not compile current file first."
  (interactive "P")
  (setq lyre-window-conf (current-window-configuration))
  (let* ((source (buffer-file-name))
         (midi (file-name-with-extension source ".midi")))
    (compile (concat
              (unless (or as-is (file-newer-than-file-p midi source))
                (concat (lyre--compile-command) " && "))
              lyre-midi-command " " midi))))

(provide 'lyre-mode)
;;; lyre-mode.el ends here
