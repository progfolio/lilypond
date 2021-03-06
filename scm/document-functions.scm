;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>
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

(use-modules
 (ice-9 regex))

(cond-expand
  (guile-2
    (use-modules (ice-9 session)
                 (ice-9 curried-definitions)))
  (else))

(define (dashify-underscores str)
  (regexp-substitute/global #f "_" str 'pre "-" 'post))

(define (format-c-header c-h)
  (regexp-substitute/global
   #f ","
   (regexp-substitute/global #f "(SCM|\\)|\\() *" (dashify-underscores c-h)
                             'pre "" 'post)
   'pre " " 'post))

;; If there is `::` in the function name, insert a breakpoint to avoid
;; overlong index entries that would otherwise stick out to the right
;; in the two-column output of the PDF documentation.
(define (document-function name arguments doc-string)
  (string-append
   "@defun " (regexp-substitute/global #f "::" (symbol->string name)
                                       'pre "::@/" 'post)
   " " arguments "\n"
   doc-string
   "\n@end defun\n\n"))

;; Map function names (as strings) to full documentation entries
;; including signature and doc-string.
(define all-primitive-function-docs-alist
  (hash-map->list
    (lambda (name header-doc-string-pair)
      (cons (symbol->string name)
            (document-function name
                              (format-c-header (car header-doc-string-pair))
                              (cdr header-doc-string-pair))))
    (ly:get-all-function-documentation)))

;; Guile 1.9 broke procedure-source, Guile 2 introduced
;; procedure-arguments (in (ice-9 session)).
(cond-expand
  (guile-2
    (define (format-scheme-signature proc)
      (let* ((signature (procedure-arguments proc))
             (required (assq-ref signature 'required))
             (optional (assq-ref signature 'optional))
             (keyword (assq-ref signature 'keyword))
             (rest (assq-ref signature 'rest)))
        (string-join
          (append
            (map
              (lambda (sym)
                (format #f "~a" sym))
              required)
            (map
              (lambda (sym)
                (format #f "[~a]" sym))
              optional)
            (map
              (lambda (pair)
                (format #f "#:~a" (car pair)))
              keyword)
            (if rest
                (list
                  (format #f ". ~a" rest))
                '()))
          " "))))
  (else
    (define (format-scheme-signature proc)
      ;; Format as "(a b . c)" and trim parentheses.
      (let ((source (procedure-source proc)))
        (if source
            (let* ((signature (cadr source))
                   (formatted-signature (format #f "~a" signature))
                   (without-lparen (string-drop formatted-signature 1))
                   (without-both-parens (string-drop-right without-lparen 1)))
              without-both-parens)
            '(...))))))

;; Same format as all-primitive-function-docs-alist.
(define all-scheme-function-docs-alist
  (let* ((module (resolve-module '(lily)))
         ;; Restrict to public functions.
         (iface (module-public-interface module))
         (alist (ly:module->alist iface)))
    (filter-map
      (lambda (entry)
        (let ((name (car entry))
              (value (cdr entry)))
          (if (and (procedure? value)
                   ;; Exclude functions that are documented through the
                   ;; C++ infrastructure (all-primitive-function-docs-alist
                   ;; above).
                   (not (hashq-get-handle (ly:get-all-function-documentation)
                                          name))
                   ;; Exclude xxx-markup functions, they are documented in NR.
                   ;; Note that make-xxx-markup is excluded by the documentation
                   ;; check (those don't have doc-strings).
                   (not (or (markup-function? value)
                            (markup-list-function? value))))
              (let* ((doc-string (procedure-documentation value)))
                ;; Many functions are publicly defined but do not need
                ;; documentation, like all Scheme-defined callbacks,
                ;; which are often self-telling and present in individual
                ;; grobs' pages.  We only retain functions that have
                ;; doc-strings.  If your pet function doesn't appear in
                ;; the internals, please give it a doc-string!
                (if doc-string
                    (cons (symbol->string name)
                          (document-function name
                                             (format-scheme-signature value)
                                             doc-string))
                  #f))
              #f)))
      alist)))

;; We want to sort without taking the ly: prefix into account.
(define (no-ly-prefix-key name-doc-alist-entry)
  (let* ((name (car name-doc-alist-entry)))
    (if (string-startswith name "ly:")
        (string-drop name 3)
        name)))

(define (all-functions-doc)
  (let* ((all-function-doc-alists
           (append
             all-primitive-function-docs-alist
             all-scheme-function-docs-alist))
         (sorted-alist
           (sort all-function-doc-alists
                 (comparator-from-key no-ly-prefix-key ly:string-ci<?)))
         (formatted-docs
           (map cdr sorted-alist)))
    (make <texi-node>
      #:name "Scheme functions"
      #:desc "Functions exported by LilyPond."
      #:text
      (string-concatenate formatted-docs))))

;; (dump-node (all-functions-doc)  (current-output-port) 0 )
