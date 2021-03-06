;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2000--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;; Jan Nieuwenhuizen <janneke@gnu.org>
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

(use-modules ((ice-9 list)
              #:select (rassoc)))

(define (sort-grob-properties props)
  ;; force 'meta to the end of each prop-list
  (let ((meta (assoc 'meta props)))
    (append (sort (assoc-remove! props 'meta) ly:alist-ci<?)
            (list meta))))

;; properly sort all properties and interfaces
;; within the all-grob-descriptions alist
(set! all-grob-descriptions
      (map!
       (lambda (grob-description)
         (let* ((grob-key      (car grob-description))
                (props         (assoc-ref all-grob-descriptions grob-key))
                (meta          (assoc-ref props 'meta))
                (interfaces    (assoc-ref meta 'interfaces))
                (sorted-ifaces (sort interfaces ly:symbol-ci<?))
                (new-meta      (assoc-set! meta 'interfaces sorted-ifaces))
                (new-props     (assoc-set! props 'meta new-meta))
                (sorted-props  (sort-grob-properties new-props)))
           (cons grob-key sorted-props)))
       all-grob-descriptions))

;; sort all grobs in the all-grob-descriptions alist
(set! all-grob-descriptions (sort all-grob-descriptions ly:alist-ci<?))

(define (interface-doc-string interface grob-description)
  (let* ((name (car interface))
         (desc (cadr interface))
         (props (caddr interface))
         (docfunc (lambda (pr)
                    (property->texi
                     'backend pr grob-description)))
         (iprops (filter (lambda (x) (object-property x 'backend-internal))
                         props))
         (uprops (filter
                  (lambda (x) (not (object-property x 'backend-internal)))
                  props))
         (user-propdocs (map docfunc uprops))
         (internal-propdocs (map docfunc iprops)))

    (string-append
     desc

     (if (pair? uprops)
         (string-append
          "\n\n@subsubheading User settable properties:\n"
          (description-list->texi user-propdocs #t))
         "")

     (if (pair? iprops)
         (string-append
          "\n\n@subsubheading Internal properties:\n"
          (description-list->texi internal-propdocs #t))
         ""))))

(define iface->grob-table (make-hash-table 61))
;; extract ifaces, and put grob into the hash table.
(for-each
 (lambda (x)
   (let* ((meta (assoc-get 'meta (cdr x)))
          (ifaces (assoc-get 'interfaces meta)))

     (for-each (lambda (iface)
                 (hashq-set!
                  iface->grob-table iface
                  (cons (car x)
                        (hashq-ref iface->grob-table iface '()))))
               ifaces)))
 all-grob-descriptions)

(define class-specific-interfaces
  '((Item . item-interface)
    (Spanner . spanner-interface)
    (Paper_column . paper-column-interface)
    (System . system-interface)))

;; First level Interface description
(define (interface-doc interface)
  (let* ((name (symbol->string (car interface)))
         (interface-list (human-listify
                          (map ref-ify
                               (sort
                                (map symbol->string
                                     (hashq-ref iface->grob-table
                                                (car interface)
                                                '()))
                                ly:string-ci<?)))))
    (make <texi-node>
      #:code-tag #t
      #:name name
      #:text (string-append
              (interface-doc-string (cdr interface) '())
              "\n\n@raggedRight\n"
              "This grob interface "
              (if (not (equal? interface-list "none"))
                  (format #f
                          "is used in the following graphical object(s): ~a"
                          interface-list)
                  (let ((class (rassoc (car interface)
                                       class-specific-interfaces
                                       eq?)))
                    (if class
                        (format
                          #f
                          "is added dynamically to grobs of class @code{~a}"
                          (car class))
                        "is not used in any graphical object")))
              "."
              "\n@endRaggedRight"))))

(define (grob-alist->texi alist)
  (let* ((uprops (filter (lambda (x) (not (object-property x 'backend-internal)))
                         (map car alist))))

    (description-list->texi
     (map (lambda (y) (property->texi 'backend y alist))
          uprops)
     #t)))

(define (grob-doc description)
  "Given a property alist DESCRIPTION, make a documentation
node."

  (let* ((meta (assoc-get 'meta description))
         (name (assoc-get 'name meta))
         (ifaces (map lookup-interface (assoc-get 'interfaces meta)))
         (ifacedoc (map ref-ify
                        (sort
                         (map (lambda (iface)
                                (if (pair? iface)
                                    (symbol->string (car iface))
                                    (ly:error (_ "pair expected in doc ~s") name)))
                              ifaces)
                         ly:string-ci<?)))
         (classes (assoc-get 'classes meta))
         (class-list (map
                       (lambda (class)
                         (ref-ify
                           (symbol->string
                             (assoc-get class class-specific-interfaces))
                           (symbol->string class)))
                       classes))
         (engravers (filter
                     (lambda (x) (engraver-makes-grob? name x))
                     all-engravers-list))
         (namestr (symbol->string name))
         (engraver-names (map symbol->string
                              (map ly:translator-name engravers)))
         (engraver-list (human-listify
                         (map ref-ify
                              (map engraver-name engraver-names)))))

    (make <texi-node>
      #:code-tag #t
      #:name namestr
      #:text
      (string-append
       "@raggedRight\n"
       "@code{"
       namestr "} objects "
       (if (equal? engraver-list "none")
           "are not created by any engraver"
           (string-append
            "are created by: "
            engraver-list))
       "."
       "\n@endRaggedRight"

       "\n\nStandard settings:\n"
       (grob-alist->texi description)
       "\n\n@raggedRight\n"
       "This object supports the following interface(s):\n"
       (human-listify ifacedoc)
       ".\n\n"
       (if (eqv? 1 (length class-list))
           (format #f "This object is of class ~a." (first class-list))
           (format #f
                   "This object can be of either of the following classes: ~a."
                   (human-listify class-list)))
       "\n\n@endRaggedRight"))))

(define (all-grobs-doc)
  (make <texi-node>
    #:name "All layout objects"
    #:desc "Description and defaults for all graphical objects (grobs)."
    #:children
    (map (lambda (x) (grob-doc (cdr x)))  all-grob-descriptions)))

(define interface-description-alist
  (hash-fold
   (lambda (key val prior)
     (cons (cons key val)  prior))
   '() (ly:all-grob-interfaces)))

;; sort user-settable and internal props within each grob-interface
(set! interface-description-alist
      (map! (lambda (iface-desc)
              (let* ((key-name-docstr (list-head iface-desc 3))
                     (props           (list-tail iface-desc 3))
                     (sorted-props    (list (sort (car props) ly:symbol-ci<?))))
                (append key-name-docstr sorted-props)))
            interface-description-alist))

;; sort list of grob interfaces
(set! interface-description-alist
      (sort interface-description-alist ly:alist-ci<?))

;;;;;;;;;; check for dangling backend properties.
(define (mark-interface-properties entry)
  (for-each (lambda (x) (set-object-property! x 'iface-marked #t))
            (caddr (cdr entry))))

(for-each mark-interface-properties interface-description-alist)

(define (check-dangling-properties prop)
  (if (not (object-property prop 'iface-marked))
      (ly:error (string-append "define-grob-properties.scm: "
                               (_ "cannot find interface for property: ~S")) prop)))

(for-each check-dangling-properties all-backend-properties)

;;;;;;;;;;;;;;;;

(define (lookup-interface name)
  (let* ((entry (hashq-ref (ly:all-grob-interfaces) name #f)))
    (if entry
        entry
        (ly:error (_ "unknown Grob interface: ~S") name))))

(define (all-interfaces-doc)
  (make <texi-node>
    #:name "Graphical Object Interfaces"
    #:desc "Building blocks of graphical objects."
    #:children
    (map interface-doc interface-description-alist)))

(define (backend-properties-doc-string lst)
  (let* ((ps (sort (map symbol->string lst) ly:string-ci<?))
         (descs (map (lambda (prop)
                       (property->texi 'backend (string->symbol prop) '())) ps))
         (texi (description-list->texi descs #f)))
    texi))

;;(dump-node (grob-doc (cdadr all-grob-descriptions)) (current-output-port) 0 )
(define (backend-doc-node)
  (make <texi-node>
    #:name "Backend"
    #:desc "Reference for the layout engine."
    #:children
    (list
     (all-grobs-doc)
     (all-interfaces-doc)
     (make <texi-node>
       #:name "User backend properties"
       #:desc "All tunable properties in a big list."
       #:text (backend-properties-doc-string all-user-grob-properties))
     (make <texi-node>
       #:name "Internal backend properties"
       #:desc "All internal layout properties in a big list."
       #:text (backend-properties-doc-string all-internal-grob-properties)))))
