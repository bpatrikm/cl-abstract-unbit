;;Copyright 2017 Patrik Magnusson

;;This file is part of cl-abstract-unbit.

;;cl-abstract-unbit is free software: you can redistribute it and/or modify
;;it under the terms of the GNU General Public License as published by
;;the Free Software Foundation, either version 3 of the License, or
;;(at your option) any later version.

;;cl-abstract-unbit is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU General Public License for more details.

;;You should have received a copy of the GNU General Public License
;;along with cl-abstract-unbit.  If not, see <http://www.gnu.org/licenses/>.

(in-package :read-per)

(defmethod read-per-constrained-whole (lb ub read-indefinite (bit-reader bit-reader-aligned))
  (let ((range (+ 1 (- ub lb))))
    (+ lb
       (cond ((= 1 range)
              0)
             ((> 256 range)
              (read-bits (ceiling (log range 2)) bit-reader))
             ((= 256 range)
              (pre-align-read-bits 8 bit-reader))
             ((> 65537 range) ;64K+1
              (pre-align-read-bits 16 bit-reader))
             (t
              (read-octets-unsigned (funcall read-indefinite)))))))

(defmethod read-per-constrained-whole (lb ub read-indefinite (bit-reader bit-reader-unaligned))
  (declare (ignore read-indefinite))
  (if (= ub lb)
      ub
      (let ((range (+ 1 (- ub lb))))
        (+ lb
           (read-bits (ceiling (log range 2)) bit-reader)))))

(defun read-per-normally-small-non-negative (bit-reader)
  (let ((size-indicator (read-bits 1 bit-reader)))
    (if (= size-indicator 0)
        (read-bits 6 bit-reader)
        (read-per-semi-constrained-whole 0
                                             (lambda ()
                                               (read-per-general-length-unconstrained :octet bit-reader nil))))))

(defun read-per-semi-constrained-whole (lb read-indefinite)
  (+ lb (read-octets-unsigned (funcall read-indefinite))))

(defun read-per-unconstrained (bit-reader read-indefinite)
  (when (typep bit-reader 'bit-reader-aligned) (align bit-reader));;TODO pre-align?
  (nread-octets-signed (funcall read-indefinite)))

(defun read-per-general-length-normally-small (n-units-reader bit-reader)
  (let ((size-indicator (read-bits 1 bit-reader)))
    (if (= 0 size-indicator)
        (let ((length (+ 1 (read-bits 6 bit-reader))))
          (funcall n-units-reader length nil))
        (read-per-general-length-unconstrained n-units-reader bit-reader nil))))

(defun read-per-general-length-constrained (lb ub n-units-reader bit-reader &optional align-unfragmented-p)
  (if (< ub 65536)
      (let ((length (read-per-constrained-whole lb ub #'error bit-reader)))
        (when (> length 0)
          (when (and align-unfragmented-p (typep bit-reader 'bit-reader-aligned)) (align bit-reader))
          (funcall n-units-reader length nil)))
      (read-per-general-length-unconstrained n-units-reader bit-reader nil)))

(defun read-per-general-length-unconstrained (n-units-reader bit-reader previous-fragments)
  (when (typep bit-reader 'bit-reader-aligned) (align bit-reader));TODO pre-align?
  (let ((size-indicator (read-bits 1 bit-reader)))
    (if (= size-indicator 0)
        (let ((length (read-bits 7 bit-reader)))
          (funcall n-units-reader length previous-fragments))
        (let ((size-indicator (read-bits 1 bit-reader)))
          (if (= size-indicator 0)
              (let ((length (read-bits 14 bit-reader)))
                (funcall n-units-reader length previous-fragments))
              (let ((length (* 16384 (read-bits 6 bit-reader))))
                (read-per-general-length-unconstrained
                 n-units-reader
                 bit-reader
                 (funcall n-units-reader length previous-fragments))))))))

(defun read-per-boolean (bit-reader)
  (= 1 (read-bits 1 bit-reader)))

(defun read-per-integer (lb ub extensible-p bit-reader)
  (flet ((n-units-reader (length previous-fragments)
           (read-per-n-units length :octet bit-reader previous-fragments)))
    (let ((extended-p (when extensible-p
                        (= 1 (read-bits 1 bit-reader)))))
      (cond ((and (not extended-p)
                  lb ub (= lb ub))
             lb)
            ((and (not extended-p)
                  lb ub)
             (let ((range-ub (ceiling (/ (log ub 2) 8))))
               (read-per-constrained-whole lb ub
                                               (lambda ()
                                                 (read-per-general-length-constrained
                                                  1 range-ub #'n-units-reader bit-reader t))
                                               bit-reader)))
            ((and (not extended-p)
                  lb)
             (read-per-semi-constrained-whole lb (lambda ()
                                                       (read-per-general-length-unconstrained
                                                        #'n-units-reader bit-reader nil))))
            (t
             (read-per-unconstrained bit-reader (lambda ()
                                                      (read-per-general-length-unconstrained
                                                       #'n-units-reader bit-reader nil))))))))

;;caller must sort root-items
(defun read-per-enumerated (max-root-index extensible-p bit-reader)
  (if (when extensible-p
        (= 1 (read-bits 1 bit-reader)))
      (read-per-normally-small-non-negative bit-reader)
      (read-per-integer 0 max-root-index nil bit-reader)))

;;no constraints are PER-visible
(defmethod read-per ((pt parse-enumerated-type) textual-context invokations constraints assign-ht bit-reader)
  (declare (ignore constraints assign-ht))
  (read-per-enumerated (- (length (root pt)) 1) (extensible-p pt) bit-reader))
  
;;TODO named bits
;;only length constraints are PER-visible
(defun read-per-bitstring (length-lb length-ub length-extensible-p bit-reader)
  (flet ((n-units-reader (length previous-fragments)
           (read-per-n-units length :bit bit-reader previous-fragments)))
    (let ((extended-p (when length-extensible-p
                        (= 1 (read-bits 1 bit-reader)))))
      (when extended-p (error "TODO extended bitstring")))
    (cond ((and length-ub
                (= 0 length-ub))
           nil)
          ((and length-lb length-ub
                (= length-lb length-ub)
                (< length-lb 17))
           (read-per-n-units length-lb :bit bit-reader nil))
          ((and length-lb length-ub
                (= length-lb length-ub)
                (< length-lb 65536))
           (when (typep bit-reader 'bit-reader-aligned) (align bit-reader))
           (read-per-n-units length-lb :bit bit-reader nil))
          (length-ub
           (when (typep bit-reader 'bit-reader-aligned) (align bit-reader))
           (read-per-general-length-constrained (or length-lb 0) length-ub #'n-units-reader bit-reader))
          (t
           (when (typep bit-reader 'bit-reader-aligned) (align bit-reader))
           (read-per-general-length-unconstrained #'n-units-reader bit-reader nil)))))

;;only length constraints are PER-visible
(defun read-per-octetstring (length-lb length-ub length-extensible-p bit-reader)
  (flet ((n-units-reader (length previous-fragments)
           (read-per-n-units length :octet bit-reader previous-fragments)))
    (let ((extended-p (when length-extensible-p
                        (= 1 (read-bits 1 bit-reader)))))
      (when extended-p (error "TODO extended octetstring")))
    (cond ((and length-ub
                (= 0 length-ub))
           nil)
          ((and length-lb length-ub
                (= length-lb length-ub)
                (< length-lb 3))
           (read-per-n-units length-lb :octet bit-reader nil))
          ((and length-lb length-ub
                (= length-lb length-ub)
                (< length-lb 65536))
           (when (typep bit-reader 'bit-reader-aligned) (align bit-reader))
           (read-per-n-units length-lb :octet bit-reader nil))
          (length-ub
           (read-per-general-length-constrained (or length-lb 0) length-ub #'n-units-reader bit-reader t))
          (t
           (read-per-general-length-unconstrained #'n-units-reader bit-reader nil)))))

(defmethod read-per ((pt parse-bit-string) textual-context invokations constraints assign-ht bit-reader)
  (declare (ignore textual-context invokations))
  (assert (typep assign-ht 'assign-ht))
  (let ((lb (lower-bound-by-constraint constraints assign-ht))
        (ub (upper-bound-by-constraint constraints assign-ht))
        (extensible-p (extensible-size-constraints-p constraints)))
    (read-per-bitstring lb ub extensible-p bit-reader)))

(defmethod read-per ((pt symbol) textual-context invokations constraints assign-ht bit-reader)
  (declare (ignore textual-context invokations))
  (assert (typep assign-ht 'assign-ht))
  (let ((lb (lower-bound-by-constraint constraints assign-ht))
        (ub (upper-bound-by-constraint constraints assign-ht))
        (extensible-p (extensible-size-constraints-p constraints)))
    (case pt
      (:boolean (read-per-boolean bit-reader))
      (:integer (read-per-integer lb ub extensible-p bit-reader))
      (:bit-string (read-per-bitstring lb ub extensible-p bit-reader))
      (:octet-string (read-per-octetstring lb ub extensible-p bit-reader))
      (:null nil)
      (t (error "missing symbol-case or method")))))
                     

(defun read-per-component-presence (component-presence-unknown bit-reader)
  (let ((number-of-bits (reduce #'+ (mapcar (lambda (component-unknown)
                                              (if component-unknown 1 0))
                                            component-presence-unknown))))
    (let ((presence-bit-reader
           (unless (eql 0 number-of-bits)
             (make-instance
              'bit-reader-unaligned-bitstring
              :fragments
              (read-per-general-length-constrained number-of-bits number-of-bits
                                                   (lambda (length previous-fragments)
                                                     (read-per-n-units length :bit bit-reader previous-fragments))
                                                   bit-reader)))))
      (mapcar (lambda (cpu)
                (if cpu
                    (= 1 (read-bits 1 presence-bit-reader))
                    t))
              component-presence-unknown))))

(defmethod read-per ((pt parse-sequence-type) textual-context invokations constraints assign-ht bit-reader)
  (assert (typep assign-ht 'assign-ht))
  (let ((extended-p (when (extensible-p pt)
                      (= 1 (read-bits 1 bit-reader)))))
    (when extended-p (error "TODO extensible sequence")))
  ;;component-presence is t for those components that should be read from the stream
  ;;recurse the reader into each of those
  ;;for each recursion, the textual-context needs to end with data for this current level, with results so far; this is needed to evaluate if table constraints resolve open types to known types
  ;;(there is also an "internal" recursion which iterates over the components)
  (labels ((read-per-w (components presences defaults ongoing-data-parse)
             (if (not components)
                 ongoing-data-parse;terminate internal recursion
                 (let ((component (car components))
                       (presence (car presences))
                       (default (car defaults)))
                   (let ((ongoing-data-parse
                          (or
                           (when (or presence default)
                             (make-instance
                              'data-parse-sequence
                              :syntax pt
                              :data (append (data ongoing-data-parse)
                                            (list
                                             (make-instance
                                              'data-parse-sequence-component
                                              :name (c-name component)
                                              :syntax component
                                              :data (or (when presence
                                                          (read-per (c-type component)
                                                                        (append textual-context
                                                                                (list
                                                                                 ongoing-data-parse))
                                                                        invokations
                                                                        constraints
                                                                        assign-ht
                                                                        bit-reader))
                                                        default))))))
                           ongoing-data-parse)))
                     (read-per-w (cdr components) (cdr presences) (cdr defaults) ongoing-data-parse))))))
    (let ((presences (read-per-component-presence (mapcar #'optional-p (components pt)) bit-reader))
          (defaults (mapcar (lambda (component)
                              (when (default-value component)
                                (read-value (default-value component) component assign-ht)))
                            (components pt))))
      (read-per-w (components pt) presences defaults (make-instance 'data-parse-sequence
                                                                        :syntax pt
                                                                        :data nil)))))

(defmethod read-per ((pt parse-sequence-of-type) textual-context invokations constraints assign-ht bit-reader)
  (assert (typep assign-ht 'assign-ht))
  (unless constraints (error "TODO unconstrained sequence"))
  ;;TODO extensible-p (which comes from constraint)
  ;;this level is not useful for table constraints, so add dummy
  (flet ((n-units-reader (length previous-fragments)
           (read-per-n-types length (component-type pt) (append textual-context (list :dummy)) invokations assign-ht bit-reader previous-fragments)))
    (let ((lb (lower-bound-by-constraint constraints assign-ht))
          (ub (upper-bound-by-constraint constraints assign-ht)))
      (let ((components
             (cond ((and lb ub (= lb ub) (< ub 645536))
                    (read-per-n-types ub (component-type pt) (append textual-context (list :dummy)) invokations assign-ht bit-reader nil))
                   ((and ub (< ub 645536))
                    (read-per-general-length-constrained (or lb 0) ub #'n-units-reader bit-reader))
                   (t
                    (read-per-general-length-unconstrained #'n-units-reader bit-reader nil)))))
        (make-instance 'data-parse-sequence-of
                       :type-name (when (typep (component-type pt) 'parse-named-type) ;eller parse-component-type 
                                    (c-name (component-type pt)))
                       :syntax pt
                       :data components)))))

;;no constraints are PER-visible
(defmethod read-per ((pt parse-choice-type) textual-context invokations constraints assign-ht bit-reader)
  (assert (typep assign-ht 'assign-ht))
  (when constraints (error "TODO constrained choice"))
  ;;TODO canonical sorting
  (let ((extended-p (when (extensible-p pt)
                      (= 1 (read-bits 1 bit-reader)))))
    (when extended-p (error "TODO extended")))
  (let ((chosen-component
         (let ((number-of-alternatives (- (length (root pt)) 1)))
           (elt (root pt)
                (read-per-integer 0 number-of-alternatives nil bit-reader)))))
    (make-instance 'data-parse-choice
                   :choice-name (first chosen-component)
                   :syntax pt
                   :data (read-per (second chosen-component) (append textual-context (list :dummy)) invokations nil assign-ht bit-reader))))

;;Ha en invokations-kontext parallellt med textual-context. Gör ingen översättning direkt vid dyk ner i en invokation, bara lägg den på listan och dyk ner i type-template. Vid dereferens, kolla invokations-kontext inifrån och ut. Vid en träff, betrakta det som ett uppslag ur assign-ht (vilket också är den högsta nivån). Alltså om det ger en naken referens, så dereferera i sin tur den, men ifall det är en komplicerad typ gör read-per. Notera när man väl upp till en viss nivå i invokations-kontexten, så skall man vid vidare dereferering början på nivån ovanför den - anropet till read-per skall alltså ha en reducerad invokations-kontext. Poängen är att om man i den näst högsta nivån har en komplicerad typ som är skriven i termer av en parameter från den högsta nivån, så skulle den kunna skuggas i lägre nivåer - men då är det fel referens man får tag i.
;;Det blir mest praktisk om invokations-kontexten är en baklänges-lista - så att den högsta nivån är det sista elementet.

;;alltid när man slår upp en typ-/värdereferens kan det vara så att man befinner sig i en type-template, tillhörande en parametriserad type - och man måste översätta sin referens genom anropet till den invokationen. Men se kommentar i parse-high-parameter
(defmethod read-per ((pt parse-parameterized-type-invokation) textual-context invokations constraints assign-ht bit-reader)
  (let ((invoked-type (make-invoked-parameterized-type (parameterized-type pt)
                                                       (actual-parameter-list pt))))
    (read-per (type-template invoked-type) nil (cons invoked-type invokations) constraints assign-ht bit-reader)))

;;lambda av dessa tar length och previous-fragments som argument
(defun read-per-n-units (length unit-syntax bit-reader previous-fragments)
  (case unit-syntax
    (:bit (let ((fragment (make-array (list (ceiling (/ length 8))) :element-type '(unsigned-byte 8))))
            (dotimes (i (floor (/ length 8)))
              (setf (aref fragment i) (read-bits 8 bit-reader)))
            (let ((remaining-bits-in-fragment (mod length 8)))
              (unless (eql remaining-bits-in-fragment 0)
                (setf (aref fragment (floor (/ length 8)))
                      (* (read-bits remaining-bits-in-fragment bit-reader)
                         (expt 2 (- 8 remaining-bits-in-fragment))))));;flytta upp bitarna, så att de oanvända hamnar sist
            (append previous-fragments (list fragment))))
    (:octet (let ((octet-array (or (when previous-fragments
                                     (setf (fill-pointer previous-fragments)
                                           (+ length (length previous-fragments)))
                                     previous-fragments)
                                   (make-array (list length) :fill-pointer t :element-type '(unsigned-byte 8)))))
              (dotimes (i length)
                (setf (aref octet-array (+ (- (length octet-array) length) i)) (read-bits 8 bit-reader)))
              octet-array))
    (t (error "probably use read-per-n-types instead"))))

(defun read-per-n-types (length unit-syntax textual-context invokations assign-ht bit-reader previous-fragments)
  (assert (typep assign-ht 'assign-ht))
  (let (results)
    (dotimes (i length)
      (push (read-per unit-syntax textual-context invokations nil assign-ht bit-reader) results))
    (append previous-fragments (nreverse results))))

(defun read-per-open-type (known-type invokations assign-ht bit-reader)
  (flet ((n-units-reader (length previous-fragments)
           (read-per-n-units length :octet bit-reader previous-fragments)))
    ;;open type is in octets, so we inherit the alignment from general-length-unconstrained
    (let ((type-data-octets (read-per-general-length-unconstrained
                             #'n-units-reader bit-reader nil)))
      (if known-type
          (let ((open-bit-reader
                 (make-instance
                  (if (typep bit-reader 'bit-reader-aligned);;TODO, this is only the simplest case
                      'bit-reader-aligned-bitstring
                      'bit-reader-unaligned-bitstring)
                  :fragments (list type-data-octets))))
            (read-per known-type nil invokations nil assign-ht open-bit-reader))
          type-data-octets))))

(defmethod read-per ((pt parse-object-class-field-type) textual-context invokations constraints assign-ht bit-reader)
  ;;lookup the class field, do following if type field - otherwise, recursive call on (known) type directly;
  ;;for type fields, check if a table-constraint limits to a single type
  (let* ((object-class-fields
          (get-dereferenced-class (reference (class-name pt)) (object-class-definition-ht assign-ht) invokations))
         (object-class-field (find (car (name-path (field-name pt))) object-class-fields :key #'field-name)))
    ;;TODO, field-name och class-name är komplicerade
    (if (typep object-class-field 'object-class-type-field);TODO variable type fields
        (read-per-open-type (resolve-open-type (field-name pt) textual-context invokations constraints assign-ht)
                        invokations assign-ht bit-reader)
        (read-per (c-type object-class-field) textual-context invokations (remove-table-constraints constraints) assign-ht bit-reader))))

(defmethod read-per ((pt parse-constrained-type) textual-context invokations constraints assign-ht bit-reader)
  (assert (typep assign-ht 'assign-ht))
  (read-per (c-type pt) textual-context invokations (combine-constraints constraints (constraint pt)) assign-ht bit-reader))

(defmethod read-per ((pt parse-type-by-reference) textual-context invokations constraints assign-ht bit-reader)
  (declare (ignore textual-context))
  (if invokations
      (labels ((w (parameter-list parameter-analysis-list actual-parameter-list)
                 (if (not parameter-list)
                     (read-per pt nil (cdr invokations) constraints assign-ht bit-reader)
                     ;;if reference matches anything other than a type (not governor for value/value-set), then it is an error for this method to have been called
                     (cond ((eql :dummy (cdr (car parameter-analysis-list)))
                            ;;two actual parameters consumed by parameter
                            (when (or (eql (reference pt) (cdr (first (car parameter-list))))
                                      (eql (reference pt) (cdr (second (car parameter-list)))))
                              (error "only types can be read from bit-stream"))
                            (w (cdr parameter-list) (cdr parameter-analysis-list) (cddr actual-parameter-list)))
                           ((eql :type (car (car parameter-analysis-list)))
                            (if (eql (reference pt) (cdr (car parameter-list)))
                                (progn ;(print (reference pt))
                                  (read-per (car actual-parameter-list) nil (cdr invokations) constraints assign-ht bit-reader))
                                (w (cdr parameter-list) (cdr parameter-analysis-list) (cdr actual-parameter-list))))
                           ((cdr (car parameter-analysis-list))
                            (when (eql (reference pt) (cdr (second (car parameter-list))))
                              (error "only types can be read from bit-stream"))
                            (w (cdr parameter-list) (cdr parameter-analysis-list) (cdr actual-parameter-list)))
                           (t
                            (when (eql (reference pt) (cdr (car parameter-list)))
                              (error "only types can be read from bit-stream"))
                            (w (cdr parameter-list) (cdr parameter-analysis-list) (cdr actual-parameter-list)))))))
        (w (parameter-list (car invokations))
           (parameter-analysis-list (car invokations))
           (actual-parameter-list (car invokations))))
      (progn ;(print (reference pt))
        (read-per (gethash (reference pt) (type-assignment-ht assign-ht)) nil nil constraints assign-ht bit-reader))))
      
(defmethod read-per ((pt parse-named-type) textual-context invokations constraints assign-ht bit-reader)
  ;;sequence/choice picks up the name, so this is a transparent method
  (read-per (c-type pt) textual-context invokations constraints assign-ht bit-reader))


;;ännu ingen översättning av parametrisering i get-value-by-reference
