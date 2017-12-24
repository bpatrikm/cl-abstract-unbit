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

(in-package :read-general)

(defun combine-constraints (old-constraints new-constraint)
  (cons new-constraint old-constraints))

(defun get-table-constraint (constraints)
  (find-if (lambda (constraint)
             (typep constraint 'parse-table-constraint))
           constraints))

(defun remove-table-constraints (constraints)
  (remove-if (lambda (constraint)
               (typep constraint 'parse-table-constraint))
             constraints))

(defun get-dereferenced-class (reference object-class-definition-ht invokations)
  (if (not invokations);man skulle kunna avbryta tidigare baserat på om reference är en :objectclassreference, men eftersom det försvunnit vid anropet utifrån, är det lättare att bara strunta i den infon
      (gethash reference object-class-definition-ht)
      (labels ((w (parameter-list parameter-analysis-list actual-parameter-list)
                 (if (not parameter-list)
                     (get-dereferenced-class reference object-class-definition-ht (cdr invokations))
                     (cond ((eql :dummy (cdr (car parameter-analysis-list)))
                            (when (or (eql reference (cdr (first (car parameter-list))))
                                      (eql reference (cdr (second (car parameter-list)))))
                              (error "should be class name/not governor (1)"))
                            (w (cdr parameter-list) (cdr parameter-analysis-list) (cddr actual-parameter-list)))
                           ((cdr (car parameter-analysis-list))
                            (when (eql reference (cdr (second (car parameter-list))))
                              (error "should be class name/not governor (2)"))
                            (w (cdr parameter-list) (cdr parameter-analysis-list) (cdr actual-parameter-list)))
                           ((not (eql :class-name (car (car parameter-analysis-list))))
                            (when (eql reference (cdr (car parameter-list)))
                              (error "should be class name/not governor (3)"))
                            (w (cdr parameter-list) (cdr parameter-analysis-list) (cdr actual-parameter-list)))
                           ((eql reference (cdr (car parameter-list)))
                            (get-dereferenced-class  (cdr (car actual-parameter-list)) object-class-definition-ht (cdr invokations)))
                           (t
                            (w (cdr parameter-list) (cdr parameter-analysis-list) (cdr actual-parameter-list)))))))
        (w (parameter-list (car invokations))
           (parameter-analysis-list (car invokations))
           (actual-parameter-list (car invokations))))))

(defun get-dereferenced-value (value value-assignments)
  (assert value)
  (cond ((numberp value)
         value)
        ((and (consp value)
              (numberp (cdr value)))
         (cdr value))
        ((consp value)
         (get-dereferenced-value (gethash (cdr value) value-assignments) value-assignments))
        (t
         (get-dereferenced-value (gethash value value-assignments) value-assignments))))

(defmethod lower-bound-by-constraint ((constraint parse-size-constraint) assign-ht)
  (assert (typep assign-ht 'assign-ht))
  (get-dereferenced-value (lower-bound constraint) (value-assignment-ht assign-ht)))

(defmethod upper-bound-by-constraint ((constraint parse-size-constraint) assign-ht)
  (assert (typep assign-ht 'assign-ht))
  (get-dereferenced-value (upper-bound constraint) (value-assignment-ht assign-ht)))

(defun filter-size-constraints-root (constraints)
  (reduce #'append (mapcar (lambda (constraint)
                             (cond ((listp constraint)
                                    (filter-size-constraints-root constraint))
                                   ((typep constraint 'parse-element-set-union)
                                    (filter-size-constraints-root (subsets constraint)))
                                   ((typep constraint 'parse-element-set-specs)
                                    (filter-size-constraints-root (list (root constraint))))
                                   ((typep constraint 'parse-size-constraint)
                                    (list constraint))
                                   (t nil)))
                           constraints)))

(defmethod lower-bound-by-constraint (constraints assign-ht)
  (assert (typep assign-ht 'assign-ht))
  (let ((lower-bounds (mapcar (lambda (constraint)
                                (lower-bound-by-constraint constraint assign-ht))
                              (filter-size-constraints-root constraints))))
    (or (when (and lower-bounds (cdr lower-bounds))
          (reduce #'max lower-bounds))
        (when lower-bounds (car lower-bounds)))))

(defmethod upper-bound-by-constraint (constraints assign-ht)
  (assert (typep assign-ht 'assign-ht))
  (let ((upper-bounds (mapcar (lambda (constraint)
                                (upper-bound-by-constraint constraint assign-ht))
                              (filter-size-constraints-root constraints))))
    (or (when (and upper-bounds (cdr upper-bounds))
          (reduce #'min upper-bounds))
        (when upper-bounds (car upper-bounds)))))

(defun extensible-size-constraints-p (constraints)
  (let ((constraints
         (labels ((w (constraints)
                    (reduce #'append
                            (mapcar (lambda (constraint)
                                      (cond ((listp constraint)
                                             (w constraint))
                                            ((typep constraint 'parse-element-set-union)
                                             (w (subsets constraint)))
                                            ((and (typep constraint 'parse-element-set-specs)
                                                  (typep (root constraint) 'parse-size-constraint))
                                             (list constraint))
                                            ((typep constraint 'parse-element-set-specs)
                                             (list (root constraint)))
                                            (t nil)))
                                    constraints))))
           (w constraints))))
    (when (> (length constraints) 1)
      (error "TODO set arithmetic"))
    (when constraints
      (extensible-p (car constraints)))))

;;för sequence-value, måste man veta om en komponent är en enumerated-type, det värdet finns inte i value-assignments. Det framgår bara av motsvarande sequence, inte av sequence-value.
;;t ex (c-type object-class-instance-fixed-type-value-field), ifall det är därifrån värdet också kommer
;;läsa in värdestrukturer m h a parse-tree är analogt med att läsa en bit-ström.
(defmethod read-value ((vpt parse-sequence-value) (pt parse-sequence-type) assign-ht)
  (make-instance 'data-parse-sequence
                 :syntax pt
                 :data (mapcar (lambda (value-component)
                                 (let ((type-component (find (c-name value-component)
                                                             (components pt)
                                                             :key #'c-name)))
                                   (unless type-component (error "value reader find type"))
                                   (read-value value-component type-component assign-ht)))
                               (components vpt))))

(defmethod read-value ((vpt parse-named-value) (pt parse-component-type) assign-ht)
  (make-instance 'data-parse-sequence-component
                 :name (c-name vpt)
                 :syntax pt
                 :data (read-value (c-value vpt) (c-type pt) assign-ht)))

(defmethod read-value (vpt (pt parse-enumerated-type) assign-ht)
  (declare (ignore assign-ht))
  (position vpt (append (root pt) (additional pt))))

(defmethod read-value (vpt (pt parse-type-by-reference) assign-ht)
  (read-value vpt (gethash (reference pt) (type-assignment-ht assign-ht)) assign-ht))

(defmethod read-value (vpt (pt parse-constrained-type) assign-ht)
  (read-value vpt (c-type pt) assign-ht))

(defmethod read-value (vpt pt assign-ht)
  (declare (ignore pt))
  (get-dereferenced-value vpt (value-assignment-ht assign-ht)))

(defmethod value-eql ((v1 data-parse-sequence) (v2 data-parse-sequence))
  (and (eql (length (data v1)) (length (data v2)))
       (every (lambda (v1-comp)
                (let ((v2-comp (find (name v1-comp) (data v2) :key #'name)))
                  (and v2-comp
                       (value-eql (data v1-comp) (data v2-comp)))))
              (data v1))))

(defmethod value-eql (v1 v2)
  (equal v1 v2))


(defun read-octets-unsigned (octet-array)
  (let ((sum 0)
        (factor (length octet-array)))
    (dotimes (i (length octet-array))
      (incf sum
            (* (aref octet-array i) (expt 256 (decf factor)))))
    sum))

(defun nread-octets-signed (octet-array)
  (let ((leading-bit 0)
        (length (length octet-array)))
    (setf (ldb (byte 1 0) leading-bit)
          (ldb (byte 1 7) (aref octet-array 0))
          (ldb (byte 1 7) (aref octet-array 0))
          (ldb (byte 1 0) 0))
    (- (read-octets-unsigned octet-array)
       (expt 2 (* 8 length)))))

;;textual-context byggs ut under gång. Men när man följer component-ids i at-notation så är det inte i textual-context-listan, utan direkt i data-parse-trädet. Så, det är där som saker måste läggas till under gång. Alltså, innan varje steg av rekursion från en nod, skall den vara tillagd till sin föräldernod och själv ha redan avklarade noder i sitt innehåll. (Om at-level aldrig pekar onödigt långt ner i textual-context, skulle det innebära att man alltid går till en redan avslutat del av trädet när man följer en component-id. I så fall duger koden som den är nu)

(defun filter-objects-by-component-relations (objects table-constraint textual-context assign-ht)
  (assert (typep table-constraint 'parse-component-relation-constraint))
  ;;konvertera at-notations till objekt-filter
  ;;;för varje at-notation, leta upp värdet av indikerat fält i textual-context och i parse-tree vilket fält i objektet det gäller (at-notationen pekar ut en object-class-field-type som refererar till fältet)
  ;;kan ge nil ifall spåret inte kan följas genom choice, då ges ingen filtrering av objekt av denna at-notation
  ;;component-ids är tillåtna att gå genom sequence, set och choice (varav set inte är implementerat)
  (labels ((data-track-component-ids (dpt component-ids);hitta värdet i data-parse-tree
             (if (not component-ids)
                 dpt
                 (if (typep dpt 'data-parse-sequence)
                     (data-track-component-ids (data (find (car component-ids) (data dpt) :key #'name));;TODO may need to follow default values
                                               (cdr component-ids))
                     (when (eql (choice-name dpt) (car component-ids))
                       (data-track-component-ids (data dpt)
                                                 (cdr component-ids))))))
           (syntax-track-component-ids (pt component-ids);hitta objekt-fältet i parse-tree, måste hoppa över constrained type som en transparent nivå
             (if (not component-ids)
                 pt
                 (syntax-track-component-ids
                  (let ((next-level
                         (c-type (find (car component-ids) (components pt) :key #'c-name))))
                    (if (typep next-level 'parse-constrained-type)
                        (c-type next-level)
                        next-level))
                  (cdr component-ids)))))
    (let ((object-filters
           (remove nil
                   (mapcar (lambda (at-notation)
                             (let ((starting-data-parse-tree;the at-level decides where the track of component-ids starts
                                    (if (= 0 (at-level at-notation))
                                        (car textual-context)
                                        (elt textual-context (- (length textual-context) 1 (at-level at-notation))))))
                               (let ((filter-value (data-track-component-ids starting-data-parse-tree
                                                                             (component-ids at-notation)))
                                     (filter-field (syntax-track-component-ids (syntax starting-data-parse-tree)
                                                                               (component-ids at-notation))))
                                 (when filter-value
                                   (cons (field-name filter-field) ;filter-field should be parse-object-class-field-type
                                         filter-value)))))
                           (at-notations table-constraint)))))
      (remove-if-not (lambda (object)
                       (every
                        (lambda (object-filter)
                          (let* ((object-class-instance-field
                                  (find (car (name-path (car object-filter))) (fields object) :key #'field-name))
                                 ;;TODO field-name är komplicerad
                                 (filter-value
                                  (or (content object-class-instance-field)
                                      (default-content object-class-instance-field))))
                            (unless filter-value
                              (format t "~a~&" (car (name-path (car object-filter))))
                              (error "couldn't find value"))
                            ;;;(format t "~a~&" filter-value);sequence-value!
                            (value-eql
                             (cdr object-filter)
                             (read-value
                              filter-value
                              (c-type object-class-instance-field)
                              assign-ht))))
                        object-filters))
                     objects))))

(defun resolve-open-type (field-name textual-context invokations constraints assign-ht);field-name från (field-name pt)
  (let ((table-constraint (get-table-constraint constraints)))
    (unless table-constraint (error "open type no constraint"))
    ;;content/default-content fälten med det namnet i de objekten som ingår i object-set som table-constraint specar
    ;;dessa fält bör alla vara typ-fält (enda uppgiften är att 'sluta' öppna typ-fält)
    ;;fälten innehåller parse-type-by-reference och anses lika ifall de har samma referens (innan uppslag)
    (let ((objects
           (let ((objects (flatten-object-sets (object-set table-constraint) (object-assignment-ht assign-ht) (object-set-assignment-ht assign-ht) invokations)))
             (if (typep table-constraint 'parse-simple-table-constraint)
                 objects
                 (filter-objects-by-component-relations objects table-constraint textual-context assign-ht)))))
      ;;av filtrerade objekt, vilka alternativ finns nu för nuvarande (pt) fält
      (let ((type-alternatives
             (mapcar (lambda (object-class-instance-type-field)
                       (or (content object-class-instance-type-field)
                           (default-content object-class-instance-type-field)))
                     (mapcar (lambda (object);(typep object 'parse-object-instantiated)
                               (find (car (name-path field-name))
                                     (fields object) ;varje är av någon type object-class-instance-*-field
                                     :key #'field-name))
                             objects))))
        ;;till slut är frågan om det nu bara finns en en typ kvar
        ;;är alla parse-type-by-reference och har alla i så fall identiska referenser
        (when (and (every (lambda (type-alternative)
                              (typep type-alternative 'parse-type-by-reference))
                            type-alternatives)
                     (= 1 (length (remove-duplicates (mapcar #'reference type-alternatives)))))
        ;;;(print (reference (car type-alternatives)))
          (car type-alternatives))))))
