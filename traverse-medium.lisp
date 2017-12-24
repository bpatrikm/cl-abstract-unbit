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

(in-package :traverse-medium)

(defgeneric sub-parse-tree-to-traverse (others)
  (:method-combination append))

(defmethod sub-parse-tree-to-traverse append (others)
  nil)

(defmethod sub-parse-tree-to-traverse append ((list list))
  (when (listp (cdr list)) list));;alltså inte (cons symbol value)

(defmethod sub-parse-tree-to-traverse append ((ocf object-class-field))
  (when (default-content ocf) (list (default-content ocf))))

(defmethod sub-parse-tree-to-traverse append ((ocf object-class-fixed-type-value-field))
  (list (c-type ocf)))

(defmethod sub-parse-tree-to-traverse append ((ocf object-class-fixed-type-value-set-field))
  (list (c-type ocf)))

(defmethod sub-parse-tree-to-traverse append ((ocf object-class-instance-field))
  (when (content ocf) (list (content ocf))))

(defmethod sub-parse-tree-to-traverse append ((pt parse-object-by-definition))
  (list (object-definition pt)))

(defmethod sub-parse-tree-to-traverse append ((pt parse-object-instantiated))
  (fields pt))

(defmethod sub-parse-tree-to-traverse append ((pt parse-object-assignment))
  (list (object pt)))

(defmethod sub-parse-tree-to-traverse append ((pt parse-object-set-raw))
  (list (root pt) (additional pt)))

(defmethod sub-parse-tree-to-traverse append ((pt parse-object-set-union))
  (subsets pt))

(defmethod sub-parse-tree-to-traverse append ((pt parse-object-set-assignment))
  (list (object-set pt)))

(defmethod sub-parse-tree-to-traverse append ((pt parse-component-type))
  (cons (c-type pt) (when (default-value pt) (list (default-value pt)))))

(defmethod sub-parse-tree-to-traverse append ((pt parse-sequence-type))
  (components pt))

(defmethod sub-parse-tree-to-traverse append ((pt parse-sequence-of-type))
  (list (component-type pt)))

(defmethod sub-parse-tree-to-traverse append ((pt parse-named-type))
  (list (c-type pt)))

(defmethod sub-parse-tree-to-traverse append ((pt parse-named-value))
  (list (c-value pt)))

(defmethod sub-parse-tree-to-traverse append ((pt parse-sequence-value))
  (components pt))

(defmethod sub-parse-tree-to-traverse append ((pt parse-choice-type))
  (append (root pt) (additional pt)))

(defmethod sub-parse-tree-to-traverse append ((pt parse-enumerated-type))
  (append (root pt) (additional pt)))

(defmethod sub-parse-tree-to-traverse append ((pt parse-table-constraint))
  (list (object-set pt)))

(defmethod sub-parse-tree-to-traverse append ((pt parse-constrained-type))
  (list (c-type pt) (constraint pt)))

(defmethod object-class-context (others)
  nil);TODO externalmodule

(defmethod object-class-context ((ocf object-class-object-field))
  (list (reference (defined-object-class ocf))))

(defmethod object-class-context ((ocf object-class-object-set-field))
  (list (reference (defined-object-class ocf))))

(defmethod object-class-context ((pt parse-object-class-field-type))
  (list (class-name pt)))

(defmethod object-class-context ((pt parse-object-assignment))
  (list (reference (defined-object-class pt))))

(defmethod object-class-context ((pt parse-object-set-assignment))
  (list (reference (defined-object-class pt))))

(defmethod sub-parse-tree-with-class-context (pt assign-ht)
  (declare (ignore assign-ht))
  (values (sub-parse-tree-to-traverse pt)
          (object-class-context pt)))

;;TODO parameter-list innehåller element som i sig kan vara lista över a:b, men har inte skrivit in det här
;;eftersom invokationer inte lagras i parse-tree, måste en sådan utföras här (också)
(defmethod sub-parse-tree-with-class-context ((pt parse-parameterized-type-invokation) assign-ht)
  (labels ((w (parameter-analysis-list actual-parameter-list &optional collected-sub-parse-trees collected-class-contexts)
             (if (not parameter-analysis-list)
                 (values collected-sub-parse-trees collected-class-contexts)
                 (case (car (car parameter-analysis-list))
                   (:type (w (cdr parameter-analysis-list)
                             (cdr actual-parameter-list)
                             (cons (car actual-parameter-list) collected-sub-parse-trees)
                             (cons nil collected-class-contexts)))
                   (:class (w (cdr parameter-analysis-list)
                              (cdr actual-parameter-list)
                              collected-sub-parse-trees
                              collected-class-contexts))
                   ((:value
                     :value-set) (if (eql :dummy (cdr (car parameter-analysis-list)))
                                     (w (cdr parameter-analysis-list)
                                        (subseq actual-parameter-list 2)
                                        (append (subseq actual-parameter-list 0 2)
                                                collected-sub-parse-trees)
                                        (cons nil collected-class-contexts))
                                     (w (cdr parameter-analysis-list)
                                        (cdr actual-parameter-list)
                                        (cons (car actual-parameter-list) collected-sub-parse-trees)
                                        (cons nil collected-class-contexts))))
                   ((:object
                     :object-set) (if (eql :dummy (cdr (car parameter-analysis-list)))
                                      (w (cdr parameter-analysis-list)
                                         (subseq actual-parameter-list 2)
                                         (cons (cadr actual-parameter-list)
                                               collected-sub-parse-trees)
                                         (cons (car actual-parameter-list) collected-class-contexts))
                                      (w (cdr parameter-analysis-list)
                                         (cdr actual-parameter-list)
                                         (cons (car actual-parameter-list) collected-sub-parse-trees)
                                         (cons (cdr (car parameter-analysis-list)) collected-class-contexts))))))))
    (w (parameter-analysis-list (parameterized-type pt))
       (actual-parameter-list pt))))

(defmethod sub-parse-tree-with-class-context ((pt parse-parameterized-type-invokation-raw) assign-ht)
  (declare (ignore pt assign-ht))
  (error "Convert pt to parse-parameterized-type-invokation"))

;;i det här läget är den analysen gjord för den här specifika parametriserade typen. För varje element i parameter-list (a:b räknas som ett element) har vi (cons b a), där b är :type, :class, :value, :value-set, :object eller :object-set och a är governor som antingen är en type-by-reference, class-by-reference, :dummy eller nil. nil ifall a inte kräver governor och :dummy ifall det är en dummygovernor (som konsumerar en actual-parameter).

(defmethod sub-parse-tree-with-class-context ((pt parse-parameterized-type-assignment) assign-ht)
  (list (parameterized-type pt)))

(defmethod sub-parse-tree-with-class-context ((pt parse-parameterized-type) assign-ht)
  (declare (ignore assign-ht))
  (list (type-template pt)));;Skall class context kunna komma med i dyket från fix classgovernor (inte dummy) i parameter-list? Nej, för definition av objekt enligt den klass-kontexten skulle i så fall ske i actual-parameter-list. I type-template är det bara en upprepning av dummyreference.
