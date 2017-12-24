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

(in-package :traverse-high-parameter)

;;referens=vad som kan ersättas av en dummy parameter vid invokation av en parametriserad typ
;;ett element är (cons referens-namn analys)
(defmethod parameter-analysis (others)
  (declare (ignore others))
  nil)

(defmethod parameter-analysis ((pt parse-defined-class))
  (list (cons (reference pt) :class)))

(defmethod parameter-analysis ((pt object-class-instance-type-field))
  (list (cons (or (content pt) (default-content pt)) :type)))

(defmethod parameter-analysis ((pt object-class-instance-fixed-type-value-field))
  (list (cons (or (content pt) (default-content pt)) :value)
        (cons (c-type pt) :type)))

(defmethod parameter-analysis ((pt object-class-instance-variable-type-value-field))
  (list (cons (or (content pt) (default-content pt)) :value)))

(defmethod parameter-analysis ((pt object-class-instance-fixed-type-value-set-field))
  (list (cons (or (content pt) (default-content pt)) :value-set)
        (cons (c-type pt) :type)))

(defmethod parameter-analysis ((pt object-class-instance-variable-type-value-set-field))
  (list (cons (or (content pt) (default-content pt)) :value-set)))

(defmethod parameter-analysis ((pt object-class-instance-object-field))
  (list (cons (or (content pt) (default-content pt)) :object)
        (cons (defined-object-class pt) :class)))

(defmethod parameter-analysis ((pt object-class-instance-object-set-field))
  (list (cons (or (content pt) (default-content pt)) :object-set)
        (cons (defined-object-class pt) :class)))

(defmethod parameter-analysis ((pt parse-object-class-field-type))
  (list (cons (class-name pt) :class)))

(defmethod parameter-analysis ((pt parse-defined-object))
  (list (cons (reference pt) :object)))

(defmethod parameter-analysis ((pt parse-defined-object-set))
  (list (cons (reference pt) :object-set)))

(defmethod parameter-analysis ((pt parse-component-type))
  (list (cons (c-type pt) :type)
        (cons (default-value pt) :value)));TODO men kanske :value-set

(defmethod parameter-analysis ((pt parse-sequence-of-type))
  (list (cons (component-type pt) :type)))

(defmethod parameter-analysis ((pt parse-named-type))
  (list (cons (c-type pt) :type)))

(defmethod parameter-analysis ((pt parse-named-value))
  (list (cons (c-value pt) :value)));TODO men kanske :value-set

(defmethod parameter-analysis ((pt parse-type-by-reference))
  (list (cons (reference pt) :type)))

(defmethod parameter-analysis ((pt parse-contents-constraint))
  (list (cons (c-type pt) :type)))

(defmethod parameter-analysis ((pt parse-size-fixed))
  (unless (or (numberp (single-value pt))
              (and (listp (single-value pt))
                   (numberp (cdr (single-value pt)))))
    (list (cons (single-value pt) :value))));TODO value-set

(defmethod parameter-analysis ((pt parse-size-range))
  (list (unless (or (numberp (lower-bound pt))
              (and (listp (lower-bound pt))
                   (numberp (cdr (lower-bound pt)))))
          (cons (lower-bound pt) :value))
        (unless (or (numberp (upper-bound pt))
              (and (listp (upper-bound pt))
                   (numberp (cdr (upper-bound pt)))))
          (cons (upper-bound pt) :value))))

(defmethod parameter-analysis ((pt parse-constrained-type))
  (list (cons (c-type pt) :type)))

;;Skall anropas av main-traverse på vägen upp, så att underliggande 'är gjort'
(defun traverse-analyse-parameterized-type (pt assign-ht type-references)
  (assert (typep pt 'parse-parameterized-type))
  (labels ((reference-bound-p (ref)
             (or (eql :objectclassreference (car ref))
                 (find (cdr ref) type-references)))
           (w (pt)
             ;;samla ihop alla referenser i sub-trädet, tillsammans med respektive klassificering till nyckelord (se form för parameter-analysis-list)
             ;;ett element är (cons referens-namn analys)
             (append (parameter-analysis pt)
                     (reduce #'append
                             (mapcar (lambda (pt)
                                       (w pt))
                                     (sub-parse-tree-with-class-context pt assign-ht))))));ignorerar class-context
    ;;behåll bara det som är relevant för parameter-list, utgå från att det är konsekvent i remove-duplicates
    ;;klassificera governor efter om det är type eller defined-object-class som behövs och i så fall... logiken verkar vara att för governor så skall man inte se det som en dummy enbart baserat på att en bindning faktiskt finns - men det är rätt grumligt specat. 
    (let ((analysis-list (remove-duplicates (w (type-template pt)) :key #'car)))
      (change-class pt 'parse-parameterized-type-analyzed
                    :parameter-analysis-list
                    (mapcar (lambda (parameter-list-element)
                              (let ((reference (if (listp parameter-list-element)
                                                   (second parameter-list-element)
                                                   parameter-list-element))
                                    (governor (when (listp parameter-list-element)
                                                (first parameter-list-element))))
                                (let ((analysis (cdr (find (cdr reference) analysis-list :key #'car))))
                                  (cons
                                   analysis
                                   (when (find analysis '(:value :value-set :object :object-set))
                                     (if (reference-bound-p governor)
                                         (cdr governor)
                                         :dummy))))))
                            (parameter-list pt))))))
