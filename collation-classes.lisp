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

(in-package :collation-classes)

(defclass parse-tree ()
  ())

(defclass parse-defined-thing (parse-tree)
  ((reference :initarg :reference :reader reference)
   (modulereference :initarg :modulereference :initform nil :reader modulereference)))

(defclass parse-defined-class (parse-defined-thing)
  ())

(defun make-defined-class (objectclassreference modulereference)
  (make-instance 'parse-defined-class :reference objectclassreference :modulereference modulereference))

(defclass object-class-field (parse-tree)
  ((field-name :initarg :field-name :reader field-name)
   (optional-p :initarg :optional-p :reader optional-p)
   (default-content :initarg :default-content :reader default-content)))

(defclass object-class-type-field (object-class-field)
  ())

(defmethod content-non-terminal ((ocf object-class-type-field))
  (declare (ignore ocf)) 'asn.1::%type)

(defclass object-class-fixed-type-value-field (object-class-field)
  ((c-type :initarg :c-type :reader c-type)
   (unique-p :initarg :unique-p :reader unique-p)))

(defmethod content-non-terminal ((ocf object-class-fixed-type-value-field))
  (declare (ignore ocf)) 'asn.1::%value)

(defclass object-class-variable-type-value-field (object-class-field)
  ((type-field-name :initarg :type-field-name :reader type-field-name)))

(defmethod content-non-terminal ((ocf object-class-variable-type-value-field))
  (declare (ignore ocf)) 'asn.1::%value)

(defclass object-class-fixed-type-value-set-field (object-class-field)
  ((c-type :initarg :c-type :reader c-type)))

(defmethod content-non-terminal ((ocf object-class-fixed-type-value-set-field))
  (declare (ignore ocf)) 'asn.1::%value-set)

(defclass object-class-variable-type-value-set-field (object-class-field)
  ((type-field-name :initarg :type-field-name :reader type-field-name)))

(defmethod content-non-terminal ((ocf object-class-variable-type-value-set-field))
  (declare (ignore ocf)) 'asn.1::%value-set)

(defclass object-class-object-field (object-class-field)
  ((defined-object-class :initarg :defined-object-class :reader defined-object-class)))

(defmethod content-non-terminal ((ocf object-class-object-field))
  (declare (ignore ocf)) 'asn.1::%object)

(defclass object-class-object-set-field (object-class-field)
  ((defined-object-class :initarg :defined-object-class :reader defined-object-class)))

(defmethod content-non-terminal ((ocf object-class-object-set-field))
  (declare (ignore ocf)) 'asn.1::%object-set)

(defclass object-class-instance-field ();mixin
  ((content :initarg :content :reader content)))

(defclass object-class-instance-type-field
    (object-class-type-field object-class-instance-field)
  ())

(defclass object-class-instance-fixed-type-value-field
    (object-class-fixed-type-value-field object-class-instance-field)
  ())

(defclass object-class-instance-variable-type-value-field
    (object-class-variable-type-value-field object-class-instance-field)
  ())

(defclass object-class-instance-fixed-type-value-set-field
    (object-class-fixed-type-value-set-field object-class-instance-field)
  ())

(defclass object-class-instance-variable-type-value-set-field
    (object-class-variable-type-value-set-field object-class-instance-field)
  ())

(defclass object-class-instance-object-field
    (object-class-object-field object-class-instance-field)
  ())

(defclass object-class-instance-object-set-field
    (object-class-object-set-field object-class-instance-field)
  ())

(defclass parse-object-definition (parse-tree)
  ())

(defclass parse-object-definition-raw (parse-object-definition)
  ((tokens :initarg :tokens :reader tokens)))

(defun make-object-definition-raw (tokens)
  (make-instance 'parse-object-definition-raw :tokens tokens))

(defclass parse-object-class-field-type (parse-tree)
  ((class-name :initarg :class-name :reader class-name)
   (field-name :initarg :field-name :reader field-name)))

(defun make-object-class-field-type (class-name field-name)
  (make-instance 'parse-object-class-field-type :class-name class-name :field-name field-name))

(defclass parse-defined-object (parse-defined-thing)
  ())

(defun make-defined-object (objectreference modulereference)
  (make-instance 'parse-defined-object :reference objectreference :modulereference modulereference))

(defclass parse-object (parse-tree)
  ())

(defclass parse-object-by-definition (parse-object)
  ((object-definition :initarg :object-definition :reader object-definition :type parse-object-definition)))

(defun make-object-by-definition (object-definition)
  (make-instance 'parse-object-by-definition :object-definition object-definition))

(defclass parse-object-instantiated (parse-object)
  ((fields :initarg :fields :reader fields)
   (class-name :initarg :class-name :reader class-name :type symbol)))

(defclass parse-object-by-reference (parse-object)
  ((defined-object :initarg :defined-object :reader defined-object :type parse-defined-object)))

(defun make-object-by-reference (defined-object)
  (make-instance 'parse-object-by-reference :defined-object defined-object))

(defclass parse-object-assignment (parse-tree)
  ((object-name :initarg :object-name :reader object-name)
   (defined-object-class :initarg :defined-object-class :reader defined-object-class :type parse-defined-object-class)
   (object :initarg :object :reader object :type parse-object)))

(defun make-object-assignment (object-name defined-object-class object)
  (make-instance 'parse-object-assignment
                 :object-name object-name
                 :defined-object-class defined-object-class
                 :object object))

(defclass parse-defined-object-set (parse-defined-thing)
  ())

(defun make-defined-object-set (objectsetreference modulereference)
  (make-instance 'parse-defined-object-set :reference objectsetreference :modulereference modulereference))

(defclass parse-object-set (parse-tree)
  ())

(defclass parse-object-set-raw (parse-object-set)
  ((root :initarg :root :reader root :type parse-object-set-union)
   (extensible-p :initarg :extensible-p :reader extensible-p)
   (additional :initarg :additional :reader additional :type parse-object-set-union)))

;;the union set operation has yet to be performed, along with verification of class of referenced objects. This is deferred since it requires the context of a specific object-class. (any constituent object-by-definition needs this also)
(defun make-object-set-raw (root extensible-p additional)
  (make-instance 'parse-object-set-raw :root root :extensible-p extensible-p :additional additional))

(defclass parse-object-set-by-reference (parse-object-set)
  ((defined-object-set :initarg :defined-object-set :reader defined-object-set :type parse-defined-object-set)))

(defun make-object-set-by-reference (defined-object-set)
  (make-instance 'parse-object-set-by-reference :defined-object-set-name defined-object-set))

(defclass parse-object-set-union (parse-tree)
  ((subsets :initarg :subsets :reader subsets)));;each subset can be objects or object-sets, raw or by reference

(defun make-object-set-union (subsets);a subset can be another parse-object-set-union
  (make-instance 'parse-object-set-union
                 :subsets
                 (reduce #'append
                         (mapcar (lambda (subset)
                                   (if (typep subset 'parse-object-set-union)
                                       (subsets subset)
                                       (list subset)))
                                 subsets))))

(defclass parse-object-set-assignment (parse-tree)
  ((object-set-name :initarg :object-set-name :reader object-set-name)
   (defined-object-class :initarg :defined-object-class :reader defined-object-class :type parse-defined-object-class)
   (object-set :initarg :object-set :reader object-set :type parse-object-set)))

(defun make-object-set-assignment (object-set-name defined-object-class object-set)
  (make-instance 'parse-object-set-assignment
                 :object-set-name object-set-name
                 :defined-object-class defined-object-class
                 :object-set object-set))

(defclass parse-field-name (parse-tree)
  ((name-path :initarg :name-path :reader name-path)))

(defun make-field-name (primitive-field-name)
  (make-instance 'parse-field-name :name-path (list primitive-field-name)))

(defun append-field-name (parse-field-name primitive-field-name)
  (assert (typep parse-field-name 'parse-field-name))
  (make-instance 'parse-field-name :name-path (append (name-path parse-field-name) (list primitive-field-name))))

(defclass parse-component-type (parse-tree)
  ((c-name :initarg :c-name :reader c-name)
   (c-type :initarg :c-type :reader c-type)
   (optional-p :initarg :optional-p :initform nil :reader optional-p);true also for those marked as DEFAULT
   (default-value :initarg :default-value :initform nil :reader default-value)))

(defun make-component-type (c-name c-type optional-p default-value)
  (make-instance 'parse-component-type
                 :c-name c-name :c-type c-type
                 :optional-p optional-p :default-value default-value))

(defclass parse-sequence-type (parse-tree)
  ((components :initarg :components :reader components)
   (exensible-p :initarg :extensible-p :initform nil :reader extensible-p)))

(defun make-sequence-type (components extensible-p)
  (make-instance 'parse-sequence-type :components components :extensible-p extensible-p))

(defclass parse-sequence-of-type (parse-tree)
  ((component-type :initarg :component-type :reader component-type)))

(defun make-sequence-of-type (component-type)
  (make-instance 'parse-sequence-of-type :component-type component-type))

(defclass parse-named-type (parse-tree);ofta är parse-component-type bättre
  ((c-name :initarg :c-name :reader c-name)
   (c-type :initarg :c-type :reader c-type)))

(defun make-named-type (c-name c-type)
  (make-instance 'parse-named-type :c-name c-name :c-type c-type))

(defclass parse-named-value (parse-tree)
  ((c-name :initarg :c-name :reader c-name)
   (c-value :initarg :c-value :reader c-value)))

(defun make-named-value (c-name c-value)
  (make-instance 'parse-named-value :c-name c-name :c-value c-value))

(defclass parse-sequence-value (parse-tree)
  ((components :initarg :components :reader components)))

(defun make-sequence-value (components)
  (make-instance 'parse-sequence-value :components components))

(defclass parse-choice-type (parse-tree)
  ((root :initarg :root :reader root)
   (additional :initarg :additional :reader additional)
   (extensible-p :initarg :extensible-p :initform nil :reader extensible-p)))

(defun make-choice-type (components)
  (let ((ext-marker-pos (position :extensible components)))
    (if ext-marker-pos
        (make-instance 'parse-choice-type :root (subseq components 0 ext-marker-pos) :additional (subseq components (+ 1 ext-marker-pos)) :extensible-p t)
        (make-instance 'parse-choice-type :root components :additional nil :extensible-p nil))))

(defclass parse-enumerated-type (parse-tree)
  ((root :initarg :root :reader root)
   (additional :initarg :additional :reader additional)
   (extensible-p :initarg :extensible-p :initform nil :reader extensible-p)))

(defun make-enumerated-type (components)
  (let ((ext-marker-pos (position :extensible components)))
    (if ext-marker-pos
        (make-instance 'parse-enumerated-type :root (subseq components 0 ext-marker-pos) :additional (subseq components (+ 1 ext-marker-pos)) :extensible-p t)
        (make-instance 'parse-enumerated-type :root components :additional nil :extensible-p nil))))

(defclass parse-type-by-reference (parse-tree)
  ((reference :initarg :reference :reader reference)))

(defun make-type-by-reference (reference)
  (make-instance 'parse-type-by-reference :reference reference))

(defclass parse-constraint (parse-tree)
  ())

(defclass parse-contents-constraint (parse-tree)
  ((c-type :initarg :c-type :reader c-type)))

(defun make-contents-constraint (c-type)
  (make-instance 'parse-contents-constraint :c-type c-type))

(defclass parse-size-constraint (parse-tree)
  ())

(defclass parse-size-fixed (parse-size-constraint)
  ((single-value :initarg :single-value :reader single-value)))

(defun make-size-fixed (single-value)
  (make-instance 'parse-size-fixed :single-value single-value))

(defmethod lower-bound ((pt parse-size-fixed))
  (single-value pt))

(defmethod upper-bound ((pt parse-size-fixed))
  (single-value pt))

(defclass parse-size-range (parse-size-constraint)
  ((lower-bound :initarg :lower-bound :reader lower-bound)
   (upper-bound :initarg :upper-bound :reader upper-bound)))

(defun make-size-range (lower-bound upper-bound)
  (make-instance 'parse-size-range :lower-bound lower-bound :upper-bound upper-bound))

(defclass parse-table-constraint (parse-constraint)
  ((object-set :initarg :object-set :reader object-set :type parse-object-set)))

(defclass parse-simple-table-constraint (parse-table-constraint)
  ())

(defun make-simple-table-constraint (object-set)
  (make-instance 'parse-simple-table-constraint :object-set object-set))

(defclass parse-at-notation (parse-tree)
  ((at-level :initarg :at-level :reader at-level :type number)
   (component-ids :initarg :component-ids :reader component-ids)))

(defun make-at-notation (at-level component-ids)
  (make-instance 'parse-at-notation :at-level at-level :component-ids component-ids))

(defclass parse-component-relation-constraint (parse-table-constraint)
  (;%object-set can only be { %defined-object-set }, but writing that in the rule gives a shift-reduce-error
   (at-notations :initarg :at-notations :reader at-notations)))

(defun make-component-relation-constraint (object-set at-notations)
  (make-instance 'parse-component-relation-constraint
                 :object-set object-set
                 :at-notations at-notations))

(defclass parse-constrained-type (parse-tree)
  ((c-type :initarg :c-type :reader c-type)
   (constraint :initarg :constraint :reader constraint :type parse-constraint)))

(defun make-constrained-type (c-type constraint)
  (make-instance 'parse-constrained-type :c-type c-type :constraint constraint))

(defclass parse-parameterized-type (parse-tree)
  ((parameter-list :initarg :parameter-list :reader parameter-list)
   (type-template :initarg :type-template :reader type-template :type parse-tree)))

(defun make-parameterized-type (parameter-list type-template)
  (make-instance 'parse-parameterized-type :parameter-list parameter-list :type-template type-template))

(defclass parse-parameterized-type-analyzed (parse-parameterized-type)
  ((parameter-analysis-list :initarg :parameter-analysis-list :reader parameter-analysis-list)))

(defclass invoked-parameterized-type (parse-parameterized-type-analyzed)
  ((actual-parameter-list :initarg :actual-parameter-list :reader actual-parameter-list)))

;;anropas av read-aligned för parse-parameterized-type, som lägger denna i textual-context istället, och rekursivt anropar på type-template
(defun make-invoked-parameterized-type (definition actual-parameter-list)
  (assert (typep definition 'parse-parameterized-type))
  (make-instance 'invoked-parameterized-type :parameter-list (parameter-list definition) :parameter-analysis-list (parameter-analysis-list definition) :type-template (type-template definition) :actual-parameter-list actual-parameter-list))

(defclass parse-parameterized-type-assignment (parse-tree)
  ((parameterized-type-name :initarg :parameterized-type-name :reader parameterized-type-name)
   (parameterized-type :initarg :parameterized-type :reader parameterized-type)))
;;I allmänhet används namn i slutändan för att derefereras på något sätt, så det är bara i situationer när referenser följs som parameter-lista måste kontrolleras först.

;;Lyckligtvis så innebär en invokation av parametriserad typ också ett brott av textuell kontext, så det finns bara en parameter-lista att ta hänsyn till. Dessutom är det inte fält-namn som ersätts, bara referenser till typ, värde, objekt osv... 

(defun make-parameterized-type-assignment (parameterized-type-name parameterized-type)
  (make-instance 'parse-parameterized-type-assignment :parameterized-type-name parameterized-type-name :parameterized-type parameterized-type))

(defclass parse-parameterized-type-invokation-raw (parse-tree)
  ;;måste göra parse-later tills vi gjort analys av vad actual-parameter-list innehåller via type-template
  ((parameterized-type-name :initarg :parameterized-type-name :reader parameterized-type-name)
   (actual-parameter-token-list :initarg :actual-parameter-token-list :reader actual-parameter-token-list)))

(defun make-parameterized-type-invokation-raw (parameterized-type-name actual-parameter-token-list)
  (make-instance 'parse-parameterized-type-invokation-raw :parameterized-type-name parameterized-type-name :actual-parameter-token-list actual-parameter-token-list))

;;konvertera till denna genom analys av vad actual-parameter-list innehåller (via type-template)
(defclass parse-parameterized-type-invokation (parse-tree)
  ((parameterized-type :initarg :parameterized-type :reader parameterized-type)
   (actual-parameter-list :initarg :actual-parameter-list :reader actual-parameter-list)))

(defclass parse-integer-with-names (parse-tree)
  ((number-names :initarg :number-names :reader number-names)
   (number-values :initarg :number-values :reader number-values)))

(defun make-integer-with-names (number-names number-values)
  (make-instance 'parse-integer-with-names :number-names number-names :number-values number-values))

(defclass parse-element-set-union (parse-tree)
  ((subsets :initarg :subsets :reader subsets)))

(defun make-element-set-union (subsets);a subset can be another parse-element-set-union
  (make-instance 'parse-element-set-union
                 :subsets
                 (reduce #'append
                         (mapcar (lambda (subset)
                                   (if (typep subset 'parse-element-set-union)
                                       (subsets subset)
                                       (list subset)))
                                 subsets))))

(defclass parse-element-set-specs (parse-tree)
  ((root :initarg :root :reader root)
   (additional :initarg :additional :reader additional)
   (extensible-p :initarg :extensible-p :reader extensible-p)))

(defun make-element-set-specs (root additional extensible-p)
  (make-instance 'parse-element-set-specs :root root :additional additional :extensible-p extensible-p))

(defclass parse-bit-string (parse-tree)
  ())

(defun make-bit-string ()
  (make-instance 'parse-bit-string))

(defclass parse-bit-string-with-names (parse-bit-string)
  ((bit-names :initarg :bit-names :reader bit-names)))

(defun make-bit-string-with-names (bit-names)
  (make-instance 'parse-bit-string-with-names :bit-names bit-names))

(do-symbols (sym)
  (multiple-value-bind (symbol sym-status)
      (find-symbol (symbol-name sym))
    (declare (ignore symbol))
    (when (eql :internal sym-status)
      (export sym))))
  
