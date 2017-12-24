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

(in-package :asn.1)

;;WITH SYNTAX defines a secondary syntax for instantiation of information objects. So the output of this subset of the grammer is another grammer. The stack data of this secondary grammer is a flat list of pairs of primitive-field-name with setting, specifying the field values of the new instance information object.

(defparameter *with-syntax-syntax*
  '(
    ((%syntax-list { %token-or-group-spec+ }) (lambda (pl) (third pl)))

    ;;2.continue collection into first production of +-group
    ((%token-or-group-spec+ %token-or-group-spec+ %token-or-group-spec)
     (lambda (pl) (let ((ongoing-production (caar (second pl)))
                        (old-complete-productions (cdr (second pl)))
                        (new-complete-productions (third pl))
                        (non-terminal (gensym "added"))
                        (prev-non-terminal (caaar (third pl))))
                    `((,(append ongoing-production (list non-terminal))
                        (lambda (pl2)
                          (reduce #'append (cdr pl2))))
                      ((,non-terminal ,prev-non-terminal)
                       (lambda (pl2) (second pl2)))
                      ,@old-complete-productions
                      ,@new-complete-productions))))
                    
    ;;1.needs a production to collect into
    ((%token-or-group-spec+ %token-or-group-spec) (lambda (pl)
                                                    (let ((non-terminal (gensym "begin"))
                                                          (prev-non-terminal (caaar (second pl))))
                                                      `(((,non-terminal ,prev-non-terminal)
                                                         (lambda (pl2) (second pl2)))
                                                        ,@(second pl)))))

    ;;Make a subproduction which is optional
    ;;the stack data of optional-group is a list a productions, the first of which is the latest, so the first symbol of the first production is the non-terminal that we want to make a subproduction for now
    ((%token-or-group-spec %optional-group) (lambda (pl)
                                              (let ((non-terminal (gensym "opt"))
                                                    (prev-non-terminal (caaar (second pl))))
                                                `(((,non-terminal ,prev-non-terminal)
                                                   (lambda (pl2) (second pl2)))
                                                  ((,non-terminal))
                                                  ,@(second pl)))))

    ((%optional-group [ %token-or-group-spec+ ]) (lambda (pl) (third pl)))

    ;;Make a subproduction which is not optional
    ((%token-or-group-spec %required-token) (lambda (pl)
                                              (let ((non-terminal (gensym "nonopt")))
                                                `(((,non-terminal ,@(first (second pl)))
                                                   ,@(rest (second pl)))))))
                                              
    ((%required-token %primitive-field-name-ws) (lambda (pl) (second pl)))
    ((%required-token %literal) (lambda (pl) (second pl)))
    
    ;;primitive-field-name can be the lexical items typefieldreference, valuefieldreference, valuesetfieldreference, objectfieldreference, objectsetfieldreference
    ((%primitive-field-name-ws :fieldname)
     (lambda (pl) `((:setting-standin ,(cdr (second pl)))
                                        ;based on field-spec, this later gets replaced by e g %type
                                        ;it also gets spliced into a production for (gensym "nonopt") above where the action is set to this next line
                    (lambda (pl2) (list (list :set-field ',(cdr (second pl)) (second pl2)))))));one level of list is consumed by the reduce in 2.
    ((%primitive-field-name-ws :fieldid)
     (lambda (pl) `((:setting-standin ,(cdr (second pl)))
                    (lambda (pl2) (list (list :set-field ',(cdr (second pl)) (second pl2)))))))
    
    ((%literal |,|) (lambda (pl) `((,(second pl)))))
    ;;word=only upcase, no numbers
    ((%literal :typereference) (lambda (pl) `((,(cdr (second pl))))))))

(defparameter *asn.1-syntax-mod*
  '(
    ((%root %module-definition) #'second)
    ((%module-definition :typereference
      ;;TODO, någon sorts OID-spec här i NBAP
      DEFINITIONS
      %tag-default
      %extension-default
      |::=|
      BEGIN
      %module-body
      END)
     (lambda (pl) (list :module (second pl) (eighth pl))));;pl stands for 'production-list'
    ((%tag-default))
    ((%tag-default AUTOMATIC TAGS))
    ((%extension-default))
    ((%module-body %exports %imports %assignment-list)
     (lambda (pl) (list (list (second pl) (third pl)) (fourth pl))))
    ((%module-body))
    ((%exports EXPORTS ALL |;|) (lambda (pl) (declare (ignore pl)) (list :export :all)))
    ((%exports EXPORTS %symbol* |;|) (lambda (pl) (cons :export (third pl))))
    ((%exports))
    ((%imports IMPORTS %symbols-from-modules |;|) (lambda (pl) (cons :import (third pl))))
    ((%imports))
    ((%symbols-from-modules %symbols-from-module) (lambda (pl) (list (second pl))))
    ((%symbols-from-modules %symbols-from-modules %symbols-from-module)
     (lambda (pl) (append (second pl) (list (third pl)))))
    ((%symbols-from-module %symbol+ FROM :typereference)
     (lambda (pl) (list (second pl) :from (fourth pl))))
    ((%assignment-list %assignment-list %assignment)
     (lambda (pl) (append (second pl) (list (third pl)))))
    ((%assignment-list %assignment) (lambda (pl) (list (second pl))))
    ((%assignment %type-assignment) #'second)
    ((%assignment %value-assignment) #'second)
    ((%assignment %object-assignment) #'second)
    ((%assignment %object-set-assignment) #'second)
    ((%assignment %object-class-assignment) #'second)
    ;; ASN.1 Macro
    ((%macro-definition %macro-name
      MACRO |::=|
      BEGIN
      %general-list
      END) (lambda (pl) (list :macro (second pl))))
    ((%value-assignment :id %type |::=| %value)
     (lambda (pl) (list :value-assignment (second pl) (third pl) (fifth pl))))
    ((%value-assignment :id %macro-name %macro-arguments+
      |::=| %object-identifier-value)
     (lambda (pl) (list :define (third pl) (list (second pl) (fourth pl)) (sixth pl))))
    ((%value-assignment :id %macro-name %macro-arguments+
      |::=| :number)
     (lambda (pl) (list :define (third pl) (list (second pl) (fourth pl)) (sixth pl))))
    ((%type-assignment :typereference |::=| %type)
     (lambda (pl) (list :type-assignment (second pl) (fourth pl))))
    ((%type %builtin-type) #'second)
    ((%type %referenced-type) #'second)
    ((%type %constrained-type) #'second)
    ((%named-type :id %type) (lambda (pl) (list (cdr (second pl)) (third pl))) -1);;lösare än constrained-type -> type constraint
    ((%builtin-type %object-identifier-type) #'second)
    ((%builtin-type %choice-type) #'second)
    ((%builtin-type %octet-string-type) #'second)
    ((%builtin-type %integer-type) #'second)
    ((%builtin-type %sequence-type) #'second)
    ((%builtin-type %sequence-of-type) #'second)
    ((%builtin-type %enumerated-type) #'second)
    ((%builtin-type NULL) (lambda (pl) (declare (ignore pl)) :null))
    ((%builtin-type |UTCTime|))
    
    ((%builtin-type BIT STRING)
     (lambda (pl) (declare (ignore pl)) (col-c:make-bit-string)))
    ((%builtin-type BIT STRING { %named-bit-list })
     (lambda (pl) (col-c:make-bit-string-with-names (fifth pl))))
    ((%named-bit-list %named-bit) (lambda (pl) (list (second pl))))
    ((%named-bit-list %named-bit-list |,| %named-bit)
     (lambda (pl) (append (second pl) (list (fourth pl)))))
    ((%named-bit :id |(| :number |)|) (lambda (pl) (list (cdr (second pl)) (cdr (fourth pl)))))
    
    ((%builtin-type BOOLEAN)
     (lambda (pl) (declare (ignore pl)) :boolean))
    ((%object-identifier-type OBJECT IDENTIFIER)
     (lambda (pl) (declare (ignore pl)) :object-identifier))
    
    ((%choice-type CHOICE { %alternative-type-list+ })
     (lambda (pl) (col-c:make-choice-type (nreverse (fourth pl)))))
    ((%alternative-type-list+ %alternative-type-list) (lambda (pl) (list (second pl))))
    ((%alternative-type-list+ %alternative-type-list+ |,| %alternative-type-list)
     (lambda (pl) (cons (fourth pl) (second pl))))
    ((%alternative-type-list %named-type) #'second)
    ((%alternative-type-list |...|) (lambda (pl) (declare (ignore pl)) :extensible))
    
    ((%string-type OCTET STRING %string-options)
     (lambda (pl) (list :octet-string (fourth pl))))
    ((%numbers+ %numbers+ |\|| %splited-numbers)
     (lambda (pl) (append (second pl) (list (fourth pl)))))
    ((%numbers+ %splited-numbers) (lambda (pl) (list (second pl))))
    ((%integer-type INTEGER)
     (lambda (pl) (declare (ignore pl)) :integer))
    ((%integer-type INTEGER { %named-number+ })
     (lambda (pl) (col-c:make-integer-with-names (mapcar #'first (fourth pl)) (mapcar #'second (fourth pl)))))
    ((%splited-numbers :number) #'second)
    ((%splited-numbers :number |..| :number)
     (lambda (pl) (list (second pl) (fourth pl))))
    ((%named-number+ %named-number) (lambda (pl) (list (second pl))))
    ((%named-number+ %named-number+ |,| %named-number)
     (lambda (pl) (append (second pl) (list (fourth pl)))))
    ((%named-number :id |(| :number |)|)
     (lambda (pl) (list (second pl) (fourth pl))))
    ((%tagged-type %tag IMPLICIT %builtin-type)
     (lambda (pl) (list :implicit (second pl) (fourth pl))))
    ((%tagged-type %tag EXPLICIT %builtin-type)
     (lambda (pl) (list :explicit (second pl) (fourth pl))))
    ((%tag [ %class :number ])
     (lambda (pl) (list (third pl) (fourth pl))))
    ((%class UNIVERSAL) (lambda (pl) (declare (ignore pl)) :universal))
    ((%class APPLICATION) (lambda (pl) (declare (ignore pl)) :application))
    ((%class PRIVATE) (lambda (pl) (declare (ignore pl)) :private))
    ((%class))
    ((%value :string) #'second)
    ((%value :number) #'second)
    ((%value :id) #'second)
    ((%value %builtin-value) #'second)
    ((%builtin-value %sequence-value) #'second)
    ((%sequence-value { }) (lambda (pl) (declare (ignore pl)) (col-c:make-sequence-value nil)))
    ((%sequence-value { %component-value-list }) (lambda (pl) (col-c:make-sequence-value (third pl))))
    ((%component-value-list %named-value) (lambda (pl) (list (second pl))))
    ((%component-value-list %component-value-list |,| %named-value)
     (lambda (pl) (append (second pl) (list (fourth pl)))))
    
    ((%named-value :id %value)
     (lambda (pl) (col-c:make-named-value (cdr (second pl)) (cdr (third pl)))))
    ((%object-identifier-value { %obj-id-component+ }) (lambda (pl) (third pl)))
    ((%obj-id-component+ %obj-id-component+ %obj-id-component)
     (lambda (pl) (append (second pl) (list (third pl)))))
    ((%obj-id-component+ %obj-id-component) (lambda (pl) (list (second pl))))
    ((%obj-id-component %name-and-number-form) #'second)
    ((%obj-id-component :id) #'second)
    ((%obj-id-component :number) #'second)
    ((%name-and-number-form :id |(| :number |)|)
     (lambda (pl) (list (second pl) (fourth pl))))
    ((%sequence-of-type SEQUENCE OF %type)
     (lambda (pl) (col-c:make-sequence-of-type (fourth pl))) -1);lägre prio, enligt text i X.680 / 49.2
    ((%sequence-type SEQUENCE { })
     (lambda (pl) (declare (ignore pl)) (col-c:make-sequence-type nil nil)))
    ((%sequence-type SEQUENCE { %component-type-list })
     (lambda (pl) (col-c:make-sequence-type (first (fourth pl)) (second (fourth pl)))))

    ((%component-type-list %component-type)
     (lambda (pl) (if (eql (second pl) :extensible)
                      (list nil t)
                      (list (list (second pl)) nil))))
    ((%component-type-list %component-type-list |,| %component-type)
     (lambda (pl) (if (eql (fourth pl) :extensible)
                      (list (first (second pl)) t)
                      (list (append (first (second pl)) (list (fourth pl))) nil))))
    
    ((%component-type %named-type OPTIONAL)
     (lambda (pl) (col-c:make-component-type (first (second pl)) (second (second pl)) t nil)))
    ((%component-type %named-type DEFAULT %value)
     (lambda (pl) (col-c:make-component-type (first (second pl)) (second (second pl)) t (fourth pl))))
    ((%component-type %named-type)
     (lambda (pl) (col-c:make-component-type (first (second pl)) (second (second pl)) nil nil)))
    ;;((%component-type COMPONENTS OF %type) `(:components-of ,$3)) "Type" in the "COMPONENTS OF Type" notation shall be a sequence type.
    ((%component-type |...|) (lambda (pl) (declare (ignore pl)) :extensible))
    
    ((%textual-convention-type TEXTUAL-CONVENTION %tc-args SYNTAX %type)
     (lambda (pl) (list :textual-convention (third pl) (list :syntax (fifth pl)))))
    ((%tc-args %tc-arg) (lambda (pl) (list (second pl))))
    ((%tc-args %tc-args %tc-arg) (lambda (pl) (append (second pl) (list (third pl)))))
    ((%tc-arg :id :id) (lambda (pl) (list (second pl) (third pl))))
    ((%tc-arg :id :string) (lambda (pl) (list (second pl) (third pl))))
    ;; Symbol+ and Symbol*
    ((%symbol+ %symbol) (lambda (pl) (list (second pl))))
    ((%symbol+ %symbol+ |,| %symbol) (lambda (pl) (append (second pl) (list (fourth pl)))))
    ((%implied-symbol+ %implied-symbol) (lambda (pl) (list (second pl))))
    ((%implied-symbol+ %implied-symbol+ |,| %implied-symbol)
     (lambda (pl) (append (second pl) (list (fourth pl)))))
    ((%symbol* %symbol) (lambda (pl) (list (second pl))))
    ((%symbol* %symbol* |,| %symbol)
     (lambda (pl) (append (second pl) (list (fourth pl)))))
    ((%symbol %macro-name) #'second)
    ((%symbol :typereference) #'second)
    ((%symbol :typereference { }) #'second)
    ((%symbol :id) #'second);valuereference
    ((%symbol :objectclassreference) #'second)
    ((%implied-symbol :id) #'second)
    ((%implied-symbol IMPLIED :id) (lambda (pl) (list :implied (third pl))))
    ((%macro-name MODULE-IDENTITY) #'second)
    ((%macro-name OBJECT-TYPE) #'second)
    ((%macro-name NOTIFICATION-TYPE) #'second)
    ((%macro-name TEXTUAL-CONVENTION) #'second)
    ((%macro-name MODULE-COMPLIANCE) #'second)
    ((%macro-name OBJECT-GROUP) #'second)
    ((%macro-name NOTIFICATION-GROUP) #'second)
    ((%macro-name OBJECT-IDENTITY) #'second)
    ((%macro-name AGENT-CAPABILITIES) #'second)
    ((%macro-name TRAP-TYPE) #'second)
    ((%value-set { %element-set-specs }) #'third)
    ((%element-set-specs %element-set-spec) (lambda (pl) (col-c:make-element-set-specs (second pl) nil nil)))
    ((%element-set-specs %element-set-spec |,| |...|) (lambda (pl) (col-c:make-element-set-specs (second pl) nil t)))
    ((%element-set-specs %element-set-spec |,| |...| |,| %element-set-spec)
     (lambda (pl) (col-c:make-element-set-specs (second pl) (sixth pl) t)))
    
    ((%element-set-spec %elements) #'second)
    ((%element-set-spec %element-set-spec |\|| %elements) (lambda (pl) (col-c:make-element-set-union (list (second pl) (fourth pl)))))

    ((%constraint |(| %constraint-spec |)|) #'third)
    ((%constraint-spec %subtype-constraint) #'second)
    ((%constraint-spec %general-constraint) #'second)
    ((%subtype-constraint %element-set-specs) #'second)
    ((%constrained-type %type %constraint)
     (lambda (pl) (col-c:make-constrained-type (second pl) (third pl))))
    ((%constrained-type %type-with-constraint) #'second)
    ((%general-constraint %contents-constraint) (lambda (pl) (col-c:make-contents-constraint (second pl))))
    ((%general-constraint %table-constraint) #'second)
    ((%contents-constraint CONTAINING %type) #'third)
    ((%type-with-constraint SEQUENCE %constraint OF %type)
     (lambda (pl) (col-c:make-constrained-type (col-c:make-sequence-of-type (fifth pl)) (third pl))) -1)
    ((%type-with-constraint SEQUENCE %size-constraint OF %named-type)
     (lambda (pl) (col-c:make-constrained-type (col-c:make-sequence-of-type (fifth pl)) (third pl))))
    ((%type-with-constraint SEQUENCE %constraint OF %named-type)
     (lambda (pl) (col-c:make-constrained-type (col-c:make-sequence-of-type (fifth pl)) (third pl))))
    ((%size-constraint SIZE %constraint) #'third)
    ((%elements %subtype-elements) #'second)
    ((%subtype-elements %size-constraint) #'second)
    ((%subtype-elements %value-range) #'second)
    ((%subtype-elements %single-value) #'second)
    ((%value-range %value |..| %value) (lambda (pl) (col-c:make-size-range (second pl) (fourth pl))))
    ((%single-value %value) (lambda (pl) (col-c:make-size-fixed (second pl))))
    ((%referenced-type %defined-type) #'second)
    ((%defined-type :typereference) (lambda (pl) (col-c:make-type-by-reference (cdr (second pl)))))
    ((%table-constraint %object-set) (lambda (pl) (col-c:make-simple-table-constraint (second pl))))
    ((%table-constraint %object-set { %at-notation+ });%object-set can only be { %defined-object-set }, but writing that in the rule gives a shift-reduce-error
     (lambda (pl) (col-c:make-component-relation-constraint (second pl) (fourth pl))))
    ((%at-notation+ %at-notation+ |,| %at-notation) (lambda (pl) (append (second pl) (list (fourth pl)))))
    ((%at-notation+ %at-notation) (lambda (pl) (list (second pl))))
    ((%at-notation :at-with-level %component-id-list+)
     (lambda (pl) (col-c:make-at-notation (cdr (second pl)) (third pl))))
    ((%component-id-list+ %component-id-list+ |.| :id) (lambda (pl) (append (second pl) (list (car (fourth pl))))))
    ((%component-id-list+ :id) (lambda (pl) (list (cdr (second pl)))))
    ((%defined-object-class :objectclassreference) (lambda (pl) (col-c:make-defined-class (cdr (second pl)) nil)))
    ((%defined-object-class :id |.| :objectclassreference)
     (lambda (pl) (col-c:make-defined-class (cdr (fourth pl)) (cdr (second pl)))))
    ;;typereference in place of objectsetreference
    ((%defined-object-set :typereference) (lambda (pl) (col-c:make-defined-object-set (cdr (second pl)) nil)))
    ((%defined-object-set :id |.| :typereference)
     (lambda (pl) (col-c:make-defined-object-set (cdr (fourth pl)) (cdr (second pl)))))
    ((%defined-object :id |.| :id) (lambda (pl) (col-c:make-defined-object (cdr (fourth pl)) (cdr (second pl)))))
    ((%defined-object :id) (lambda (pl) (col-c:make-defined-object (cdr (second pl)) nil)))
    ((%builtin-type %object-class-field-type) #'second)
    ((%enumerated-type ENUMERATED { %enumerations })
     (lambda (pl) (col-c:make-enumerated-type (nreverse (fourth pl)))))
    ((%enumerations %root-enumeration) #'second)
    ((%root-enumeration %enumeration) #'second)
    ((%enumeration %enumeration-item) (lambda (pl) (list (second pl))))
    ((%enumeration %enumeration |,| %enumeration-item) (lambda (pl) (cons (fourth pl) (second pl))))
    ((%enumeration-item :id) (lambda (pl) (cdr (second pl))))
    ((%enumeration-item |...|) (lambda (pl) (declare (ignore pl)) :extensible))
    
    ((%octet-string-type OCTET STRING) (lambda (pl) (declare (ignore pl)) :octet-string))
    ((%parameter :typereference) (lambda (pl) (list nil (second pl))))
    ((%parameter %type |:| :id) (lambda (pl) (list (second pl) (fourth pl))))
    ((%parameter %type |:| :typereference) (lambda (pl) (list (second pl) (fourth pl))))
    ((%parameter :objectclassreference |:| :id) (lambda (pl) (list (second pl) (fourth pl))))
    ((%parameter :objectclassreference |:| :typereference) (lambda (pl) (list (second pl) (fourth pl))))

    ((%parameter+ %parameter) (lambda (pl) (list (second pl))))
    ((%parameter+ %parameter+ |,| %parameter) (lambda (pl) (append (second pl) (list (fourth pl)))))

    ((%parameter-list { %parameter+ }) #'third)

    ((%parameterized-type-assignment :typereference %parameter-list |::=| %type)
     (lambda (pl) (col-c:make-parameterized-type-assignment
                   (cdr (second pl))
                   (col-c:make-parameterized-type (third pl)
                                                     (fifth pl)))))

    ((%assignment %parameterized-type-assignment) #'second)
    ((%type %parameterized-type) #'second)
    ((%parameterized-type :typereference { %parse-later+ })
     (lambda (pl) (col-c:make-parameterized-type-invokation-raw (cdr (second pl)) (fourth pl))))
    
    ((%field-name :fieldname) (lambda (pl) (col-c:make-field-name (cdr (second pl)))))
    ((%field-name :fieldid) (lambda (pl) (col-c:make-field-name (cdr (second pl)))))
    ((%field-name %field-name |.| :fieldname) (lambda (pl) (col-c:append-field-name (second pl) (cdr (fourth pl)))))
    ((%field-name %field-name |.| :fieldid) (lambda (pl) (col-c:append-field-name (second pl) (cdr (fourth pl)))))
    ((%primitive-field-name :fieldname) #'second)
    ((%primitive-field-name :fieldid) #'second)
    ((%object-assignment :id %defined-object-class |::=| %object)
     (lambda (pl) (col-c:make-object-assignment (cdr (second pl)) (third pl) (fifth pl))))
    ((%object %defined-object) (lambda (pl) (col-c:make-object-by-reference (second pl))));check somewhere that defined-object is of specified class
    ((%object %object-definition) (lambda (pl) (col-c:make-object-by-definition (second pl))))
    ((%object-set-assignment :typereference %defined-object-class |::=| %object-set)
     (lambda (pl) (col-c:make-object-set-assignment (cdr (second pl)) (third pl) (fifth pl))))
    ((%object-set { %object-set-spec })
     (lambda (pl) (destructuring-bind (root extensible-p additional)
                      (third pl)
                    (col-c:make-object-set-raw root extensible-p additional))))
    ;;I can't figure out how these element-set-spec are meant to overlap the ones used in subtype-constraint without conflict
    ((%object-set-spec %element-set-spec-o) (lambda (pl) (list (second pl) nil nil)))
    ((%object-set-spec %element-set-spec-o |,| |...|) (lambda (pl) (list (second pl) t nil)))
    ((%object-set-spec |...|) (lambda (pl) (declare (ignore pl)) (list nil t nil)))
    ((%object-set-spec |...| |,| %element-set-spec-o) (lambda (pl) (list nil t (fourth pl))))
    ((%object-set-spec %element-set-spec-o |,| |...| |,| %element-set-spec-o)
     (lambda (pl) (list (second pl) t (sixth pl))))
    ((%element-set-spec-o %object-set-elements) #'second)
    ((%element-set-spec-o %element-set-spec-o |\|| %object-set-elements)
     (lambda (pl) (col-c:make-object-set-union (list (second pl) (fourth pl)))))
    ((%object-set-elements %object) (lambda (pl) (col-c:make-object-set-union (list (second pl)))))
    ;;check somewhere that defined-object-set is of specified class
    ((%object-set-elements %defined-object-set)
     (lambda (pl) (col-c:make-object-set-union (list (second pl)))))
    ((%object-class-field-type %defined-object-class |.| %field-name)
     (lambda (pl) (col-c:make-object-class-field-type (second pl) (fourth pl))))
    ((%object-definition { %parse-later+ }) (lambda (pl) (col-c:make-object-definition-raw (third pl))))
    ((%object-definition { }) (lambda (pl) (declare (ignore pl)) (col-c:make-object-definition-raw nil)))
    ((%parse-later+ %parse-later+ %parse-later) (lambda (pl) (append (second pl) (if (listp (cdr (third pl)))
                                                                                     ;list but not of type (cons sym val)
                                                                                     (third pl)
                                                                                     (list (third pl))))))
    ((%parse-later+ %parse-later) (lambda (pl) (if (listp (cdr (second pl)))
                                        ;list but not of type (cons sym val)
                                                   (second pl)
                                                   (list (second pl)))))
    ((%parse-later { %parse-later+ })
     (lambda (pl) (append (list (cons '{ '{)) (third pl) (list (cons '} '})))));to stop the parse-later at the correct nesting of { }
    ((%parse-later :id) #'second)
    ((%parse-later :typereference) #'second)
    ((%parse-later :fieldid) #'second)
    ((%parse-later :fieldname) #'second)
    ((%parse-later :objectclassreference) #'second)
    ((%parse-later :number) #'second)
    ((%parse-later :string) #'second)))

;;these productions (only) need to be available for pass 2; parsing the class definitions. They don't rely on class defintions themselves (at least not of the selfsame class)
(defparameter *asn.1-syntax-for-pass-2*
  '(
    ((%object-class-assignment :objectclassreference |::=| %object-class)
     (lambda (pl) (list :class-assignment (cdr (second pl)) (fourth pl))))
    ((%object-class %defined-object-class) #'second)
    ((%object-class %object-class-definition) #'second)
    ((%object-class-definition CLASS { %field-spec+ } %with-syntax-spec)
     (lambda (pl) (list :class (fourth pl) (sixth pl))))
    ((%field-spec+ %field-spec) (lambda (pl) (list (second pl))))
    ((%field-spec+ %field-spec+ |,| %field-spec) (lambda (pl) (append (second pl) (list (fourth pl)))))
    ((%with-syntax-spec))
    ((%with-syntax-spec WITH SYNTAX %syntax-list) #'fourth)
    ((%field-spec %type-field-spec) #'second)
    ((%field-spec %fixed-type-value-field-spec) #'second)
    ((%field-spec %variable-type-value-field-spec) #'second)
    ((%field-spec %fixed-type-value-set-field-spec) #'second)
    ((%field-spec %variable-type-value-set-field-spec) #'second)
    ((%field-spec %object-field-spec) #'second)
    ((%field-spec %object-set-field-spec) #'second)
    ((%type-field-spec :fieldname %type-optionality-spec)
     (lambda (pl) (make-instance 'col-c:object-class-type-field
                                 :field-name (cdr (second pl))
                                 :optional-p (first (third pl))
                                 :default-content (second (third pl)))))
    ((%type-optionality-spec) (lambda (pl) (declare (ignore pl)) (list nil nil)))
    ((%type-optionality-spec OPTIONAL) (lambda (pl) (declare (ignore pl)) (list t nil)))
    ((%type-optionality-spec DEFAULT %type) (lambda (pl) (list t (third pl))))
    ((%fixed-type-value-field-spec :fieldid %type %unique-p %value-optionality-spec)
     (lambda (pl) (make-instance 'col-c:object-class-fixed-type-value-field
                                 :field-name (cdr (second pl))
                                 :optional-p (first (fifth pl))
                                 :default-content (second (fifth pl))
                                 :c-type (third pl)
                                 :unique-p (fourth pl))))
    ((%unique-p) (lambda (pl) (declare (ignore pl)) nil))
    ((%unique-p UNIQUE) (lambda (pl) (declare (ignore pl)) t))
    ((%value-optionality-spec) (lambda (pl) (declare (ignore pl)) (list nil nil)))
    ((%value-optionality-spec OPTIONAL) (lambda (pl) (declare (ignore pl)) (list t nil)))
    ((%value-optionality-spec DEFAULT %value) (lambda (pl) (list t (third pl))))
    ((%variable-type-value-field-spec :fieldid %field-name %value-optionality-spec)
     (lambda (pl) (make-instance 'col-c:object-class-variable-type-value-field
                                 :field-name (cdr (second pl))
                                 :optional-p (first (fourth pl))
                                 :default-content (second (fourth pl))
                                 :type-field-name (third pl))))
    ((%fixed-type-value-set-field-spec :fieldname %type %value-set-optionality-spec)
     (lambda (pl) (make-instance 'col-c:object-class-fixed-type-value-set-field
                                 :field-name (cdr (second pl))
                                 :optional-p (first (fourth pl))
                                 :default-content (second (fourth pl))
                                 :c-type (third pl))))
    ((%value-set-optionality-spec) (lambda (pl) (declare (ignore pl)) (list nil nil)))
    ((%value-set-optionality-spec OPTIONAL) (lambda (pl) (declare (ignore pl)) (list t nil)))
    ((%value-set-optionality-spec DEFAULT %value-set) (lambda (pl) (list t (third pl))))
    ((%variable-type-value-set-field-spec :fieldname %field-name %value-set-optionality-spec)
     (lambda (pl) (make-instance 'col-c:object-class-variable-type-value-set-field
                                 :field-name (cdr (second pl))
                                 :optional-p (first (fourth pl))
                                 :default-content (second (fourth pl))
                                 :type-field-name (third pl))))
    ((%object-field-spec :fieldid %defined-object-class %object-optionality-spec)
     (lambda (pl) (make-instance 'col-c:object-class-object-field
                                 :field-name (cdr (second pl))
                                 :optional-p (first (fourth pl))
                                 :default-content (second (fourth pl))
                                 :defined-object-class (third pl))))
    ((%object-optionality-spec) (lambda (pl) (declare (ignore pl)) (list nil nil)))
    ((%object-optionality-spec OPTIONAL) (lambda (pl) (declare (ignore pl)) (list t nil)))
    ((%object-optionality-spec DEFAULT %object) (lambda (pl) (list t (third pl))))
    ((%object-set-field-spec :fieldname %defined-object-class %object-set-optionality-spec)
     (lambda (pl) (make-instance 'col-c:object-class-object-set-field
                                 :field-name (cdr (second pl))
                                 :optional-p (first (fourth pl))
                                 :default-content (second (fourth pl))
                                 :defined-object-class (third pl))))
    ((%object-set-optionality-spec) (lambda (pl) (declare (ignore pl)) (list nil nil)))
    ((%object-set-optionality-spec OPTIONAL) (lambda (pl) (declare (ignore pl)) (list t nil)))
    ((%object-set-optionality-spec DEFAULT %object-set) (lambda (pl) (list t (third pl))))))
    
