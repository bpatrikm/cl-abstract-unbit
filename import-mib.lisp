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

(in-package :import-mib)

(defun import-mib (filepath assign-ht)
  ;;Definitions of classes and parameterized types need to be handled before the rest of the mib - and among them they need to be handled in a certain (to be found) order
  ;;This is because instantiations of classes and invokations of parameterized types are done in clauses that have case-specifics grammers
  ;;So, those definitions are cut out of the main mib on the token-list level, and the import of them is then tried over and over as long as something is successful in each try. After that, the main mib is processed.
  ;;Names of defined classes are identified in the first step, and with that data the lexer will read those names as :objectclassreference instead of :typereference.
  ;;The case-specific parsing of instantiations and invokations also require require the corresponding case-specific terminals to be read as keywords. This is done by retokenizing those symbols before the parse.
  ;;In order to be able to apply the case-specific syntax and retokenizing, the clauses that would be read as instantiations are instead read by the %parse-later-productions. These will simply collect all the tokens within a pair of {} to a separate token-list.
  ;;In each import, the lalr-parser builds a parse-tree. This is collated by recursively (bottom-up) invoking the collation-function which is part of the productions. The resulting tree is then traversed again in a top-down process where parse-later-clauses are evaluated by case-specific grammers, and the results are again collated and traversed. (Please note that, in this traversal the recursion from an invokation of a parameterized type goes into the arguments of the actual-parameter-list, not into the body /template of the type).
  ;;For imports of definitions of parameterized types, there is another kind of traverse which is also performed. This consists of finding all references within the template. Comparing these to the parameter-list allows us to determine which parameters are dummies, and if so what type they are (:type, :class-name, :value, :value-set, :object or :object-set) by analyzing their use within the template. With this information, the case-specific grammer can be constructed.
  
  (destructuring-bind (main-token-list class-definitions class-syntaxes)
      (lex-and-excise-class-assignments-by-definition filepath)
    (let ((object-class-references (append (remove-duplicates (mapcar #'cdar class-definitions))
                                           (let (old-object-class-references)
                                             (maphash (lambda (k v)
                                                        (declare (ignore v))
                                                        (push k old-object-class-references))
                                                      (object-class-definition-ht assign-ht))
                                             old-object-class-references))))
      (let ((main-mib-collated
             (collate-parse-tree
              (parse-from-tokens (retokenize-object-class-references main-token-list object-class-references)
                                 (import-asn.1-grammer
                                  (append (make-parse-later-productions asn.1::*reserved-words*)
                                          asn.1::*asn.1-syntax-mod*))
                                 'asn.1::%root)))
            type-references)
        ;;in parameterized type parameter lists, governor is distinguished from dummygovernor by having an existing binding
        (flet ((pushref (k v) (declare (ignore v)) (push k type-references)))
          (maphash #'pushref (type-assignment-ht assign-ht))
          (maphash #'pushref (parameterized-type-assignment-ht assign-ht)))
        (dolist (module-expression (second (third main-mib-collated)))
          (when (typep module-expression 'parse-parameterized-type-assignment)
            (push (parameterized-type-name module-expression) type-references))
          (when (and (listp module-expression)
                     (eql :type-assignment (car module-expression)))
            (push (cdr (second module-expression)) type-references)))
        
        (labels ((import-classes (class-definitions class-syntaxes &optional failed-definitions failed-syntaxes something-succeeded-p)
                   (if (not class-definitions)
                       (values (nreverse failed-definitions)
                               (nreverse failed-syntaxes)
                               something-succeeded-p)
                       (if (handler-case
                               (progn
                                 (parse-and-store-class-assignment (car class-definitions) (car class-syntaxes) object-class-references type-references assign-ht)
                                 t)
                             (parse-out-of-order nil))
                           (import-classes (cdr class-definitions) (cdr class-syntaxes) failed-definitions failed-syntaxes t)
                           (import-classes (cdr class-definitions) (cdr class-syntaxes) (cons (car class-definitions) failed-definitions) (cons (car class-syntaxes) failed-syntaxes) something-succeeded-p))))
                 
                 (import-parameterized-types (assignments &optional failed-assignments something-succeeded-p)
                   (if (not assignments)
                       (values (nreverse failed-assignments)
                               something-succeeded-p)
                       (if (handler-case
                               (progn
                                 (traverse-process-parse-tree (car assignments) assign-ht type-references)
                                 t)
                             (parse-out-of-order nil))
                           (progn (setf (gethash (parameterized-type-name (car assignments)) (parameterized-type-assignment-ht assign-ht))
                                        (parameterized-type (car assignments)))
                                  (import-parameterized-types (cdr assignments) failed-assignments t))
                           (import-parameterized-types (cdr assignments) (cons (car assignments) failed-assignments) something-succeeded-p))))
                 
                 (keep-trying-imports (class-definitions class-syntaxes parameterized-type-assignments)
                   (when (or class-definitions parameterized-type-assignments)
                     (multiple-value-bind (class-definitions class-syntaxes some-class-succeeded-p)
                         (import-classes class-definitions class-syntaxes)
                       (multiple-value-bind (parameterized-type-assignments some-type-succeeded-p)
                           (import-parameterized-types parameterized-type-assignments)
                         (unless (or some-class-succeeded-p some-type-succeeded-p)
                           (error 'parse-out-of-order))
                         (keep-trying-imports class-definitions class-syntaxes parameterized-type-assignments))))))
          
          (keep-trying-imports class-definitions
                               class-syntaxes
                               (remove-if-not (lambda (module-expression)
                                                (typep module-expression 'parse-parameterized-type-assignment))
                                              (second (third main-mib-collated))))
          (let ((remaining-module-expressions
                 (remove-if (lambda (module-expression)
                              (typep module-expression 'parse-parameterized-type-assignment))
                            (second (third main-mib-collated)))))
            (mapcar (lambda (module-expression)
                      (cond ((and (listp module-expression)
                                  (eql :value-assignment (car module-expression)))
                             (setf (gethash (cdr (second module-expression)) (value-assignment-ht assign-ht))
                                   ;;the third element of module-expression is type TODO complex values (like sequence-values or value-sets)
                                   (cdr (fourth module-expression))))
                            ((and (listp module-expression)
                                  (eql :type-assignment (car module-expression)))
                             (setf (gethash (cdr (second module-expression)) (type-assignment-ht assign-ht))
                                   (third module-expression)))
                            ((typep module-expression 'parse-object-set-assignment)
                             (setf (gethash (object-set-name module-expression) (object-set-assignment-ht assign-ht))
                                   (object-set module-expression)))
                            ((typep module-expression 'parse-object-assignment)
                             (setf (gethash (object-name module-expression) (object-assignment-ht assign-ht))
                                   (object module-expression)))))
                    remaining-module-expressions)
            (traverse-process-parse-tree remaining-module-expressions assign-ht type-references)))))))

(defun tester-nbap ()
  (let ((assign-ht (make-instance 'assign-ht)))
    (import-mib "/home/patrik/3gpp/25.433_UMTS_NBAP_common_definitions.mib" assign-ht)
    (format t "1 klar~%")
    (import-mib "/home/patrik/3gpp/25.433_UMTS_NBAP_constant_definitions.mib" assign-ht)
    (format t "2 klar~%")
    (import-mib "/home/patrik/3gpp/25.433_UMTS_NBAP_container_definitions.mib" assign-ht)
    (format t "3 klar~%")
    (import-mib "/home/patrik/3gpp/25.433_UMTS_NBAP_information_element_definitions.mib" assign-ht)
    (format t "4 klar~%")
    (import-mib "/home/patrik/3gpp/25.433_UMTS_NBAP_elementary_procedure_definitions.mib" assign-ht)
    (format t "5 klar~%")
    (import-mib "/home/patrik/3gpp/25.433_UMTS_NBAP_pdu_definitions.mib" assign-ht)
    assign-ht))

(defun tester-rrc ()
  (let ((assign-ht (make-instance 'assign-ht)))
    (import-mib "/home/patrik/3gpp/25.331_UMTS_RRC_class_definitions.mib" assign-ht)
    (format t "1 klar~%")
    (import-mib "/home/patrik/3gpp/25.331_UMTS_RRC_constant_definitions.mib" assign-ht)
    (format t "2 klar~%")
    (import-mib "/home/patrik/3gpp/25.331_UMTS_RRC_informationelements.mib" assign-ht)
    (format t "3 klar~%")
    (import-mib "/home/patrik/3gpp/25.331_UMTS_RRC_internode_definitions.mib" assign-ht)
    (format t "4 klar~%")
    (import-mib "/home/patrik/3gpp/25.331_UMTS_RRC_pdu_definitions.mib" assign-ht)
    assign-ht))
