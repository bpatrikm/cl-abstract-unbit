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

(in-package :traverse-high)

;;parse, av object-definition eller parameterized type invocation görs på nedvägen. Analyse görs på uppvägen

(defun traverse-process-parse-tree (parse-tree assign-ht type-references &optional context-class-name)
  (when (typep parse-tree 'parse-object-definition-raw)
    (unless context-class-name (error 'parse-out-of-order))
    (let ((set-fields
           (collate-parse-tree
            (parse-object-definition-by-class context-class-name
                                              (object-class-definition-ht assign-ht)
                                              (object-class-syntax-ht assign-ht)
                                              (tokens parse-tree)))))
      (change-to-object-class-instance context-class-name
                                       (object-class-definition-ht assign-ht)
                                       set-fields
                                       parse-tree)))
  (when (typep parse-tree 'parse-parameterized-type-invokation-raw)
    (parse-convert-invokation parse-tree assign-ht))
  ;;verifiera defined-object/defined-object-set mot context-class
  (multiple-value-bind (sub-parse-tree-list new-context-class-name-list)
      (sub-parse-tree-with-class-context parse-tree assign-ht)
    (if new-context-class-name-list
        (mapcar (lambda (sub-parse-tree new-context-class-name)
                  (traverse-process-parse-tree sub-parse-tree assign-ht type-references (or new-context-class-name
                                                                                            context-class-name)))
                sub-parse-tree-list
                new-context-class-name-list)
        (mapcar (lambda (sub-parse-tree)
                  (traverse-process-parse-tree sub-parse-tree assign-ht type-references context-class-name))
                sub-parse-tree-list)))
  (when (typep parse-tree 'parse-object-by-definition);;anneal a (now useless) level
    (let ((class-name (class-name (object-definition parse-tree)))
          (fields (fields (object-definition parse-tree))))
      (change-class parse-tree 'parse-object-instantiated :class-name class-name :fields fields)))
  (when (typep parse-tree 'parse-parameterized-type)
    (traverse-analyse-parameterized-type parse-tree assign-ht type-references)))
