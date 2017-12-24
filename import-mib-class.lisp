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

(in-package :import-mib-class)

;;släpper parse-out-of-order förbi sig
(defun parse-and-store-class-assignment (class-definition class-syntax object-class-references type-references assign-ht)
  (let ((class-definition (retokenize-object-class-references class-definition object-class-references))
        (class-syntax (retokenize-for-with-syntax class-syntax))
        (rules-definition (import-asn.1-grammer (append asn.1::*asn.1-syntax-mod*
                                                        asn.1::*asn.1-syntax-for-pass-2*
                                                        '(((%object-class-c asn.1::%object-class-assignment asn.1::END) #'second)))))
        (rules-syntax (import-asn.1-grammer (append asn.1::*with-syntax-syntax*))))
    (let ((class-definition-parse
           (collate-parse-tree (parse-from-tokens class-definition
                                                  rules-definition
                                                  '%object-class-c
                                                  'asn.1::END)))
          (class-syntax-parse
           (collate-parse-tree (parse-from-tokens class-syntax
                                                  rules-syntax
                                                  'asn.1::%syntax-list))))
      (mapcar (lambda (field-spec)
                (traverse-process-parse-tree field-spec assign-ht type-references))
              (second (third class-definition-parse)))
      ;;This is past the point where parse-out-of-order can occur
      (setf (gethash (second class-definition-parse) (object-class-definition-ht assign-ht))
            (second (third class-definition-parse))
            (gethash (second class-definition-parse) (object-class-syntax-ht assign-ht))
            class-syntax-parse))))
