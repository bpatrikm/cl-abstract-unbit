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

(in-package :parse-high-class)

(defun parse-object-definition-by-class (class-name object-class-definition-ht object-class-syntax-ht token-list)
  ;;signalera när definitionen för klass inte finns tillgänglig
  (let* ((defined-syntax-productions (gethash class-name object-class-syntax-ht))
         (field-specs (gethash class-name object-class-definition-ht))
         (literal-keywords
                 (remove-duplicates
                  (reduce #'append
                          (mapcar (lambda (production)
                                    (remove-if-not (lambda (term)
                                                     (and
                                                      (not (consp term))
                                                      (symbol-package term);;nil for gensym:ed
                                                      (not (eql #\& (elt (symbol-name term) 0)))
                                                      (not (eql #\% (elt (symbol-name term) 0)))))
                                                   (cdr production)))
                                  (mapcar #'car defined-syntax-productions)))
                  :test #'equal))
         (token-list (cons (cons 'asn.1::{ 'asn.1::{) (retokenize-as-terminals token-list literal-keywords)))
         (parse-later-token-productions (make-parse-later-productions literal-keywords))
         (object-definition-syntax-productions
          (if defined-syntax-productions
              (defined-syntax defined-syntax-productions field-specs)
              (default-syntax field-specs)))
         (rules
          (import-asn.1-grammer (append asn.1::*asn.1-syntax-mod*
                                        parse-later-token-productions
                                        object-definition-syntax-productions))))
    (parse-from-tokens token-list rules 'asn.1::%object-definition-syntax 'asn.1::})))
