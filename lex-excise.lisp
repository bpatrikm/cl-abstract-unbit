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

(in-package :lex-excise)

(defun lex-and-excise-class-assignments-by-definition (file-path)
  ;;object-class assignments are lifted out, and the rest is left as is
  (let ((token-list
         (with-open-file (in file-path)
           (labels ((lexer () (asn.1::asn.1-lexer in))
                    (lex-and-collect (accumulated)
                      (multiple-value-bind (sym val)
                          (lexer)
                        (if (not sym)
                            (nreverse accumulated)
                            (lex-and-collect (cons (cons sym val) accumulated))))))
             (lex-and-collect nil)))))
    (labels ((excise (token-list main-accumulated class-definitions class-syntaxes)
               (if (< (length token-list) 4)
                   (list (append (nreverse main-accumulated) token-list);terminate recursion
                         class-definitions
                         class-syntaxes)
                   (if (and (eql :typereference (car (first token-list)))
                            (eql 'asn.1::|::=| (car (second token-list)))
                            (eql 'asn.1::CLASS (car (third token-list)))
                            (eql 'asn.1::{ (car (fourth token-list))))
                       (destructuring-bind
                             (token-list class-definition)
                           (divert-clause (subseq token-list 4)
                                          1
                                          (nreverse (subseq token-list 0 4)))
                         (if (and (eql 'asn.1::WITH (car (first token-list)))
                                  (eql 'asn.1::SYNTAX (car (second token-list)))
                                  (eql 'asn.1::{ (car (third token-list))))
                             (destructuring-bind
                                   (token-list class-syntax)
                                 (divert-clause (subseq token-list 3)
                                                1
                                                (list (third token-list)))
                               (excise token-list main-accumulated (cons class-definition class-definitions) (cons class-syntax class-syntaxes)))
                             (excise token-list main-accumulated (cons class-definition class-definitions) class-syntaxes)))
                       (excise (cdr token-list) (cons (car token-list) main-accumulated) class-definitions class-syntaxes))))
             (divert-clause (token-list nesting-level accumulated)
               (if (eql nesting-level 0)
                   (list token-list (nreverse accumulated));terminate recursion
                   (divert-clause (cdr token-list)
                                  (if (eql (car (first token-list)) 'asn.1::{)
                                      (+ nesting-level 1)
                                      (if (eql (car (first token-list)) 'asn.1::})
                                          (- nesting-level 1)
                                          nesting-level))
                                  (cons (car token-list) accumulated)))))
      (excise token-list nil nil nil))))
