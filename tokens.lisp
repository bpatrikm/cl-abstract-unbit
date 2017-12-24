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

(in-package :tokens)

(defun retokenize-object-class-references (token-list object-class-references)
  (mapcar (lambda (token)
            (if (and (eql (car token) :typereference)
                     (find (cdr token) object-class-references))
                (cons :objectclassreference (cdr token))
                token))
          token-list))

(defun retokenize-as-terminals (token-list to-be-terminals)
  (mapcar (lambda (token)
            (if (find (cdr token) to-be-terminals)
                (cons (cdr token) (cdr token))
                token))
          token-list))

;;get rid of any keywords except for {}[],
(defun retokenize-for-with-syntax (class-syntax)
  (mapcar (lambda (token)
            (if (find (cdr token) '(asn.1::{ asn.1::} asn.1::[ asn.1::] asn.1::|,|))
                token
                (cons (asn.1::lexicality (cdr token)) (cdr token))))
          class-syntax))

(defun parse-from-tokens (token-list rules start-non-terminal &optional extra-terminal)
  (let ((token-list (if extra-terminal
                        (append (cdr token-list) (list (cons extra-terminal extra-terminal)));copy-seq implied
                        (copy-seq (cdr token-list))))
        (lookahead (car token-list)))
    (flet ((sec-lexer ()
             (let ((popped-cons (pop token-list)))
               (values (car popped-cons) (cdr popped-cons)))))
      (let* ((states (build-state-machine rules start-non-terminal))
             (starting-state (car (last states))))
        (run-state-machine (list starting-state) nil lookahead #'sec-lexer)))))
