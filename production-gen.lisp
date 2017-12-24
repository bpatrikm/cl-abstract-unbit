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

(in-package :asn.1-mod)

;;generated rules for parse-later to produce any keyword except for { }
(defun make-parse-later-productions (keywords)
  (mapcar (lambda (keyword)
            `((asn.1::%parse-later ,keyword) #'second))
          (remove 'asn.1::{ (remove 'asn.1::} keywords))))

(defun default-syntax (field-specs)
  (if (not field-specs)
      '(((asn.1::%object-definition-syntax asn.1::{ asn.1::})))
      `(((asn.1::%object-definition-syntax asn.1::{ asn.1::%field-setting+ asn.1::}) #'third)
        ((asn.1::%field-setting+ asn.1::%field-setting+ asn.1::|,| asn.1::%field-setting) (lambda (pl) (append (second pl)
                                                                                                               (list (fourth pl)))))
        ((asn.1::%field-setting+ asn.1::%field-setting) (lambda (pl) (list (second pl))))
        ,@(mapcar (lambda (field-spec)
                    `((asn.1::%field-setting ,(field-name field-spec) ,(content-non-terminal field-spec))
                      (lambda (pl) (list ,(field-name field-spec) (third pl)))))
                  field-specs))))

;;specific choice of setting->... for each field
(defun defined-syntax (defined-syntax-productions field-specs)
  ;;within defined-syntax-productions there a entries with two elements, the second being (:setting-standin name) that needs to be replaced by one of the non-terminals that %setting can produce, based on the corresponding field-spec
  ;;the field-specs are collected into lists where the first entry is a keyword for the kind of field it is, then the name, followed by class if applicable (object and object-set)
  (cons `((asn.1::%object-definition-syntax asn.1::{ ,(caaar defined-syntax-productions) asn.1::}) #'third)
        (mapcar (lambda (prod)
                  (if (and (eql (length (car prod)) 3)
                           (eql :setting-standin (second (car prod))))
                      (let* ((field-name (third (car prod)))
                             (field-spec (find field-name field-specs :test #'equal :key #'field-name)))
                        (unless field-spec (error "didn't find field-spec"))
                        `((,(first (car prod)) ,(content-non-terminal field-spec))
                          ,@(cdr prod)));the action
                      prod))
                defined-syntax-productions)))
