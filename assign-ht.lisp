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

(in-package :assign-ht)

(defclass assign-ht ()
  ((value-assignment-ht :initform (make-hash-table) :reader value-assignment-ht)
   (type-assignment-ht :initform (make-hash-table) :reader type-assignment-ht)
   (object-class-definition-ht :initform (make-hash-table) :reader object-class-definition-ht)
   (object-class-syntax-ht :initform (make-hash-table) :reader object-class-syntax-ht)
   (object-assignment-ht :initform (make-hash-table) :reader object-assignment-ht)
   (object-set-assignment-ht :initform (make-hash-table) :reader object-set-assignment-ht)
   (parameterized-type-assignment-ht :initform (make-hash-table) :reader parameterized-type-assignment-ht)))
