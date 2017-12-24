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

(in-package :data-parse-classes)

(defclass data-parse ()
  ((syntax :initarg :syntax :reader syntax)
   (data :initarg :data :reader data)))

(defclass data-parse-sequence-of (data-parse)
  ((type-name :initarg :type-name :reader type-name)))
;;data är listan av typ-värden

(defclass data-parse-sequence-component (data-parse)
  (;;(optional-p :initarg :optional-p :reader optional-p);tillhör syntax, inte data
   ;;(default :initarg :default :reader default)
   (name :initarg :name :reader name)))
;;data är värdet

(defclass data-parse-sequence (data-parse)
  ())
;;data är lista av data-parse-sequence-component

(defclass data-parse-choice (data-parse)
  ((choice-name :initarg :choice-name :reader choice-name)))
;;data är värdet av det alternativ som pekades ut, som också hade namnet choice-name
