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

(in-package :parse-out-of-order)

;;During either traverse-instantiate of an object class assignment, collection of references in a type-template (by traversal) or parsing of actual parameter list for an invocation of a parameterized type it may happen that we encounter an object definition for a class whose class assignment has not yet been processed or an invocation of a parameterized type whose assignment has not yet been processed.
;;(The processing of the object class assignment includes traverse-instantiate. The processing of a parameterized type assignment involves both collection of references and parsing of the actual parameter list.)
(define-condition parse-out-of-order (error)
  ())
