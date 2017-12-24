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

(in-package :asdf)

(defsystem "lalr-asn.1-prelim"
  :components ((:file "snmp-rewrites")
               (:file "package-collation")
               (:file "collation-classes" :depends-on ("package-collation"))
               (:file "packages" :depends-on ("collation-classes")))
  :depends-on (snmp))

(defsystem "lalr-asn.1"
  :components ((:file "assign-ht")
               (:file "lalr-asn.1")
               (:file "asn.1-mod-parse-later")
               (:file "production-gen")
               (:file "lex-excise")
               (:file "parse-out-of-order")
               (:file "parsing-medium")
               (:file "traverse-medium")
               (:file "tokens")
               (:file "instantiate-object-class")
               (:file "parse-high-class")
               (:file "parse-high-parameter")
               (:file "traverse-high-parameter")
               (:file "traverse-high")
               (:file "import-mib-class")
               (:file "import-mib")
               (:file "data-parse-classes")
               (:file "bit-reader")
               (:file "flatten-object-sets")
               (:file "read-general")
               (:file "read-per"))
  :depends-on (lalr-asn.1-prelim))
