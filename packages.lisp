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

;;the collation-classes package exports all of its internal symbols

(defpackage assign-ht
  (:export assign-ht
           value-assignment-ht
           type-assignment-ht
           object-class-definition-ht
           object-class-syntax-ht
           object-assignment-ht
           object-set-assignment-ht
           parameterized-type-assignment-ht)
  (:use :cl))

(defpackage lalr
  (:export rule
           collator
           build-state-machine
           run-state-machine
           import-asn.1-grammer)
  (:use :cl))

(defpackage asn.1-mod
  (:export make-parse-later-productions
           defined-syntax
           default-syntax)
  (:shadow reference)
  (:use :cl :asn.1 :collation-classes))

(defpackage lex-excise
  (:export lex-and-excise-class-assignments-by-definition)
  (:use :cl))

(defpackage parse-out-of-order
  (:export parse-out-of-order)
  (:use :cl))

(defpackage parsing-medium
  (:export collate-parse-tree)
  (:use :cl)
  (:import-from :lalr
                rule
                collator))

(defpackage traverse-medium
  (:export sub-parse-tree-with-class-context)
  (:use :collation-classes :cl)
  (:import-from :assign-ht parameterized-type-assignment-ht))

(defpackage tokens
  (:export retokenize-object-class-references
           retokenize-as-terminals
           retokenize-for-with-syntax
           parse-from-tokens)
  (:use :cl)
  (:import-from :lalr
                build-state-machine
                run-state-machine))

(defpackage instantiate-object-class
  (:export change-to-object-class-instance)
  (:use :collation-classes :cl))

(defpackage parse-high-class
  (:export parse-object-definition-by-class)
  (:use :cl)
  (:import-from :tokens
                retokenize-as-terminals
                parse-from-tokens)
  (:import-from :asn.1-mod
                make-parse-later-productions
                defined-syntax
                default-syntax)
  (:import-from :lalr import-asn.1-grammer))

(defpackage parse-high-parameter
  (:export parse-convert-invokation)
  (:use :cl)
  (:import-from :collation-classes
                parse-parameterized-type-invokation-raw
                parse-parameterized-type-invokation
                parameterized-type-name
                parameter-analysis-list
                actual-parameter-token-list)
  (:import-from :parse-out-of-order parse-out-of-order)
  (:import-from :assign-ht parameterized-type-assignment-ht)
  (:import-from :parsing-medium collate-parse-tree)
  (:import-from :tokens parse-from-tokens)
  (:import-from :lalr import-asn.1-grammer))

(defpackage traverse-high-parameter
  (:export traverse-analyse-parameterized-type)
  (:use :cl :collation-classes)
  (:import-from :traverse-medium sub-parse-tree-with-class-context))

(defpackage traverse-high
  (:export traverse-process-parse-tree)
  (:use :cl :collation-classes :assign-ht)
  (:import-from :parsing-medium collate-parse-tree)
  (:import-from :parse-out-of-order parse-out-of-order)
  (:import-from :parse-high-class parse-object-definition-by-class)
  (:import-from :parse-high-parameter parse-convert-invokation)
  (:import-from :instantiate-object-class change-to-object-class-instance)
  (:import-from :traverse-medium sub-parse-tree-with-class-context)
  (:import-from :traverse-high-parameter traverse-analyse-parameterized-type))

(defpackage import-mib-class
  (:export parse-and-store-class-assignment)
  (:use :cl)
  (:import-from :lalr import-asn.1-grammer)
  (:import-from :assign-ht
                object-class-definition-ht
                object-class-syntax-ht)
  (:import-from :parsing-medium collate-parse-tree)
  (:import-from :tokens
                retokenize-object-class-references
                retokenize-for-with-syntax
                parse-from-tokens)
  (:import-from :traverse-high traverse-process-parse-tree))

(defpackage import-mib
  (:export import-mib tester-nbap tester-rrc)
  (:use :cl :collation-classes :assign-ht)
  (:import-from :lex-excise lex-and-excise-class-assignments-by-definition)
  (:import-from :lalr import-asn.1-grammer)
  (:import-from :asn.1-mod make-parse-later-productions)
  (:import-from :tokens
                parse-from-tokens
                retokenize-object-class-references)
  (:import-from :parse-out-of-order parse-out-of-order)
  (:import-from :parsing-medium collate-parse-tree)
  (:import-from :traverse-high traverse-process-parse-tree)
  (:import-from :import-mib-class parse-and-store-class-assignment))

(defpackage data-parse-classes
  (:export data-parse
           data-parse-sequence-of
           data-parse-sequence-component
           data-parse-sequence
           data-parse-choice
           syntax
           data
           type-name
           name
           choice-name)
  (:use :cl))
  
(defpackage bit-reader
  (:export bit-reader
           bit-reader-file
           bit-reader-bitstring
           bit-reader-aligned
           bit-reader-unaligned
           bit-reader-aligned-file
           bit-reader-aligned-bitstring
           bit-reader-unaligned-file
           bit-reader-unaligned-bitstring
           read-bits
           align
           pre-align-read-bits)
  (:use :cl))

(defpackage flatten-object-sets
  (:export flatten-object-sets)
  (:use :cl :collation-classes))

(defpackage read-general
  (:export combine-constraints
           get-table-constraint
           remove-table-constraints
           get-dereferenced-class
           get-dereferenced-value
           lower-bound-by-constraint
           upper-bound-by-constraint
           extensible-size-constraints-p
           read-value
           value-eql
           read-octets-unsigned
           nread-octets-signed
           resolve-open-type)
  (:use :cl :collation-classes :data-parse-classes :assign-ht :flatten-object-sets))

(defpackage read-per
  (:export read-per)
  (:use :cl :collation-classes :data-parse-classes :assign-ht :bit-reader :read-general))
