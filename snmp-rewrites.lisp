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

;;The following is a modified copy of parts of the cl-net-snmp package
;;(https://github.com/binghe/cl-net-snmp), which included this notice and license

;;Copyright (c) 2007-2009, 2011 Chun Tian (binghe) <binghe.lisp@gmail.com>

;;Permission is hereby granted, free of charge, to any person obtaining a copy
;;of this software and associated documentation files (the "Software"), to deal
;;in the Software without restriction, including without limitation the rights
;;to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;copies of the Software, and to permit persons to whom the Software is
;;furnished to do so, subject to the following conditions:

;;The above copyright notice and this permission notice shall be included in
;;all copies or substantial portions of the Software.

;;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;THE SOFTWARE.

(in-package :asn.1)

(defparameter *reserved-words*
  '(ABSENT ENCODED INTEGER RELATIVE-OID
    ABSTRACT-SYNTAX END INTERSECTION SEQUENCE 
    ALL ENUMERATED |ISO646String| SET
    APPLICATION EXCEPT MAX SIZE
    AUTOMATIC EXPLICIT MIN STRING
    BEGIN EXPORTS MINUS-INFINITY SYNTAX 
    BIT EXTENSIBILITY NULL |T61String|
    |BMPString| EXTERNAL |NumericString| TAGS
    BOOLEAN FALSE OBJECT |TeletexString|
    BY FROM |ObjectDescriptor| TRUE
    CHARACTER |GeneralizedTime| OCTET TYPE-IDENTIFIER
    CHOICE |GeneralString| OF UNION
    CLASS |GraphicString| OPTIONAL UNIQUE
    COMPONENT |IA5String| PATTERN UNIVERSAL
    COMPONENTS IDENTIFIER PDV |UniversalString|
    CONSTRAINED IMPLICIT PLUS-INFINITY |UTCTime|
    CONTAINING IMPLIED PRESENT |UTF8String|
    DEFAULT IMPORTS |PrintableString| |VideotexString|
    DEFINITIONS INCLUDES PRIVATE |VisibleString|
    EMBEDDED INSTANCE REAL WITH
    ;; Additional keywords
    |::=| |[| |]| { } |,| |(| |)| |;| |:| |.| |..| |...| |\|| < >
    MACRO TYPE VALUE NOTATION
    MODULE-IDENTITY OBJECT-TYPE NOTIFICATION-TYPE TRAP-TYPE
    TEXTUAL-CONVENTION MODULE-COMPLIANCE OBJECT-GROUP
    NOTIFICATION-GROUP OBJECT-IDENTITY
    MODULE AGENT-CAPABILITIES
    WRITE-SYNTAX))

(defun asn.1-lexer (stream)
  (let ((*readtable* *asn.1-readtable*)
        (*package* *asn.1-package*))
    (let ((token (read stream nil nil nil)))
      (when token
        (if (and (stringp token)
                 (> (length token) 0)
                 (eql (elt token 0) #\@))
            (values :at-with-level (- (length token) 1))
            (values (detect-token token) token))))))

(defun lexicality (token)
  (cond 
    ((upper-case-p (elt (symbol-name token) 0))
     :typereference)
    ((eql #\& (elt (symbol-name token) 0))
     (if (upper-case-p (elt (symbol-name token) 1))
         :fieldname;typefieldreference, valuesetfieldreference, objectsetfieldreference
         :fieldid));valuefieldreference, objectfieldreference
    (t
     :id)))

(defmethod detect-token ((token symbol))
  (gethash token *reserved-words-table* (lexicality token)))

(defun |@-reader| (stream char &optional (accumulated "@"))
  (declare (ignore char))
  (let ((next-char (read-char stream nil)));;no valid mib can have eof in this situation
    (case next-char
      (#\. (|@-reader| stream next-char (format nil "~a." accumulated)))
      (t (unread-char next-char stream)
         accumulated))))

(defun make-asn.1-readtable ()
  ;; Create a new readtable
  (setf *asn.1-readtable* (copy-readtable nil))
  ;; Case-Sensitivity
  (setf (readtable-case *asn.1-readtable*) :preserve)
  ;; ASN.1 One-Line Comment
  (set-macro-character #\- #'|--reader| t *asn.1-readtable*)
  ;; ASN.1 Multi-Line Comment
  (set-macro-character #\/ #'|/-reader| nil *asn.1-readtable*)
  ;; Asn.1 Non-Decimal Number
  (set-macro-character #\' #'|'-reader| nil *asn.1-readtable*)
  ;; Special Combine Symbol (::=)
  (set-macro-character #\: #'|:-reader| nil *asn.1-readtable*)
  ;; Special Combine Symbol (.. and ...)
  (set-macro-character #\. #'|.-reader| nil *asn.1-readtable*)
  ;; Special Combine Symbol (</ and <)
  (set-macro-character #\< #'|<-reader| nil *asn.1-readtable*)
  ;; Special Combine Symbol ([, [[, ], ]])
  (set-macro-character #\[ #'single-or-double-macro-character
                       nil *asn.1-readtable*)
  (set-macro-character #\] #'single-or-double-macro-character
                       nil *asn.1-readtable*)
  (set-macro-character #\@ #'|@-reader| nil *asn.1-readtable*);Patrik
  ;; ASN.1 Single Character Symbol
  (set-macro-character #\, #'single-macro-character nil *asn.1-readtable*)
  (set-macro-character #\{ #'single-macro-character nil *asn.1-readtable*)
  (set-macro-character #\} #'single-macro-character nil *asn.1-readtable*)
  (set-macro-character #\> #'single-macro-character nil *asn.1-readtable*)
  (set-macro-character #\( #'single-macro-character nil *asn.1-readtable*)
  (set-macro-character #\) #'single-macro-character nil *asn.1-readtable*)
  (set-macro-character #\; #'single-macro-character nil *asn.1-readtable*)
  ;;(set-macro-character #\@ #'single-macro-character nil *asn.1-readtable*)
  (set-macro-character #\! #'single-macro-character nil *asn.1-readtable*)
  (set-macro-character #\^ #'single-macro-character nil *asn.1-readtable*)
  (set-macro-character #\| #'single-macro-character nil *asn.1-readtable*))

(eval-when (:load-toplevel :execute)
  (fill-reserved-words))

(eval-when (:load-toplevel :execute)
  (make-asn.1-readtable))
