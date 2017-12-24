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

(in-package :instantiate-object-class)

(defmethod create-object-class-instance-field ((ocf object-class-type-field) content)
  (make-instance 'object-class-instance-type-field :content content :field-name (field-name ocf) :optional-p (optional-p ocf) :default-content (default-content ocf)))

(defmethod create-object-class-instance-field ((ocf object-class-fixed-type-value-field) content)
  (make-instance 'object-class-instance-fixed-type-value-field :content content :field-name (field-name ocf) :optional-p (optional-p ocf) :default-content (default-content ocf) :c-type (c-type ocf) :unique-p (unique-p ocf)))

(defmethod create-object-class-instance-field ((ocf object-class-variable-type-value-field) content)
  (make-instance 'object-class-instance-variable-type-value-field :content content :field-name (field-name ocf) :optional-p (optional-p ocf) :default-content (default-content ocf) :type-field-name (type-field-name ocf)))

(defmethod create-object-class-instance-field ((ocf object-class-fixed-type-value-set-field) content)
  (make-instance 'object-class-instance-fixed-type-value-set-field :content content :field-name (field-name ocf) :optional-p (optional-p ocf) :default-content (default-content ocf) :c-type (c-type ocf)))

(defmethod create-object-class-instance-field ((ocf object-class-variable-type-value-set-field) content)
  (make-instance 'object-class-instance-variable-type-value-set-field :content content :field-name (field-name ocf) :optional-p (optional-p ocf) :default-content (default-content ocf) :type-field-name (type-field-name ocf)))

(defmethod create-object-class-instance-field ((ocf object-class-object-field) content)
  (make-instance 'object-class-instance-object-field :content content :field-name (field-name ocf) :optional-p (optional-p ocf) :default-content (default-content ocf) :defined-object-class (defined-object-class ocf)))

(defmethod create-object-class-instance-field ((ocf object-class-object-set-field) content)
  (make-instance 'object-class-instance-object-set-field :content content :field-name (field-name ocf) :optional-p (optional-p ocf) :default-content (default-content ocf) :defined-object-class (defined-object-class ocf)))

(defun change-to-object-class-instance (class-name object-class-definition-ht set-fields parse-object)
  (change-class
   parse-object
   'parse-object-instantiated
   :class-name class-name
   :fields
   (let ((field-specs (gethash class-name object-class-definition-ht)));TODO, class-name kan vara en dummyreferens (men felet ligger isf inte just här - utan där class-name inte översattes i enlighet med invokation)
     (mapcar (lambda (field-spec)
               (let ((set-field (find (field-name field-spec) set-fields :key #'second)))
                 (cond (set-field
                        (create-object-class-instance-field field-spec (third set-field)))
                       ((default-content field-spec)
                        (create-object-class-instance-field field-spec (default-content field-spec)))
                       ((optional-p field-spec)
                        (create-object-class-instance-field field-spec nil))
                       (t (error "non-optional field without setting")))))
             field-specs)))
  parse-object)
