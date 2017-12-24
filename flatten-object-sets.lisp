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

;;object-set -> platt lista av object. root/additional/extensible-p spelar ingen roll
;;parse-object-set-raw innehåller root och additional, vardera är en parse-object-set-union
;;TODO referenser kan vara dummies
(in-package :flatten-object-sets)

(defmethod flatten-object-sets ((pt parse-object-instantiated) object-ht object-set-ht invokations)
  (declare (ignore object-ht object-set-ht invokations))
  (list pt))

(defmethod flatten-object-sets ((pt parse-object-set-union) object-ht object-set-ht invokations)
  (reduce #'append
          (mapcar (lambda (subset)
                    (flatten-object-sets subset object-ht object-set-ht invokations))
                  (subsets pt))))

(defmethod flatten-object-sets ((pt parse-defined-object-set) object-ht object-set-ht invokations)
  (if invokations
      (labels ((w (parameter-list parameter-analysis-list actual-parameter-list)
                 (if (not parameter-list)
                     (flatten-object-sets pt object-ht object-set-ht (cdr invokations))
                     (cond ((find (car (car parameter-analysis-list)) '(:type :class-name))
                            (if (eql (reference pt) (cdr (car parameter-list)))
                                (error "reference should be to object-set (1)")
                                (w (cdr parameter-list) (cdr parameter-analysis-list) (cdr actual-parameter-list))))
                           ((eql :dummy (cdr (car parameter-analysis-list)))
                            (when (eql (reference pt) (cdr (first (car parameter-list))))
                              (error "reference should be to object-set (2)"))
                            (if (eql (reference pt) (cdr (second (car parameter-list))))
                                (progn
                                  (when (not (eql :object-set (car (car parameter-analysis-list))))
                                    (error "reference should be to object-set (3)"))
                                  (flatten-object-sets (cadr actual-parameter-list) object-ht object-set-ht (cdr invokations)))
                                (w (cdr parameter-list) (cdr parameter-analysis-list) (cddr actual-parameter-list))))
                           (t
                            (if (eql (reference pt) (cdr (second (car parameter-list))))
                                (progn
                                  (when (not (eql :object-set (car (car parameter-analysis-list))))
                                    (error "reference should be to object-set (4)"))
                                  (flatten-object-sets (car actual-parameter-list) object-ht object-set-ht (cdr invokations)))
                                (w (cdr parameter-list) (cdr parameter-analysis-list) (cdr actual-parameter-list))))))))
        (w (parameter-list (car invokations))
           (parameter-analysis-list (car invokations))
           (actual-parameter-list (car invokations))))
      (flatten-object-sets (gethash (reference pt) object-set-ht) object-ht object-set-ht nil)))

(defmethod flatten-object-sets ((pt parse-object-set-raw) object-ht object-set-ht invokations)
  (append (when (root pt)
            (flatten-object-sets (root pt) object-ht object-set-ht invokations))
          (when (additional pt)
            (flatten-object-sets (additional pt) object-ht object-set-ht invokations))))

(defmethod flatten-object-sets ((pt parse-object-by-reference) object-ht object-set-ht invokations)
  (if invokations
      (labels ((w (parameter-list parameter-analysis-list actual-parameter-list)
                 (if (not parameter-list)
                     (flatten-object-sets pt object-ht object-set-ht (cdr invokations))
                     (cond ((find (car (car parameter-analysis-list)) '(:type :class-name))
                            (if (eql (reference (defined-object pt)) (cdr (car parameter-list)))
                                (error "reference should be to object (1)")
                                (w (cdr parameter-list) (cdr parameter-analysis-list) (cdr actual-parameter-list))))
                           ((eql :dummy (cdr (car parameter-analysis-list)))
                            (when (eql (reference (defined-object pt)) (cdr (first (car parameter-list))))
                              (error "reference should be to object (2)"))
                            (if (eql (reference (defined-object pt)) (cdr (second (car parameter-list))))
                                (progn
                                  (when (not (eql :object (car (car parameter-analysis-list))))
                                    (error "reference should be to object (3)"))
                                  (flatten-object-sets (cadr actual-parameter-list) object-ht object-set-ht (cdr invokations)))
                                (w (cdr parameter-list) (cdr parameter-analysis-list) (cddr actual-parameter-list))))
                           (t
                            (if (eql (reference (defined-object pt)) (cdr (second (car parameter-list))))
                                (progn
                                  (when (not (eql :object (car (car parameter-analysis-list))))
                                    (error "reference should be to object (4)"))
                                  (flatten-object-sets (car actual-parameter-list) object-ht object-set-ht (cdr invokations)))
                                (w (cdr parameter-list) (cdr parameter-analysis-list) (cdr actual-parameter-list))))))))
        (w (parameter-list (car invokations))
           (parameter-analysis-list (car invokations))
           (actual-parameter-list (car invokations))))
      (flatten-object-sets (gethash (reference (defined-object pt)) object-ht) object-ht object-set-ht nil)))
