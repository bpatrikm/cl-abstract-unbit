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

(in-package :parse-high-parameter)

;;returvärde är en lista (tänkt att läggas i actual-parameter-list), där varje element är den sammanställda formen av ett argument att invändas för invokering av parametriserad typ
(defun parse-actual-parameter-list (pt assign-ht)
  (assert (typep pt 'parse-parameterized-type-invokation-raw))
  (labels ((w (parameter-analysis-list &optional collected-non-terminals)
             (if (not parameter-analysis-list)
                 (nreverse collected-non-terminals)
                 (case (car (car parameter-analysis-list))
                   (:type (w (cdr parameter-analysis-list)
                             (cons 'asn.1::%type collected-non-terminals)))
                   (:class (w (cdr parameter-analysis-list)
                              (cons 'asn.1::%defined-object-class collected-non-terminals)))
                   (:value (if (eql :dummy (cdr (car parameter-analysis-list)))
                               (w (cdr parameter-analysis-list)
                                  (append '(asn.1::%value asn.1::%type) collected-non-terminals));skall bli nreverse:ad
                               ;;ev måste %value läsas med kunskap av parsningen av dummygovernortype - då funkar inte det här upplägget
                               (w (cdr parameter-analysis-list)
                                  (cons 'asn.1::%value collected-non-terminals))))
                   (:value-set (if (eql :dummy (cdr (car parameter-analysis-list)))
                                   (w (cdr parameter-analysis-list)
                                      (append '(asn.1::%value-set asn.1::%type) collected-non-terminals))
                                   (w (cdr parameter-analysis-list)
                                      (cons 'asn.1::%value-set collected-non-terminals))))
                   (:object (if (eql :dummy (cdr (car parameter-analysis-list)))
                                (w (cdr parameter-analysis-list);;denna del av parse kommer möjligen att resultera i en object-definition-raw, som måste sändas i traverse-instantiate med klass-kontext enligt parameterlist/actualparameterlist (beroende på :dummy)
                                   (append '(asn.1::%object asn.1::%defined-object-class) collected-non-terminals))
                                (w (cdr parameter-analysis-list)
                                   (cons 'asn.1::%object collected-non-terminals))))
                   (:object-set (if (eql :dummy (cdr (car parameter-analysis-list)))
                                    (w (cdr parameter-analysis-list)
                                       (append '(asn.1::%object-set asn.1::%defined-object-class) collected-non-terminals))
                                    (w (cdr parameter-analysis-list)
                                       (cons 'asn.1::%object-set collected-non-terminals))))))))
    (let* ((parameterized-type (gethash (parameterized-type-name pt) (parameterized-type-assignment-ht assign-ht)))
           (actual-non-terminals (w (parameter-analysis-list parameterized-type)))
           ;;skapa en produktion som ger det specifika anropet till denna parametriserade typ
           (actual-parameter-list-production
            `((asn.1::%actual-parameter-list
               ,@(reduce #'append (mapcar (lambda (actual-non-terminal)
                                            `(,actual-non-terminal asn.1::|,|))
                                          (butlast actual-non-terminals)))
               ,@(last actual-non-terminals)
               asn.1::})
              (lambda (pl) (list ,@(let (parts)
                                     (dotimes (i (length actual-non-terminals))
                                       (push `(elt pl ,(+ 1 (* 2 i))) parts))
                                     (nreverse parts)))))))
      (collate-parse-tree
       (parse-from-tokens (actual-parameter-token-list pt);TODO är någon form av retokenize nödvändig?
                          (import-asn.1-grammer (cons actual-parameter-list-production
                                                      asn.1::*asn.1-syntax-mod*))
                          'asn.1::%actual-parameter-list
                          'asn.1::})))))

(defun parse-convert-invokation (pt assign-ht)
  (assert (typep pt 'parse-parameterized-type-invokation-raw))
  (let ((parameterized-type (gethash (parameterized-type-name pt) (parameterized-type-assignment-ht assign-ht))));
    ;;Notera, man kan inte ge namnet till en parametriserad typ i parameterlistan och invokera den i mallen, man kan inte heller ha ett klass-namn i parameterlistan och en tillhörande objektdefinition i typ-mallen
    ;;vid inläsning av mib görs inga textuella hopp - så man går inte in i en typ-mall i en invokationskontext. konsekvensen är att referenser till parametriserade typer och klass-namn för objektdefinitioner aldrig kan vara dummyreferenser. (Det är bara så dereferenser görs i mib-import-fasen)
    (unless parameterized-type (error 'parse-out-of-order))
    (change-class pt 'parse-parameterized-type-invokation
                  :parameterized-type parameterized-type
                  :actual-parameter-list (parse-actual-parameter-list pt assign-ht))))
