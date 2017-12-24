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

(in-package :parsing-medium)

(defun collate-parse-tree (parse-tree)
  (if (not (typep (car parse-tree) 'rule))
      parse-tree
      ;;recurse into elements corresponding, and then collate this level
      (when (collator (car parse-tree))
        (funcall (collator (car parse-tree))
                 (cons (car parse-tree)
                       (mapcar #'collate-parse-tree (cdr parse-tree)))))))

