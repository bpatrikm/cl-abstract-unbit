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

(in-package :bit-reader)

(defclass bit-reader ()
  ((bits-left-in-byte :initform 0 :accessor bits-left-in-byte)
   (current-byte :initform 0 :accessor current-byte)))

(defclass bit-reader-file (bit-reader)
  ((filein :initarg :filein :reader filein)))

(defclass bit-reader-bitstring (bit-reader)
  ((fragments :initarg :fragments :accessor fragments)
   (current-fragment-length :accessor current-fragment-length)
   (latest-read-index :accessor latest-read-index)))

(defclass bit-reader-aligned ()
  ())

(defclass bit-reader-unaligned ()
  ())

;;instanser skall tas från någon av dessa fyra
(defclass bit-reader-aligned-file (bit-reader-file bit-reader-aligned)
  ())

(defclass bit-reader-aligned-bitstring (bit-reader-bitstring bit-reader-aligned)
  ())

(defclass bit-reader-unaligned-file (bit-reader-file bit-reader-unaligned)
  ())

(defclass bit-reader-unaligned-bitstring (bit-reader-bitstring bit-reader-unaligned)
  ())

(defmethod initialize-instance :after ((b bit-reader-bitstring) &key)
  (setf (current-fragment-length b) (length (car (fragments b)))
        (latest-read-index b) -1))

(defgeneric read-a-byte (bit-reader)
  )

(defmethod read-a-byte ((bit-reader bit-reader-file))
  (let ((b
         (read-byte (filein bit-reader) nil nil)))
    ;;;(format t "~%~x~%" b)
    b))

(defmethod read-a-byte ((bit-reader bit-reader-bitstring))
  (let ((read-index (incf (latest-read-index bit-reader))))
    (when (eql read-index (current-fragment-length bit-reader))
      (progn
        (pop (fragments bit-reader))
        (setf (latest-read-index bit-reader) 0
              read-index 0
              (current-fragment-length bit-reader) (length (car (fragments bit-reader))))))
    (if (car (fragments bit-reader))
        (aref (car (fragments bit-reader)) read-index)
        nil)))

(defgeneric read-bits (number-of-bits bit-reader)
  )

(defmethod read-bits (number-of-bits (bit-reader bit-reader))
  ;;;(format t "~&bit-reader-state ~a left of ~x~&" (bits-left-in-byte bit-reader) (current-byte bit-reader))
  (when (eql 0 number-of-bits) (error "what"))
  (do* ((output 0)
	(number-of-bits number-of-bits (- number-of-bits bits-to-transfer))
	(bits-to-transfer (min number-of-bits
			       (bits-left-in-byte bit-reader))
			  (min number-of-bits
			       (bits-left-in-byte bit-reader))))
       ((eql number-of-bits 0)
        ;;(format t "~&~b~&" output)
	output)
    (setf (ldb (byte bits-to-transfer (- number-of-bits bits-to-transfer)) output)
          (ldb (byte bits-to-transfer (- (bits-left-in-byte bit-reader) bits-to-transfer)) (current-byte bit-reader)))
    (setf (bits-left-in-byte bit-reader) (- (bits-left-in-byte bit-reader) bits-to-transfer))
    (when (eql (bits-left-in-byte bit-reader) 0)
      (setf (current-byte bit-reader) (read-a-byte bit-reader))
      (setf (bits-left-in-byte bit-reader) 8))))

;;vad betyder egentligen align - här är tanken att man når en multipel av 8 bitar innan man läser det som är 'aligned bit-field'
(defmethod align ((br bit-reader))
  (unless (= 0 (mod (bits-left-in-byte br) 8))
    (read-bits (bits-left-in-byte br) br)))

;;align kan också betyda att man skall nå en multipel av 8 biter genom att läsa av 'aligned bit-field' - antalet nollbitar anpassas till det, men läggs till innan
(defmethod pre-align-read-bits (bits-to-read (br bit-reader))
  (let ((padding-bits (mod (- (bits-left-in-byte br) bits-to-read) 8)))
    (unless (= 0 padding-bits)
      (read-bits padding-bits br))
    (read-bits bits-to-read br)))
