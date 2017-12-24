# cl-abstract-unbit
Sample session (required mib-files can be found on 3gpp.org and protocol sample files from https://wiki.wireshark.org/SampleCaptures):
(require :lalr-asn.1)
(use-package :assign-ht)
(use-package :bit-reader)
(use-package :import-mib)
(use-package :read-per)
(tester-rrc) ;will output 4 progress-lines
(defparameter *rrc-assign-ht* *)
(with-open-file (bitin "/home/patrik/3gpp/ul-dcch-message_85.bin" :element-type '(unsigned-byte 8))
	   (let ((bit-reader (make-instance 'bit-reader-unaligned-file :filein bitin)))
	     (read-per (gethash (intern "UL-DCCH-Message" (find-package :asn.1)) (type-assignment-ht *rrc-assign-ht*)) nil nil nil *rrc-assign-ht* bit-reader)))
