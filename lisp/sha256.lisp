(in-package :sha2)


(deftype u8 ()
  '(unsigned-byte 8))

(deftype u32 ()
  '(unsigned-byte 32))


(defparameter +initial-hash-values+
  (make-array '(8)
              :element-type 'u32
              :initial-contents #(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))


(defparameter +round-constants+
  (make-array '(64)
              :element-type 'u32
              :initial-contents #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
                                  #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
                                  #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
                                  #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
                                  #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
                                  #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
                                  #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
                                  #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)))


(defclass sha256 ()
  ((schedule
     :accessor schedule
     :initform (make-array '(256) :element-type 'u8))
   (hash
     :accessor hash
     :initform (copy-seq +initial-hash-values+))
   (work
     :accessor work
     :initform (copy-seq +initial-hash-values+))
   (data-length
     :accessor data-length
     :initform 0)
   (schedule-length
     :accessor schedule-length
     :initform 0)))


(defun make-sha256 ()
  (make-instance 'sha256))


(defun begin (instance)
  (setf (hash instance) (copy-seq +initial-hash-values+)
        (data-length instance) 0
        (schedule-length instance) 0)
  (values))


(declaim (ftype (function (&rest u32) u32) +/32))
(defun +/32 (&rest rest)
  (ldb (byte 32 0) (apply #'+ rest)))


(declaim (ftype (function (u32) u32) not/32))
(defun not/32 (value)
  (ldb (byte 32 0) (lognot value)))


(declaim (ftype (function (&rest u32) u32) and/32))
(defun and/32 (&rest rest)
  (ldb (byte 32 0) (apply #'logand rest)))


(declaim (ftype (function (&rest u32) u32) xor/32))
(defun xor/32 (&rest rest)
  (ldb (byte 32 0) (apply #'logxor rest)))


(declaim (ftype (function (&rest u32) u32) ior/32))
(defun ior/32 (&rest rest)
  (ldb (byte 32 0) (apply #'logior rest)))


(declaim (ftype (function (u32 integer) u32) rotate/32))
(defun rotate/32 (val count)
  (declare (type u32 val))
  (ior/32 (ldb (byte 32 0) (ash val (- count)))
          (ldb (byte 32 0) (ash val (- 32 count)))))


(declaim (ftype (function (u32 integer) u32) shift/32))
(defun shift/32 (val count)
  (declare (type u32 val))
  (ldb (byte 32 0) (ash val (- count))))


(defun s0 (x)
  (xor/32 (rotate/32 x 7)
          (rotate/32 x 18)
          (shift/32 x 3)))


(defun s1 (x)
  (xor/32 (rotate/32 x 17)
          (rotate/32 x 19)
          (shift/32 x 10)))


(defun temp1 (schedule work index)
  (if (< index 64)
    (let ((e (aref work (mod (- 4 index) 8)))
          (f (aref work (mod (- 5 index) 8)))
          (g (aref work (mod (- 6 index) 8)))
          (h (aref work (mod (- 7 index) 8))))
      (+/32 h
            (xor/32 (rotate/32 e 6)
                    (rotate/32 e 11)
                    (rotate/32 e 25))
            (xor/32 (and/32 e f)
                    (and/32 (not/32 e)
                            g))
            (nibbles:ub32ref/be schedule (* 4 index))
            (aref +round-constants+ index)))
    0))


(defun temp2 (schedule work index)
  (declare (ignore schedule))
  (if (< index 64)
    (let ((a (aref work (mod (- index) 8)))
          (b (aref work (mod (- 1 index) 8)))
          (c (aref work (mod (- 2 index) 8))))
      (+/32 (xor/32 (rotate/32 a 2)
                    (rotate/32 a 13)
                    (rotate/32 a 22))
            (xor/32 (and/32 a b)
                    (and/32 a c)
                    (and/32 b c))))
    0))


(defun process-schedule (instance)
  (with-slots (schedule hash schedule-length work)
              instance
    (do ((index 64 (+ index 4)))
        ((= index 256))
      (setf (nibbles:ub32ref/be schedule index)
            (+/32 (nibbles:ub32ref/be schedule (- index 64))
                  (s0 (nibbles:ub32ref/be schedule (- index 60)))
                  (nibbles:ub32ref/be schedule (- index 28))
                  (s1 (nibbles:ub32ref/be schedule (- index 8))))))
    (replace work hash)
    (do* ((index 0 (1+ index))
          (d 3 (mod (1- d) 8))
          (h 7 (mod (1- h) 8))
          (temp1 (temp1 schedule work index) (temp1 schedule work index))
          (temp2 (temp2 schedule work index) (temp2 schedule work index)))
         ((= 64 index))
       (declare (type u32 temp1 temp2))
       (setf (aref work d) (+/32 (aref work d) temp1)
             (aref work h) (+/32 temp1 temp2)))
    (dotimes (index 8)
      (setf (aref hash index) (+/32 (aref hash index) (aref work index))))
    (setf schedule-length 0)))


(defun process (instance octets)
  (with-slots (schedule data-length schedule-length)
              instance
    (incf data-length (length octets))
    (do ((index 0))
        ((= (length octets) index) data-length)
      (do ()
          ((or (= schedule-length 64)
               (= index (length octets))))
        (setf (aref schedule schedule-length) (aref octets index))
        (incf schedule-length)
        (incf index))
      (when (= 64 schedule-length)
        (process-schedule instance)))))


(defun end (instance)
  (let ((octets (make-array (list (- 73 (rem (+ 9 (data-length instance)) 64))) :element-type 'u8))
        (final-hash (make-array '(32) :element-type 'u8)))
    (setf (aref octets 0) #x80
          (nibbles:ub64ref/be octets (- (length octets) 8)) (* 8 (data-length instance)))
    (process instance octets)
    (dotimes (index 8 final-hash)
      (setf (nibbles:ub32ref/be final-hash (* 4 index)) (aref (hash instance) index)))))


(defun octets-to-hex-string (bytes)
  (format nil "~(~{~2,'0X~}~)" (coerce bytes 'list)))


