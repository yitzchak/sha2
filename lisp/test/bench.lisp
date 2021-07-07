(in-package #:sha2/test)


(defun bench ()
  (format t "sha2 empty message: ")
  (the-cost-of-nothing:bench
    (let ((s (sha2:make-sha256)))
      (sha2:end s)))

  (format t "~%ironclad empty message: ")
  (the-cost-of-nothing:bench
    (let ((s (ironclad:make-digest :sha256)))
      (ironclad:produce-digest s))))
                              
