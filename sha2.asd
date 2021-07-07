(asdf:defsystem #:sha2
  :description "A SHA-2 implementation for Common Lisp"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on (:nibbles)
  :in-order-to ((asdf:test-op (asdf:test-op #:sha2/test)))
  :components
    ((:module lisp
      :serial t
      :components
        ((:file "packages")
         (:file "sha256"))))
  . #+asdf3
      (:version "0.1"
       :homepage "https://yitzchak.github.io/sha2/"
       :bug-tracker "https://github.com/yitzchak/sha2/issues")
    #-asdf3 ())


(asdf:defsystem #:sha2/test
  :description "Test suite for sha2"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on
    (:sha2 :the-cost-of-nothing :ironclad)
  :perform (asdf:test-op (op c) (uiop:symbol-call :sha2/test :bench))
  :components
    ((:module lisp
      :components
      ((:module test
        :serial t
        :components
          ((:file "packages")
           (:file "bench")))))))
        
