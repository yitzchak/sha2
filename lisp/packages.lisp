(defpackage #:sha2
  (:use #:common-lisp)
  (:documentation "A SHA-2 implementation for Common Lisp")
  (:export
    #:make-sha256
    #:process
    #:octets-to-hex-string
    #:begin
    #:end))

