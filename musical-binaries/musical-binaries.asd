(asdf:defsystem #:musical-binaries
  :depends-on (:cffi
	       :filesystem-util)
    :components 
    ((:file "musical-binaries")))
