(asdf:defsystem #:ffmpeg-bindings
  :depends-on (:cffi
	       :musical-binaries
	       :alexandria)
    :components 
    ((:file "ffmpeg-bindings")
     (:file "ffmpeg")))
