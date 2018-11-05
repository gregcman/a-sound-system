(asdf:defsystem #:ffmpeg-bindings
  :depends-on (:cffi
	       :musical-binaries)
    :components 
    ((:file "ffmpeg-bindings")))
