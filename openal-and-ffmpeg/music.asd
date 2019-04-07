(asdf:defsystem #:music
  :depends-on (:cl-openal
	       :cl-alc
	       :bordeaux-threads
	       :musical-binaries
	       :ffmpeg-bindings
	       :bodge-sndfile
	       :sndfile-blob
	       :iterate
	       :lparallel
	       :float-features)
    :serial t
    :components 
    ((:file "openal")))
