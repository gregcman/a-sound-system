(asdf:defsystem #:music
  :depends-on (;;:cl-openal
	       ;;:cl-alc

	       :bodge-openal
	       :openal-blob
	       :bordeaux-threads
	       ;;:musical-binaries
	       ;;:ffmpeg-bindings
	       :bodge-sndfile
	       :sndfile-blob
	       :iterate
	       :lparallel
	       :float-features
	       :cffi)
    :serial t
    :components 
    ((:file "al")
     (:file "openal")))
