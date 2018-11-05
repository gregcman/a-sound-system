(asdf:defsystem #:music
  :depends-on (:cl-openal
	       :cl-alc
	       :bordeaux-threads
	       :musical-binaries
	       :ffmpeg-bindings
	       :iterate
	       :lparallel
	       :float-features)
    :serial t
    :components 
    ((:file "openal")))
