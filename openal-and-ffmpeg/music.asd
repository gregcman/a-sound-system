(asdf:defsystem #:music
  :depends-on (:cl-openal
	       :cl-alc
	       :bordeaux-threads
	       :utility
	       :musical-binaries
	       :ffmpeg-bindings
	       :singleton-lparallel
	       :bad-floats)
    :serial t
    :components 
    ((:file "ffmpeg")
     (:file "openal")))
