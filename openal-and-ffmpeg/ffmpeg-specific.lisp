(defun update-playable (datobj)
  (macrolet ((exit (x)
	       `(return-from exit ,x)))
    (block exit
      (with-slots (status time-remaining (format playback) (music data)) datobj
	(let* ((rate
		(slot-value 
		 (slot-value 
		  music
		  'cl-ffmpeg::sound)
		 'cl-ffmpeg::rate))
	       (threshold 10.0)
	       (target 10.0))
	  (when (not rate)
	    (setf status 'aborted)
	    (exit 'aborted))
	  (when (eq status 'aborted)
	    (exit 'aborted))
	  (tagbody move
	     (free-buffers datobj)
	     (when (>= (* rate threshold) time-remaining)
	       (let ((target-samples (* rate target)))
		 (setf
		  status 
		  (cl-ffmpeg::%get-sound-buff (data samples channels audio-format rate) music
		    (flet ((conv (arr)
			     (multiple-value-bind (pcm playsize)
				 (convert
				  channels
				  data
				  samples
				  audio-format
				  format
				  arr)
			       (let ((buffer (get-buffer)))
				 (al:buffer-data buffer format pcm playsize rate)
				 (source-queue-buffer datobj buffer)))))
		      (let ((arrcount (w-al::ecases format
						    ((%al:+format-stereo8+
						      %al:+format-stereo16+)
						     (* samples 2))
						    ((%al:+format-mono8+
						      %al:+format-mono16+)
						     samples))))
			(w-al::ecases format
				      ((%al:+format-mono8+ %al:+format-stereo8+)
				       (cffi:with-foreign-object (arr :uint8 arrcount)
								 (conv arr)))
				      ((%al:+format-mono16+ %al:+format-stereo16+ )
				       (cffi:with-foreign-object (arr :int16 arrcount)
								 (conv arr))))))
		    (when (eq status 'aborted)
		      (return 'aborted))
		    (decf target-samples samples)
		    (when (>= 0 target-samples)
		      (return nil)))))
	       (when
		   (eq status nil)
		 (go move)))))))))

(defun load-all (file &optional (format *format*))
  (when (pathnamep file)
    (setf file (namestring file)))
  (let ((music nil))
    (unwind-protect
	 (progn
	   (setf music (cl-ffmpeg::init-music-stuff file))
	   (let ((rate (slot-value 
			(slot-value 
			 music
			 'cl-ffmpeg::sound)
			'cl-ffmpeg::rate))
		 (sound-buffers ())
		 (completed? nil))
	     (when (not rate)
	       (return-from load-all (values nil nil)))
	     (when
		 (cl-ffmpeg::%get-sound-buff (data samples channels audio-format rate) music
		   (flet ((conv (arr)
			    (multiple-value-bind (pcm playsize)
				(convert
				 channels
				 data
				 samples
				 audio-format
				 format
				 arr)
			      (let ((buffer (get-buffer)))
				(al:buffer-data buffer format pcm playsize rate)
				(push buffer sound-buffers)))))
		     (let ((arrcount (w-al::ecases format
						   ((%al:+format-stereo8+
						     %al:+format-stereo16+)
						    (* samples 2))
						   ((%al:+format-mono8+
						     %al:+format-mono16+)
						    samples))))
		       (w-al::ecases format
				     ((%al:+format-mono8+ %al:+format-stereo8+)
				      (cffi:with-foreign-object (arr :uint8 arrcount)
								(conv arr)))
				     ((%al:+format-mono16+ %al:+format-stereo16+ )
				      (cffi:with-foreign-object (arr :int16 arrcount)
								(conv arr)))))))
	       (setf completed? t))
	     (let ((inst
		    (make-instance 'preloaded-music)))
	       (with-slots (buffers complete info) inst
		 (setf buffers (coerce (nreverse sound-buffers) 'vector)
		       complete completed?
		       info (slot-value music 'cl-ffmpeg::sound)))
	       inst)))
      (cl-ffmpeg::free-music-stuff music))))

(defmethod free ((obj cl-ffmpeg::music-stuff))
  (cl-ffmpeg::free-music-stuff obj))

(defun load-file (music-file)
  (let ((ffmpeg-stuff (cl-ffmpeg::init-music-stuff music-file)))
    (when ffmpeg-stuff
      (let ((datobj (make-instance 'datobj)))
	(setf *datobj* datobj)
	(with-slots (source data) datobj
	  (setf source (al:gen-source))	
	  (setf data ffmpeg-stuff)
	  (values datobj
		  source))))))
