(defpackage :w-alc
  (:use :cl))
(in-package :w-alc)

;;;;ripped from zkat's cl-openal alc

(defun open-device (&optional device-name)
  (let ((foreign-dev (%alc:open-device (or device-name
                                           (cffi:null-pointer)))))
    (if (cffi:null-pointer-p foreign-dev)
        nil
        foreign-dev)))

(defun close-device (device)
  (%alc:close-device device))

(defun create-context (device &rest attributes)
  (let ((foreign-ctx
         (%alc:create-context device
                              (if attributes
                                  (let ((n (length attributes)))
                                    (cffi:with-foreign-object (attrlist :pointer ;;FIXME -> claw?
									n)
                                      (loop for i below n
                                         do (setf (cffi:mem-aref attrlist :pointer ;;FIXME also
								 i)
                                                  (elt attributes i)))
                                      attrlist))
                                  (cffi:null-pointer)))))
    (if (cffi:null-pointer-p foreign-ctx)
        nil
        foreign-ctx)))

(defun make-context-current (context)
  (%alc:make-context-current context))

(defun process-context (context)
  (%alc:process-context context))
(defun suspend-context (context)
  (%alc:suspend-context context))
(defun destroy-context (context)
  (%alc:destroy-context context))

(defun get-current-context ()
  (%alc:get-current-context))
(defun get-contexts-device (context)
  (%alc:get-contexts-device context))

(defun get-error (device)
  (%alc:get-error device))

(defun extension-present-p (device extname)
  (%alc:is-extension-present device extname))
(defun get-proc-address (funcname)
  (%alc:get-proc-address funcname))

(defun get-enum-value (device enum-name)
  (%alc:get-enum-value device enum-name))

(defun get-string (device param)
  (%alc:get-string device param))
(defun get-integer (device param)
  (cffi:with-foreign-object (size-arr :int 1)
			    (%alc:get-integerv device
					       %alc:+attributes-size+

					       1 size-arr)
    (let ((size (cffi:mem-aref size-arr :int 0)))
      (cffi:with-foreign-object (int-list :int size)
        (%alc:get-integerv device param size int-list)
        (loop for i below size
           collect (cffi:mem-aref int-list :int i))))))

(defun capture-open-device (device-name frequency format buffer-size)
  (%alc:capture-open-device device-name
                            (coerce frequency 'integer)
                            format
                            (coerce buffer-size 'integer)))
(defun capture-close-device (device)
  (%alc:capture-close-device device))
(defun capture-start (device)
  (%alc:capture-start device))
(defun capture-stop (device)
  (%alc:capture-stop device))
(defun capture-samples (device samples)
  (let ((n-samples (length samples)))
    (cffi:with-foreign-object (buffer :pointer n-samples)
      (loop for i below n-samples
         do (setf (cffi:mem-aref buffer i)
                  (elt samples i)))
      (%alc:capture-samples device buffer n-samples))))

;;;
;;; Helper macros to keep the world tidy.
;;;

(defmacro with-capture-device ((var device-name frequency
                                    format buffer-size) &body body)
  `(let ((,var (capture-open-device ,device-name ,frequency
                                    ,format ,buffer-size)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var (capture-close-device ,var)))))

(defmacro with-device ((var &optional (device-name nil)) &body body)
  `(let ((,var (open-device ,device-name)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var (close-device ,var)))))

(defmacro with-context ((var device &rest attributes) &body body)
  `(let ((,var ,(if attributes 
                    `(create-context ,device ,@attributes)
                    `(create-context ,device))))
     (unwind-protect
          (progn
            ,@body)
       (when ,var
         (when (cffi:pointer-eq ,var (get-current-context))
           (make-context-current (cffi:null-pointer)))
         (destroy-context ,var)))))


(defpackage :w-al
  (:use :cl))
(in-package :w-al)

;;;Ripped from zkat's cl-openal

;; Renderer State management
(defun enable (capability)
  (%al:enable capability))
(defun disable (capability)
  (%al:disable capability))
(defun enabledp (capability)
  (%al:is-enabled capability))

;; State retrieval
(defun get-string (param)
  (%al:get-string param))
(defun get-boolean (param)
  (%al:get-boolean param))
(defun get-integer (param)
  (%al:get-integer param))

;; Errors
(defun get-error ()
  (%al:get-error))

;; Extensions
(defun extension-present-p (extension-string)
  (%al:is-extension-present extension-string))
(defun get-proc-address (fname)
  (%al:get-proc-address fname))
(defun get-enum-value (enum-name)
  (%al:get-enum-value enum-name))

;;;;

(defun foo (&optional (sym :start) (bar "%AL"))
  (find-symbol
   (concatenate 'string "+" (symbol-name sym) "+")
   (find-package bar)))

;;;
;;; Listener
;;;
(defun listener (param value)
  (ecase param
    ((%AL:+POSITION+ %AL:+VELOCITY+)
     (assert (= 3 (length value)))
     (%al:listener3f param (elt value 0) (elt value 1) (elt value 2)))
    ((%AL:+ORIENTATION+)
     (assert (= 6 (length value)))
     (cffi:with-foreign-object (array :float 6)
       (loop for i below 6
          doing (setf (cffi:mem-aref array :float i)
                      (coerce (elt value i) 'float))
          finally (%al:listenerfv param array))))
    ((%AL:+GAIN+)
     (%al:listenerf param value))))

(defun get-listener (param)
  (ecase param
    ((%AL:+GAIN+)
     (cffi:with-foreign-object (ptr :float)
       (%al:get-listenerf param ptr)
       (cffi:mem-ref ptr :float)))
    ((%AL:+ORIENTATION+)
     (cffi:with-foreign-object (listener-array :float 6)
       (%al:get-listenerfv param listener-array)
       (loop for i below 6
          collecting (cffi:mem-aref listener-array :float i))))
    ((%AL:+POSITION+ %AL:+VELOCITY+)
     (cffi:with-foreign-object (listener-array :float 3)
       (%al:get-listenerfv param listener-array)
       (loop for i below 3
          collect (cffi:mem-aref listener-array :float i))))))

;;;
;;; Sources
;;;
(defun gen-sources (n)
  (cffi:with-foreign-object (source-array :uint n)
    (%al:gen-sources n source-array)
    (loop for i below n
       collect (cffi:mem-aref source-array :uint i))))
(defun delete-sources (sources)
  (let ((n (length sources)))
    (cffi:with-foreign-object (source-array :uint n)
      (loop for i below n
         do (setf
             (cffi:mem-aref source-array :uint i)
             (elt sources i)))
      (%al:delete-sources n source-array))))
(defun gen-source ()
  (car (gen-sources 1)))
(defun delete-source (sid)
  (delete-sources (list sid)))

(defun sourcep (sid)
  (%al:is-source sid))

(defun source (sid param value)
  (ecase param
    ((%AL:+GAIN+ %AL:+PITCH+ %AL:+MIN-GAIN+ %AL:+MAX-GAIN+ %AL:+REFERENCE-DISTANCE+
		 %AL:+ROLLOFF-FACTOR+ %AL:+MAX-DISTANCE+ %AL:+SEC-OFFSET+ %AL:+SAMPLE-OFFSET+
		 %AL:+BYTE-OFFSET+ %AL:+CONE-INNER-ANGLE+ %AL:+CONE-OUTER-ANGLE+
		 %AL:+CONE-OUTER-GAIN+)
     (%al:sourcef sid param value))
    ((%AL:+LOOPING+ %AL:+SOURCE-RELATIVE+)
     (%al:sourcei sid param (if value 1 0)))
    ((%AL:+SOURCE-TYPE+ %AL:+BUFFER+)
     (%al:sourcei sid param value))
    ((%AL:+POSITION+ %AL:+VELOCITY+ %AL:+DIRECTION+)
     (assert (= 3 (length value)))
     (%al:source3f sid param (elt value 0) (elt value 1) (elt value 2)))))

(defun get-source (sid param)
  (ecase param
    ((%AL:+GAIN+ %AL:+PITCH+ %AL:+MIN-GAIN+ %AL:+MAX-GAIN+ %AL:+REFERENCE-DISTANCE+
		 %AL:+SEC-OFFSET+ %AL:+ROLLOFF-FACTOR+ %AL:+MAX-DISTANCE+
		 %AL:+CONE-INNER-ANGLE+ %AL:+CONE-OUTER-ANGLE+ %AL:+CONE-OUTER-GAIN+
		 %AL:+SAMPLE-OFFSET+ %AL:+BYTE-OFFSET+)
     (cffi:with-foreign-object (ptr :float)
       (%al:get-sourcef sid param ptr)
       (cffi:mem-ref ptr :float)))
    ((%AL:+LOOPING+ %AL:+SOURCE-RELATIVE+)
     (cffi:with-foreign-object (ptr :int)
       (%al:get-sourcei sid param ptr)
       (cffi:mem-ref ptr :boolean)))
    ((%AL:+SOURCE-TYPE+ %AL:+BUFFER+ %AL:+BUFFERS-QUEUED+ %AL:+BUFFERS-PROCESSED+)
     (cffi:with-foreign-object (ptr :int)
       (%al:get-sourcei sid param ptr)
       (cffi:mem-ref ptr :int)))
    (%AL:+SOURCE-STATE+
     (cffi:with-foreign-object (ptr :int)
       (%al:get-sourcei sid param ptr)
       (cffi:foreign-enum-keyword '%al:enum (cffi:mem-ref ptr :int))))
    ((%AL:+POSITION+ %AL:+VELOCITY+ %AL:+DIRECTION+)
     (cffi:with-foreign-object (source-array :float 3)
       (%al:get-sourcefv sid param source-array)
       (loop for i below 3
             collect (cffi:mem-aref source-array :float i))))))

;; Playback
(defun source-play (sid)
  (%al:source-play sid))
(defun source-stop (sid)
  (%al:source-stop sid))
(defun source-rewind (sid)
  (%al:source-rewind sid))
(defun source-pause (sid)
  (%al:source-pause sid))

;; queueing
(defun source-queue-buffers (sid buffers)
  (let ((n (length buffers)))
    (cffi:with-foreign-object (buffer-array :uint n)
      (loop for i below n
         do (setf (cffi:mem-aref buffer-array :uint i)
                  (elt buffers i)))
      (%al:source-queue-buffers sid n buffer-array))))

(defun source-unqueue-buffers (sid &optional (num-buffers 1))
  (cffi:with-foreign-object (buffer-array :uint)
    (setf (cffi:mem-ref buffer-array :uint) 0)
    (%al:source-unqueue-buffers sid num-buffers buffer-array)
    (unless (zerop (cffi:mem-ref buffer-array :uint))
      (loop for i below num-buffers
         collect (cffi:mem-aref buffer-array :uint i)))))

;;;
;;; Buffers
;;;
(defun gen-buffers (n)
  (cffi:with-foreign-object (buffer-array :uint n)
    (%al:gen-buffers n buffer-array)
    (loop for i below n
       collect (cffi:mem-aref buffer-array :uint i))))
(defun delete-buffers (buffers)
  (let ((n (length buffers)))
    (cffi:with-foreign-object (buffer-array :uint n)
      (loop for i below n
         do (setf
             (cffi:mem-aref buffer-array :uint i)
             (elt buffers i)))
      (%al:delete-buffers n buffer-array))))
(defun gen-buffer ()
  (car (gen-buffers 1)))
(defun delete-buffer (bid)
  (delete-buffers (list bid)))

(defun bufferp (buffer-id)
  (%al:is-buffer buffer-id))

(defun buffer (bid param value)
  (%al:bufferi bid param value))

(defun buffer-data (bid format data size freq)
  (%al:buffer-data bid format data size freq))

(defun get-buffer (bid param)
  (cffi:with-foreign-object (ptr :int)
    (%al:get-bufferi bid param ptr)
    (cffi:mem-ref ptr :int)))

;;;
;;; Global parameters
;;;
(defun doppler-factor (value)
  (%al:doppler-factor value))
(defun doppler-velocity (value)
  (%al:doppler-velocity value))
(defun speed-of-sound (value)
  (%al:speed-of-sound value))
(defun distance-model (model-param)
  (%al:distance-model model-param))


;;;
;;; Helper macros to keep the world tidy.
;;;

(defmacro with-sources ((n var) &body body)
  `(let ((,var (gen-sources ,n)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var (delete-sources ,var)))))

(defmacro with-source ((var) &body body)
  `(let ((,var (gen-source)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var (delete-source ,var)))))

(defmacro with-buffers ((n var) &body body)
  `(let ((,var (gen-buffers ,n)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var (delete-buffers ,var)))))

(defmacro with-buffer ((var) &body body)
  `(let ((,var (gen-buffer)))
     (unwind-protect
          (progn
            ,@body)
       (when ,var (delete-buffer ,var)))))
