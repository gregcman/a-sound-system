(defpackage #:musical-binaries
  (:use :cl
	:filesystem-util))
(in-package :musical-binaries)

(defparameter *binaries-directory* (this-directory))

(defparameter *folder*
  '(#+darwin
    "darwin/"
    #+(and windows x86)
    "win32/"
    #+(and windows x86-64)
    "win64/"
    #+(and linux x86-64)
    "linux/x86-64/"))
(when *folder*
  (pushnew
   (merge-pathnames
    (first *folder*)
    *binaries-directory*)
   cffi:*foreign-library-directories*
   :test #'equal))
