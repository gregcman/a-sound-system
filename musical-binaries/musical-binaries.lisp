(defpackage #:musical-binaries
  (:use :cl))
(in-package :musical-binaries)

(defparameter *binaries-directory*
  (asdf:system-source-directory :musical-binaries))

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
