#|
  This file is a part of marching-cubes project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage marching-cubes
  (:use :cl)
  (:export :marching-cubes
           :marching-cubes-smooth
           :make-vec3
           :vec3-x :vec3-y :vec3-z
           :vec3=
           :normalize-vec3
           :make-triangle
           :triangle-vertex
           :triangle=
           :make-smooth-triangle
           :smooth-triangle-vertex
           :smooth-triangle-normal
           :smooth-triangle=))
