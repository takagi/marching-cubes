#|
  This file is a part of marching-cubes project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage marching-cubes
  (:use :cl)
  (:export :marching-cubes
           :make-vec3
           :vec3-x :vec3-y :vec3-z
           :vec3=
           :make-triangle
           :vertex
           :triangle=))
