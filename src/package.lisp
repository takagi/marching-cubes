#|
  This file is a part of marching-cubes project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :cl-user)
(defpackage marching-cubes
  (:use :cl)
  (:export :make-vec3
           :vec3=
           :make-triangle
           :triangle=))
