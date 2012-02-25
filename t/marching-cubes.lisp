#|
  This file is a part of marching-cubes project.
  Copyright (c) 2012 Masayuki Takagi (kamonama@gmail.com)
|#

(in-package :marching-cubes-test)

(defparameter *vertex-positions*
  (make-array
    '(8)
    :initial-contents
    (list
      (make-vec3 0 0 0) (make-vec3 1 0 0) (make-vec3 1 1 0) (make-vec3 0 1 0)
      (make-vec3 0 0 1) (make-vec3 1 0 1) (make-vec3 1 1 1) (make-vec3 0 1 1))))

(defun make-test-grid (v0 v1 v2 v3 v4 v5 v6 v7)
  (let ((values (make-array
                  '(8)
                  :initial-contents (list v0 v1 v2 v3 v4 v5 v6 v7))))
    (marching-cubes::make-grid-cell *vertex-positions* values)))

(defun test-triangles (got expected)
  (and (= (length got) (length expected))
       (every #'triangle= got expected)))


(plan nil)

(is (marching-cubes::polygonise (make-test-grid 0 0 0 0 0 0 0 0) 1)
    nil "polygonise 1")

(is (marching-cubes::polygonise (make-test-grid 2 0 0 0 0 0 0 0) 1)
    (list (make-triangle (make-vec3 1/2 0   0)
                         (make-vec3 0   1/2 0)
                         (make-vec3 0   0   1/2)))
    "polygonise 2" :test #'test-triangles)

(is (marching-cubes::polygonise (make-test-grid 2 2 0 0 0 0 0 0) 1)
    (list (make-triangle (make-vec3 1 1/2 0)
                         (make-vec3 0 1/2 0)
                         (make-vec3 0 0   1/2))
          (make-triangle (make-vec3 1 0   1/2)
                         (make-vec3 1 1/2 0)
                         (make-vec3 0 0   1/2)))
    "polygonise 3" :test #'test-triangles)

(is (marching-cubes::polygonise (make-test-grid 2 2 0 2 0 0 0 0) 1)
    (list (make-triangle (make-vec3 1   1/2 0  )
                         (make-vec3 1/2 1   0  )
                         (make-vec3 0   1   1/2))
          (make-triangle (make-vec3 1   1/2 0  )
                         (make-vec3 0   1   1/2)
                         (make-vec3 1   0   1/2))
          (make-triangle (make-vec3 1   0   1/2)
                         (make-vec3 0   1   1/2)
                         (make-vec3 0   0   1/2)))
    "polygonise 4" :test #'test-triangles)

(defun fn (x y z)
  (if (= x y z 0) 2 0))

(defvar grid1 (marching-cubes::make-grid #'fn
                                         (make-vec3 0 0 0)
                                         (make-vec3 1 2 3)
                                         1))
(is (marching-cubes::grid-min grid1) (make-vec3 0 0 0)
    "grid-min" :test #'vec3=)
(is (marching-cubes::grid-max grid1) (make-vec3 1 2 3)
    "grid-max 1" :test #'vec3=)
(is (marching-cubes::grid-size-x grid1) 1
    "grid-size-x")
(is (marching-cubes::grid-size-y grid1) 2
    "grid-size-y")
(is (marching-cubes::grid-size-z grid1) 3
    "grid-size-z")
(is (marching-cubes::grid-delta grid1) 1
    "grid-delta")
(is (marching-cubes::grid-point grid1 0 0 0) (make-vec3 0 0 0)
    "grid-point 1" :test #'vec3=)
(is (marching-cubes::grid-point grid1 1 2 3) (make-vec3 1 2 3)
    "grid-point 2" :test #'vec3=)
(is-error (marching-cubes::grid-point grid1 2 0 0)
    simple-error "grid-point 3")
(is (marching-cubes::grid-value grid1 0 0 0) 2
    "grid-value 1")
(is (marching-cubes::grid-value grid1 1 2 3) 0
    "grid-value 2")
(is-error (marching-cubes::grid-value grid1 2 0 0)
          simple-error "grid-value 3")

(defvar grid2 (marching-cubes::make-grid #'fn
                                         (make-vec3 0 0 0)
                                         (make-vec3 1 1 1)
                                         2))
(is (marching-cubes::grid-max grid2) (make-vec3 2 2 2)
    "grid-max 2" :test #'vec3=)

(is-error (marching-cubes::make-grid #'fn
                                     (make-vec3 1 0 0)
                                     (make-vec3 0 1 1)
                                     2)
          simple-error "make-grid")

(is (marching-cubes::marching-cubes #'fn
                                    (make-vec3 0 0 0)
                                    (make-vec3 1 1 1)
                                    1 1)
    (list (make-triangle (make-vec3 1/2 0   0  )
                         (make-vec3 0   1/2 0  )
                         (make-vec3 0   0   1/2)))
    "marching-cubes 1" :test #'test-triangles)

(defun fn2 (x y z)
  (if (= y z 0) 2 0))

(is (marching-cubes::marching-cubes #'fn2
                                    (make-vec3 0 0 0)
                                    (make-vec3 2 2 2)
                                    1 1)
    (list (make-triangle (make-vec3 2 1/2 0  )
                         (make-vec3 1 1/2 0  )
                         (make-vec3 1 0   1/2))
          (make-triangle (make-vec3 2 0   1/2)
                         (make-vec3 2 1/2 0  )
                         (make-vec3 1 0   1/2))
          (make-triangle (make-vec3 1 1/2 0  )
                         (make-vec3 0 1/2 0  )
                         (make-vec3 0 0   1/2))
          (make-triangle (make-vec3 1 0   1/2)
                         (make-vec3 1 1/2 0)
                         (make-vec3 0 0   1/2)))
    "marching-cubes 1" :test #'test-triangles)

(finalize)
