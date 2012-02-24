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

(finalize)
