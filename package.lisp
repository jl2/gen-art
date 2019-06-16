;;;; package.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(defpackage #:gen-art
  (:use #:cl #:3d-vectors)
  (:export #:parametric-function
           #:spiral
           #:fractal-tree
           #:animate
           #:animated-parametric-function
           #:random-threads

           #:random-lines
           #:random-halton-sequence-lines
           #:random-cubics
           #:random-parametric-cubics
           #:parametric-cubic-animation
           #:centered-circles
           #:gradient-sine-circles
           #:bl-fractal-tree
           #:bl-fractal-tests
           #:bl-fractal-wtf
           #:gradient-function-fill
           #:quadtree-viz
           ))
