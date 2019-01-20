;;;; gen-art.lisp
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(in-package #:gen-art)

(defun home-dir (path)
  "Utility function to make relative path names relative to the user's home directory to work around Cairo weirdness."
  (merge-pathnames path (user-homedir-pathname)))

(defun parametric-function (png-file-name
                            &key
                              (width 1200) (height 1200)
                              (steps 8000)
                              (open-png t)
                              (tmin 0.0)
                              (tmax (* 2 pi))
                              (line-width 0.8)
                              (x-scale (/ width 2.0))
                              (y-scale (/ height 2.0))
                              (x-function (lambda (tv) (cos (* 5.0 tv))))
                              (y-function (lambda (tv) (cos (* 7.0 tv)))))
  "Test writing a PNG file with Cairo."
  (let ((real-file-name (home-dir png-file-name))
        (half-width (/ width 2.0))
        (half-height (/ height 2.0))
         (dt (/ (- tmax tmin) steps))
         (xp nil)
         (yp nil))
    (cl-cairo2:with-png-file (real-file-name :argb32 width height)
                             (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
                             (cl-cairo2:paint)
                             (cl-cairo2:set-line-width line-width)
                             (cl-cairo2:translate half-width half-height)
                             (cl-cairo2:set-source-rgba 0 1 0 1.0)
                             (dotimes (i steps)
                               (let* ((tv (* i dt))
                                      (x (* x-scale (funcall x-function tv)))
                                      (y (* y-scale (funcall y-function tv))))
                                 (if (not (and xp yp))
                                     (cl-cairo2:move-to x y)
                                   (cl-cairo2:line-to x y))
                                 (setf xp x)
                                 (setf yp y)))
                             (cl-cairo2:stroke))
    (when open-png
      (swank:eval-in-emacs (list 'find-file-other-window (namestring real-file-name))))))

(defun deg-to-rad (deg)
  "Convert degrees to radians."
  (declare (type double-float deg))
  (* deg (/ pi 180)))

(defun spiral (png-file-name
               &key
                 (open-png t)
                 (twists 30)
                 (width 1200)
                 (height 1200)
                 (steps (* twists 360)))
  "Draw a spiral with the specified number of twists, saving into the given file name."
  (let ((real-file-name (home-dir png-file-name)))
    (cl-cairo2:with-png-file (real-file-name :argb32 width height)
      (cl-cairo2:set-source-rgba 1.0 1.0 1.0 0.0)
      (cl-cairo2:paint)
      (cl-cairo2:translate (/ width 2.0) (/ height 2.0))
      (cl-cairo2:set-line-width 2)
      (cl-cairo2:scale (/ width 2.0) (/ height 2.0))
      (cl-cairo2:set-source-rgba 0 0 0 1.0)
      (cl-cairo2:move-to 0 0)
      (dotimes (i steps)
        (let* ((ifloat (coerce i 'double-float))
               (xoff (* (cos (deg-to-rad ifloat)) (/ ifloat steps)))
               (yoff (* (sin (deg-to-rad ifloat)) (/ ifloat steps))))
          ;; (cl-cairo2:move-to 0 0)
          (cl-cairo2:rel-line-to xoff yoff)))
      (cl-cairo2:stroke))
    (when open-png
      (swank:eval-in-emacs (list 'find-file-other-window (namestring real-file-name))))))

(defun fractal-tree (png-file-name
                     &key
                       (open-png t)
                       (width 1200) (height 1200)
                       (length 200)
                       (maxdepth 4)
                       (limbs 2)
                       )
  "Draw a fractal tree into the specified file, recursing to maxdepth, with the specified number of limbs at each level."
  (let ((real-file-name (home-dir png-file-name)))
    (cl-cairo2:with-png-file (real-file-name :argb32 width height)

      (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
      (cl-cairo2:paint)
      
      (cl-cairo2:scale 1 1)

      (labels
          ((to-cart (len angle)
             "Convert a length and angle (polar coordinates) into x,y rectangular coordinates."
             (values (* (cos (deg-to-rad angle)) len) (* (sin (deg-to-rad angle)) len)))
           (draw-tree
               (x y length angle transparency depth )

             (multiple-value-bind
                   (nx ny) (to-cart length angle)

               (cl-cairo2:set-source-rgba 0 1.0 0 transparency)
               (cl-cairo2:set-line-width (/ (1+ depth) 0.5))
               (cl-cairo2:move-to x y)
               (cl-cairo2:line-to (+ x nx) (+ y ny))
               (cl-cairo2:stroke)

               (if (> depth 0)
                   (dotimes (i limbs)

                     (let ((nnx (+ x nx))
                           (nny (+ y ny))
                           (nl (/ length 2.0))
                           (ang1 (+ angle (/ 45 limbs) (* i (/ 180 limbs))))
                           (ang2 (- angle (/ 45 limbs) (* i (/ 180 limbs))))
                           (ntrans (* 1.75 (/ depth maxdepth) transparency))
                           (ndepth (- depth 1)))
                       (draw-tree nnx nny nl ang1 ntrans ndepth)
                       (draw-tree nnx nny nl ang2 ntrans ndepth)))))))

        (draw-tree (/ width 2) height length -90.0 1.0 maxdepth)))
    (when open-png
      (swank:eval-in-emacs (list 'find-file-other-window (namestring real-file-name))))))


(defmacro simple-animation ((variable duration) &body body)
  `(dotimes (,variable (ceiling (1+ (* 60 ,duration))))
     ,@body
     ))

(defun animated-parametric-function (output-directory mp4-file-name
                                     &key
                                       (width 1200) (height 1200)
                                       (steps 8000)
                                       (duration 60)
                                       (fps 30)
                                       (tmin 0.0)
                                       (tmax (* 2 pi))
                                       (line-width 0.8)
                                       (x-scale (/ width 2.0))
                                       (y-scale (/ height 2.0))
                                       (x-function (lambda (tv i) (declare (ignorable tv i)) (* (cos (* (/ i 60.0) tv)) (sin tv))))
                                       (y-function (lambda (tv i) (declare (ignorable tv i)) (* (cos (* (/ i 59.0) tv)) (cos tv)))))
  (ensure-directories-exist output-directory)
  (let ((frames (* duration fps)))
    (dotimes (i frames)
      (let ((current-png-name (format nil "~a/frame~8,'0d.png" output-directory i))
            (half-width (/ width 2.0))
            (half-height (/ height 2.0))
            (dt (/ (- tmax tmin) steps))
            (xp nil)
            (yp nil))
        (cl-cairo2:with-png-file (current-png-name :argb32 width height)
          (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
          (cl-cairo2:paint)
          (cl-cairo2:set-line-width line-width)
          (cl-cairo2:translate half-width half-height)
          (cl-cairo2:set-source-rgba 0 1 0 1.0)
          (dotimes (j steps)
            (let* ((tv (* j dt))
                   (x (* x-scale (funcall x-function tv i)))
                   (y (* y-scale (funcall y-function tv i))))
              (when (not (and xp yp))
                (cl-cairo2:move-to x y))
              (cl-cairo2:line-to x y)
              (setf xp x)
              (setf yp y)))
          (cl-cairo2:stroke))))
    (anim-utils:make-movie :directory output-directory :file-name mp4-file-name)))
      
