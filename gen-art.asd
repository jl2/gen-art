;;;; gen-art.asd
;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


(asdf:defsystem #:gen-art
  :description "Describe gen-art here"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-cairo2 #:lparallel)
  :components ((:file "package")
               (:file "gen-art")))
