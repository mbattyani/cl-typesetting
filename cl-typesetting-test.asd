;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package asdf)

(defsystem :cl-typesetting-test
    :name "cl-typesetting-test"
    :author "Marc Battyani <marc.battyani@fractalconcept.com>"
    :version "1.0"
    :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
    :licence "BSD like licence"
    :description "Common Lisp Typesetting Tests"
    :long-description "Tests for the cl-typesetting, cl-typegraph typesetting systems."
    :components ((:file "test"))
    :depends-on (:cl-typesetting :cl-typegraph))

