;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package asdf)

(defsystem :cl-typegraph
    :name "cl-typegraph"
    :author "Marc Battyani <marc.battyani@fractalconcept.com>"
    :version "0.5"
    :maintainer "Marc Battyani <marc.battyani@fractalconcept.com>"
    :licence "BSD like licence"
    :description "Common Lisp Graph Typesetting"
    :long-description "The cl-typegraph package is a stand-alone Common Lisp graph typesetting system."
    :perform (load-op :after (op cl-typegraph)
		      (pushnew :cl-typegraph cl:*features*))
    :components ((:file "graph"))
    :depends-on (:cl-typesetting))

