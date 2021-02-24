;;; cl-typesetting copyright 2002-2021 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

;;
;; Proposed initialize! function and warning when loading without hyphen-patterns directory,
;;
;; Dave Cooper, david -dot- cooper -at- genworks -dot- com
;;


(in-package #:typeset)


(defun initialize! (&key hyphen-patterns-directory afm-files-directories)
  "Directory. Set the hyphen-patterns directory to the correct runtime
value, and force initialization of American and French hyphenation tries."

  (when afm-files-directories
    (pdf:initialize! :afm-files-directories afm-files-directories))

  (when (pdf::confirm-afm-files-directories)
    (setq *default-font* (pdf:get-font)
	  *font* *default-font*))

  (when hyphen-patterns-directory
    (setq cl-tt-hyph::*hyphen-patterns-directory* hyphen-patterns-directory))
  (when (confirm-hyphen-patterns-directory)
    (setf cl-tt-hyph::*american-hyphen-trie* (cl-tt-hyph:load-language :american))
    (setf cl-tt-hyph::*french-hyphen-trie*   (cl-tt-hyph:load-language :french))
    cl-tt-hyph::*hyphen-patterns-directory*))

(defun confirm-hyphen-patterns-directory ()
  (or (#-clisp probe-file #+clisp ext:probe-directory
	       cl-tt-hyph::*hyphen-patterns-directory*)
      (warn "You have set the following non-existent hyphen-patterns directory:

~a

Before attempting to run any cl-pdf functions, you will want to
initialize the system with something like this:

  (typeset:initialize! :hyphen-patterns-directory \"/usr/share/hyphen-patterns/\")
"
	    cl-tt-hyph::*hyphen-patterns-directory*)))

(eval-when (:load-toplevel)
  (initialize!))
