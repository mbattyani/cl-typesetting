;;; cl-typesetting copyright 2002 Marc Battyani see license.txt for details of the license
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net

(in-package typeset)

(defun hyphenate-string (string)
(when (> (length string) 4)
  (loop with hyphen-points
        for start = 0 then (position-if #'alpha-char-p string :start (1+ end))
        for end = (when start (position-if-not #'alpha-char-p string :start (1+ start)))
        while end
        when (> (- end start) 4)
        nconc (mapcar #'(lambda (n) (+ start n))
		      (nix::hyphen-find-hyphen-points
		       nix::*american-hyphen-trie* (subseq string start end))))))
