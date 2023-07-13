;;;; pota.asd

(asdf:defsystem #:pota
  :description "Screen-scrapes the POTA web site in place of an API."
  :author "Jeff Francis <jeff@gritch.org>"
  :license "MIT, see file LICENSE"
  :depends-on (#:drakma
	       #:babel
	       #:alexandria
	       #:cl-json
	       #:jsown
	       #:jsown-utils
	       #:jeffutils
	       #:local-time
               #:bordeaux-threads
	       #:aviation-formulary
	       #:split-sequence)
  :serial t
  :components ((:file "package")
               (:file "pota")))

