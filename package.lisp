;;;; package.lisp

(defpackage #:pota
  (:use #:cl)
  (:import-from #:jeffutils
		#:cdr-assoc
		#:any-true
		#:histogram
		#:join
		#:english-join
		#:float-or-nil
		#:quotes-if-null
		#:remove-duplicate-strings)
  (:import-from #:local-time
		#:timestamp-to-unix
		#:now
		#:parse-timestring)
  (:import-from #:split-sequence
		#:split-sequence)
  (:import-from #:aviation-formulary
		#:2d-point
		#:rad-to-sm
		#:rad-to-deg
		#:calc-distance
		#:calc-gc-bearing
		#:deg-to-cardinal-course)
  )
