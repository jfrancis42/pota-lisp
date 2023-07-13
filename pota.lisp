;;;; pota.lisp

(in-package #:pota)

(defparameter *here* (make-instance '2d-point :lat 39.3553 :lon -104.673))

(defun fetch-pota ()
  "Do an HTML GET on the specified URI and return the result as parsed
JSON."
  (json:decode-json-from-string
   (babel:octets-to-string
    (nth-value 0 
	       (drakma:http-request "https://api.pota.app/spot/activator"
				    :method :get
				    :accept "application/json"
				    :content-type "application/json")))))

(defclass pota-spot ()
  ((spot-id :accessor spot-id :initarg :spot-id :initform nil)
   (activator :accessor activator :initarg :activator :initform nil)
   (freq :accessor freq :initarg :freq :initform nil)
   (mode :accessor mode :initarg :mode :initform nil)
   (reference :accessor reference :initarg :reference :initform nil)
   (park-name :accessor park-name :initarg :park-name :initform nil)
   (spot-time :accessor spot-time :initarg :spot-time :initform nil)
   (spotter :accessor spotter :initarg :spotter :initform nil)
   (comments :accessor comments :initarg :comments :initform nil)
   (source :accessor source :initarg :source :initform nil)
   (invalid :accessor invalid :initarg :invalid :initform nil)
   (name :accessor name :initarg :name :initform nil)
   (location :accessor location :initarg :location :initform nil)
   (grid-4 :accessor grid-4 :initarg :grid-4 :initform nil)
   (grid-6 :accessor grid-6 :initarg :grid-6 :initform nil)
   (lat :accessor lat :initarg :lat :initform nil)
   (lon :accessor lon :initarg :lon :initform nil)
   (spot-count :accessor spot-count :initarg :spot-count :initform nil)
   (expire :accessor expire :initarg :expire :initform nil))
  (:documentation "A class to hold a POTA spot."))

(defmethod point ((s pota-spot))
  "Return an aviation-formulary:2d-point object representing the location of a pota-spot."
  (make-instance '2d-point
		 :lat (lat s)
		 :lon (lon s)
		 :name (reference s)))

(defmethod dist ((s pota-spot))
  "Distance in miles from *here*."
  (rad-to-sm
   (calc-distance *here* (point s))))

(defmethod direction ((s pota-spot))
  "Direction in degrees from *here*."
  (rad-to-deg
   (calc-gc-bearing *here* (point s))))

(defmethod pp ((s pota-spot))
  "Print a pota-spot point nicely."
  (format t "Activator: ~A (~F/~A) @ ~A~%"
	  (activator s)
	  (freq s)
	  (quotes-if-null (mode s) "UNK")
	  (spot-time s))
  (format t "Park: ~A/~A/~A~%"
	  (reference s)
	  (location s)
	  (quotes-if-null (name s) "Unknown"))
  (if *here*
      (format t "Where: ~,1F mi @ ~,1F deg ~A ~F/~F~%"
	      (dist s)
	      (direction s)
	      (grid-6 s)
	      (lat s)
	      (lon s))
      (format t "Where: ~A ~F/~F~%"
	      (grid-6 s)
	      (lat s)
	      (lon s)))
  (format t "~%"))

(defun make-pota-spot (spot)
  "Create a pota-spot object from the returned JSON data from the pota.app API."
  (make-instance 'pota-spot
		 :spot-id (cdr-assoc :spot-id spot)
		 :activator (cdr-assoc :activator spot)
		 :freq (cdr-assoc :frequency spot)
		 :mode (if (equal "" (cdr-assoc :mode spot))
			   nil
			   (cdr-assoc :mode spot))
		 :reference (cdr-assoc :reference spot)
		 :park-name (cdr-assoc :park-name spot)
		 :spot-time (cdr-assoc :spot-time spot)
		 :spotter (cdr-assoc :spotter spot)
		 :comments (if (equal "" (cdr-assoc :comments spot))
			       nil
			       (cdr-assoc :comments spot))
		 :source (cdr-assoc :source spot)
		 :invalid (cdr-assoc :invalid spot)
		 :name (cdr-assoc :name spot)
		 :location (cdr-assoc :location-desc spot)
		 :grid-4 (cdr-assoc :grid-4 spot)
		 :grid-6 (cdr-assoc :grid-6 spot)
		 :lat (cdr-assoc :latitude spot)
		 :lon (cdr-assoc :longitude spot)
		 :spot-count (cdr-assoc :count spot)
		 :expire (cdr-assoc :expire spot)))

(defun dist-sort (spots)
  "Sort a list of pota-spot objects by distance from *here*."
  (sort spots (lambda (a b) (< (dist a) (dist b)))))

(defun filter-location (location spots)
  "Given a list of pota-spots, return those that match the given location."
  (remove-if-not
   (lambda (s)
     (member location (split-sequence #\, (location s)) :test #'equal))
   spots))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
