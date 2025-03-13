;;;; pota.lisp

(in-package #:pota)

(defparameter *here* (make-instance '2d-point :lat 39.3553 :lon -104.673))
(defparameter *spots* nil)
(defparameter *users* (make-hash-table :test #'equal))
(defparameter *profiles* (make-hash-table :test #'equal))
(defparameter *parks* (make-hash-table :test #'equal))
(defparameter *spots-lock* (bt:make-lock))
(defparameter *users-lock* (bt:make-lock))
(defparameter *profiles-lock* (bt:make-lock))
(defparameter *parks-lock* (bt:make-lock))
(defparameter *spot-thread* nil)

;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; API Interaction
;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun fetch-and-decode (uri &optional (method :get))
  (json:decode-json-from-string
   (babel:octets-to-string
    (nth-value 0 
	       (drakma:http-request uri
				    :method method
				    :accept "application/json"
				    :content-type "application/json")))))
  
;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; Spot Stuff
;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun fetch-spots ()
  (bt:with-lock-held (*spots-lock*)
    (setf *spots*
	  (mapcar
	   #'make-pota-spot
	   (fetch-and-decode "https://api.pota.app/spot/activator" :get))))
    (mapcar #'get-user (mapcar #'activator *spots*))
    (mapcar #'get-park (mapcar #'reference *spots*))
  t)

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

(defmethod locator ((s pota-spot))
  (if (grid-6 s)
      (grid-6 s)
      (grid-4 s)))

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

(defmethod age ((s pota-spot))
  (- (timestamp-to-unix (now))
     (timestamp-to-unix (spot-time s))))

(defmethod pp ((s pota-spot))
  "Print a pota-spot point nicely."
  (format t "Activator: ~A (~F/~A) @ ~A (~A sec ago)~%"
	  (activator s)
	  (freq s)
	  (quotes-if-null (mode s) "UNK")
	  (spot-time s)
	  (age s))
  (format t "Park: ~A/~A/~A~%"
	  (reference s)
	  (location s)
	  (quotes-if-null (name s) "Unknown"))
  (if *here*
      (format t "Where: ~,1F mi @ ~,1F deg (~A) ~A ~F/~F~%"
	      (dist s)
	      (direction s)
	      (deg-to-cardinal-course (direction s))
	      (locator s)
	      (lat s)
	      (lon s))
      (format t "Where: ~A ~F/~F~%"
	      (locator s)
	      (lat s)
	      (lon s)))
  (format t "~%"))

(defun make-pota-spot (spot)
  "Create a pota-spot object from the returned JSON data from the pota.app API."
  (make-instance 'pota-spot
		 :spot-id (cdr-assoc :spot-id spot)
		 :activator (cdr-assoc :activator spot)
		 :freq (float-or-nil (cdr-assoc :frequency spot))
		 :mode (if (equal "" (cdr-assoc :mode spot))
			   nil
			   (cdr-assoc :mode spot))
		 :reference (cdr-assoc :reference spot)
		 :park-name (cdr-assoc :park-name spot)
		 :spot-time (parse-timestring (cdr-assoc :spot-time spot))
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

;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; User Stuff
;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun fetch-user (callsign)
  (let ((info (fetch-and-decode (concatenate 'string "https://api.pota.app/stats/user/"
					     (string-upcase callsign)))))
    (if (equal "Callsign not found" info)
	(setf (gethash callsign *users*) nil)
	(make-pota-user info))))

(defclass pota-user ()
  ((callsign :accessor callsign :initarg :callsign :initform nil)
   (name :accessor name :initarg :name :initform nil)
   (qth :accessor qth :initarg :qth :initform nil)
   (award-count :accessor award-count :initarg :award-count :initform nil)
   (endorsements :accessor endorsements :initarg :endorsements :initform nil)
   (gravatar :accessor gravatar :initarg :gravatar :initform nil)
   (activator-activations :accessor activator-activations :initarg :activator-activations :initform nil)
   (activator-parks :accessor activator-parks :initarg :activator-parks :initform nil)
   (activator-qsos :accessor activator-qsos :initarg :activator-qsos :initform nil)
   (attempts-activations :accessor attempts-activations :initarg :attempts-activations :initform nil)
   (attempts-parks :accessor attempts-parks :initarg :attempts-parks :initform nil)
   (attempts-qsos :accessor attempts-qsos :initarg :attempts-qsos :initform nil)
   (hunter-parks :accessor hunter-parks :initarg :hunter-parks :initform nil)
   (hunter-qsos :accessor hunter-qsos :initarg :hunter-qsos :initform nil))
  (:documentation "A class to hold a POTA user."))

(defun make-pota-user (user)
  "Create a pota-user object from the returned JSON data from the pota.app API."
  (make-instance 'pota-user
		 :callsign (cdr-assoc :callsign user)
		 :name (if (equal "" (cdr-assoc :name user))
			   nil
			   (cdr-assoc :name user))
		 :qth  (if (equal "" (cdr-assoc :qth user))
			   nil
			   (cdr-assoc :qth user))
		 :award-count (cdr-assoc :awards user)
		 :endorsements (cdr-assoc :endorsements user)
		 :gravatar  (if (equal "" (cdr-assoc :gravatar user))
				nil
				(cdr-assoc :gravatar user))
		 :activator-activations (cdr-assoc :activations (cdr-assoc :activator user))
		 :activator-parks (cdr-assoc :parks (cdr-assoc :activator user))
		 :activator-qsos (cdr-assoc :qsos (cdr-assoc :activator user))
		 :attempts-activations (cdr-assoc :activations (cdr-assoc :attempts user))
		 :attempts-parks (cdr-assoc :parks (cdr-assoc :attempts user))
		 :attempts-qsos (cdr-assoc :qsos (cdr-assoc :attempts user))
		 :hunter-parks (cdr-assoc :parks (cdr-assoc :hunter user))
		 :hunter-qsos (cdr-assoc :qsos (cdr-assoc :hunter user))))

(defmethod pp ((u pota-user))
  "Print a pota-user nicely."
  (format t "Callsign: ~A~%" (callsign u))
  (when (name u) (format t "Name: ~A~%" (name u)))
  (when (qth u) (format t "QTH: ~A~%" (qth u)))
  (format t "Award Count: ~A~%" (award-count u))
  (format t "Endorsements: ~A~%" (endorsements u))
  (format t "Activator - Activations: ~A, Parks ~A, QSOs: ~A~%"
	  (activator-activations u) (activator-parks u) (activator-qsos u))
  (format t "Attempts - Activations: ~A, Parks ~A, QSOs: ~A~%"
	  (attempts-activations u) (attempts-parks u) (attempts-qsos u))
  (format t "Hunter - Parks ~A, QSOs: ~A~%"
	  (hunter-parks u) (hunter-qsos u))
  (format t "~%"))

(defun get-user (callsign &optional force-update)
  (bt:with-lock-held (*users-lock*)
    (let* ((call (string-upcase (first (split-sequence #\/ callsign))))
	   (user (gethash call *users*)))
      (if (and user (null force-update))
	  user
	  (setf (gethash call *users*)
		(fetch-user call))))))

;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; Park Stuff
;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun fetch-park (reference)
  (make-pota-park
   (fetch-and-decode (concatenate 'string "https://api.pota.app/park/"
				  (string-upcase reference)))))

(defclass pota-park ()
  ((park-id :accessor park-id :initarg :park-id :initform nil)
   (reference :accessor reference :initarg :reference :initform nil)
   (name :accessor name :initarg :name :initform nil)
   (lat :accessor lat :initarg :lat :initform nil)
   (lon :accessor lon :initarg :lon :initform nil)
   (grid-4 :accessor grid-4 :initarg :grid-4 :initform nil)
   (grid-6 :accessor grid-6 :initarg :grid-6 :initform nil)
   (parktype-id :accessor parktype-id :initarg :parktype-id :initform nil)
   (active :accessor active :initarg :active :initform nil)
   (park-comments :accessor park-comments :initarg :park-comments :initform nil)
   (accessibility :accessor accessibility :initarg :accessibility :initform nil)
   (sensitivity :accessor sensitivity :initarg :sensitivity :initform nil)
   (access-methods :accessor access-methods :initarg :access-methods :initform nil)
   (activation-methods :accessor activation-methods :initarg :activation-methods :initform nil)
   (agencies :accessor agencies :initarg :agencies :initform nil)
   (agency-url :accessor agency-url :initarg :agency-url :initform nil)
   (park-url :accessor park-url :initarg :park-url :initform nil)
   (website :accessor website :initarg :website :initform nil)
   (created-by-admin :accessor created-by-admin :initarg :created-by-admin :initform nil)
   (parktype-desc :accessor parktype-desc :initarg :parktype-desc :initform nil)
   (location-desc :accessor location-desc :initarg :location-desc :initform nil)
   (location-name :accessor location-name :initarg :location-name :initform nil)
   (entity-id :accessor entity-id :initarg :entity-id :initform nil)
   (entity-name :accessor entity-name :initarg :entity-name :initform nil)
   (reference-prefix :accessor reference-prefix :initarg :reference-prefix :initform nil)
   (entity-deleted :accessor entity-deleted :initarg :entity-deleted :initform nil)
   (first-activator :accessor first-activator :initarg :first-activator :initform nil)
   (first-activation-date :accessor first-activation-date :initarg :first-activation-date :initform nil))
  (:documentation "A class to hold a POTA park."))

(defmethod locator ((s pota-park))
  (if (grid-6 s)
      (grid-6 s)
      (grid-4 s)))

(defun make-pota-park (park)
  "Create a pota-park object from the returned JSON data from the pota.app API."
  (make-instance 'pota-park
		 :park-id (cdr-assoc :park-id park)
		 :reference (cdr-assoc :reference park)
		 :name (cdr-assoc :name park)
		 :lat (cdr-assoc :latitude park)
		 :lon (cdr-assoc :longitude park)
		 :grid-4 (cdr-assoc :grid-4 park)
		 :grid-6 (cdr-assoc :grid-6 park)
		 :parktype-id (cdr-assoc :parktype-id park)
		 :active (cdr-assoc :active park)
		 :park-comments (cdr-assoc :park-comments park)
		 :accessibility (cdr-assoc :accessibility park)
		 :sensitivity (cdr-assoc :sensitivity park)
		 :access-methods (cdr-assoc :access-methods park)
		 :activation-methods (cdr-assoc :activation-methods park)
		 :agencies (cdr-assoc :agencies park)
		 :agency-url (cdr-assoc :agency-+ur+-ls park)
		 :park-url (cdr-assoc :park-+ur+-ls park)
		 :website (cdr-assoc :website park)
		 :created-by-admin (cdr-assoc :created-by-admin park)
		 :parktype-desc (cdr-assoc :parktype-desc park)
		 :location-desc (cdr-assoc :location-desc park)
		 :location-name (cdr-assoc :location-name park)
		 :entity-id (cdr-assoc :entity-id park)
		 :entity-name (cdr-assoc :entity-name park)
		 :reference-prefix (cdr-assoc :reference-prefix park)
		 :entity-deleted (cdr-assoc :entity-deleted park)
		 :first-activator (cdr-assoc :first-activator park)
		 :first-activation-date (cdr-assoc :first-activation-date park)))

(defmethod point ((s pota-park))
  "Return an aviation-formulary:2d-point object representing the location of a pota-park."
  (make-instance '2d-point
		 :lat (lat s)
		 :lon (lon s)
		 :name (reference s)))

(defmethod dist ((s pota-park))
  "Distance in miles from *here*."
  (rad-to-sm
   (calc-distance *here* (point s))))

(defmethod direction ((s pota-park))
  "Direction in degrees from *here*."
  (rad-to-deg
   (calc-gc-bearing *here* (point s))))

(defmethod pp ((p pota-park))
  ;;; todo: placeholder. make mo betta
  (describe p))

(defun get-park (reference &optional force-update)
  (bt:with-lock-held (*parks-lock*)
    (let ((park (gethash (string-upcase reference) *parks*)))
      (if (and park (null force-update))
	  park
	  (setf (gethash (string-upcase reference) *parks*)
		(fetch-park reference))))))

;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; History Stuff
;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun fetch-history (callsign reference)
  (fetch-and-decode (concatenate 'string "https://api.pota.app/spot/comments/"
				 (string-upcase callsign)
				 "/"
				 (string-upcase reference))))

;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; Activation Stuff
;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun fetch-activations ()
  (mapcar
   #'make-pota-activation
   (fetch-and-decode "https://api.pota.app/activation" :get)))

(defclass pota-activation ()
  ((scheduled-activities-id :accessor scheduled-activities-id :initarg :scheduled-activities-id :initform nil)
   (scheduler-user-id :accessor scheduler-user-id :initarg :scheduler-user-id :initform nil)
   (activator :accessor activator :initarg :activator :initform nil)
   (name :accessor name :initarg :name :initform nil)
   (reference :accessor reference :initarg :reference :initform nil)
   (location-desc :accessor location-desc :initarg :location-desc :initform nil)
   (activity-start :accessor activity-start :initarg :activity-start :initform nil)
   (activity-end :accessor activity-end :initarg :activity-end :initform nil)
   (start-date :accessor start-date :initarg :start-date :initform nil)
   (end-date :accessor end-date :initarg :end-date :initform nil)
   (start-time :accessor start-time :initarg :start-time :initform nil)
   (end-time :accessor end-time :initarg :end-time :initform nil)
   (frequencies :accessor frequencies :initarg :frequencies :initform nil)
   (comments :accessor comments :initarg :comments :initform nil))
  (:documentation "A class to hold a POTA activation."))

(defmethod pp ((p pota-activation))
  ;;; todo: placeholder. make mo betta
  (describe p))

(defun make-pota-activation (act)
  "Create a pota-activation object from the returned JSON data from the pota.app API."
  (make-instance 'pota-activation
		 :scheduled-activities-id (cdr-assoc :scheduled-activities-id act)
		 :scheduler-user-id (cdr-assoc :scheduler-user-id act)
		 :activator (cdr-assoc :activator act)
		 :name (cdr-assoc :name act)
		 :reference (cdr-assoc :reference act)
		 :location-desc (cdr-assoc :location-desc act)
		 :activity-start (cdr-assoc :activity-start act)
		 :activity-end (cdr-assoc :activity-end act)
		 :start-date (cdr-assoc :start-date act)
		 :end-date (cdr-assoc :end-date act)
		 :start-time (cdr-assoc :start-time act)
		 :end-time (cdr-assoc :end-time act)
		 :frequencies (cdr-assoc :frequencies act)
		 :comments (cdr-assoc :comments act)))

;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; Profile Stuff
;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun fetch-profile (callsign)
  (make-pota-profile
   (fetch-and-decode (concatenate 'string "https://api.pota.app/profile/"
				  (string-upcase callsign)))))

(defclass pota-profile (pota-user)
  ((id :accessor id :initarg :id :initform nil)
   (other-callsigns :accessor other-callsigns :initarg :other-callsigns :initform nil)
   (recent-activations :accessor recent-activations :initarg :recent-activations :initform nil)
   (recent-qsos :accessor recent-qsos :initarg :recent-qsos :initform nil)
   (awards :accessor awards :initarg :awards :initform nil))
  (:documentation "A class to hold a POTA profile."))

(defun make-pota-profile (profile)
  "Create a pota-profile object from the returned JSON data from the pota.app API."
  (make-instance 'pota-profile
		 :id (cdr-assoc :id profile)
		 :other-callsigns (cdr-assoc :other--callsigns profile)
		 :callsign (cdr-assoc :callsign profile)
		 :name (if (equal "" (cdr-assoc :name profile))
			   nil
			   (cdr-assoc :name profile))
		 :qth  (if (equal "" (cdr-assoc :qth profile))
			   nil
			   (cdr-assoc :qth profile))
		 :award-count (cdr-assoc :awards (cdr-assoc :stats profile))
		 :awards (cdr-assoc :awards profile)
		 :endorsements (cdr-assoc :endorsements profile)
		 :gravatar  (if (equal "" (cdr-assoc :gravatar profile))
				nil
				(cdr-assoc :gravatar profile))
		 :activator-activations (cdr-assoc :activations
						   (cdr-assoc :activator
							      (cdr-assoc :stats profile)))
		 :activator-parks (cdr-assoc :parks
					     (cdr-assoc :activator
							(cdr-assoc :stats profile)))
		 :activator-qsos (cdr-assoc :qsos
					    (cdr-assoc :activator
						       (cdr-assoc :stats profile)))
		 :attempts-activations (cdr-assoc :activations
						  (cdr-assoc :attempts
							     (cdr-assoc :stats profile)))
		 :attempts-parks (cdr-assoc :parks
					    (cdr-assoc :attempts
						       (cdr-assoc :stats profile)))
		 :attempts-qsos (cdr-assoc :qsos
					   (cdr-assoc :attempts
						      (cdr-assoc :stats profile)))
		 :hunter-parks (cdr-assoc :parks
					  (cdr-assoc :hunter
						     (cdr-assoc :stats profile)))
		 :hunter-qsos (cdr-assoc :qsos
					 (cdr-assoc :hunter
						    (cdr-assoc :stats profile)))
		 :recent-activations (cdr-assoc :activations
						(cdr-assoc :recent--activity profile))
		 :recent-qsos (cdr-assoc :hunter--qsos
					 (cdr-assoc :recent--activity profile))))

(defun get-profile (callsign &optional force-update)
  (bt:with-lock-held (*profiles-lock*)
    (let ((profile (gethash (string-upcase callsign) *profiles*)))
      (if (and profile (null force-update))
	  profile
	  (setf (gethash (string-upcase callsign) *profiles*)
		(fetch-profile callsign))))))

;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;;; Utilities
;;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defun sort-age (spots)
  (sort spots (lambda (a b) (< (age a) (age b)))))

(defun sort-dist (spots)
  "Sort a list of pota-activator objects by distance from *here*."
  (sort spots (lambda (a b) (< (dist a) (dist b)))))

(defun filter-location (location spots)
  "Given a list of pota-activators, return those that match the given location."
  (remove-if-not
   (lambda (s)
     (member location (split-sequence #\, (location s)) :test #'equal))
   spots))

(defun band (freq)
  "Given a frequency in khz, return the ham band. Works between 160m
and 70cm."
  (cond
    ((and (>= freq 1800) (<= freq 2000)) :160m)
    ((and (>= freq 3500) (<= freq 4000)) :80m)
    ((and (>= freq 5330) (<= freq 5410)) :60m)
    ((and (>= freq 7000) (<= freq 7300)) :40m)
    ((and (>= freq 10100) (<= freq 10150)) :30m)
    ((and (>= freq 14000) (<= freq 14350)) :20m)
    ((and (>= freq 17068) (<= freq 17168)) :17m)
    ((and (>= freq 21000) (<= freq 21450)) :15m)
    ((and (>= freq 24890) (<= freq 24990)) :12m)
    ((and (>= freq 28000) (<= freq 29700)) :10m)
    ((and (>= freq 50000) (<= freq 54000)) :6m)
    ((and (>= freq 144000) (<= freq 148000)) :2m)
    ((and (>= freq 219000) (<= freq 225000)) :1.25m)
    ((and (>= freq 420000) (<= freq 450000)) :70cm)
    (t nil)))

(defun pota-thread (sleep-time)
  (loop
    (fetch-spots)
    (sleep sleep-time)))

(defun start-pota-thread (&optional (sleep-time 60))
  (setf *spot-thread*
	(bt:make-thread (lambda () (pota-thread sleep-time)) :name "pota-thread"))
  (format t "pota-thread is running...~%"))

;;; Local Variables:
;;; mode: Lisp
;;; coding: utf-8
;;; End:
