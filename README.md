# pota-lisp
A Common Lisp library for retrieving POTA spots from the pota.app website.

This library is still in development, but the basics are present. It's able to fetch current POTA spots from the (undocumented) POTA.app API and allow you to view and manipulate them in useful ways. Or at least ways that are useful to me.

For example to fetch current spots and cache them for use, try this:

```
POTA> (defparameter *spots* (mapcar #'make-pota-spot (fetch-pota)))
*SPOTS*
POTA> 
```

You now have a list of objects representing the individual POTA spots. If you want to start a background thread that periodically fetches spots automatically (60 seconds by default, or specify as an optional parameter to (start-pota-thread), run this:

```
POTA> (start-pota-thread)
pota-thread is running...
NIL
POTA> 
```

This is what a random spot looks like:

```
POTA> (describe (nth 3 (bt:with-lock-held (*spots-lock*) *spots*)))
#<POTA-SPOT {101FCE80B3}>
  [standard-object]

Slots with :INSTANCE allocation:
  SPOT-ID                        = 18095769
  ACTIVATOR                      = "N2AKJ"
  FREQ                           = "14061.0"
  MODE                           = "CW"
  REFERENCE                      = "K-2114"
  PARK-NAME                      = NIL
  SPOT-TIME                      = "2023-07-13T19:56:19"
  SPOTTER                        = "K2PO/7-#"
  COMMENTS                       = "RBN 4 dB 22 WPM via K2PO/7-#"
  SOURCE                         = "RBN"
  INVALID                        = NIL
  NAME                           = "Nissequogue River State Park"
  LOCATION                       = "US-NY"
  GRID-4                         = "FN30"
  GRID-6                         = "FN30jv"
  LAT                            = 40.9006
  LON                            = -73.2309
  SPOT-COUNT                     = 13
  EXPIRE                         = 1705
; No value
POTA> 
```

There are various functions and methods to view and manipulate this data. For example, if you specify your location, functions like pp will show you the distance and bearing to the spot:

```
POTA> (pp (nth 3 (bt:with-lock-held (*spots-lock*) *spots*)))
Activator: N2AKJ (14061.0/CW) @ 2023-07-13T19:56:19
Park: K-2114/US-NY/Nissequogue River State Park
Where: 1654.4 mi @ 76.1 deg FN30jv 40.9006/-73.2309

NIL
POTA> 
```

(dist ...) and (direction ...) are methods you can use yourself:

```
POTA> (dist (nth 3 (bt:with-lock-held (*spots-lock*) *spots*)))
1654.4366978084647d0
POTA> (direction (nth 3 (bt:with-lock-held (*spots-lock*) *spots*)))
76.1314063565838d0
POTA> 
```

(filter-location ...) will only show you spots where the location field matches what you supply. and (dist-sort ...) will sort the returned list in ascending order of distance from your location:

```
POTA> (filter-location "US-CO" (bt:with-lock-held (*spots-lock*) *spots*)
(#<POTA-SPOT {101FCE86B3}> #<POTA-SPOT {101FCE8EF3}> #<POTA-SPOT {101FCE98B3}>)
POTA> (mapcar #'pp (filter-location "US-CO" *spots*))
Activator: WW8L (14075/FT8) @ 2023-07-13T19:43:19
Park: K-1210/US-CO/Boyd Lake State Park
Where: 76.8 mi @ 345.2 deg DN70lk 40.4298/-105.045

Activator: K5SJC (10117.9/CW) @ 2023-07-13T19:54:39
Park: K-4404/US-CO/Pike National Forest
Where: 82.0 mi @ 236.9 deg DM78aq 38.7005/-105.948

Activator: K2JH (14326/SSB) @ 2023-07-13T19:44:43
Park: K-0226/US-CO/Rocky Mountain Arsenal National Wildlife Refuge
Where: 34.4 mi @ 345.3 deg DM79nu 39.8367/-104.837

(NIL NIL NIL)
POTA> (mapcar #'pp (dist-sort (filter-location "US-CO" (bt:with-lock-held (*spots-lock*) *spots*)))
Activator: K2JH (14326/SSB) @ 2023-07-13T19:44:43
Park: K-0226/US-CO/Rocky Mountain Arsenal National Wildlife Refuge
Where: 34.4 mi @ 345.3 deg DM79nu 39.8367/-104.837

Activator: WW8L (14075/FT8) @ 2023-07-13T19:43:19
Park: K-1210/US-CO/Boyd Lake State Park
Where: 76.8 mi @ 345.2 deg DN70lk 40.4298/-105.045

Activator: K5SJC (10117.9/CW) @ 2023-07-13T19:54:39
Park: K-4404/US-CO/Pike National Forest
Where: 82.0 mi @ 236.9 deg DM78aq 38.7005/-105.948

(NIL NIL NIL)
POTA> 
```

That's about all I've implemented so far, but it's a solid foundation to build from. I anticipate subsequent versions of the code to implement automatic periodic fetching of the data and other useful goodies.
