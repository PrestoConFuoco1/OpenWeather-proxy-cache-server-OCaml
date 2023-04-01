open Cache
open Sexplib.Conv

type config =
  { port : int
  ; locations : location_identifier list [@sexp.list]
  ; update_period_sec : int option [@sexp.option]
  ; time_accuracy_sec : int option [@sexp.option]
  ; distance_accuracy_km : int option [@sexp.option]
  } [@@deriving repr, sexp]

let rzhev_lat = 56.262955
let rzhev_lon = 34.334267
let moscow_lat = 55.7522
let moscow_lon = 37.6156

let default_locations =
  [ Coords {lat = rzhev_lat; lon = rzhev_lon}
  ; Coords {lat = moscow_lat; lon = moscow_lon}
  ]

let default_config =
  { port = 9120
  ; locations = default_locations
  ; update_period_sec = Some 11
  ; time_accuracy_sec = Some 9
  ; distance_accuracy_km = Some 20
  }
