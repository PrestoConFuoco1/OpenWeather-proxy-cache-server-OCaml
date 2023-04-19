open Sexplib.Conv
open Types

type config =
  { port : int
  ; locations : location_identifier list
  ; update_period_sec : int option [@sexp.option]
  ; time_accuracy_sec : int option [@sexp.option]
  ; distance_accuracy_km : int option [@sexp.option]
  } [@@deriving repr, sexp]

let rzhev_lat = 56.262955
let rzhev_lon = 34.334267
let rzhev = {lat = rzhev_lat; lon = rzhev_lon}
let moscow_lat = 55.7522
let moscow_lon = 37.6156
let moscow = {lat = moscow_lat; lon = moscow_lon}

let default_locations =
  [ Coords rzhev
  ; Coords moscow
  ]

let default_config =
  { port = 9120
  ; locations = default_locations
  ; update_period_sec = Some 11
  ; time_accuracy_sec = Some 9
  ; distance_accuracy_km = Some 20
  }

let load_config opt_filename =
  let filename = Option.value opt_filename ~default:"config" in
  Sexplib.Sexp.load_sexp_conv_exn filename config_of_sexp
