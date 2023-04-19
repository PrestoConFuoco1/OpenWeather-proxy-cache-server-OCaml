open Base

open Types

module GeoDistance = struct

let pi = 4.0 *. Float.atan 1.0 (* Define pi *)

let earth_radius = 6371.0 (* Define Earth's radius in km *)

let to_radians deg = deg /. 180.0 *. pi (* Convert degrees to radians *)

let haversine ({lat=lat1; lon=lon1} : latlong) ({lat=lat2; lon=lon2} : latlong) =
  let open Float in
  let d_lat = to_radians (lat2 -. lat1)
  and d_lon = to_radians (lon2 -. lon1) in
  let a = (sin (d_lat /. 2.0)) ** 2.0 +. cos (to_radians lat1) *. cos (to_radians lat2) *. (sin (d_lon /. 2.0)) ** 2.0 in
  let c = 2.0 *. atan2 (sqrt a) (sqrt (1.0 -. a)) in
  earth_radius *. c (* Calculate distance *)

end



type timeseries = (int * current) List.t
    [@@deriving repr]
  type place_info =
    { latlong : latlong
    ; timezone : string
    ; timezone_offset : int
    }
    [@@deriving repr]
  type entry =
    { place_info : place_info
    ; time : int
    ; entry : current
    }
  type t =
    { time_accuracy_sec : int
    ; distance_accuracy_km : int
    ; cache : (place_info * timeseries) list
    } [@@deriving repr]

  type print_cache = (latlong * int list) list
    [@@deriving repr]

  let to_print_cache t =
    List.map t.cache ~f:(fun (p, tsr) -> (p.latlong, List.map tsr ~f:fst))

  let create ~time_accuracy_sec ~distance_accuracy_km =
    { time_accuracy_sec; distance_accuracy_km; cache = [] }

let opt_min_by lst ~compare =
  if List.is_empty lst
  then None
  else List.min_elt lst ~compare

let find_by_minimal_proximity lst compare_proximity max_proximity get_abs_proximity =
  let matching_units = List.filter_map lst ~f:(fun x ->
    let prox = get_abs_proximity x in
    if compare_proximity prox max_proximity <= 0
    then Some (x, prox)
    else None) in
  Option.map ~f:fst (opt_min_by matching_units
    ~compare:(fun u1 u2 -> compare_proximity (snd u1) (snd u2)))

  let find searched_l searched_time t =
    let open Option.Let_syntax in
    let%bind place = find_by_minimal_proximity t.cache
      Float.compare
      (Float.of_int t.distance_accuracy_km)
      (fun (l, _tsr) -> GeoDistance.haversine searched_l l.latlong) in
    let%bind time_entry = find_by_minimal_proximity (snd place)
      Int.compare
      t.time_accuracy_sec
      (fun (dt, _te) -> abs (dt - searched_time)) in
    return
      { place_info = fst place
      ; time = fst time_entry
      ; entry = snd time_entry
      } 

  let add_time_entry {place_info; time; entry} t =
    let upd_ref = ref 0 in
    let new_t = List.map t.cache ~f:(fun ((l, tsr) as elt) ->
      if Float.(GeoDistance.haversine l.latlong place_info.latlong <= Float.of_int t.distance_accuracy_km)
      then (Int.incr upd_ref; (l, List.Assoc.add tsr ~equal:(Int.equal) time entry))
      else elt) in
    if !upd_ref = 0
    then { t with cache = (place_info, List.return (time, entry)) :: t.cache }
    else { t with cache = new_t }

let res_of_entry e : response =
  let p = e.place_info in
  { lat = p.latlong.lat
  ; lon = p.latlong.lon
  ; timezone = p.timezone
  ; timezone_offset = p.timezone_offset
  ; current = e.entry
  }

let entry_of_res res : entry =
  { place_info =
    { latlong = { lat = res.lat; lon = res.lon }
    ; timezone = res.timezone
    ; timezone_offset = res.timezone_offset
    }
  ; time = res.current.dt
  ; entry = res.current
  }


