open Base
open Opium
open Cache
open Sexplib.Conv
open Sexplib.Sexp

let rec asum_option opts =
  match opts with
  | [] -> None
  | o :: os ->
      match o with
      | None -> asum_option os
      | Some _ as res -> res

let get_location_identifier (req : Request.t) =
  Fmt.pf Fmt.stdout "%a" Request.pp_hum req;
  let open Router in
  let zipcode = Base.Option.try_with (fun () ->
    let zip = Request.query_exn "zipcode" req in
    Zipcode zip) in
  let name = Base.Option.try_with (fun () -> 
    let place_name = Request.query_exn "name" req in
    Name place_name) in
  let coords = Base.Option.try_with (fun () ->
    Stdio.printf "before lat";
    let lat = Request.query_exn "lat" req |> Float.of_string in
    Stdio.printf "before lon";
    let lon = Request.query_exn "lon" req |> Float.of_string in
    Stdio.printf "after lon";
    Coords { lat; lon }) in
  asum_option [zipcode; name; coords]

let identifier_to_latlong = function
  | Coords latlong -> Lwt.return latlong
  | _ -> failwith "not implemented"

let usage = "Usage: ?lat=FLOAT&lon=FLOAT or ?name=STRING or ?zipcode=STRING"

(*
module TimeSeries = struct
  module T = struct
    type t = current
    let compare (x : current) (y : current) = Int.compare x.dt y.dt
  end
  include T
  include Interval_map.Make(T)
end
*)

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

let time_accuracy = 1
let distance_accuracy = 1.

module Cache = struct
  type timeseries = (int * current) List.t
  type t = (latlong * timeseries) List.t
  let find searched_l searched_time t =
    let open Option.Let_syntax in
    let%bind place = find_by_minimal_proximity t Float.compare distance_accuracy
      (fun (l, _tsr) -> haversine searched_l l) in
    let%bind time_entry = find_by_minimal_proximity (snd place) Int.compare time_accuracy
      (fun (dt, _te) -> abs (dt - searched_time)) in
    return (fst place, fst time_entry, snd time_entry)

  let add_time_entry (ll, dt, current) t =
    let upd_ref = ref 0 in
    let new_t = List.map t ~f:(fun ((l, tsr) as elt) ->
      if Float.(haversine l ll <= distance_accuracy)
      then (Int.incr upd_ref; (l, tsr))
      else elt) in
    if !upd_ref = 0
    then (ll, List.return (dt, current)) :: t
    else new_t

end

let handler ~api_key req : Response.t Lwt.t =
  let opt_location_identifier = get_location_identifier req in
  match opt_location_identifier with
  | None -> Lwt.return (yojson_of_string usage |> Response.of_json)
  | Some location_identifier ->
    let%lwt latlong = identifier_to_latlong location_identifier in
    let%lwt res = call_current_weather_by_coords ~api_key latlong in
    Lwt.return (yojson_of_response res |> Response.of_json)

let io_mutex = Lwt_mutex.create ()





let rec cache_filler_thread idx cfg api_key location () =
  let%lwt res = call_current_weather_by_coords ~api_key location in
  let%lwt () = Lwt_mutex.with_lock io_mutex (fun () ->
    let%lwt () = Lwt_fmt.printf "thread %d\n%!" idx in
    just_print_lwt response_t res) in
  let%lwt () = Lwt_unix.sleep 20. in
  cache_filler_thread idx cfg api_key location ()

let api_key = Sys.getenv_exn "OW_APIKEY"

let load_config (opt_filename : string option) : Config.config =
  let filename = Option.value opt_filename ~default:"config" in
  load_sexp_conv_exn filename Config.config_of_sexp

let run () =
  let config = load_config None in
  let print_exception e = Stdio.eprintf "%s\n" @@ Stdlib.Printexc.to_string_default e in
  let locations = Lwt_main.run (Lwt_list.map_p identifier_to_latlong config.locations) in
  let () = List.iteri locations ~f:(fun idx loc -> Lwt.dont_wait (cache_filler_thread idx config api_key loc) print_exception) in
  App.empty
  |> App.port config.port
  |> App.get "/weather" (handler ~api_key)
  |> App.run_command
