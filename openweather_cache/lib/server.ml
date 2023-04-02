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
(*   Fmt.pf Fmt.stdout "%a" Request.pp_hum req; *)
  let open Router in
  let zipcode = Base.Option.try_with (fun () ->
    let zip = Request.query_exn "zipcode" req in
    Zipcode zip) in
  let name = Base.Option.try_with (fun () -> 
    let place_name = Request.query_exn "name" req in
    Name place_name) in
  let coords = Base.Option.try_with (fun () ->
    let lat = Request.query_exn "lat" req |> Float.of_string in
    let lon = Request.query_exn "lon" req |> Float.of_string in
    Coords { lat; lon }) in
  asum_option [zipcode; name; coords]

let get_time (req : Request.t) =
  let open Router in
  let opt_dt_str = Request.query "dt" req in
  match opt_dt_str with
  | None ->
    let time = Unix.time () |> Int.of_float |> Option.some in
    Stdio.printf "current time: %d\n%!" (Option.value_exn time);
    time
  | Some dt_str -> Option.try_with (fun () -> Int.of_string dt_str)

let get_params (req : Request.t) =
  Option.both (get_location_identifier req) (get_time req)

let identifier_to_latlong = function
  | Coords latlong -> Lwt.return latlong
  | _ -> failwith "not implemented"

let usage = "Usage: ?lat=FLOAT&lon=FLOAT or ?name=STRING or ?zipcode=STRING; optional dt=INT (default: now)"

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

module Cache1 = struct
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
    ; cache : (place_info * timeseries) List.t
    } [@@deriving repr]
  let create ~time_accuracy_sec ~distance_accuracy_km =
    { time_accuracy_sec; distance_accuracy_km; cache = [] }

  let find searched_l searched_time t =
    let open Option.Let_syntax in
    let%bind place = find_by_minimal_proximity t.cache
      Float.compare
      (Float.of_int t.distance_accuracy_km)
      (fun (l, _tsr) -> haversine searched_l l.latlong) in
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
      if Float.(haversine l.latlong place_info.latlong <= Float.of_int t.distance_accuracy_km)
      then (Int.incr upd_ref; (l, (time, entry) :: tsr))
      else elt) in
    if !upd_ref = 0
    then { t with cache = (place_info, List.return (time, entry)) :: t.cache }
    else { t with cache = new_t }

end

let res_of_entry e : response =
  let p = e.Cache1.place_info in
  { lat = p.latlong.lat
  ; lon = p.latlong.lon
  ; timezone = p.timezone
  ; timezone_offset = p.timezone_offset
  ; current = e.entry
  }

let entry_of_res res : Cache1.entry =
  { place_info =
    { latlong = { lat = res.lat; lon = res.lon }
    ; timezone = res.timezone
    ; timezone_offset = res.timezone_offset
    }
  ; time = res.current.dt
  ; entry = res.current
  }

let io_mutex = Lwt_mutex.create ()

let handler ~api_key ~cache_ref req : Response.t Lwt.t =
  let opt_location_identifier_time = get_params req in
  match opt_location_identifier_time with
  | None -> Lwt.return (yojson_of_string usage |> Response.of_json)
  | Some (location_identifier, time) ->
    let%lwt latlong = identifier_to_latlong location_identifier in
    let opt_cached = Cache1.find latlong time !cache_ref in
    let%lwt res = match opt_cached with
    | None ->
      let%lwt () = Lwt_fmt.printf "No entry found in cache, calling openweather API\n%!" in
      let%lwt res = call_current_weather_by_coords ~api_key latlong in
      let entry = entry_of_res res in
      let%lwt () = Lwt_mutex.with_lock io_mutex (fun () ->
        cache_ref := Cache1.add_time_entry entry !cache_ref;
        let%lwt () = just_print_lwt Cache1.t !cache_ref in
        Lwt.return ()) in
      Lwt.return res
    | Some entry ->
      let%lwt () = Lwt_fmt.printf "Using cache\n%!" in
      Lwt.return (res_of_entry entry) in
    Lwt.return (yojson_of_response res |> Response.of_json)

let rec cache_filler_thread idx cfg api_key location cache_ref () =
  let%lwt res = call_current_weather_by_coords ~api_key location in
  let%lwt () = Lwt_mutex.with_lock io_mutex (fun () ->
    let%lwt () = Lwt_fmt.printf "thread %d\n%!" idx in
    let%lwt () = just_print_lwt response_t res in
    let time_entry = entry_of_res res in
    cache_ref := Cache1.add_time_entry time_entry !cache_ref;
    let%lwt () = just_print_lwt Cache1.t !cache_ref in
    Lwt.return ()) in
  let%lwt () = Lwt_unix.sleep (Float.of_int (Option.value_exn cfg.Config.update_period_sec)) in
  cache_filler_thread idx cfg api_key location cache_ref ()

let api_key = Sys.getenv_exn "OW_APIKEY"

let load_config (opt_filename : string option) : Config.config =
  let filename = Option.value opt_filename ~default:"config" in
  load_sexp_conv_exn filename Config.config_of_sexp

let setup_logs =
  let level = Some Logs.Debug in
  Fmt_tty.setup_std_outputs ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let run () =
  let config = load_config None in
  let cache_ref = ref @@ Cache1.create
    ~time_accuracy_sec:(Option.value_exn config.time_accuracy_sec)
    ~distance_accuracy_km:(Option.value_exn config.distance_accuracy_km) in
  let print_exception e = Stdio.eprintf "%s\n" @@ Stdlib.Printexc.to_string_default e in
  let locations = Lwt_main.run (Lwt_list.map_p identifier_to_latlong config.locations) in
(*   let () = List.iteri locations ~f:(fun idx loc -> Lwt.dont_wait (cache_filler_thread idx config api_key loc cache_ref) print_exception) in *)
  App.empty
  |> App.port config.port
  |> App.get "/weather" (handler ~cache_ref ~api_key)
  |> App.run_command
