open Base

open Opium
open Types

module OWParams = struct
let rec asum_option opts =
  match opts with
  | [] -> None
  | o :: os ->
      match o with
      | None -> asum_option os
      | Some _ as res -> res

let get_location_identifier (req : Request.t) =
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
  let opt_dt_str = Request.query "dt" req in
  match opt_dt_str with
  | None ->
    Unix.time () |> Int.of_float |> Option.some
  | Some dt_str -> Option.try_with (fun () -> Int.of_string dt_str)

let get_params (req : Request.t) =
  Option.both (get_location_identifier req) (get_time req)

let usage = "Usage: ?lat=FLOAT&lon=FLOAT or ?name=STRING or ?zipcode=STRING; optional dt=INT (default: now)"
end

module PlaceResolver = struct
  let identifier_to_latlong = function
    | Coords latlong -> Lwt.return latlong
    (* TODO: use some Openweather APIs to resolve locations *)
    | _ -> failwith "not implemented"
end

module SharedCache : sig

  type cache_ref
  val create : Cache.t -> cache_ref
  val get_cache : cache_ref -> Cache.t
  val with_cache : cache_ref -> (Cache.t -> Cache.t Lwt.t) -> unit Lwt.t

  end = struct
  type cache_ref = Cache.t ref
  
  let create = ref
  let get_cache cache_ref = !cache_ref

  let cache_mutex = Lwt_mutex.create ()

  let with_cache (cache_ref : cache_ref) (action : Cache.t -> Cache.t Lwt.t) : unit Lwt.t =
    Lwt_mutex.with_lock cache_mutex (fun () ->
      let%lwt new_cache = action !cache_ref in
      cache_ref := new_cache;
      Lwt.return ())

end

module ThreadLogTags = struct
let thread_tag = Logs.Tag.def "tid" String.pp

let mk_thread_tags filler_id =
  Logs.Tag.(empty |> add thread_tag filler_id)

end

module Handler = struct

let main_tags = ThreadLogTags.mk_thread_tags "server"

let handler ~api_key ~(cache_ref : SharedCache.cache_ref) req : Response.t Lwt.t =
  let tags = main_tags in
  let opt_location_identifier_time = OWParams.get_params req in
  match opt_location_identifier_time with
  | None -> Lwt.return (yojson_of_string OWParams.usage |> Response.of_json)
  | Some (location_identifier, time) ->
    let%lwt latlong = PlaceResolver.identifier_to_latlong location_identifier in
    let opt_cached = Cache.find latlong time (SharedCache.get_cache cache_ref) in
    let%lwt res = match opt_cached with
    | None ->
      let%lwt () = Logs_lwt.info (fun m -> m "No entry found in cache, calling openweather API" ~tags) in
      let%lwt res = call_current_weather_by_coords ~api_key latlong in
      let entry = Cache.entry_of_res res in
      let%lwt () = SharedCache.with_cache cache_ref (fun old_cache ->
        let new_cache = Cache.add_time_entry entry old_cache in
        let%lwt () = Logs_lwt.debug (fun m -> m "Cache:\n%!%a" (Repr.pp_dump Cache.print_cache_t) (Cache.to_print_cache new_cache) ~tags) in
        Lwt.return new_cache) in
      Lwt.return res
    | Some entry ->
      let%lwt () = Logs_lwt.info (fun m -> m "Using cache" ~tags) in
      Lwt.return (Cache.res_of_entry entry) in
    Lwt.return (yojson_of_response res |> Response.of_json)

end

module CacheFiller = struct
  (** Filler environment. *)
  type env =
    { idx : int (** A filler id. *)
    ; cfg : Config.config (** Full config. *)
    ; api_key : string (** Openweather api key *)
    ; location : latlong (** Coordinates of location which this filler is responsible of *)
    ; cache_ref : SharedCache.cache_ref (** Shared cache with weather data *)
    }

  let run_filler_thread env =
    let tags = ThreadLogTags.mk_thread_tags ("thread-" ^ Int.to_string env.idx) in

    let log_exception e = Logs.err (fun m -> m "%s\n" @@ Stdlib.Printexc.to_string_default e) in

    let rec cache_filler_thread env () =
      let%lwt res = call_current_weather_by_coords ~api_key:env.api_key env.location in
      let%lwt () = SharedCache.with_cache env.cache_ref (fun old_cache ->
        let%lwt () = Logs_lwt.debug (fun m -> m "Response:\n%!%a" (Repr.pp_dump response_t) res ~tags) in
        let time_entry = Cache.entry_of_res res in
        let new_cache = Cache.add_time_entry time_entry old_cache in
        let%lwt () = Logs_lwt.debug (fun m -> m "Cache:\n%!%a" (Repr.pp_dump Cache.print_cache_t) (Cache.to_print_cache new_cache) ~tags) in
        Lwt.return new_cache
        ) in
      let%lwt () = Lwt_unix.sleep (Float.of_int (Option.value_exn env.cfg.Config.update_period_sec)) in
      cache_filler_thread env () in
    Lwt.dont_wait (cache_filler_thread env) log_exception

end

module OWLog = struct
  let reporter () =
    let report _src level ~over k msgf =
      let k _ =
        over () ;
        k ()
      in
      let with_stamp h tags k fmt =
        let dt = Unix.time () |> Int.of_float in
        let thread_id : string = Option.value ~default:"" (match tags with
        | None -> None
        | Some tags -> (Logs.Tag.find ThreadLogTags.thread_tag tags)) in
        Caml.(Fmt.kpf k Fmt.stdout ("%d %a [%s] @[" ^^ fmt ^^ "@]@.")
          dt
          Logs_fmt.pp_header (level, h)
          thread_id)
      in
      msgf @@ fun ?header ?tags fmt ->
        with_stamp header tags k fmt
    in
    {Logs.report = report}
  
  let setup_logs () =
    let level = Some Logs.Debug in
    Logs.set_level level;
    Logs.set_reporter (reporter ())

end

let run () =
  let api_key = Sys.getenv_exn "OW_APIKEY" in
  let config = Config.load_config None in
  OWLog.setup_logs ();
  let cache_ref = SharedCache.create @@ Cache.create
    ~time_accuracy_sec:(Option.value_exn config.time_accuracy_sec)
    ~distance_accuracy_km:(Option.value_exn config.distance_accuracy_km) in
  let locations = Lwt_main.run (Lwt_list.map_p PlaceResolver.identifier_to_latlong config.locations) in
  let filler_env idx loc = CacheFiller.{idx; cfg = config; api_key; location = loc; cache_ref} in
  let () = List.iteri locations ~f:(fun idx loc ->
    CacheFiller.run_filler_thread @@ filler_env idx loc) in

  App.empty
  |> App.port config.port
  |> App.get "/weather" (Handler.handler ~cache_ref ~api_key)
  |> App.run_command
