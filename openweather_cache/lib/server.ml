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
    Printf.printf "before lat";
    let lat = Request.query_exn "lat" req |> Float.of_string in
    Printf.printf "before lon";
    let lon = Request.query_exn "lon" req |> Float.of_string in
    Coords { lat; lon }) in
  asum_option [zipcode; name; coords]

let identifier_to_latlong = function
  | Coords latlong -> Lwt.return latlong
  | _ -> failwith "not implemented"

let usage = "Usage: ?lat=FLOAT&lon=FLOAT or ?name=STRING or ?zipcode=STRING"

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
    let%lwt () = Lwt_fmt.printf "thread %d\n" idx in
    just_print_lwt response_t res) in
  let%lwt () = Lwt_unix.sleep 20. in
  cache_filler_thread idx cfg api_key location ()

let api_key = Sys.getenv "OW_APIKEY"

let load_config (opt_filename : string option) : Config.config =
  let filename = Option.value opt_filename ~default:"config" in
  load_sexp_conv_exn filename Config.config_of_sexp

let run () =
  let config = load_config None in
  (*
  let print_exception e = Printf.eprintf "%s\n" @@ Printexc.to_string_default e in
  let locations = Lwt_main.run (Lwt_list.map_p identifier_to_latlong config.locations) in
  let () = List.iteri (fun idx loc -> Lwt.dont_wait (cache_filler_thread idx config api_key loc) print_exception) locations in *)
(*   let () = Lwt.dont_wait (cache_filler_thread config api_key locations) print_exception in *)
  App.empty
(*   |> App.port 9120 *)
  |> App.port config.port
  |> App.get "/weather" (handler ~api_key)
  |> App.run_command
