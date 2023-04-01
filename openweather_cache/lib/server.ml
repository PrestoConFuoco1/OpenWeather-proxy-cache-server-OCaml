open Opium
open Cache



type location_identifier =
  | Zipcode of string
  | Name of string
  | Coords of latlong
    [@@deriving repr]

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
  let coords = (try
    Printf.printf "before lat";
    let lat = Request.query_exn "lat" req |> Float.of_string in
    Printf.printf "before lon";
    let lon = Request.query_exn "lon" req |> Float.of_string in
    Some (Coords { lat; lon })
  with | ex -> print_endline (Printexc.to_string_default ex); None) in
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

let api_key = Sys.getenv "OW_APIKEY"

let run () =
  App.empty
  |> App.port 9120
  |> App.get "/weather" (handler ~api_key)
  |> App.run_command
