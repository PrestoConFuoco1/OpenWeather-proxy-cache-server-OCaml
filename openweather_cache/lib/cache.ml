open Sexplib.Conv

(* [@@@ocaml.warning "-26-22-32-33-27"] *)

open Cohttp
open Cohttp_lwt_unix
open Uri

(* let print_sexp s = Format.printf "%a@." Sexplib.Sexp.pp_hum s *)

let api_key = "08a9fc08a909b0f3de55b60ce736fbfa"
let rzhev_lat = 56.262955
let rzhev_lon = 34.334267

(* Types *)
type latlong =
  { lat : float
  ; lon : float
  } [@@deriving repr, sexp]

type current =
  { dt : int
  ; temp : float
  ; pressure : float
  ; humidity : float
  } [@@deriving yojson, sexp, repr]
    [@@yojson.allow_extra_fields]

let q = "wrer"

type response =
  { lat : float
  ; lon : float
  ; timezone : string
  ; timezone_offset : int
  ; current : current
  } [@@deriving yojson, sexp, repr]
    [@@yojson.allow_extra_fields]

type location_identifier =
  | Zipcode of string
  | Name of string
  | Coords of latlong
    [@@deriving repr, sexp]

(* Call *)
let response_of_json_text resp =
  response_of_yojson @@ Yojson.Safe.from_string resp

let base_uri = Uri.of_string "https://api.openweathermap.org/data/3.0/onecall"

let call_current_weather_by_coords ~api_key ({lat; lon} : latlong) =
  let uri_with_query_params =
    let base_uri = Uri.of_string "https://api.openweathermap.org/data/3.0/onecall" in
    let query_params =
      [ ("lat", Float.to_string lat)
      ; ("lon", Float.to_string lon)
      ; ("appid", api_key)
      ; ("exclude", "hourly,daily,minutely,alerts")
      ] in
    List.fold_left (fun uri (k, v) -> add_query_param uri (k, [v])) base_uri query_params in
  let%lwt (_resp, body) = Client.get uri_with_query_params in
  let%lwt body = Cohttp_lwt.Body.to_string body in
(*   Printf.printf "%s\n" body; *)
(*   let json_body = Yojson.Safe.from_string body in *)
(*   (Yojson.Safe.pretty_to_channel Stdio.stdout json_body); *)
  let decoded_response = response_of_json_text body in
(*   print_sexp ([%sexp_of : response] decoded_response); *)
  Lwt.return decoded_response

let just_print x_t x = Fmt.pf Fmt.stdout "%a" (Repr.pp_dump x_t) x
let just_print_lwt x_t x = Lwt_fmt.printf "%a" (Repr.pp_dump x_t) x

module For_testing = struct

let example_current =
  {|
  {
        "dt": 1679951592,
        "sunrise": 1679973887,
        "sunset": 1680019846,
        "temp": 273.23,
        "feels_like": 270.85,
        "pressure": 1006,
        "humidity": 95,
        "dew_point": 272.6,
        "uvi": 0,
        "clouds": 53,
        "visibility": 10000,
        "wind_speed": 1.95,
        "wind_deg": 304,
        "wind_gust": 1.96,
        "weather": [
            {
                "id": 803,
                "main": "Clouds",
                "description": "broken clouds",
                "icon": "04n"
            }
        ]
  }
  |}

let example_response = {|
{
    "lat": 56.263,
    "lon": 34.3343,
    "timezone": "Europe/Moscow",
    "timezone_offset": 10800,
    "current": {
        "dt": 1679951592,
        "sunrise": 1679973887,
        "sunset": 1680019846,
        "temp": 273.23,
        "feels_like": 270.85,
        "pressure": 1006,
        "humidity": 95,
        "dew_point": 272.6,
        "uvi": 0,
        "clouds": 53,
        "visibility": 10000,
        "wind_speed": 1.95,
        "wind_deg": 304,
        "wind_gust": 1.96,
        "weather": [
            {
                "id": 803,
                "main": "Clouds",
                "description": "broken clouds",
                "icon": "04n"
            }
        ]
    }
}
|}

let current_mine = 
  { dt = 3
  ; temp = 300.
  ; pressure = 1000.
  ; humidity = 50.
  }

let%expect_test "2" =
   let current_raw = Yojson.Safe.to_string (yojson_of_current current_mine) in
  Stdio.printf "%s" current_raw;
  [%expect {| {"dt":3,"temp":300.0,"pressure":1000.0,"humidity":50.0} |}]

let%expect_test "3" =
  let example_json = Yojson.Safe.from_string example_current in
  let current = current_of_yojson example_json in
  Fmt.pf Fmt.stdout "%a" (Repr.pp_dump current_t) current;
  [%expect {|
    { dt = 1679951592;
      temp = 273.23;
      pressure = 1006.;
      humidity = 95. } |}]

let%expect_test "4" =
  let example_json = Yojson.Safe.from_string example_response in
  let response = response_of_yojson example_json in
  just_print response_t response;
  Fmt.pf Fmt.stdout "%a" (Repr.pp_dump response_t) response;
  [%expect {|
    { lat = 56.263;
      lon = 34.3343;
      timezone = "Europe/Moscow";
      timezone_offset = 10800;
      current = { dt = 1679951592;
                  temp = 273.23;
                  pressure = 1006.;
                  humidity = 95. } } |}]

end
