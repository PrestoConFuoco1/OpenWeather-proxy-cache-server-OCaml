type entry

type t

type print_cache = (Types.latlong * int list) list [@@deriving repr]

val to_print_cache : t -> print_cache

val find : Types.latlong -> int -> t -> entry option

val res_of_entry : entry -> Types.response

val entry_of_res : Types.response -> entry

val add_time_entry : entry -> t -> t

val create : time_accuracy_sec:int -> distance_accuracy_km:int -> t
