let just_print x_t x = Fmt.pf Fmt.stdout "%a" (Repr.pp_dump x_t) x

let just_print_lwt x_t x = Lwt_fmt.printf "%a\n%!" (Repr.pp_dump x_t) x
