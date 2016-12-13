open Lwt
open Type_info

module MakeServer(H : Server_handler.Handler) = struct
  let send_response oc resp = 
    resp 
    |> Type_info.resp_to_string 
    |> Lwt_io.write_line oc


    (* [handle_connection ic oc] sets up a listening system for 
     * ic which will call handle_request on any lines which are
     * read *)
  let rec handle_connection ic oc () =
    Lwt_io.read_line_opt ic >>= function
    | Some ln -> 
        ln |> req_from_string |> H.handle_request |> 
        send_response oc >>= 
        handle_connection ic oc
    | None -> Lwt_log.info "Connection closed!" >>= return

  (* creates the ic and oc from the connection,
   * and passes them onto handle_connection. Also
   * handles failure *)
  let accept_connection conn =
    (* conn is (fd * sock_addr) pair *)
    let fd, _ = conn in
    let ic = Lwt_io.of_fd Lwt_io.Input fd in
    let oc = Lwt_io.of_fd Lwt_io.Output fd in
    Lwt.on_failure (handle_connection ic oc ()) (fun e -> Lwt_log.ign_error (Printexc.to_string e));
    Lwt_log.info "New connection" >>= return

  let backlog = 10

  (* creates a server which is continuously accepting connections
   * and handling them with accept_connection*)
  let create_server sock =
    let rec serve () =
      Lwt_unix.accept sock >>= accept_connection >>= serve
    in serve

  let get_my_addr () =
    let addr = 
      match Server_args.obj with
      | Web -> 
          (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0)
      | Local -> Unix.inet_addr_loopback in
    let s = Unix.string_of_inet_addr addr in
    Lwt.async ( fun () ->
      ("attempting to run on port 3110 of " ^ s)
      |> Lwt_io.write_line Lwt_io.stdout
    );
    addr

  let create_socket () =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    bind sock @@ ADDR_INET(get_my_addr (), 3110);
    listen sock backlog;
    sock

  let server =
    try 
      let soc = create_socket () in
      create_server soc
    with
    | Unix.Unix_error _ -> 
        (print_endline "port seems to be busy"; exit 1)

  let _ =
    try server () |> Lwt_main.run with
    | _ -> print_endline "unknown error"; exit 1
    
end

module S = MakeServer(Handler)

