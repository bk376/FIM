open Unix
open Lwt
open Type_info

module type Client = sig
  (* [init host port] will ultimately yield a
   * function for making requests. Because of the
   * nature of the Unix library, the first deferred
   * evaluates when the connection is accepted, and the
   * second is evaluated at every subsequent response.
   * Thus, every call to init will open a new connection
   * with the server. What this means is that init should
   * be used with care*)
  val init : string -> int -> (request -> response Lwt.t) Lwt.t

end

(*[mut] is a mutex we will use
 * to make the result of the init
 * call thread safe *)
let mut = Lwt_mutex.create ()

(* [init server port] takes a string [server] and
 * an integer [port] and produces a function (request -> response Lwt.t) Lwt.t
 * which may be used to send requests to the server at port [port].
 * This function is thread safe. *)
module Cl = struct
  let init server port =
    let server_addr =
      try Unix.inet_addr_of_string server
      with Failure _  -> 
        try (Unix.gethostbyname server).Unix.h_addr_list.(0)
        with Not_found ->
          Printf.eprintf "%s : unknown server\n" server ;
          exit 2 in
    let sockaddr = Unix.ADDR_INET(server_addr, port) in
    (Lwt_io.open_connection sockaddr >|= fun (ic, oc) -> 
      (fun req ->
        Lwt_mutex.lock mut >>= fun () ->
        let req' = req |> req_to_string in
        Lwt_io.write_line oc req' >>= fun () ->
        Lwt_io.flush oc >>= fun () ->
        Lwt_io.read_line ic >|= resp_from_string >|= fun r ->
        Lwt_mutex.unlock mut; r))
end
