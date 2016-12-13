open Lwt
open Type_info

(* This module is nothing more
 * than a utility module for communicating 
 * with the server. All it does is takes the
 * necessary information for a request, sends
 * out the request, and returns the useful 
 * contents of the deferred response  *)

exception ServerError of string
exception ClientError

module type RequesterMaker =
 functor (Cl : Chat_client.Client ) -> Requester.Req

module MakeRequester (Cl : Chat_client.Client) = struct

  let info = ref None

  (*[send_req req] is what the api uses to send
   * requests. It has to be crafted delicately so
   * that it doesn't continuously perform Cl.init.
   * Instead, observe the closure that is created
   * in order to recycle the value of the one call
   * to Cl.init. If the program arguments are malformatted
   * the program exits here. *)
  let send_req =  
    try
      let open Client_args in
      let {host = h; port = p} = Client_args.obj () in
      Lwt.async (fun () -> 
        let s = "connecting to " ^ h ^ " at port " ^ p in
        Lwt_io.write_line Lwt_io.stdout s); 
      let sender = Cl.init h (int_of_string p) in
      (fun req_cont -> 
        sender >>= fun s -> 
        let req = (!info, req_cont) in
        s req)
    with
    | Failure s -> print_endline s; exit 1

  (* A wrapper for response handling that raises a
   * vague error if the request was unsuccessful. In
   * general, this should only be used when there is
   * no known reason that the request should fail.
   *)
  let handle_response f (cont, succ) =
    match succ with
    | Success -> f cont
    | Fail s -> raise (ServerError s)

  let register id pswd = 
    let req = Register (id,pswd) in 
    send_req req >|= snd

  let auth identifier pswd =
    let req = Auth (identifier, pswd) in
    send_req req >|= function
    | (Sessionkey k, Success) ->
        info := Some (identifier, k);
        Success
    | (_, Success) -> raise ClientError
    | (_, Fail s) -> Fail s

  let see_chatrooms () =
    let req = Listrooms in
    let f = function
      | Chatrooms clst -> clst | _ -> raise ClientError in
    send_req req >|= (handle_response f)

  let see_users () =
    let f = function
      | Users lst -> lst | _ -> raise ClientError in
    send_req Listusers >|= (handle_response f)
 
  let get_room crname =
    let req = Getroom crname in
    let f = function
    | (Chatroom cr, Success) -> (cr, Success)
    | (_, Success) -> raise ClientError
    | (_,Fail s) -> ({participants = []; name = ""}, Fail s) in
    send_req req >|= f
   
  let see_messages last cr = 
    let f = function
      | Messages lst -> lst | _ -> raise ClientError in
    send_req (Listmessages (last, cr)) >|= 
    (handle_response f)

  let block_user target =
    let req = Block target in
    send_req req >|= snd

  let unblock_user target =
    let req = Unblock target in
    send_req req >|= snd

  let send_message cr content =
    let stub = {
      user = ""; 
      room = cr; 
      message = content; 
      timestamp = Unix.time ();
    } in
    match !info with
    | Some (identifier, _) ->
      let cont = {stub with user = identifier} in
      let req = Message cont in
      send_req req >|= fun resp ->
      (cont, snd resp)
    | None -> 
      (stub, Type_info.Fail "Login first!") |> return

  let new_room members crname =
    let req = Newroom {
      name = crname;
      participants = members;
    } in
    send_req req >|= snd
  
  let new_game members gname = 
    let req = Newgame {
      name = gname ;
      players = members }
    in send_req req >|= snd 
  
  let see_games () = 
    let req = Listgames in
    let f = function
    | Gamerooms glst -> glst 
    | _ -> raise ClientError 
    in send_req req >|= (handle_response f)

  let get_game grname =
    let req = Getgame grname in
    let f = function
      | (Gamestate (gr,st), Success) -> ((gr,st), Success) 
      | (_, Success) -> raise ClientError
      | (_, Fail s) -> (({name="";players=[]},[]),Fail s)
    in 
    send_req req >|= f

  let fill_board gr sq_num = 
    let req = Changegamest (gr, sq_num) in 
    send_req req >|= snd 

  let reset_board gr = 
    let req = Resetgame gr in 
    send_req req >|= snd 

  let getwl () = 
    let req = Getwl in 
    let f = function 
    | Wl (w,l) -> (w,l) 
    | _ -> raise ClientError 
    in send_req req >|= (handle_response f)   

  let add_user_to_room target crname =
    let req = AddToRoom (target, crname) in
    send_req req >|= snd

  let leave_room crname =
    let req = LeaveRoom crname in
    send_req req >|= snd

end

