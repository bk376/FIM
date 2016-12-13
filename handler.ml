open Lwt
open Type_info

(* [user_info] is the type of the state of
 * a given user*)
type user_info = {
  username: id; 
  mutable wl: (int*int);
  mutable blocked: string list;
  mutable key : int option;
  seed: int;
  hashed: int;
}


(* [game_info] maintains the state of a given
 * game*)
type game_info = {
  gr: gameroom ; 
  mutable board: square list ;
  mutable last_turn: id option 
}

(* [clients] is a hashtable mapping user ids to
 * user info. This means that user ids must be
 * unique*)
let (clients : (id, user_info) Hashtbl.t) = Hashtbl.create 100
(* [rooms] is a hashtable mapping room names to
 * the chatroom and its message history. This means
 * chatroom names must be unique*)
let (rooms : (string, chatroom * msg list) Hashtbl.t) = Hashtbl.create 100

(*Hashtable mapping names of games to their game_infos*)
let (games : (string, game_info) Hashtbl.t) = Hashtbl.create 100 


(***************************************************)
(****** handlers and helpers for handle_request ****)
(***************************************************)

(*[u |>? f] ensures that [u] is registered, if so, it
 * applies f to the clients entry for [u]. If not, it
 * returns a failure response. It was inspired
 * by the bind operator *)
let (|>?) u f =
  if Hashtbl.mem clients u then 
    Hashtbl.find clients u |> f
  else (Nothing, Fail (u ^ " is not registered"))

(*[r |>?? f] ensures that [r] is registered. If so, it
 * applies r to the rooms entry for [r]. If not, it
 * returns a failure response.*)
let (|>??) r f = 
  if Hashtbl.mem rooms r then 
    Hashtbl.find rooms r |> f
  else (Nothing, Fail ("the room " ^ r ^ " does not exist"))

(*makes sure the given user/password combination is valid*)
let proper_pass user pass =
  Hashtbl.seeded_hash user.seed pass = user.hashed

(*generates a new random seed*)
let _ = Random.self_init ()

(*handles registering a user with password [pswd] and gives
 *them a random authentication token*)
let handle_reg uname pswd = 
  if Hashtbl.mem clients uname then 
    (Nothing, Fail "That username has already been taken\n")
  else 
    let s = Random.int 100000000 in
    let h = Hashtbl.seeded_hash s pswd in
    let info = { 
        username = uname;
        wl = (0,0);
        blocked = [];
        key = None;
        seed = s;
        hashed = h;
      } in
    Hashtbl.add clients uname info;
    (Nothing, Success)

(*handles logging in and makes sure the username/password combination
 *is valid. Gives the user a random authentication token*)
let handle_auth u pswd = 
  let fail = (Nothing, Fail "Invalid username or password\n") in
  if Hashtbl.mem clients u then
    let usr = Hashtbl.find clients u in
    if proper_pass usr pswd then 
      let k = Random.int 10000000 in
      usr.key <- Some k;
      (Sessionkey k, Success)
    else fail
  else fail

(*[common_elem l1 l2] is l3 where x is in l3 iff
 * x is in l1 and x is in l2 *)
let common_elem lst1 lst2 =
  let mem x = List.mem x lst2 in
  lst1 |> List.filter mem

(* [check_room cr] is a list of tuples (x,y) where 
 * (x,y) is in the list iff y is blocked by x
 * Raises Not_found if a participant is not actually 
 * a registered user *)
let check_room cr =
  let fold lst member =
    let user = Hashtbl.find clients member in
    let blocked = 
      common_elem cr.participants user.blocked in
    let stringified = 
      blocked |> List.map 
      (fun x -> user.username ^ " has blocked " ^ x) in
    stringified @ lst in
  List.fold_left fold [] cr.participants

(*[check_game gr] is a list of tuples (x,y) where
 *(x,y) is in the list iff y is blocked by x
 *Raises Not_found if a participant is not a registered
 *user*)
let check_game gr = 
  let fold lst member =
    let user = Hashtbl.find clients member in
    let blocked = 
      common_elem gr.players user.blocked in
    let stringified = 
      blocked |> List.map 
      (fun x -> user.username ^ " has blocked " ^ x) in
    stringified @ lst in
  List.fold_left fold [] gr.players

(*[list_to_string lst] is a string which has the
 * elements of lst concatenated with a new line
 * character.*)
let list_to_string lst =
  let fold joined s = joined ^ "\n" ^ s in
  List.fold_left fold "" lst

(*[unpackaged_get_rooms i] is the list of all rooms
 * for which [i] is a participant *)
let unpackaged_get_rooms i =
  let fold _ (cr,_) lst =
    if List.mem i cr.participants then cr :: lst
    else lst in
  Hashtbl.fold fold rooms []

(*[get_rooms i] handles the get_rooms request. It
 * returns all the rooms that [i] is in *)
let get_rooms i =
  let lst = unpackaged_get_rooms i in
  (Chatrooms lst, Success)

(* [leave u t] goes through all the chatrooms
 * that u has in common with [t] and removes [u]
 * from them. Raises Not_found if [t] is not registered *)
let leave u target =
  let replace ({name = nm; participants = p} as cr) =
    let m_hst = Hashtbl.find rooms nm |> snd in
    let p' = p |> List.filter ((<>) u) in
    let rm' = ({ cr with participants = p'}, m_hst) in
    Hashtbl.replace rooms nm rm' in
  let rms = unpackaged_get_rooms target.username  in
  rms |> List.iter replace

(* [handle_block u t] removes [u] from all the
 * chat rooms [t] is in and adds [t] to [u]'s
 * block list *)
let handle_block user target =
  user |>? fun u_info ->
  target |>? fun t ->
  u_info.blocked <- target :: u_info.blocked;
  leave user t;
  (Nothing, Success)

(*[handle_unblock user target] removes [target] from [user]'s blocked
 * list*)
let handle_unblock user target =
  user |>? fun u_info ->
  target |>? fun _ ->
  let blocked' = u_info.blocked |> List.filter ((<>) target) in
  u_info.blocked <- blocked'; 
  (Nothing, Success)


(* [post_message msg] posts a message to the room
 * specified in [msg] provided that the sender id
 * has access to the room and the room exists. *)
let post_message msg = 
  msg.room.name |>?? fun (rm, msgs) ->
  if rm.participants |> (List.mem msg.user) then
    let msgs' = msg :: msgs in
    Hashtbl.replace rooms rm.name (rm, msgs');
    (Nothing, Success)
  else (Nothing, Fail "You don't have access to that room")

(* [post_room cr] creates a new chat room provided
 * that the chat room name does not exist already
 * and none of the members blocked each other *)
let post_room (cr : chatroom) = 
  if Hashtbl.mem rooms cr.name |> not then
    try 
      let blocked = check_room cr in
      if List.length blocked = 0 then
        let p' = cr.participants |> List.sort_uniq compare in
        let cr' = {cr with participants = p'} in
        (Hashtbl.add rooms cr.name (cr', []); 
        (Nothing, Success))
      else 
        let err_msg = blocked |> list_to_string in
        (Nothing, Fail err_msg)
    with
    | Not_found -> 
        (Nothing, Fail "one or more participants is not registered")
  else (Nothing, Fail "room name exists already")

(*[post_game gr] creates a new game room specified by [gr] as long
 *as the desired name is not already a game and both players are registered*)
let post_game gr = 
  if Hashtbl.mem games gr.name |> not then 
    try 
      let blocked = check_game gr in 
      if List.length blocked = 0 then 
        (Hashtbl.add games gr.name {gr=gr;board=[N;N;N;N;N;N;N;N;N];last_turn=None} ; 
        (Nothing, Success))
      else 
        let err_msg = blocked |> list_to_string in 
        (Nothing, Fail err_msg)
    with
    | Not_found -> (Nothing, Fail "one or more players is not registered")
  else (Nothing, Fail "game name exists already")

(*[get_games id] returns a list of all the games that the user 
 *specified by [id] is a part of*)
let get_games id = 
  let fold _ {gr=gr;board=_;last_turn=_} lst = 
    if List.mem id gr.players then gr :: lst
    else lst in 
  let lst = Hashtbl.fold fold games [] in 
  (Gamerooms lst, Success) 

(*returns the reversed list of all messages
 * more recent than m in mlst. Requires
 * that mlst be sorted such that most recent
 * messages are first *)
let shorten_messages m mlst =
  let rec get_lst lst acc m =
    match lst with
    | h :: t -> 
        if h = m then acc 
        else get_lst t (h :: acc) m
    | [] -> [] in
  match m,mlst with
  | (None, _::_) -> List.rev mlst
  | (Some x, _::_) -> get_lst mlst [] x
  | (_, []) -> []


(*returns sorted list of all messages since [last] in the chatroom*)
let get_messages uname last (cr : chatroom) =
  cr.name |>?? fun _ ->
  if List.mem uname cr.participants then
    let msgs = Hashtbl.find rooms cr.name |> snd in
    let msgs' = msgs |> shorten_messages last in
    (Messages msgs', Success)
  else (Nothing, Fail "you don't have access to that room")

(*gets the chatroom specified by [crname] as long as i
 *has access to it*)
let get_room i crname =
  crname |>?? fun _ ->
  let cr = Hashtbl.find rooms crname |> fst in
  if List.mem i cr.participants then
    (Chatroom cr, Success)
  else (Nothing, Fail "You don't have access to that room")

(*gets the list of all registered users*)
let get_users () =
  let fold i _ lst = i :: lst in
  let lst = Hashtbl.fold fold clients [] in
  (Users lst, Success)

(*[handle_get_game id grname] returns a Gamestate response with the 
 *gameroom and board of the desired game as long as user [id] is a 
 *valid player in the game and grname is a valid game*)
let handle_get_game id grname =
  if Hashtbl.mem games grname then 
    let g = Hashtbl.find games grname in 
    if List.mem id g.gr.players then 
      (Gamestate (g.gr,g.board), Success)
    else (Nothing, Fail "invalid id for this game")
  else (Nothing, Fail "invalid game")

(*[replace_nth lst n x] returns the square list lst with the nth
 *element replaced with x (starting from n=0). Cannot replace a
 *square that has already been played (i.e. occupied by X or O)
 *requires: 0 <= n < List.length lst*)
let rec replace_nth lst n x = match lst with 
  | h::t -> if n=0 then 
              if h=N then x::t else failwith "invalid move"
            else h::(replace_nth t (n-1) x)
  | [] -> failwith "failure in replace_nth"

(*[update_wl wid lid] updates the win loss ratios of the winner [wid]
 *and loser [lid]*)
let update_wl wid lid = 
  let wui = Hashtbl.find clients wid in 
  let lui = Hashtbl.find clients lid in 
  let w,l' = wui.wl in 
  let w',l = lui.wl in 
  wui.wl <- (w+1,l') ;
  lui.wl <- (w',l+1)

(*[get_wl id] returns a Wl response with the user [id]'s win-loss info
 *Sends a failure message if the user is invalid*)
let get_wl id =
  if Hashtbl.mem clients id
    then let (w,l) = (Hashtbl.find clients id).wl in  
    (Wl (w,l), Success)
  else (Nothing, Fail "Invalid user")  

(*[change_game_st id gr sq_num] updates the game board of [gr] at the square
 *specified by [sq_num] with either X or O depending on [id]. Also makes 
 *sure that it is [id]'s turn and that the move is a valid one (i.e. it
 *is a square on the board and the square is not already filled
 *requires: gr.players has length 2*)
let change_game_st id gr sq_num = 
  let st = (Hashtbl.find games gr.name).board in 
  let p1::p2::[] = gr.players in 
  (*assigns X or O to each player*)
  let xo = if id=p1 then X else if id=p2 then O else N in 
  if xo=N then (Nothing, Fail "Invalid player in game\n\n") 
  else try
    let lt = (Hashtbl.find games gr.name).last_turn in 
    (*checks it is [id]'s turn or if no moves have been made either player
     *can make the first move*)
    if Some id <> lt || lt = None then  
      let st' = replace_nth st sq_num xo in 
      (Hashtbl.replace games gr.name {gr=gr;board=st';last_turn=Some id} ;
      (if check_victory st' = `X then update_wl p1 p2   
      else if check_victory st' = `O then update_wl p2 p1 
      else ()) ;  
      (Nothing, Success)) 
    else (Nothing, Fail "It's not your turn\n\n")
  with Failure _ -> (Nothing, Fail "Invalid move\n\n")

(*[reset_game id gr] resets the game [gr] with an empty board and no last_turn*)
let reset_game id gr = 
  let empty = [N;N;N;N;N;N;N;N;N] in 
  if List.mem id gr.players then 
    (Hashtbl.replace games gr.name {gr=gr;board=empty;last_turn=None} ;
    (Nothing, Success))
  else (Nothing, Fail "Invalid player in game\n\n")

(*[add_to_room u t crname] handles user [u] trying to add user [t] to the
 *chatroom specified by [crname]*)
let add_to_room u t crname =
  crname |>?? fun (cr, messages) ->
  t |>? fun _ ->
  if cr.participants |> List.mem u then 
    if cr.participants |> List.mem t |> not then
      let cr' = {cr with participants = t :: cr.participants} in
      let conflicts = check_room cr' in
      if 0 = List.length conflicts then
        (Hashtbl.replace rooms crname (cr', messages);
        (Nothing, Success))
      else 
        let err_msg = conflicts |> list_to_string in
        (Nothing, Fail (err_msg))
    else (Nothing, Fail (t ^ " is already in the room"))
  else (Nothing, Fail (u ^ " does not have access to this room"))

(*[leave_room u crname] handles [u] leaving the chatroom [crname]*)
let leave_room u crname = 
  u |>? fun _ ->
  crname |>?? fun _ -> 
  let (cr, msg_hist) = Hashtbl.find rooms crname in
  let p' = cr.participants |> List.filter ((<>) u) in
  if List.length p' <> List.length cr.participants then
    let cr' = {cr with participants = p'} in
    Hashtbl.replace rooms crname (cr', msg_hist);
    (Nothing, Success)
  else (Nothing, Fail(u ^ " is not in room " ^ crname))
    
(*checks that the given authentication key is correct for user [nm]*)
let valid nm ky =
  if Hashtbl.mem clients nm then
    let usr = Hashtbl.find clients nm in
    match usr.key with
    | Some k -> k = ky
    | None -> false
  else false


(***************************************************)
(****** end of handlers and helpers *****************)
(***************************************************)
let handle_user_request quest identifier = 
  match quest with 
  | Block (target) -> handle_block identifier target
  | Unblock (target) -> handle_unblock identifier target
  | Message msg -> post_message msg
  | Listrooms -> get_rooms identifier
  | Listmessages (last, cr) -> get_messages identifier last cr
  | Newroom cr -> post_room cr
  | Getroom (crname) -> get_room identifier crname
  | Listusers -> get_users ()
  | Newgame gr -> post_game gr
  | Listgames -> get_games identifier
  | Getwl -> get_wl identifier
  | Getgame grname -> handle_get_game identifier grname
  | Changegamest (gr, st) -> change_game_st identifier gr st  
  | Resetgame gr -> reset_game identifier gr 
  | AddToRoom (target, crname) -> add_to_room identifier target crname
  | LeaveRoom crname -> leave_room identifier crname
  | _ -> (Nothing, Fail "server error")

let bad_key_resp = 
  (Nothing, Fail "You have an invalid key")
let handle_request = function
  | (None, Register (identifier, pswd)) -> handle_reg identifier pswd
  | (None, Auth (identifier, pswd)) -> handle_auth identifier pswd  
  | (None, a) -> (Nothing, Fail ("this is the bad key " ^ (Type_info.req_to_string (None, a))))
  | (Some (name, key), quest) -> 
      if valid name key then
        handle_user_request quest name
      else bad_key_resp


