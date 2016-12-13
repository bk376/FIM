open Api
open Type_info 
open Lwt
open Str

(* The backbone of the client side. This should
 * have (and make available) all the information
 * that a UI would need to display the current state *)
module MakeInterface (Quester : Requester.Req) = struct

  (********** Reiterating a bunch of type info ******)
  type chat = {
    cr : Type_info.chatroom;
    last : Type_info.msg option;
  }

  (*represents the game info the client has. last_state is
   *the client's current knowledge of what the board looks like*)
  type game = {
    gr : Type_info.gameroom ;
    last_state : Type_info.square list ;
  }  

  type information = {
    username : id
  }

  type md = Inchat of chat | Ingame of game | General

  type state = {
    mode : md;
    info : information;
  }

  type t = state ref

  (*********** End type info ************************)

  (*********** Basic init stuff *********************)
  (* placeholder current state until it
   * get set in init *)
  let current_state = ref ( {
    mode = General;
    info = { username = "nothing"};
  })
 
  (*[set_game game] updates the current_state with [game]*)
  let set_game game = 
    current_state := {!current_state with 
      mode = Ingame game 
    }
  (************ end init stuff ***********************)

  (*reads the input line from terminal*)
  let lread () =
    Lwt_io.read_line Lwt_io.stdin
  
  (*prints s and returns unit Lwt.t*)
  let lprint s =
    Lwt_io.write Lwt_io.stdout s 
  
  (*prompt for command in General mode*)
  let command_prompt () =
    lprint "\n>> "
  
  (*hides the password as the user types it in so nobody
   *can be sneaky*)
  let hide_password () =
    let with_echo = Unix.tcgetattr Unix.stdin in
    let no_echo = { with_echo with Unix.c_echo = false } in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW no_echo;
    try
      lread () >>= fun pass ->
      lprint "\n" >|= fun () ->
      Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH with_echo;
      pass
    with e ->
      Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH with_echo;
      raise e
  
  (*handles the logging-in/user creation that occurs upon
   *connecting to the server*)
  let rec handle_register () = 
    lprint "Choose your id: " >>= 
    lread >>= fun uid -> 
    if String.contains uid ' ' then
      lprint "id should not contain spaces!\n" >>= handle_register
    else
      lprint "Create Password: " >>= hide_password >>= fun pswd1 ->
      if (String.length pswd1 >=4) then 
        lprint "Confirm Password: " >>= hide_password >>= fun pswd2 ->
        if pswd1=pswd2 then Quester.register uid pswd2
        else (lprint "Passwords not matching!\n") >>= handle_register
      else (lprint "Password should be at least 4 characters\n") >>= handle_register

      
(************ end init stuff ***********************)

  (* Displays the prompt for login *)
  let login_prompt () = 
    lprint "Enter your (id) or (N) for new user\nid: "

  (*[display_game_st st] prints the board represented by the state st
   *with proper formatting*)
  let display_game_st st = 
    if List.length st <> 9 then failwith "invalid state to display"
    else let string_of_square sq = match sq with 
    | N -> "_"
    | X -> "X"
    | O -> "O"
    (*format is a formatting variable to make sure the board is printed nicely*)
    in let format = ref 0 in 
    let rec p st' = match st' with 
    | h::t -> if !format = 2 then 
      (format := 0 ; 
      lprint (string_of_square h) >>= fun _ ->
      lprint "\n" >>= fun _ -> p t) 
      else (incr format ; lprint (string_of_square h) >>= fun _ ->
      lprint " " >>= fun _ -> p t) 
    | [] ->  if check_victory st = `X 
              then lprint "Player X has won! Use \\reset to clear the board and play again.\n\n"
             else if check_victory st = `O
              then lprint "Player O has won! Use \\reset to clear the board and play again.\n\n"
             else if check_victory st = `Draw 
              then lprint "The game ended in a draw! Use \\reset to clear the board and play again.\n\n"
             else lprint "\n\n" 
    in p st 

  (* Displays an arbitrary list with an arbitrary
   * print function. No requirement on type of
   * list element *)
  let display_list prnt lst =
    let rec print_seq = function
      | h :: t -> prnt h >>= fun _ ->
          lprint "\n" >>= fun _ -> print_seq t
      | [] -> () |> return in
    print_seq lst

  (* Displays a list with a title. Requires
   * that list elements be strings *)
  let display_title_list title lst =
    lprint (title^"\n") >>= fun _ ->
    let lst' = lst |> List.map ((^) "\t") in
    display_list lprint lst'
  
  (*handles creating a new room and prompts user for 
   *the room name and users who will be in the room*)
  let handle_new_room () =
    lprint "room name: " >>= command_prompt >>=
    lread >>= fun rmname ->
    lprint "participant list: " >>= command_prompt >>=
    lread >>= fun plst ->
    let lst = plst |> Str.split (Str.regexp " ") in
    let lst' = !current_state.info.username :: lst in
    Quester.new_room lst' rmname >>= function
    | Success -> return ()
    | Fail s -> lprint s
  
  (*[handle_new_game ()] is called when a user tries to create a new
   *game. Prompts for the game name and players. Does not proceed if 
   *the number of players provided is invalid. The user is automatically
   *a player by default but may specify himself with no consequence*)
  let handle_new_game () = 
    lprint "game name: " >>= command_prompt >>=
    lread >>= fun gname ->
    lprint "players: " >>= command_prompt >>=
    lread >>= fun players ->
    let plist = players |> Str.split (Str.regexp " ") in 
    (*automatically make the user a player*)
    let plist' = !current_state.info.username :: plist |> List.sort_uniq compare in 
    if List.length plist' <> 2 then lprint "invalid number of players"
    else Quester.new_game plist' gname >>= function 
    | Success -> return () 
    | Fail s -> lprint s   

  (*handles displaying all users registered on the server*)
  let handle_ls_users () =
    Quester.see_users () >>= fun ulst -> 
    let disp = display_list lprint in
    ulst |> disp 
  
  (*handles displaying all rooms the user is a part of*)
  let handle_ls_rooms () =
    Quester.see_chatrooms () >>= fun clst ->
    let disp_rm ({name = nm; participants = mems} : Type_info.chatroom) = 
      display_title_list nm mems in
    let disp_all lst =
      display_list disp_rm lst in
    clst |> disp_all 
  
  (*[handle_ls_games ()] is called when the user tries to list the
   *games they are in. Displays the games and players in a nice way*)
  let handle_ls_games () = 
    Quester.see_games () >>= fun glst ->
    let disp_gm ({name = n ; players = p} : Type_info.gameroom) =
      display_title_list n p in 
    let disp_all lst = 
      display_list disp_gm lst in 
    glst |> disp_all 
  
  (*[handle_getwl ()] is called when the user tries to get their win-loss.
   *prints it out as: W-L*)
  let handle_getwl () = 
    Quester.getwl () >>= fun (w,l) ->
    lprint ((string_of_int w)^"-"^(string_of_int l))

  (*handles exiting a room; updates the state accordingly*)
  let handle_exit_room () = 
    let uid = !current_state.info.username in
    (current_state := {
      mode = General;
      info = {username = uid};
    }) |> return

  (*prints messages in a chatroom*)
  let print_message usr msg = 
    (let uid = !current_state.info.username in
    if usr <> uid then lprint (usr ^ ": ")
    else return ()) >>= fun _ ->
    lprint msg

  (* [check_response_validity mlist] takes in a
   * message list and makes sure that the most recent
   * message is not included in the list. If it is,
   * that means the list was created with an old
   * most recent message and the whole list is invalid*)
  let check_response_validity old_last =
    match !current_state.mode with
    | General | Ingame _ -> false
    | Inchat x -> match x.last, old_last with
      | (None, None) -> true
      | (Some a, Some b) -> a = b
      | _ -> false
  
  (*[check_game_state serv_state last_state] compares [serv_state] and
   *[last_state] to see if they are equal*)
  let check_game_state serv_state last_state = 
    match !current_state.mode with 
    | General | Inchat _ -> false 
    | Ingame _ -> last_state = serv_state 

  (*[last_elem old lst] returns the last element in [lst] unless 
   *[lst] is the empty list, in which case it returns [old]. Used
   *to get the most recent message in a chatroom*)
  let rec last_elem old lst = 
    match lst with
    | _ :: h2 :: t -> last_elem old (h2 :: t)
    | h :: _ -> Some h
    | [] -> old

  (*checks the server to see if there are any new messages to post*)
  let refresh_messages {cr = c; last = m} =
    Quester.see_messages m c >>= fun mlist ->
    if check_response_validity m then
      let last' = last_elem m mlist in
      let mode' = Inchat {cr = c; last = last'} in
      current_state := {!current_state with mode = mode'};
      let p (cont: Type_info.msg) = 
        print_message cont.user cont.message in
      mlist |> display_list p
    else return ()
  
  (*handles sending a message in a chatroom*)
  let handle_send_message {cr = c; last = _} s = 
    Quester.send_message c s >>= fun (msg, succ) ->
    match succ with
    | Success -> 
        let mode' = Inchat {last = Some msg; cr = c} in
        return (current_state := {!current_state with mode = mode'})
    | Fail b -> lprint b

  (*[refresh_game game] makes sure the client's version of the game is 
   *always up to date with the server's version*)
  let refresh_game {gr = g ; last_state = last} =
    Quester.get_game g.name >>= fun (gr_st,_) ->
    let st = snd gr_st in 
    (*do nothing if game is up to date or user is no longer in the game*)
    if check_game_state st last || !current_state.mode = General then 
      return () 
    (*if server version is different update the current state to match*)
    else
      (set_game {gr=g; last_state=st} ; 
      display_game_st st)

  (*checks the current state and refreshes the information displayed on
   *the terminal as necessary*)
  let refresh () =
    Lwt.async (fun () ->
      let rec loop () =
        match !current_state.mode with
        | Inchat x -> refresh_messages x >>= loop
        | Ingame x -> refresh_game x >>= loop 
        | General -> return () in 
      loop ()
    ); return ()

  (*updates the current state to enter the specified room*)
  let handle_enter_room s =
    let nm = Str.matched_group 1 s in
    Quester.get_room nm >>= fun ((crm : Type_info.chatroom), succ) ->
    match succ with
    | Success -> 
        let mode' = Inchat { last = None; cr = crm} in
        current_state := { !current_state with mode = mode'};
        lprint ("entered " ^ crm.name ^ "\n") >>= refresh
    | Fail s -> lprint s

  (*[handle_enter_game s] is called when a user tries to enter a game with
   *the command [s]. Updates the current_state accordingly with a dummy board
   *so it will always display the correct current board upon entering*)
  let handle_enter_game s = 
    (*extracts the game name from [s]*)
    let nm = Str.matched_group 1 s in 
    Quester.get_game nm >>= fun (((grm : Type_info.gameroom),_), succ) ->
    match succ with 
    | Success -> 
      (set_game {gr=grm;last_state=[]} ; 
      lprint ("entered " ^ grm.name ^ "\n") >>= refresh)
    | Fail s -> lprint s 

  (*[handle_exit_game ()] is called when a user tries to exit a game they are 
   *in. Updates the current state accordingly*)
  let handle_exit_game () = 
    let uid = !current_state.info.username in
    (current_state := {
      mode = General;
      info = {username = uid};
    }) |> return

  (*[handle_fill game inpt] handles trying to fill a square in 
   *the game [game]*)
  let handle_fill {gr = g ; last_state = st} inpt = 
    let sq_num = try Str.matched_group 1 inpt |> int_of_string 
                 with _ -> -1 
    in
    if sq_num < 0 || sq_num >= List.length st 
    then lprint "invalid square number\n\n"
    else  
      (*(current_state := {!current_state with mode = Ingame {gr=g;last_state=st}} ;*)
      (set_game {gr=g;last_state=st} ; 
      Quester.fill_board g sq_num >>= function 
      | Success -> return () 
      | Fail s -> lprint s)

  (*[handle_reset g] handles resetting the game g*)
  let handle_reset g = 
    Quester.reset_board g.gr >>= function 
    | Success -> return ()
    | Fail s -> lprint s

  (*handles leaving the room specified by s*)
  let handle_leave_room s =
    let crname = Str.matched_group 1 s in
    Quester.leave_room crname >>= function
    | Success -> return ()
    | Fail s -> lprint s
  
  (*handles blocking the user specified by s*)
  let handle_block s =
    let nm = Str.matched_group 1 s in
    Quester.block_user nm >>= function
    | Success -> return ()
    | Fail s -> lprint s

  (*handles unblocking the user specified by s*)
  let handle_unblock s =
    let nm = Str.matched_group 1 s in
    Quester.unblock_user nm >>= function
    | Success -> return ()
    | Fail s -> lprint s

  (*handles adding the user specified by inpt to the given 
   *chatroom*)
  let handle_add_to_room {cr = c; last = l} inpt =
    let nm = Str.matched_group 1 inpt in
    Quester.add_user_to_room nm c.name >>= function
    | Success -> 
        let newrm = {c with participants = inpt :: c.participants} in
        let cht' = {cr = newrm; last = l} in
        current_state := {!current_state with mode = Inchat cht'};
        return ()
    | Fail s -> lprint s >>= fun _ -> lprint "\n"

  (******* help messages ****************************)

  let command_help_messages = [
    "ls users           Displays a list of users";
    "ls rooms           Displays a list of your rooms";
    "new room           Initiates protocol for new room creation";
    "new game           Initiates protocol for new game creation";
    "ls games           Lists your games";
    "get wl             Gets your win loss record";
    "play <name>        Starts a game of tic tac toe with <name>";
    "enter <room-name>  Enters the room <room-name>";
    "leave <room-name>  Removes you from the room <room-name>";
    "block <user>       Blocks user <user>";
    "unblock <user>     Unblocks user <user>";
    "exit               Terminates the session";
  ]
    
  let chatroom_help_messages = [
    "\\add <user>       Adds <user> to the room";
    "\\exit             Leaves the chatroom";
  ]

  let print_cmnd_help () = 
    display_title_list "Your commands are:\n" command_help_messages

  let print_msg_help () =
    display_title_list "Keywords are:\n" chatroom_help_messages

  (******* help messages *****************************)
  (************ End Formatting and printing **********)

  (************ process command and helpers **********)

  
  (*[str_mtch s re] converts the string re into a regex and
   *matches s against it*)
  let str_mtch s re = 
    let re' = Str.regexp re in
    Str.string_match re' s 0

  (*prompts the user for commands in the General state*)
  let process_command () =
    command_prompt () >>= lread >>= fun s ->
    let sm = str_mtch s in
    if      "^ls users" |> sm then handle_ls_users ()
    else if "^ls rooms" |> sm then handle_ls_rooms ()
    else if "^new room" |> sm then handle_new_room ()
    else if "^new game" |> sm then handle_new_game () 
    else if "^ls games" |> sm then handle_ls_games () 
    else if "^get wl" |> sm then handle_getwl () 
    else if "^play \\(.*\\)" |> sm then handle_enter_game s 
    else if "^enter \\(.*\\)" |> sm then handle_enter_room s
    else if "^leave \\(.*\\)" |> sm then handle_leave_room s
    else if "^block \\(.*\\)" |> sm then handle_block s
    else if "^unblock \\(.*\\)" |> sm then handle_unblock s
    else if "^help" |> sm then print_cmnd_help ()
    else if "^exit" |> sm then lprint "Goodbye!\n" >>= fun _ -> exit 0
    else       
        lprint "unrecognized command\n"

  (*handles user inputs or commands in the Inchat state*)
  let process_msg cht =
    lread () >>= fun inpt ->
    if "\\\\add \\(.*\\)" |> str_mtch inpt then 
      handle_add_to_room cht inpt
    else
      match inpt with
      | "\\exit" -> handle_exit_room ()
      | "\\help" -> print_msg_help ()
      | s -> handle_send_message cht s

  (*note: uses unit to make sure the current state has the right state of 
   *the game. If the game itself is passed in problems arise where the 
   *client's version of the board isn't updated properly. Requires the
   *current state to be Ingame*)
  let process_game () = 
    lread () >>= fun inpt ->
    let g = match !current_state.mode with Ingame gm -> gm in  
    if "\\\\fill \\(.*\\)" |> str_mtch inpt then
      (*does not allow squares to be filled after the game is over*)
      if check_victory g.last_state <> `Not 
        then lprint "The game is over!\n\n"
      else handle_fill g inpt
    else 
      match inpt with  
      | "\\reset" -> 
        (*does not allow resetting a game that is in progress*)
        if check_victory g.last_state <> `Not 
          then handle_reset g 
        else lprint "The game isn't over yet!\n\n"          
      | "\\exit" -> handle_exit_game () 
      | "\\help" -> lprint ("\\exit to exit the game\n\\fill <num> to fill the specified square. " ^
              "Squares are numbered as follows:\n0 1 2\n3 4 5\n6 7 8\n\n")
      | _ -> lprint "unrecognized command\n\n"  

  (*runs the correct process function based on the current state*)
  let process_input () = 
    match !current_state.mode with
    | Inchat cht -> process_msg cht
    | Ingame _ -> process_game () 
    | General -> process_command ()
  
  let rec repl () = 
    process_input () >>= repl

  (*handles typing in a user's password*)
  let handle_auth identifier = 
    lprint "Password: " >>= hide_password >>= fun pass ->
    Quester.auth identifier pass
  
  (*main entry point for the client application*)
  let rec run () =
    login_prompt() >>= 
    command_prompt >>= fun _ ->
    lread() >>= fun rd ->
    if "^[Nn]" |> str_mtch rd then handle_register () >>= function
    | Success -> run ()
    | Fail s -> lprint s >>= run
    else handle_auth rd >>= function
    | Success -> 
        current_state := {!current_state with info = {username = rd}}; repl ()
    | Fail s -> lprint s >>= run

  let bad_server_msg =
    "Failed to connect to the server"

  let _ = 
    try run () |> Lwt_main.run with
    | Unix.Unix_error _ -> 
        print_endline bad_server_msg; exit 1
    | Api.ServerError s -> 
        print_endline ("Failed request yields " ^ s); exit 1
    | _ -> print_endline "\nUnknown error. Make sure the server is running"; exit 1

end

module Newquester = MakeRequester(Chat_client.Cl)
module Newinterface = MakeInterface(Newquester)
