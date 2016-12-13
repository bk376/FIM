open Core.Std

type id = string [@@deriving sexp]

type square = N | X | O [@@deriving sexp]

type chatroom =
  {
    name : string;
    participants : id list;
  } [@@deriving sexp]

type gameroom =
  {
    name : string ; 
    players : id list ;
  } [@@deriving sexp]

type msg = 
  {
    user : string; 
    room : chatroom;
    message : string; 
    timestamp : float
  } [@@deriving sexp]

type success = Success | Fail of string [@@deriving sexp]

type identifier = (id * int) option [@@deriving sexp] 

(* request is the type of requests that the client
 * may send to the server*)
type request_content = 
  | Message of msg
  | Register of id * string 
  | Auth of id * string
  | Block of id
  | Unblock of id
  | Listrooms 
  | Listmessages of msg option * chatroom
  | Newroom of chatroom 
  | Getroom of string
  | Listusers 
  | Newgame of gameroom 
  | Listgames 
  | Getgame of string 
  | Getwl 
  | Changegamest of gameroom * int
  | Resetgame of gameroom 
  | AddToRoom of id * string 
  | LeaveRoom of string [@@deriving sexp]

type request = identifier * request_content [@@deriving sexp]

type resp = 
  | Chatroom of chatroom
  | Gameroom of gameroom 
  | Messages of msg list
  | Chatrooms of chatroom list
  | Users of id list 
  | Wl of int * int 
  | Gamerooms of gameroom list 
  | Gamestate of gameroom * square list 
  | Sessionkey of int
  | Nothing [@@deriving sexp] 

(* the type of the response *)
type response = resp * success [@@deriving sexp]

let req_to_string req = 
  req |> sexp_of_request |> Sexp.to_string

let req_from_string s =
  s |> Sexp.of_string |> request_of_sexp

let resp_to_string resp =
  resp |> sexp_of_response |> Sexp.to_string

let resp_from_string s =
  s |> Sexp.of_string |> response_of_sexp

let check_victory st = 
  (*[is_full st] checks if the board has no empty squares. Used to check
   *draws*)
  let rec is_full st = match st with 
    | h::t -> (h=X||h=O) && is_full t 
    | [] -> true 
  in match st with 
  | [X;X;X;_;_;_;_;_;_] | [_;_;_;X;X;X;_;_;_] | [_;_;_;_;_;_;X;X;X]
  | [X;_;_;X;_;_;X;_;_] | [_;X;_;_;X;_;_;X;_]  | [_;_;X;_;_;X;_;_;X]
  | [X;_;_;_;X;_;_;_;X] | [_;_;X;_;X;_;X;_;_] -> `X 
  | [O;O;O;_;_;_;_;_;_] | [_;_;_;O;O;O;_;_;_] | [_;_;_;_;_;_;O;O;O]
  | [O;_;_;O;_;_;O;_;_] | [_;O;_;_;O;_;_;O;_]  | [_;_;O;_;_;O;_;_;O]
  | [O;_;_;_;O;_;_;_;O] | [_;_;O;_;O;_;O;_;_] -> `O 
  | _ -> if is_full st then `Draw else `Not 
