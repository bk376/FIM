(* id is the identifier for the user *)
type id = string

(*square is a type representing the state of a tic-tac-toe square
 *N represents an empty square*)
type square = N | X | O 

type chatroom =
  {
    name : string;
    participants : id list;
  }

type gameroom = 
  {
    name : string ; 
    players : id list ;
  }

(* msg is the type of a message sent by the user to another
 * user *)
type msg = 
  {
    user : string; 
    room : chatroom;
    message : string; 
    timestamp : float
  }

(* indicates whether a request was successful *)
type success = Success | Fail of string

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
type response = resp * success

val req_to_string : request -> string

val resp_to_string : response -> string

val req_from_string : string -> request

val resp_from_string : string -> response

(*[check_victory st] checks if [st] is a state where a player
 *has won or the game is a draw. Returns a polymorphic variant
 *representing the different possible outcomes*)
val check_victory : square list -> [> `X | `O | `Draw | `Not]
