open Sys


type client_obj = {host : string; port : string}


let malformat_message =
  "Malformatted arguments. This program takes the " ^
  "arguments \n" ^
  "[-h <host>] [-p <port_num]"

(*returns a record representing the arguments passed into the executable*)
let obj () =
  let lst = Array.to_list Sys.argv |> List.tl in
  let contains s = String.contains s '-' in
  let dashes, cont = List.partition contains lst in
  let info = 
    try List.combine dashes cont with
    | Invalid_argument _ -> raise (Failure malformat_message) in
  let h =
    if List.mem_assoc "-h" info |> not then "localhost"
    else List.assoc "-h" info in
  let p =
    if List.mem_assoc "-p" info |> not then "3110"
    else
      let candidate = List.assoc "-p" info in
      (try int_of_string candidate with
      | Failure _ -> raise (Failure "port must be a number"))
      |> ignore; candidate in
  {host = h; port = p}



