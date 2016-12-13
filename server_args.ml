open Sys

type server_obj = Local | Web

let obj = 
  if Array.mem "-o" Sys.argv then Web
  else Local
