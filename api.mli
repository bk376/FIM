open Lwt
open Type_info
(* functions in this file will be called
 * in order to make requests and get the responses in
 * an abstract, UI independent form. The front end is
 * responsible for displaying the responses. In general,
 * the success of the response is given back to the client
 * only if there is some legitimate reason that the request
 * could fail (ie. a login attempt has the wrong password)
 *)

exception ServerError of string
exception ClientError

module type RequesterMaker =
 functor (Cl : Chat_client.Client ) -> Requester.Req

module MakeRequester : RequesterMaker
