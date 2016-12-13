
module type Handler = sig
	
	(*handles the given request and returns the proper response*)
  val handle_request : Type_info.request -> Type_info.response

end
