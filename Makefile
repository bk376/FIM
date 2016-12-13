main:
	corebuild -pkgs lwt,core,str,lwt.unix interface.byte
server:
	corebuild -pkgs lwt,lwt.unix,core,str chat_server.byte
