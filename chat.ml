open Unix

(* service port number*)
let port = 5678

let get_my_addr () =
   (gethostbyname (gethostname ())).h_addr_list.(0) ;;

let get_inet_addr_of_string str =
	try inet_addr_of_string str
	with Failure _ -> 
		( try (gethostbyname str).h_addr_list.(0) 
		  with Not_found ->
		  		raise Not_found;
		)

(* setup a connection to give address,
   return (inputchannl, outpu_channel) *)
let connect_to sockaddr =
	let domain = domain_of_sockaddr sockaddr in
	let sock = socket domain SOCK_STREAM 0 in
	try connect sock sockaddr;
		(in_channel_of_descr sock,out_channel_of_descr sock)
	with exn -> close sock; raise exn

(* gemerica client creator 
	client_fun: (in_channel -> out_channel) -> 'a
	server_addr: 
*)
let client client_fun server_addr =
	let sockaddr = ADDR_INET(server_addr,port) in
	try 
		let (ic,oc) = connect_to sockaddr in
		client_fun ic oc;
		close (descr_of_out_channel oc);
	with exn -> raise exn

(* generic server creator 
	server_fun: (in_channel -> out_channel -> 'a
	sockaddr: service addr 
	max_conn: maximun connection at a time 
*)
let spinup_tcp_server server_fun sockaddr max_conn =
	let domain = domain_of_sockaddr sockaddr in
	let sock = socket domain SOCK_STREAM 0 in
	setsockopt sock SO_REUSEADDR true;
	bind sock sockaddr ;
	listen sock max_conn;
	while true do 
		let (s, cli_addr) = accept sock in
		let (cli_ip,cli_port) = match cli_addr with ADDR_UNIX _ -> raise (Sys_error "What???") (* This will never be reached!*)
												  | ADDR_INET (a,b) -> (a,b)
 		in 
		Printf.printf "Peer Connected from %s:%d \n" (string_of_inet_addr cli_ip) cli_port;
		flush Pervasives.stdout;
		let ic = in_channel_of_descr s 
		and oc = out_channel_of_descr s in
	
	 	server_fun ic oc;
	 	close s;
		
		Printf.printf "Session end!\n";
		flush Pervasives.stdout
	done  ;;


let chat ic oc =
	let m = Mutex.create () in (* lock for output_channel *)
	let l = Mutex.create () in (* lock for termination indicator *)
	let is_end = ref false in (* termination indicator *)
	let handle_input () = (* Thread that handles keyboard input*)
		try 
			while true do
				let s = input_line Pervasives.stdin in
				Mutex.lock m;
				output_string oc ("M"^s^"\n");
				flush oc;
				Mutex.unlock m
			done 
		with End_of_file ->  Mutex.lock l; is_end:= true; Mutex.unlock l;
							shutdown (descr_of_out_channel oc) SHUTDOWN_SEND; Thread.exit ()
	and handle_msg () = (* Thread that handles msg from peers *)
		try
			while true do 
				let s = input_line ic in
				let s' = if s.[0]='A' then s
						else String.sub s 1 (String.length s -1)
				in 
				Printf.printf "Recieved: %s\n" s';
				flush Pervasives.stdout;
				(*Dont send ACK for incoming ACKs *)
				if s.[0] = 'M' then begin
					Mutex.lock m;
					output_string oc "ACK\n";
					flush oc;
					Mutex.unlock m
				end
			done
		with End_of_file -> (); Thread.exit ()
	in
		let th1 = Thread.create handle_input () in  
		let th2 = Thread.create handle_msg () in
		Thread.join th2;
		Mutex.lock l;
		if !is_end= false then 
		begin
			Printf.printf "Peer hung up, type Ctrl-D to end session\n"; 
			flush Pervasives.stdout;
			is_end:=true
		end;
		Mutex.unlock l;
		Thread.join th1



		
(* Main workig function*)
let work = fun host () ->
		match host with
		  None -> 	let my_addr = get_my_addr () in
		  			Printf.printf "Satrted server at %s:%d ...\n" (string_of_inet_addr my_addr) port; 
		  			flush Pervasives.stdout;
		  			spinup_tcp_server chat (ADDR_INET(my_addr,port)) 1 ;
		| Some str ->
			let server_addr =
				try get_inet_addr_of_string str
				  with Not_found ->
				  		Printf.printf "Host not found\n"; exit 2;
			in client chat server_addr

(* Build a command*)
let cmd = Core.Std.(
	Command.basic 
	~summary:"Start waiting for incoming chat "
	Command.Spec.(
		empty 
		+> flag "-client" (optional string) ~doc:"\"hostname_or_ip\" being a client, conncet to given host( by name or IP addr)"
	)
	work
	) 
let () = Core.Std.Command.run cmd



























