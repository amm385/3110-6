open Definitions
open Constants
open Util

let host = try (Unix.gethostbyname ((Sys.argv).(1))).Unix.h_addr_list.(0)
           with _ -> failwith ("Must provide hostname of game server as " ^
                               "first command-line argument for team to " ^
                               "connect to.")
let port = try int_of_string ((Sys.argv).(2))
           with _ -> cDEFAULT_BOT_PORT_NUMBER
let addr = Unix.ADDR_INET (host, port)
let retries = 10
let conn = ref None
let mutex = Mutex.create ()
let _ = Random.self_init ()

(* --------------------- Helper Functions -------------------- *)
let valOf x = match x with Some(y) -> y | None -> failwith "Tried to valOf None"

(* -------------------- Communication Stuff ------------------ *)
(* Doesn't return until a connection is established. *)
let recreate_conn () =
  let conn' = ref None in
  while !conn' = None do
    conn' := Connection.init addr retries
  done ; conn := !conn' ;
  match !conn with
     Some conn' -> conn'
   | None -> failwith "could not create connection in recreateConn"

let get_conn () =
  match !conn with
     Some conn -> conn
   | None -> recreate_conn ()

let set_conn c = conn := c

let wait_for_game_start () =
  let input = ref(Connection.input (get_conn ())) in
  while !input = None do input := Connection.input (get_conn ()) done ;
  match !input with
     Some(Control(GameStart)) -> ()
   | _ -> failwith "didn't receive GameStart"

let send_action a id =
  let conn = get_conn() in
  if Connection.output conn (Action(id,a)) then
    match Connection.input conn with
      Some(Result(id,r)) -> r
    | Some(c) -> print_endline ("command: "(*^command_to_string c*));failwith "Didn't receive appropriate response to action request"
	| None -> print_endline "none"; failwith "Didn't receive appropriate response to action request"
  else failwith "Server didn't respond to action request"

let get_status s =
  let conn = get_conn() in
  if Connection.output conn (Status(s)) then
    match Connection.input conn with
      Some(Data(d)) -> d
    | Some(c) -> print_endline ("command: "(*^command_to_string c*));failwith "Didn't receive appropriate response to status request"
	| None -> print_endline "none"; failwith "Didn't receive appropriate response to status request"
  else failwith "Server didn't respond to status request"

let start_bot bot =
  conn:=Connection.init addr 10;
  match (!conn) with
    Some(conn) ->
      if Connection.output conn (Control(GameRequest)) then
        match Connection.input conn with
          Some(Control(Team(c))) ->
            print_endline ("Bot has color "^(string_of_color c)) ;
            wait_for_game_start () ;
            ignore(bot c);
            Connection.close conn
          | _ -> failwith "Server didn't respond to game request with team"
      else failwith "Unable to send value to game server"
  | None -> failwith "Can't connect to server"
