open Definitions
open Util

let clients = ref []
let updates = ref []
let clients_lock = Mutex.create()
let updates_lock = Mutex.create()

let cUPDATE_SEPARATOR = "#"
let cARGUMENT_SEPARATOR = "$"
let cPOINT_SEPARATOR = "@"

let string_of_point (x,y) = 
  (string_of_int (int_of_float x)) ^ cPOINT_SEPARATOR ^ (string_of_int
  (int_of_float y))

let string_of_wt wt = 
  match wt with
  Basic -> "Basic"
  |Grenader -> "Grenader"
  |MissileBlaster -> "MissileBlaster"
  |Miner -> "Miner"
  |PelletShooter -> "PelletShooter"
  |LazerGunner -> "LazerGunner"

let string_of_wep_type tp =
  match tp with
  Bomb -> "Bomb"
  |Grenade -> "Grenade"
  |Missile -> "Missile"
  |Mine -> "Mine"
  |Pellet -> "Pellet"
  |Lazer -> "Lazer"
  |Bat -> "Bat"

let string_of_game_result gr =
  match gr with
  |Winner(c) -> string_of_color c
  |Tie -> "Tie"

let string_of_obstacle obs =
  match obs with 
  Cloud(id, pos) -> "Cloud" ^ cARGUMENT_SEPARATOR ^ string_of_int id ^
  cARGUMENT_SEPARATOR ^ string_of_point pos
  |Satellite(id,pos,hp) -> "Satellite" ^ cARGUMENT_SEPARATOR ^ string_of_int id
    ^ cARGUMENT_SEPARATOR ^ string_of_point pos ^ cARGUMENT_SEPARATOR ^
    string_of_int hp

let string_of_update update = 
  match update with
  |InitGraphics(vlist) -> "InitGraphics" ^ cARGUMENT_SEPARATOR ^
      List.fold_left (fun acc pt -> acc ^ cARGUMENT_SEPARATOR ^ string_of_point
      pt) "" vlist
  |Countdown(num) -> "Countdown" ^ cARGUMENT_SEPARATOR ^ string_of_int num
  |DisplayString(col, talk) -> "DisplayString" ^ cARGUMENT_SEPARATOR ^
  string_of_color col ^ cARGUMENT_SEPARATOR ^ talk
  |AddWorm(id, pos, health) -> "AddWorm" ^ cARGUMENT_SEPARATOR ^ string_of_int id ^ cARGUMENT_SEPARATOR ^
  string_of_point pos ^ cARGUMENT_SEPARATOR ^ string_of_int health 
  |MorphWorm(id, wt) -> "MorphWorm" ^ cARGUMENT_SEPARATOR ^ string_of_int id ^ cARGUMENT_SEPARATOR ^ string_of_wt wt
  |MoveWorm(id, pos) -> "MoveWorm" ^ cARGUMENT_SEPARATOR ^ string_of_int id ^ cARGUMENT_SEPARATOR ^ string_of_point
  pos
  |RemoveWorm(id) -> "RemoveWorm" ^ cARGUMENT_SEPARATOR ^ string_of_int id
  |AddProjectile(id, wt, pos) -> "AddProjectile" ^ cARGUMENT_SEPARATOR ^
  string_of_int id ^ cARGUMENT_SEPARATOR ^ string_of_wep_type wt ^
  cARGUMENT_SEPARATOR ^ string_of_point pos
  |MoveProjectile(id, pos) -> "MoveProjectile" ^ cARGUMENT_SEPARATOR ^
  string_of_int id ^ cARGUMENT_SEPARATOR ^ string_of_point pos
  |RemoveProjectile(id) -> "RemoveProjectile" ^ cARGUMENT_SEPARATOR ^
  string_of_int id
  |UpdateScore(c, sc) -> "UpdateScore" ^ cARGUMENT_SEPARATOR ^ string_of_color c
  ^ cARGUMENT_SEPARATOR ^ string_of_int sc
  |GameOver(gr) -> "GameOver" ^ cARGUMENT_SEPARATOR ^ string_of_game_result gr
  |AddObstacle(obs) -> "AddObstacle" ^ cARGUMENT_SEPARATOR ^ string_of_obstacle
  obs
  |RemoveObstacle(id) -> "RemoveObstacle" ^ cARGUMENT_SEPARATOR ^
  string_of_int id
  |DoBat(id) -> "DoBat" ^ cARGUMENT_SEPARATOR ^ string_of_int id
(*translates the updates to strings to be passed to the gui*)
let parse_updates updates =
  Mutex.lock updates_lock;
  let string_fold acc update = acc ^ string_of_update update ^
  cUPDATE_SEPARATOR in
  let sendable = List.fold_left string_fold "" updates in
  Mutex.unlock updates_lock; sendable

let init_server port =
  let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt server Unix.SO_REUSEADDR true;
    Unix.setsockopt server Unix.SO_KEEPALIVE false;
    Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_any, port));
    Unix.listen server 100;
	server

let add_clients server =
  while true do
    let (c, a) = Unix.accept server in
      Mutex.lock clients_lock;
	  print_endline "A client connected to gui server";
      clients := (Connection.server a c)::!clients;
      Mutex.unlock clients_lock;
  done

let init_single_connection port =
  let server = init_server port in
  let (c, a) = Unix.accept server in
    Mutex.lock clients_lock;
	print_endline "A client connected to gui server";
	clients := (Connection.server a c)::!clients;
	Mutex.unlock clients_lock;
    ignore(Thread.create add_clients server)

let init port = ignore(Thread.create add_clients (init_server port))

let add_update u =
  Mutex.lock updates_lock;
  updates := u::(!updates);
  Mutex.unlock updates_lock

let send u =
  Mutex.lock clients_lock;
  let parsed_updates = parse_updates u in
    clients := List.fold_left (fun new_clients c ->
                                 if Connection.output_string c parsed_updates then
                                   c::new_clients
                                 else
                                   (Connection.close c;
                                    new_clients)) [] !clients;
    Mutex.unlock clients_lock

let send_update u = send [u]

let send_updates() =
  Mutex.lock updates_lock;
  let u = List.rev !updates in
  updates := [];
  Mutex.unlock updates_lock;
  send u
