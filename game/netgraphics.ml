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
  (string_of_int (int_of_float x)) ^ cPOINT_SEPARATOR ^
  (string_of_int (int_of_float y))

let string_of_wt wt =
  match wt with
    Basic -> "Basic"
  | Grenader -> "Grenader"
  | MissileBlaster -> "MissileBlaster"
  | Miner -> "Miner"
  | PelletShooter -> "PelletShooter"
  | LazerGunner -> "LazerGunner"

let string_of_wep_type tp =
  match tp with
    Bomb -> "Bomb"
  | Grenade -> "Grenade"
  | Missile -> "Missile"
  | Mine -> "Mine"
  | Pellet -> "Pellet"
  | Lazer -> "Lazer"
  | Bat -> "Bat"

let string_of_game_result gr =
  match gr with
    Winner(c) -> string_of_color c
  | Tie -> "Tie"

let combine_args = String.concat cARGUMENT_SEPARATOR

let string_of_obstacle obs =
  match obs with
    Cloud(id, pos) ->
      combine_args ["Cloud"; string_of_int id; string_of_point pos]
  | Satellite(id,pos,hp) ->
      combine_args ["Satellite"; string_of_int id; string_of_point pos;
                    string_of_int hp]

let string_of_update update =
  match update with
  | InitGraphics(vlist) ->
      combine_args ("InitGraphics"::(List.map string_of_point vlist))
  | Countdown(num) -> combine_args ["Countdown"; string_of_int num]
  | DisplayString(col, talk) ->
      combine_args ["DisplayString"; string_of_color col; talk]
  | AddWorm(id, pos, health, col) ->
      combine_args ["AddWorm"; string_of_int id; string_of_point pos;
                    string_of_int health; string_of_color col]
  | MorphWorm(id, wt) ->
      combine_args ["MorphWorm"; string_of_int id; string_of_wt wt]
  | MoveWorm(id, pos) ->
      combine_args ["MoveWorm"; string_of_int id; string_of_point pos]
  | RemoveWorm(id) -> combine_args ["RemoveWorm"; string_of_int id]
  | AddProjectile(id, wt, pos) ->
      combine_args ["AddProjectile"; string_of_int id; string_of_wep_type wt;
                    string_of_point pos]
  | MoveProjectile(id, pos) ->
      combine_args ["MoveProjectile"; string_of_int id; string_of_point pos]
  | RemoveProjectile(id) ->
      combine_args ["RemoveProjectile"; string_of_int id]
  | UpdateScore(c, sc) ->
      combine_args ["UpdateScore"; string_of_color c; string_of_int sc]
  | GameOver(gr) -> combine_args ["GameOver"; string_of_game_result gr]
  | AddObstacle(obs) -> combine_args ["AddObstacle"; string_of_obstacle obs]
  | RemoveObstacle(id) -> combine_args ["RemoveObstacle"; string_of_int id]
  | DoBat(id) -> combine_args ["DoBat"; string_of_int id]
  | UpdateWorm(id, health) ->
      combine_args ["UpdateWorm"; string_of_int id; string_of_int health]

let parse_updates updates =
  Mutex.lock updates_lock;
  let string_fold acc update =
   (* let _ = print_endline (string_of_update update) in*)
      acc ^ string_of_update update ^ cUPDATE_SEPARATOR in
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
