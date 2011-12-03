open Hashtbl
open Definitions


val create_wormtable: unit -> (worm_id, worm_data) t
val create_projtable: unit -> (projectile_id, projectile_data) t 
val create_obstable: unit -> (obstacle_id, obstacle_data) t 

val scores: (int*int) ref
val projectile_id_counter: int ref
val pidLock: Mutex.t
val wormWaypoints: (worm_id,vector list) Hashtbl.t
val waypointLock: Mutex.t
val futureProj: (worm_id,projectile list) Hashtbl.t
val projLock: Mutex.t
val promotionTable: (worm_id,worm_type) Hashtbl.t
val promoLock: Mutex.t

val hash2list: ('a,'b) Hashtbl.t -> 'b list
val hash2listw: color -> (worm_id,worm_data) Hashtbl.t -> worm_data list
val yfinder: float -> float
val lineInter: (float*float) -> (float*float) -> (float*float) ->
	(float*float) -> (float*float)
val checkAlive: (worm_id,worm_data) Hashtbl.t -> (int*int)
val score: color -> int
val inCloud: float -> float -> bool 