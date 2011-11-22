open Hashtbl
open Definitions


val create_wormtable: unit -> (worm_id, worm_data) t
val create_projtable: unit -> (projectile_id, projectile_data) t 
val create_obstable: unit -> (obstacle_id, obstacle_data) t 
val create_timer: unit -> (timer ref)
