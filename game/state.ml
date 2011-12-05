open Constants
open Definitions

let create_wormtable () = Hashtbl.create (cNUM_TEAMS * cTEAM_SIZE)
let create_projtable () = Hashtbl.create  (cNUM_TEAMS * cTEAM_SIZE)
let create_obstable () = Hashtbl.create ((List.length cCLOUD_POSITIONS) + 
	(List.length cSATELLITE_POSITIONS))

(*first one is Red, second one is blue score*)
(*protected by gameLock*)
let scores = ref (0,0)
let projectile_id_counter = ref 0
let pidLock = Mutex.create ()
	(* type (wormId,vector list) t *)
let wormWaypoints = Hashtbl.create (cNUM_TEAMS * cTEAM_SIZE)
let waypointLock = Mutex.create()
	(* type (wormId,projectile list) t *)
let futureProj = Hashtbl.create (cNUM_TEAMS * cTEAM_SIZE)
let projLock = Mutex.create()
	(* type (wormId,wormtype) t *)
let promotionTable = Hashtbl.create (cNUM_TEAMS * cTEAM_SIZE)
let promoLock = Mutex.create()

 		(* HELPER FUNCTIONS *)
(* Returns the y value on the map ground associated with xpos *)
let hash2list hashtable = 
  let helper id elt acc = 
	  elt::acc in 
	Hashtbl.fold helper hashtable [] 
	
let hash2listw c hashtable = 
  let helper id (wid,wormt,h,p,v,a,t1,t2) acc = 
	  let elt = (wid,wormt,h,p,v,a,t1,t2) in
	  if  c = Red then
     if wid > 0 then elt::acc else acc
    else
     if wid < 0 then elt::acc else acc in		
		 
	Hashtbl.fold helper hashtable [] 

let yfinder xpos = 
  let lineFinder ((x1,y1),(x2,y2),b) (x3,y3) = 
		if b then ((x1,y1),(x2,y2),true) else 
			(if x2 <= xpos && x3 >= xpos
			then ((x2,y2),(x3,y3),true)
			else ((x2,y2),(x2,y2),false)) in
	let calcY (x1,y1) (x2,y2) =
		let m = (y2 -. y1) /. (x2 -. x1) in
  		m *. (xpos -. x1) +. y1 in
	let (l,r,_) =
    match cBOARD with
      h::t ->
        List.fold_left lineFinder (h,h,false) t 
    | _ -> failwith "board is all funky" in
	  calcY l r 
		
let lineInter (x1,y1) (x2,y2) (x3,y3) (x4,y4) =
	let m1 = (y2 -. y1)/.(x2 -. x1) in
	let m2 = (y4 -. y3)/.(x4 -. x3) in
	let x = (m1*.x1 -. m2*.x3 -. y1 +. y3) /. (m1 -. m2) in
	let y = m1 *. (x -. x1) +. y1 in
		(x,y)
		
let checkAlive tbl = 
	let helper id (id2,wt,h,p,v,a,t1,t2) (r,b) = 
		if id > 0 && h > 0 then (r+1,b) else
		if id < 0 && h > 0 then (r,b+1) else 
			(r,b) in
	Hashtbl.fold helper tbl (0,0)
	
let score c =
	match c with 
    Red -> fst !scores
	| Blue -> snd !scores

let inCloud x y = 
  let helper acc (cx,cy) = 
	  if Pervasives.sqrt((cx -. x) ** 2.0 +. (cy -. y) ** 2.0)  < cCLOUD_RADIUS
		then acc || true
		else acc || false in
	List.fold_left helper false cCLOUD_POSITIONS 
	
(* some worm info extractions *)
	
let getCooldown wormtype = 
	match wormtype with
		Basic -> cBASIC_ATTACK_COOLDOWN
	| Grenader -> cGRENADER_COOLDOWN
	| MissileBlaster -> cMISSILE_BLASTER_COOLDOWN
	| Miner -> cMINER_COOLDOWN
	| LazerGunner -> cLAZER_GUNNER_COOLDOWN
	| PelletShooter -> cPELLET_SHOOTER_COOLDOWN
	
let getDrag weapont = 
	match weapont with 
		Bomb -> cBOMB_DRAG
	| Grenade -> cGRENADE_DRAG
	| Missile -> cMISSILE_DRAG
	| Mine -> 0.0
	| Pellet -> cPELLET_DRAG
	| Lazer -> cLAZER_DRAG 
	| Bat -> 0.0 
	
let getRadius weapont = 
	match weapont with 
		Bomb -> cBOMB_EXPLOSION_RADIUS
	|	Grenade -> cGRENADE_EXPLOSION_RADIUS
	| Missile -> cMISSILE_EXPLOSION_RADIUS
	| Mine -> cMINE_EXPLOSION_RADIUS
	| Pellet -> cPELLET_EXPLOSION_RADIUS
	| Lazer -> cLAZER_EXPLOSION_RADIUS
	| Bat -> cBAT_LENGTH
	
let getSpeed wormtype = 
	match wormtype with
		Basic -> cBASIC_SPEED
	| Grenader -> cGRENADER_SPEED
	| MissileBlaster -> cMISSILE_BLASTER_SPEED
	| Miner -> cMINER_SPEED
	| PelletShooter -> cPELLET_SHOOTER_SPEED
	| LazerGunner -> cLAZER_GUNNER_SPEED 
	
let getHealth wormtype = 
  match wormtype with 
	  Basic -> cBASIC_HEALTH
	| Grenader -> cGRENADER_HEALTH
 	|	MissileBlaster -> cMISSILE_BLASTER_HEALTH
	|	Miner -> cMINER_HEALTH
	|	PelletShooter -> cPELLET_SHOOTER_HEALTH
	|	LazerGunner -> cLAZER_GUNNER_HEALTH
		