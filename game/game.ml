open Hashtbl
open Definitions
open State
open Constants


type game = ((worm_id, worm_data) t * 
  (projectile_id, projectile_data) t * 
	(obstacle_id, obstacle_data) t * 
	(timer ref)) 
let gameLock = Mutex.create ()

let yfinder xpos = 
  let lineFinder ((x1,y1),(x2,y2),b) (x3,y3) = 
		if b then ((x1,y1),(x2,y2),true) else 
			(if x2 <= xpos && x3 >= xpos
			then ((x2,y2),(x3,y3),true)
			else ((x2,y2),(x2,y2),false)) in
	let calcY (x1,y1) (x2,y2) =
		let m = (y2 -. y1) /. (x2 -. x1) in
  		m *. (xpos -. x1) +. y1 in
	let (l,r,_) = List.fold_left lineFinder ((0.,0.),(0.,0.),false) cBOARD in
	  calcY l r 
	
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

let initGame () : game = 
   let obst = create_obstable() in
	 let count = ref 0 in
	 let helpercl elt =
     Hashtbl.add obst !count (Cloud(!count,elt)); count := !count + 1 in
	 let helpersat elt =
     Hashtbl.add obst !count (Satellite(!count,elt,cSATELLITE_HEALTH)); 
		 count := !count + 1 in
	 List.iter helpercl cCLOUD_POSITIONS;
   List.iter helpersat cSATELLITE_POSITIONS;	 
	(create_wormtable(),
		create_projtable(),
		obst,
		create_timer()) 

		(* Team 0 = Red, Team 1 = Blue *)
let initWorms (wt,pt,ot,t) = 
	let count = ref 1 in
	let addWorm team =
		(* wormID is positive for Red team, negative for Blue *)
		let id = 
			if team = 0
			then !count
			else -1 * !count in
		let xpos = 
			if team = 0 
			then Random.float cRED_START
			else (Random.float (cBOARD_WIDTH -. cBLUE_START)) +. cBLUE_START in
		let ypos = yfinder xpos in
		Hashtbl.add wt id (id,Basic, cBASIC_HEALTH, (xpos,ypos), 
			(0.,0.),(0.,cGRAVITY), cBASIC_ATTACK_COOLDOWN,-1.0) in
	let rec wormCycle m team =
		if m = cTEAM_SIZE
		then ()
		else addWorm team; count := !count + 1; wormCycle (m + 1) team in
  let rec teamCycle n =
		if n = cNUM_TEAMS 
		then ()
		else wormCycle 0 n; count := 1; teamCycle (n + 1) in
	teamCycle 0
  
let startGame g = 
  failwith "How can you make a worm happy? 
  Cut off his tail, he'll be delighted!"
	
 
	
let handleAction (wt,pt,ot,t) worm_id act c = 
	let (_,wormtype,hlth,pos,vel,a,t1,t2) = Hashtbl.find wt worm_id in
  match act with
		QueueShoot(v) ->
		 (let (weapon,timer) = 
				(match wormtype with
					Basic -> (Bomb,None)
				| Grenader -> (Grenade,Some cGRENADE_DETONATION_TIME)
				| MissileBlaster -> (Missile,None)
				| Miner -> (Mine,Some cMINE_DETONATION_TIME)
				| PelletShooter -> (Pellet,None)
				| LazerGunner -> (Lazer,None)) in
			 (* accel needs to be figured out *)
			let accel = (0.,0.) in
			let p : projectile = 
				(!projectile_id_counter,weapon,pos,v,accel,timer) in
				(try 
					let relList = Hashtbl.find futureProj worm_id in
						Mutex.lock projLock;
						Hashtbl.replace futureProj worm_id (relList@[p]);
						Mutex.unlock projLock;
				with Not_found -> 
					Mutex.lock projLock;
					Hashtbl.add futureProj worm_id [p]);
					Mutex.unlock projLock;
				Mutex.lock pidLock;
				projectile_id_counter := !projectile_id_counter + 1;
				Mutex.unlock pidLock)
	| QueueMove(v) ->
			(* set worms speed, vx *)
			let relList = Hashtbl.find wormWaypoints worm_id in
				Mutex.lock waypointLock;
				Hashtbl.replace wormWaypoints worm_id (relList@[v]);
				Mutex.unlock waypointLock
	| QueueBat -> 
			let p : projectile = 
				(!projectile_id_counter,Bat,pos,(0.,0.),(0.,0.),None) in
			(try
				let relList = Hashtbl.find futureProj worm_id in
					Mutex.lock projLock;
					Hashtbl.replace futureProj worm_id (relList@[p]);
					Mutex.unlock projLock;
			with Not_found ->
				Mutex.lock projLock;
				Hashtbl.add futureProj worm_id [p]);
				Mutex.unlock projLock;
			Mutex.lock pidLock;
			projectile_id_counter := !projectile_id_counter + 1;
			Mutex.unlock pidLock
	| ClearShoot -> 
			Mutex.lock projLock;
			Hashtbl.replace futureProj worm_id [];
			Mutex.unlock projLock;
	| ClearMove ->
			Mutex.lock waypointLock;
			Hashtbl.replace wormWaypoints worm_id [];
			Mutex.unlock waypointLock;
	| Promote(wormtype) -> 
		let newt = 
		 (match wormtype with 
				Grenader -> cGRENADE_PROMOTION_TIME
			| MissileBlaster -> cMISSILE_PROMOTION_TIME
			| Miner -> cMINER_PROMOTION_TIME
			| PelletShooter -> cPELLET_SHOOTER_PROMOTION_TIME
			| LazerGunner -> cLAZER_GUNNER_PROMOTION_TIME
			| Basic -> failwith "Attempted to promote to basic??") in
		Mutex.lock gameLock;
		Hashtbl.replace wt worm_id (worm_id,wormtype,hlth,pos,vel,a,t1,newt);
		Mutex.unlock gameLock;
		if Hashtbl.mem promotionTable worm_id 
		then () 
		else 
			Mutex.lock promoLock;
			Hashtbl.add promotionTable worm_id wormtype;
			Mutex.unlock promoLock;
	| Talk(s) ->  Netgraphics.add_update (DisplayString(c,s))
	
let handleStatus g status = failwith "poop"
 (* match status with
		WormStatus(id) -> 
	| ProjectileStatus(d) ->
	| TeamStatus(c) ->
	| ObstacleStatus ->
	| GameStatus ->*)

let checkAlive tbl = 
	let helper id (id2,wt,h,p,v,a,t1,t2) (r,b) = 
		if id > 0 && h > 0 then (r+1,b) else
		if id < 0 && h > 0 then (r,b+1) else 
			(r,b) in
	Hashtbl.fold helper tbl (0,0)
	
let score c =
	failwith "poopy"
	

let inCloud x y = 
  let helper acc (cx,cy) = 
	  if Pervasives.sqrt((cx -. x) ** 2.0 +. (cy -. y) ** 2.0)  < cCLOUD_RADIUS
		then acc || true
		else acc || false in
	List.fold_left helper false cCLOUD_POSITIONS 


	
let handleTime (wt,pt,ot,t) newt = 
	(* CHECK FOR GAME END *)
  if newt >= cTIME_LIMIT
	then
		Some (if score Red > score Blue then Winner Red
			else if score Red < score Blue then Winner Blue else Tie)
	else
		let (r,b) = checkAlive wt in
		if r = 0 then Some(Winner Blue) else
		if b = 0 then Some(Winner Red) else
	(* HANDLE PROMOTIONS *)
			let promo_helper id (id2,wormtype,h,p,v,a,t1,t2) = 
				let next = t2 -. newt +. t in
				if wormtype = Basic && next <= 0. 
				then (*perform upgrade*)
					if Hashtbl.mem promotionTable id
					then let newwt = Hashtbl.find promotionTable id in
						(Mutex.lock gameLock;
						Hashtbl.replace wt id (id,newwt,h,p,v,a,t1,next);
						Mutex.unlock gameLock;)
					else ()
				else 
					(Mutex.lock gameLock;
					Hashtbl.replace wt id (id,wormtype,h,p,v,a,t1,next);
					Mutex.unlock gameLock) in
			Hashtbl.iter promo_helper wt;
			
	(* HANDLE POSITIONS *)
			let pos_helper id lst =
				match lst with
					[] -> ()
				| (hx,hy)::tl -> 
					let (id2,wormtype,h,(px,py),v,a,t1,t2) = Hashtbl.find wt id in
					let speedtype = 
					  (match wormtype with 
						   Basic -> cBASIC_SPEED
						 | Grenader -> cGRENADER_SPEED
						 | MissileBlaster -> cMISSILE_BLASTER_SPEED
						 | Miner -> cMINER_SPEED
						 | PelletShooter -> cPELLET_SHOOTER_SPEED
						 | LazerGunner ->  cLAZER_GUNNER_SPEED
					  ) in  
					let speed = if hx >= px then speedtype else -.speedtype in
					let newx = px +. (newt -. t)*.speed in
					let finalx = 
						if (newx >= hx && px <= hx) || (newx <= hx && px >= hx)
						then 
							(Mutex.lock waypointLock;
							Hashtbl.replace wormWaypoints id tl;
							Mutex.unlock waypointLock;
							hx)
						else newx in
					let newy = yfinder finalx in
					let newspeed = 
					  (match tl with 
						  [] -> 0.
						| (x,y)::t -> if x <= newx then -.speedtype else speedtype) in
					Mutex.lock gameLock;
					Hashtbl.replace wt id 
						(id,wormtype,h,(newx,newy),(newspeed,0.),a,t1,t2);
					Mutex.unlock gameLock; in	
			Hashtbl.iter pos_helper wormWaypoints;
			
	(* PROJECTILE POSITIONS *)
			 
			let proj_helper id (_,weapont,(px,py),(vx,vy),(ax,ay),projt) = 
				  let rawdrag = 
				  match weapont with 
            Bomb -> cBOMB_DRAG
          | Grenade -> cGRENADE_DRAG
					| Missile -> cMISSILE_DRAG
					| Mine -> cMINE_DRAG
					| Pellet -> cPELLET_DRAG
					| Lazer -> cLAZER_DRAG 
					| Bat -> 0.0 in
					
				let drag = if inCloud px py then rawdrag +. cCLOUD_DRAG else rawdrag in
				(*subject to change depending on the agreed formula*)
				let newax = -.drag *. vx in
        let neway = -.drag *. vx +. cGRAVITY in				
				let newvx = vx +. newax *. (newt -. t) in
				let newvy = vy +. neway *. (newt -. t) in
			  let newpx = px +. newvx *. (newt -. t) in
				let newpy = py +. newvy *. (newt -. t) in
				Mutex.lock gameLock;
				Hashtbl.replace pt id (id,weapont,(newpx,newpy),(newvx,newvy),(newax,neway),projt);
				Mutex.lock gameLock in
				
			Hashtbl.iter proj_helper pt; 
			
			(*ADD PROJECTILES*)
			(*the problem is helper gets a list*)
			let proj_add_helper id (pid,weapont,p,(vxproj,vyproj),a,t) = 
			  let (wid,wormtype,h,(px,py),(vx,vy),(ax,ay),t1,t2) = 
				  Hashtbl.find wt id in
				if t1 <= 0 then 
				  Hashtbl.add pt pid (pid,weapont,(px,py),(vx +. vxproj,vy +. vyproj),a,t) 
        else ()	in
			
			Hashtbl.iter proj_add_helper futureProj;
			
	