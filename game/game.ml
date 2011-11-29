open Hashtbl
open Definitions
open State
open Constants

		(* DATA STRUCTURES *)
type game = ((worm_id, worm_data) t * 
  (projectile_id, projectile_data) t * 
	(obstacle_id, obstacle_data) t * 
	(timer ref)) 
let gameLock = Mutex.create ()

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
		
let lineInter (x1,y1) (x2,y2) (x3,y3) (x4,y4) =
	let m1 = (y2 -. y1)/.(x2 -. x1) in
	let m2 = (y4 -. y3)/.(x4 -. x3) in
	let x = (m1*.x1 -. m2*.x3 -. y1 +. y3) /. (m1 -. m2) in
	let y = m1 *. (x -. x1) +. y1 in
		(x,y)
					
		(* ACTUAL FUNCTIONS *)
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
				(* Here we use vx to hide the team for friendly fire analysis 
						note, vx is otherwise useless *)
				let team = float_of_int worm_id in
				(!projectile_id_counter,Bat,pos,(team,0.),(0.,0.),None) in
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
				Mutex.unlock gameLock in
				
			Hashtbl.iter proj_helper pt; 
			
	(*ADD PROJECTILES*)
			(*the problem is helper gets a list*)
			 let proj_add_helper id proj_queue = 
			   let proj_queue_helper (pid,weapont,p,(vxproj,vyproj),a,t) = 
			     let (wid,wormtype,h,(px,py),(vx,vy),(ax,ay),t1,t2) = 
				     Hashtbl.find wt id in
				   if t1 <= 0 then 
							(*Will have to remove the proj from the queue*)
						 (Mutex.lock gameLock;
						  Hashtbl.add pt pid (pid,weapont,(px,py),(vx +. vxproj,vy +. vyproj),a,t);
              Mutex.unlock gameLock;)						 
           else ()	in
			   List.iter proj_queue_helper proj_queue in 
			
			Hashtbl.iter proj_add_helper futureProj;
			
	(* HANDLE EXPLOSIONS *)					
			(* the input "team" can be one of two things:
					a) the weapon is a bat and it is a positive int if
							the bat holder is red / negative int if blue
							-In this case team * id <=0 (line 48ish) 
								if the worm hit and the worm hitting are on opposite teams
					b) the weapon is not a bat and it is 0
							-In this case, team * id = 0 always and thus
								all worms in vicinity are hurt (friendly fire!) *)
								
			let explode x y weapont team projID = 
				let (r,dam) = 
					match weapont with 
						Bomb -> (cBOMB_EXPLOSION_RADIUS,cBOMB_DAMAGE)
					|	Grenade -> (cGRENADE_EXPLOSION_RADIUS,cGRENADE_DAMAGE)
					| Missile -> (cMISSILE_EXPLOSION_RADIUS,cMISSILE_DAMAGE)
					| Mine -> (cMINE_EXPLOSION_RADIUS,cMINE_DAMAGE)
					| Pellet -> (cPELLET_EXPLOSION_RADIUS,cPELLET_DAMAGE)
					| Lazer -> (cLAZER_EXPLOSION_RADIUS,cLAZER_DAMAGE)
					| Bat -> (cBAT_LENGTH,cBAT_DAMAGE) in
				
				(* bats don't do friendly fire; not yet implemented *)
				let hurter id (_,wormtype,h,(px,py),v,a,t1,t2) =
					let wormr = 
						(match wormtype with
							Basic -> cBASIC_RADIUS
						|	Grenader -> cGRENADER_RADIUS
						| MissileBlaster -> cMISSILE_BLASTER_RADIUS
						| Miner -> cMINER_RADIUS
						| PelletShooter -> cPELLET_SHOOTER_RADIUS
						| LazerGunner -> cLAZER_GUNNER_RADIUS
						) in
					if (sqrt ((px -. x)**2.0 +. (py -. y)**2.0) < (r +. wormr)
						&& (team * id <= 0 ))
					then (* hurt the worm *)
						(Mutex.lock gameLock;
						Hashtbl.replace wt id (id,wormtype,h - dam,(px,py),v,a,t1,t2);
						Mutex.unlock gameLock;)
					else () in
				
				Hashtbl.iter hurter wt;
				Mutex.lock gameLock;
				Hashtbl.remove pt projID; (* remove projectile from active projs *)
				Mutex.unlock gameLock;
				in
	
			let explodeHelper id (_,weapont,(px,py),(vx,vy),a,projt) =
				(* do something about bats too! *)
				match weapont with
					Bat -> (* just "explode" *) 
						(* reminder that vx for a bat is the team (pos or neg) *)
						explode px py weapont (int_of_float vx) id
						
				| (Grenade | Mine) -> (* explode if timer is up *)
						(match projt with 
								None -> failwith "timer of grenade/mine is None"
							| Some (flt) ->
								if flt -. newt +. t <= 0.
								then explode px py weapont 0 id
								else ())
				| _ -> (* explode if colliding *)
						if py <= (yfinder px) 
						then (* true if colliding *)
							match cBOARD with
								[] -> failwith "thar be an empty board!"
							| h::t ->
									let boundFinder ((x1,y1),(x2,y2),b) (x3,y3) =
										if b then ((x1,y1),(x2,y2),true) else 
											(if x2 <= px && x3 >= px
											then ((x2,y2),(x3,y3),true)
											else ((x2,y2),(x2,y2),false)) in
									let (l,r,_) = List.fold_left boundFinder (h,h,false) t in
									let (x,y) = lineInter l r (px,py) (px -. vx, py -. vy) in
									explode x y weapont 0 id;
						else () in
						
			Hashtbl.iter explodeHelper pt;
					
	(*REMOVE DEAD OBJECTS*)
			let worm_remove_helper id (_,_,health,_,_,_,_,_) = 
			  if health <= 0 then 
				  (Mutex.lock gameLock;
					 Hashtbl.remove wt id;
					 Mutex.unlock gameLock;)
				else () in
				
			Hashtbl.iter worm_remove_helper wt;
			
			let obstacle_remove_helper id obst =
        match obst with 
				  Cloud(_,_) -> ()
				| Satellite(_,p,health) -> 
            if health <= 0 then
             (Mutex.lock gameLock;
					    Hashtbl.remove ot id;
					    Mutex.unlock gameLock;)
				    else () in
      
			Hashtbl.iter obstacle_remove_helper ot 						
				
			
				
			
			
	