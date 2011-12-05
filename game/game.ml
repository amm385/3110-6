open Hashtbl
open Definitions
open State
open Constants
open Netgraphics

		(* DATA STRUCTURES *)
type game = ((worm_id, worm_data) t * 
  (projectile_id, projectile_data) t * 
	(obstacle_id, obstacle_data) t * 
	(timer ref) * (timer ref) )
let gameLock = Mutex.create ()
					

let initGame () : game = 
	send_update (InitGraphics(cBOARD));
  let obst = create_obstable() in
	let count = ref 0 in
	let helpercl elt =
		add_update (AddObstacle(Cloud(!count,elt)));
    Hashtbl.add obst !count (Cloud(!count,elt)); count := !count + 1 in
	let helpersat elt =
		add_update (AddObstacle(Satellite(!count,elt,cSATELLITE_HEALTH)));
    Hashtbl.add obst !count (Satellite(!count,elt,cSATELLITE_HEALTH)); 
	 count := !count + 1 in
	List.iter helpercl cCLOUD_POSITIONS;
  List.iter helpersat cSATELLITE_POSITIONS;	 
	(create_wormtable(),
		create_projtable(),
		obst,
		ref 0.0,ref 0.0) 

		
		
		
		
		
let initWorms (wt,pt,ot,t,starttime) = 
	let count = ref 1 in
	let addWorm team =
		(* wormID is positive for Red team, negative for Blue *)
		let id = 
			if team = Red
			then !count
			else -1 * !count in
		let xpos = 
			if team = Red
			then Random.float cRED_START
			else (Random.float (cBOARD_WIDTH -. cBLUE_START)) +. cBLUE_START in
		let ypos = yfinder xpos in
		Hashtbl.add wt id (id,Basic, cBASIC_HEALTH, (xpos,ypos), 
			(0.,0.),(0.,cGRAVITY), 0.0,-1.0);
		add_update (AddWorm(id,(xpos,ypos),cBASIC_HEALTH,team)); in
	let rec wormCycle m (team : color) : unit =
		if m = cTEAM_SIZE
		then ()
		else (addWorm team; count := !count + 1; wormCycle (m + 1) team) in
	wormCycle 0 Red;
	count := 1; 
	wormCycle 0 Blue
  
	
	
	
	
let startGame (wt,pt,ot,t,stime) = 
	let startTime = Unix.gettimeofday() in
	t := startTime;
  stime := startTime 	
	
	
	
	
	
	
let handleAction (wt,pt,ot,(t:timer ref),(starttime:timer ref)) worm_id act c = 
	if !t > (!starttime +. cTIME_LIMIT) then (print_endline "over"; Control(GameEnd)) else
	let (_,wormtype,hlth,pos,vel,a,t1,t2) = Hashtbl.find wt worm_id in
  match act with
		QueueShoot(v) ->
		  let (weapon,timer,projv,projpos,maxv) = 
				(match wormtype with
					Basic -> (Bomb,None,v,pos,cMAX_BOMB_MAGNITUDE)
				| Grenader -> 
					(Grenade,Some cGRENADE_DETONATION_TIME,v,pos,cMAX_GRENADE_MAGNITUDE)
				| MissileBlaster -> (Missile,None,v,pos,cMAX_MISSILE_MAGNITUDE)
					(* only for mines, v is position vector, not velocity *)
				| Miner -> (Mine,Some cMINE_DETONATION_TIME,(0.,0.),v,0.0)
				| PelletShooter -> (Pellet,None,v,pos,cMAX_PELLET_MAGNITUDE)
				| LazerGunner -> (Lazer,None,v,pos,cMAX_LAZER_MAGNITUDE)) in
			let projv = 
				if (not (weapon = Mine)) && maxv < (Util.mag projv) 
				then Util.scale (Util.normalize projv) maxv
				else projv in
			let accel = (0.,0.) in
			let p : projectile = 
				(!projectile_id_counter,weapon,projpos,projv,accel,timer) in
				let relList = 
					if Hashtbl.mem futureProj worm_id
					then Hashtbl.find futureProj worm_id
					else [] in
				Mutex.lock projLock;
				Hashtbl.replace futureProj worm_id (relList@[p]);
				Mutex.unlock projLock;
				Mutex.lock pidLock;
				projectile_id_counter := !projectile_id_counter + 1;
				Mutex.unlock pidLock;
				Result(worm_id,Success)
	| QueueMove(vx,vy) ->
			if vx < 0. || vx > cBOARD_WIDTH
			then 
				Result(worm_id,Failed)
			else
				let relList =
					(if Hashtbl.mem wormWaypoints worm_id
					then Hashtbl.find wormWaypoints worm_id
					else []) in
				Mutex.lock waypointLock;
				Hashtbl.replace wormWaypoints worm_id (relList@[(vx,vy)]);
				Mutex.unlock waypointLock;
				Result(worm_id,Success)
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
			Mutex.unlock pidLock;
			Result(worm_id,Success)
	| ClearShoot -> 
			Mutex.lock projLock;
			Hashtbl.replace futureProj worm_id [];
			Mutex.unlock projLock;
			Result(worm_id,Success)
	| ClearMove ->
			Mutex.lock waypointLock;
			Hashtbl.replace wormWaypoints worm_id [];
			Mutex.unlock waypointLock;
			Result(worm_id,Success)
	| Promote(newwormtype) -> 
		if not (wormtype = Basic)
		then Result(worm_id,Failed)
		else
		let newt = 
		 (match newwormtype with 
				Grenader -> cGRENADE_PROMOTION_TIME
			| MissileBlaster -> cMISSILE_PROMOTION_TIME
			| Miner -> cMINER_PROMOTION_TIME
			| PelletShooter -> cPELLET_SHOOTER_PROMOTION_TIME
			| LazerGunner -> cLAZER_GUNNER_PROMOTION_TIME
			| Basic -> failwith "Attempted to promote to basic??") in
		Mutex.lock gameLock;
		Hashtbl.replace wt worm_id (worm_id,newwormtype,hlth,pos,vel,a,t1,newt);
		Mutex.unlock gameLock;
		if Hashtbl.mem promotionTable worm_id 
		then Result(worm_id,Failed)
		else 
			(Mutex.lock promoLock;
			Hashtbl.add promotionTable worm_id newwormtype;
			Mutex.unlock promoLock;
			Result(worm_id,Success))
	| Talk(s) ->  
		add_update (DisplayString(c,s));
		Result(worm_id,Success)
		
	
	
	
let handleStatus (wt,pt,ot,(t:timer ref),(starttime:timer ref)) status = 
 match status with
		WormStatus(id) -> if Hashtbl.mem wt id then
  		 Data(WormData(Hashtbl.find wt id)) 
			else 
			 Error "Worm doesn't exist!" 
	| ProjectileStatus(pid) -> if Hashtbl.mem pt pid then
  		 Data(ProjectileData(Hashtbl.find pt pid)) 
			else 
			 Error "Projectile doesn't exist!" 
	| TeamStatus(c) -> 
	    let returnedWorms = ref [] in 
			let worm_returner _ (wid,wormt,h,p,v,a,t1,t2) = 
			   (match c with 
				   Red -> if wid > 0 then 
					   returnedWorms := (wid,wormt,h,p,v,a,t1,t2)::(!returnedWorms)
						 else ()
				|  Blue -> if wid < 0 then 
					   returnedWorms := (wid,wormt,h,p,v,a,t1,t2)::(!returnedWorms)
						 else () ) in
				Hashtbl.iter worm_returner wt;
        Data(TeamData (score c,!returnedWorms))
	| ObstacleStatus -> 
	  let obstacle_returner oid elt acc = 
  		elt::acc in
		Data(ObstacleData(Hashtbl.fold obstacle_returner ot [])) 
	| GameStatus -> 
	  let redData = (score Red, hash2listw Red wt) in
	  let blueData = (score Blue, hash2listw Blue wt) in
		let proj_list = hash2list pt in
		let obstacle_list = hash2list ot in
		Data(GameData (redData,blueData,proj_list,obstacle_list,!t))

		

let testbool = ref true		
		
		

let handleTime (wt,pt,ot,t,startedtime) newt = (* how do we update the game time? *)
	(* CHECK FOR GAME END *)
  if newt -. !startedtime >= cTIME_LIMIT
	then
		let result = 
			if score Red > score Blue then Winner Red
				else if score Red < score Blue then Winner Blue else Tie in
		add_update (GameOver(result));
		Some result
	else
		let (r,b) = checkAlive wt in
		if r = 0 then (add_update (GameOver(Winner Blue)); Some(Winner Blue)) else
		if b = 0 then (add_update (GameOver(Winner Red)); Some(Winner Red)) else
		
	(* HANDLE PROMOTIONS *)
			let promo_helper id (_,wormtype,h,p,v,a,t1,t2) = 
				let next = t2 -. newt +. !t in
				if wormtype = Basic && next <= 0. 
				then (*perform upgrade*)
					if Hashtbl.mem promotionTable id
					then let newwt = Hashtbl.find promotionTable id in
						(Mutex.lock gameLock;
						let ratio = (float_of_int h) /. (1.) in
						let newHealth = int_of_float((1.) *. ratio) in
						Hashtbl.replace wt id (id,newwt,newHealth,p,v,a,t1,next);
						Mutex.unlock gameLock;
						add_update (MorphWorm(id,newwt));)
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
					let speedtype = getSpeed wormtype in  
					let speed = if hx >= px then speedtype else -.speedtype in
					let newx = px +. (newt -. !t)*.speed in
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
					Mutex.unlock gameLock;
					add_update (MoveWorm(id,(newx,newy))); in	
			Hashtbl.iter pos_helper wormWaypoints;
	
	(* PROJECTILE POSITIONS *)
			let proj_helper id (_,weapont,(px,py),(vx,vy),(ax,ay),projt) = 
				  let rawdrag = 
				  match weapont with 
            Bomb -> cBOMB_DRAG
          | Grenade -> cGRENADE_DRAG
					| Missile -> cMISSILE_DRAG
					| Mine -> 0.0
					| Pellet -> cPELLET_DRAG
					| Lazer -> cLAZER_DRAG 
					| Bat -> 0.0 in
					
				let drag = if inCloud px py then rawdrag +. cCLOUD_DRAG else rawdrag in
				(*subject to change depending on the agreed formula*)
				let newax = -.drag *. vx in
        let neway = -.drag *. vx +. cGRAVITY in				
				let newvx = vx +. newax *. (newt -. !t) in
				let newvy = vy +. neway *. (newt -. !t) in
			  let newpx = px +. newvx *. (newt -. !t) in
				let newpy = py +. newvy *. (newt -. !t) in
				
				Mutex.lock gameLock;
				(if newpx < 0.0 || newpx > cBOARD_WIDTH
				then Hashtbl.remove pt id
				else (Hashtbl.replace pt id 
					(id,weapont,(newpx,newpy),(newvx,newvy),(newax,neway),projt)));
				Mutex.unlock gameLock;
				add_update (MoveProjectile(id,(newpx,newpy))); in
				
			Hashtbl.iter proj_helper pt;
			
	(* UPDATE COOLDOWN TIMES *)
			let cooldownUpdater id (_,wormtype,h,p,v,a,t1,t2) =
				let newt1 = t1 -. newt +. !t in
				Mutex.lock gameLock;
				Hashtbl.replace wt id (id,wormtype,h,p,v,a,newt1,t2);
				Mutex.unlock gameLock; in
			Hashtbl.iter cooldownUpdater wt;
			
	(*ADD PROJECTILES*)
			 let proj_add_helper id proj_queue = 
			   let proj_queue_helper (pid,weapont,(x,y),(vxproj,vyproj),a,t) = 
					 print_endline "pre";
					 let (wid,wormtype,h,(px,py),(vx,vy),(ax,ay),t1,t2) = 
				     Hashtbl.find wt id in
					 let _ = print_endline "post" in
				   if t1 <= 0. 
					 then (
						 Mutex.lock projLock;
						 let relList = Hashtbl.find futureProj wid in
							Hashtbl.replace futureProj wid (List.tl relList);
						  Mutex.unlock projLock;
							(if weapont = Mine &&
								(Util.mag (x -. px,(yfinder x) -. py) >= cMINE_PLANT_DISTANCE)
							then () (* mine planted outside proximity *)
							else (
								let cooldownt = getCooldown wormtype in
								Mutex.lock gameLock;
								Hashtbl.add pt pid 
									(pid,weapont,(px,py),(vx +. vxproj,vy +. vyproj),a,t);
								Hashtbl.replace wt wid 
									(wid,wormtype,h,(px,py),(vx,vy),(ax,ay),cooldownt,t2);
								Mutex.unlock gameLock;
								(if weapont = Bat
								then ()
								else add_update (AddProjectile(pid,weapont,(px,py))));
							));
						)						 
           else () in
			   List.iter proj_queue_helper proj_queue in 
			
			Hashtbl.iter proj_add_helper futureProj;
			
	(* HANDLE EXPLOSIONS *)	
			let checkForSatellite x y r pID = 
				let satCollision oID obst =
					match obst with
						Cloud(_,p) -> ()
					| Satellite(_,(px,py),h) -> 
						if (Util.mag (px -. x,py -. y)) <= (r +. cSATELLITE_SIZE)
						(* satellite hit! *)
						then (
							Mutex.lock gameLock;
							(if h = 1 
							then 
								(Hashtbl.remove ot oID;
								add_update (RemoveObstacle(oID));)
							else Hashtbl.replace ot oID (Satellite(oID,(px,py),h - 1)));
							Hashtbl.remove pt pID;
							Mutex.unlock gameLock;
							add_update (RemoveProjectile(pID));
						)
						else () in
						
				Hashtbl.iter satCollision ot in
				
			(* the input worm_id can be one of two things:
					a) the weapon is a bat and it is a positive int if
							the bat holder is red / negative int if blue 
							-In this case worm_id * id <=0 (line 48ish) 
								if the worm hit and the worm hitting are on opposite teams
					b) the weapon is not a bat and it is 0
							-In this case, worm_id * id = 0 always and thus
								all worms in vicinity are hurt (friendly fire!) *)
								
			let explode x y weapont worm_id projID = 
				let (r,dam) = 
					match weapont with 
						Bomb -> (cBOMB_EXPLOSION_RADIUS,cBOMB_DAMAGE)
					|	Grenade -> (cGRENADE_EXPLOSION_RADIUS,cGRENADE_DAMAGE)
					| Missile -> (cMISSILE_EXPLOSION_RADIUS,cMISSILE_DAMAGE)
					| Mine -> (cMINE_EXPLOSION_RADIUS,cMINE_DAMAGE)
					| Pellet -> (cPELLET_EXPLOSION_RADIUS,cPELLET_DAMAGE)
					| Lazer -> (cLAZER_EXPLOSION_RADIUS,cLAZER_DAMAGE)
					| Bat -> (cBAT_LENGTH,cBAT_DAMAGE) in
				
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
					if (Util.mag (px -. x,py -. y) <= (r +. wormr)
						&& (worm_id * id <= 0 ))
					then (* hurt the worm *)
						(Mutex.lock gameLock;
						Hashtbl.replace wt id (id,wormtype,h - dam,(px,py),v,a,t1,t2);
						print_endline (string_of_int (h - dam));
						Mutex.unlock gameLock;)
					else () in
				
				Hashtbl.iter hurter wt;
				Mutex.lock gameLock;
				Hashtbl.remove pt projID; (* remove projectile from active projs *)
				Mutex.unlock gameLock;
				(if weapont = Bat
				then add_update(DoBat(worm_id)) 
				else add_update(RemoveProjectile(projID)));
				in
	
			let explodeHelper id (_,weapont,(px,py),(vx,vy),a,projt) =
				let radius = getRadius weapont in
				match weapont with
					Bat -> (* just "explode" *) 
						(* reminder that vx for a bat is the worm_id (pos or neg) *)
						explode px py weapont (int_of_float vx) id
				| (Grenade | Mine) -> (* explode if timer is up *)
						(match projt with 
								None -> failwith "timer of grenade/mine is None"
							| Some (flt) ->
								if flt -. newt +. !t <= 0.
								then explode px py weapont 0 id
								else checkForSatellite px py radius id)
				| _ -> (* explode if colliding *)
						if (py <= (yfinder px))
						then
							(match cBOARD with
								[] -> failwith "thar be an empty board!"
							| h::t ->
									let boundFinder ((x1,y1),(x2,y2),b) (x3,y3) =
										if b then ((x1,y1),(x2,y2),true) else 
											(if x2 <= px && x3 >= px
											then ((x2,y2),(x3,y3),true)
											else ((x2,y2),(x2,y2),false)) in
									let (l,r,_) = List.fold_left boundFinder (h,h,false) t in
									let (x,y) = lineInter l r (px,py) (px -. vx, py -. vy) in
									explode x y weapont 0 id;)
						else 
							checkForSatellite px py radius id in
						
			Hashtbl.iter explodeHelper pt;
					
	(*REMOVE DEAD OBJECTS*)
		(* UPDATE SCORE AND SEND GUI UPDATE OF SCORE *)
			let worm_remove_helper id (_,_,health,_,_,_,_,_) = 
			  if health <= 0 then 
				  (Mutex.lock gameLock;
					print_endline "killing!";
					let (wiid,wormt,_,_,_,_,_,_) = Hashtbl.find wt id in
					let (rscore,bscore) = !scores in
					let addScore identity amount = 
					  if identity < 0 
						then scores := (rscore + amount,bscore) 
						else scores := (rscore,bscore  + amount) in
					(match wormt with 
					   Basic -> addScore id cBASIC_KILL_SCORE
					| Grenader -> addScore id cGRENADER_KILL_SCORE
					| MissileBlaster -> addScore id cMISSILE_KILL_SCORE
					| Miner -> addScore id cMINER_KILL_SCORE
					| PelletShooter -> addScore id cPELLET_KILL_SCORE
					| LazerGunner -> addScore id cLAZER_KILL_SCORE
					);
					Hashtbl.remove wt id;
					Mutex.unlock gameLock;
					add_update (RemoveWorm(id));)
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
      
			Hashtbl.iter obstacle_remove_helper ot;
			t := newt;
			None 