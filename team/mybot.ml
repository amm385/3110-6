open Team
open Definitions
open Constants
open State

let cTIME_STEP = 0.05
let cSTART_ANGLE = 0.001
let cSTART_SCALE = 2.0

(* (id,range where they will cause havoc) pairs *)
let projectiles = create_projtable ()

(* Puts all projectiles not in the air into the projTable *)
let updateProjTable pt = 
	let helper (id,weapont,p,v,a,t) = 
		if Hashtbl.mem projectiles id then () else
		let (x,y) = projTracker (id,weapont,p,v,a,t) in
		let radius = getRadius weapont in
			Hashtbl.add projectiles id (x -. radius, x +. radius) in
	List.iter helper pt
		
(* returns true if the worm can outrun the projectile *)
let canEscape px weapont wx wormtype time =
	let speed = getSpeed wormtype in
	let radius = getRadius weapont in
	((speed *. time) +. wx > px +. radius
		|| (speed *. time) -. wx > px -. radius)

(* Given a projectile, finds out where it will explode *)
let rec projTracker (id,weapont,(px,py),(vx,vy),(ax,ay),t) = 
	let t2 = match t with None -> 1.0 | Some(t') -> t' in
	let groundy = yfinder px in
	if py <= groundy || t2 <= 0.0 then (px,groundy) else		
	let rawdrag = getDrag weapont in
	let drag = if inCloud px py then rawdrag +. cCLOUD_DRAG else rawdrag in

	let newax = -.drag *. vx in
	let neway = -.drag *. vx +. cGRAVITY in				
	let newvx = vx +. newax *. cTIME_STEP in
	let newvy = vy +. neway *. cTIME_STEP in
	let newpx = px +. newvx *. cTIME_STEP in
	let newpy = py +. newvy *. cTIME_STEP in
	
	let newt = match t with None -> t | Some(t3) -> Some(t3 +. cTIME_STEP) in
	
	projTracker (id,weapont,(newpx,newpy),(newvx,newvy),
		(newax,neway),newt)
		
(* finds the closest worm in wormlst to position x (horz dist only *)		
let findClosestWorm x wormlst =
	let helper bestx (_,_,_,(px,py),_,_,_,_) = 
		if (px -. x) < (bestx -. x) then px else bestx in
	List.fold_left helper (-.cBOARD_WIDTH) wormlst

(* given a worm location and type, and a target, finds the 
 * v vector that gets it there (ignoring satellites)*)
let findShot wormtype px py tx =
	let (weapont,v,tolerance) = 
		match wormtype with
			Basic -> Bomb,cMAX_BOMB_MAGNITUDE,cBOMB_EXPLOSION_RADIUS
		| Grenader -> Grenade,cMAX_GRENADE_MAGNITUDE,cGRENADE_EXPLOSION_RADIUS
		| MissileBlaster -> Missile,cMAX_MISSILE_MAGNITUDE,cMISSILE_EXPLOSION_RADIUS
		| Miner -> Mine,0.0,0.0
		| PelletShooter -> Pellet,cMAX_PELLET_MAGNITUDE,cPELLET_EXPLOSION_RADIUS
		| LazerGunner -> Lazer,cMAX_LAZER_MAGNITUDE,cLAZER_EXPLOSION_RADIUS in
	(* start shot at 0deg, increase til hit or 45 *)
	let rec setShot mag angle scale =
		if angle >= (acos 0.0) (* acos 0.0 = pi/2 *)
		then Util.normalize (1.0,1.0)
		else
			let (vx,vy) = (mag *. (cos angle), mag *. (sin angle)) in
			let (xhit,yhit,_) = 
				projTracker (0,weapont,(px,py),(vx,vy),(0.0,0.0),None) in
			if (xhit -. px)*.scale >= (tx -. px)*.scale
			then (
				if (xhit -. px)*.scale <= (tx -. px +. tolerance)*.scale
				then (vx,vy)
				else 
					let newScale = (5.0 -. scale) /. 4.0 in
						setShot mag (angle *. newScale) newScale
			)
			else setShot mag (angle *. scale) scale in
	setShot v cSTART_ANGLE cSTART_SCALE
	
(* checks if a point is in danger of being hit by a projectile *)
let indanger px = 
	let helper id (min,max) b =
		(min <= px && px <= max) || b
	Hashtbl.fold helper projectiles false
	
(* finds a close, safe spot, given px *)
let findnewpos px = 
	failwith "do"
	(* check current spot, if safe, use,
	 *	else, store two ends of danger zone and check
	 *  closest one for safety...recursive! *)
	
	
let bot c = 
	while true do
		let (blueteam,redteam,proj_lst,obst_lst,timer) = 
			match get_status (GameStatus) with
				GameData(a) -> a
			| _ -> failwith "blehhh" in 
		let ((scoreA,wormsA),(scoreB,wormsB)) = (* we are teamA *)
			if c = Red then (redteam,blueteam) else (blueteam,redteam) in			
		updateProjTable proj_lst;
		let wormCycle (id,wormtype,h,(px,py),v,a,t1,t2) =
			(* move to a safe location *)
			let newpos = findnewpos px in
			let _ = send_action (QueueMove(newpos,yfinder newpos)) id in

			(* make best shot possible *)
			let target = findClosestWorm px wormsB in
			if abs_float (target -. px) <= cBAT_LENGTH
			then 
				let _ = send_action QueueBat id in ()
			else (
				let shotVector = findShot wormtype px py target in
				let _ = send_action (QueueShoot(shotVector)) id in 
					()
			) in
		List.iter wormCycle wormsA;
	done
	
let () = start_bot bot