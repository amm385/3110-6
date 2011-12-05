open Team
open Definitions
open Constants
open State

let cTIME_STEP = 0.05
let cSTART_ANGLE = 0.001
let cSTART_SCALE = 2.0

(* (id,range where they will cause havoc) pairs *)
let projectiles = Hashtbl.create (cNUM_TEAMS * cTEAM_SIZE)
		
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
	if py <= groundy || t2 <= 0.0 then Some (px,groundy) else		
	let rawdrag = getDrag weapont in
	let drag = if inCloud px py then rawdrag +. cCLOUD_DRAG else rawdrag in

	let newax = -.drag *. vx in
	let neway = -.drag *. vx +. cGRAVITY in				
	let newvx = vx +. newax *. cTIME_STEP in
	let newvy = vy +. neway *. cTIME_STEP in
	let newpx = px +. newvx *. cTIME_STEP in
	let newpy = py +. newvy *. cTIME_STEP in
	
	let newt = match t with None -> t | Some(t3) -> Some(t3 +. cTIME_STEP) in
	
	if newpx < 0. || newpx > cBOARD_WIDTH 
	then None
	else
		projTracker (id,weapont,(newpx,newpy),(newvx,newvy),
			(newax,neway),newt)
		
(* Puts all projectiles not in the air into the projTable *)
let updateProjTable pt = 
	let helper (id,weapont,p,v,a,t) = 
		if Hashtbl.mem projectiles id then () else
		match projTracker (id,weapont,p,v,a,t) with
			None -> ()
		| Some(x,y) -> 
				let radius = getRadius weapont in			
					Hashtbl.add projectiles id (x -. radius, x +. radius) in
	List.iter helper pt
		
(* finds the closest worm in wormlst to position x (horz dist only *)		
let findClosestWorm x wormlst =
	let helper bestx (_,_,_,(px,py),_,_,_,_) = 
		if abs_float(px -. x) < abs_float(bestx -. x) then px else bestx in
	List.fold_left helper (-.cBOARD_WIDTH) wormlst

(* given a worm location and type, and a target, finds the 
 * v vector that gets it there (ignoring satellites)*)
let findShot wormtype px py tx =
	let (f,ineq1,ineq2) = 
		if px < tx (* true if worm is to the left of target *)
		then ((fun x -> x),(<=),(>=))
		else ((fun x -> (acos(-.1.) -. x)),(>=),(<=)) in
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
			let (vx,vy) = (f(mag *. (cos angle)), mag *. (sin angle)) in
			let _ = print_endline (string_of_float vx) in
			let _ = Thread.delay (0.1) in
			match projTracker (0,weapont,(px,py),(vx,vy),(0.0,0.0),None) with
				None -> setShot mag (angle *. scale) scale
			| Some(xhit,yhit) -> 
					if ineq1 ((xhit -. px)*.scale) ((tx -. px)*.scale)
					then (
						if ineq2 ((xhit -. px)*.scale) ((tx -. px +. tolerance)*.scale)
						then (vx,vy)
						else 
							let newScale = (5.0 -. scale) /. 4.0 in
								setShot mag (angle *. newScale) newScale
					)
					else setShot mag (angle *. scale) scale in
	setShot v cSTART_ANGLE cSTART_SCALE
	
(* checks if a point is in danger of being hit by a projectile,
 * if it is, returns the bounds of the danger zone
 * ba ba ba bum bum bunna bunna bun-na *)
let indanger px = 
	let helper id (min,max) (b,min',max') =
		if b then (b,min',max') else
		if min <= px && px <= max 
		then (true,min,max) 
		else (false,min',max') in
	Hashtbl.fold helper projectiles (false,0.,0.)
	
(* finds closest point to the left that is safe *)
let rec findleft px =
	let (b,min,max) = indanger px in
	if px = min then None else 
	if b then findleft min else Some px

(* find closest point to the right that is safe *)
let rec findright px = 
	let (b,min,max) = indanger px in
	if px = min then None else
	if b then findright max else Some px
	
(* finds a close, safe spot, given px *)
let findnewpos px = 
	match (findright px,findleft px) with
		(None,None) -> px (* update if you want to dodge bullets *)
	| (Some r,None) -> r
	| (None, Some l) -> l
	| (Some r, Some l) ->
		if abs_float(l -. px) < abs_float(r -. px)
		then l else r
	
(* main bot processor *)
let bot c = 
	while true do
		let (redteam,blueteam,proj_lst,obst_lst,timer) = 
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
			let _ = print_endline (string_of_float target) in
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