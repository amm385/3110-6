open Team
open Definitions
open Constants
open State

let cTIME_STEP = 0.05
let cSTART_ANGLE = 0.001
let cSTART_SHIFT = 0.2
let cTOLERANCE = 5.0
let pi4 = 0.785398163

(* (id,range where they will cause havoc) pairs *)
let projectiles = Hashtbl.create (cNUM_TEAMS * cTEAM_SIZE)
let projLock = Mutex.create ()
		
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
	if py < groundy || t2 <= 0.0 then Some (px,groundy) else		
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
	Mutex.lock projLock;
	Hashtbl.clear projectiles;
	let helper (id,weapont,p,v,a,t) = 
		match projTracker (id,weapont,p,v,a,t) with
			None -> ()
		| Some(x,y) -> 
				let radius = getRadius weapont in		
					print_endline ("( " ^ 
						(string_of_float(x -. radius)) ^ ", " ^ 
						(string_of_float(x+.radius)) ^" )");
					Hashtbl.add projectiles id (x -. radius, x +. radius) in
	List.iter helper pt;
	Mutex.unlock projLock
		
(* finds the closest worm in wormlst to position x (horz dist only *)		
let findClosestWorm x wormlst =
	let helper bestx (_,_,_,(px,py),_,_,_,_) = 
		if abs_float(px -. x) < abs_float(bestx -. x) then px else bestx in
	List.fold_left helper (-.cBOARD_WIDTH) wormlst

(* given a worm location and type, and a target, finds the 
 * v vector that gets it there (ignoring satellites)*)
let findShot wormtype px py tx =
	let f = 
		if px < tx (* true if worm is to the left of target *)
		then (fun x -> x)
		else (fun x -> (acos(-.1.) -. x)) in
	let (weapont,v) = 
		match wormtype with
			Basic -> Bomb,cMAX_BOMB_MAGNITUDE
		| Grenader -> Grenade,cMAX_GRENADE_MAGNITUDE
		| MissileBlaster -> Missile,cMAX_MISSILE_MAGNITUDE
		| Miner -> Mine,0.0
		| PelletShooter -> Pellet,cMAX_PELLET_MAGNITUDE
		| LazerGunner -> Lazer,cMAX_LAZER_MAGNITUDE in
	(* start shot at 0deg, increase til hit or 45 *)
	let rec setShot mag angle shift =
		if angle >= (acos 0.0) (* acos 0.0 = pi/2 *)
		then (mag *. (cos (f pi4)), mag *. (sin pi4))
		else
			let (vx,vy) = (mag *. (cos (f angle)), mag *. (sin angle)) in
			let _ = Thread.delay (0.1) in
			match projTracker (0,weapont,(px,py),(vx,vy),(0.0,0.0),None) with
				None -> setShot mag (angle +. shift) shift
			| Some(xhit,yhit) -> 
					if abs_float(xhit -. px)*.shift >=
						abs_float(tx -. px)*.shift
					then (
						if abs_float(xhit -. tx) <= cTOLERANCE
						then (vx,vy)
						else 
							let newScale = (-.shift) /. 4.0 in
								setShot mag (angle +. newScale) newScale
					)
					else setShot mag (angle +. shift) shift in
	setShot v cSTART_ANGLE cSTART_SHIFT
	
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
	if px = min then None else (* pinned against wall *) 
	if b then (print_endline (string_of_float min); findleft min)
	else Some px

(* find closest point to the right that is safe *)
let rec findright px =
	let (b,min,max) = indanger px in
	if px = max then None else
	if b then findright max else Some px
	
(* finds a close, safe spot, given px *)
let findnewpos px = 
	let (b,_,_) = indanger px in
	if b then px else
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
			let _ = 
				if newpos = px then Failed else 
				send_action (QueueMove(newpos,yfinder newpos)) id in

			(* make best shot possible *)
			(*let target = findClosestWorm px wormsB in
			if abs_float (target -. px) <= cBAT_LENGTH
			then 
				let _ = send_action QueueBat id in ()
			else (
				let shotVector = findShot wormtype px py target in
				let _ = send_action (QueueShoot(shotVector)) id in
					*)()
			(**) in
		List.iter wormCycle wormsA;
	done
	
let () = start_bot bot