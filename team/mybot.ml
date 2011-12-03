open Team
open Definitions
open Constants
open State

let TIME_STEP = 0.001

(* returns true if the worm can outrun the projectile *)
let canEscape px weapont wx wormtype time =
	let speed =
		match wormtype with
			Basic -> cBASIC_SPEED
		| Grenader -> cGRENADER_SPEED
		| MissileBlaster -> cMISSILE_BLASTER_SPEED
		| Miner -> cMINER_SPEED
		| PelletShooter -> cPELLER_SHOOTER_SPEED
		| LazerGunner -> cLAZER_GUNNER_SPEED in
	let radius =
		match weapont with 
			Bomb -> cBOMB_EXPLOSION_RADIUS
		|	Grenade -> cGRENADE_EXPLOSION_RADIUS
		| Missile -> cMISSILE_EXPLOSION_RADIUS
		| Mine -> cMINE_EXPLOSION_RADIUS
		| Pellet -> cPELLET_EXPLOSION_RADIUS
		| Lazer -> cLAZER_EXPLOSION_RADIUS
		| Bat -> cBAT_LENGTH in
	(speed*time) > (px wx)

(* Given a projectile, finds out where it will explode *)
let rec projTracker (id,weapont,(px,py),(vx,vy),(ax,ay),t) time = 
	let groundy = yfinder px in
	if py <= groundy || t <= 0 then (px,groundy,time) else		
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

	let newax = -.drag *. vx in
	let neway = -.drag *. vx +. cGRAVITY in				
	let newvx = vx +. newax *. TIME_STEP in
	let newvy = vy +. neway *. TIME_STEP in
	let newpx = px +. newvx *. TIME_STEP in
	let newpy = py +. newvy *. TIME_STEP in
	
	let newt = match t with None -> t | Some(t2) -> t2 +. TIME_STEP in
	
	projTracker (id,weapont,(newpx,newpy),(newvx,newvy),
		(newax,neway),newt) (time +. TIME_STEP)
		
(* finds the closest worm in wormlst to position x (horz dist only *)		
let findClosestWorm x wormlst =
	let helper bestx (id,wormtype,h,(px,py),v,a,t1,t2) = 
		if (px -. x) < (bestx -. x) then px else bestx in
	List.fold_left -cBOARD_WIDTH wormlst

let bot c = 
	while true do
		let blueteam = 
			match get_status (TeamStatus Blue) with
				TeamData(a) -> a
			| _ -> failwith "bleh" in
		let redteam = 
			match get_status (TeamStatus Red) with 
				TeamData(a) -> a
			| _ -> failwith "bleh" in
		let ((scoreA,wormsA),(scoreB,wormsB)) = (* we are teamA *)
			if c = Red then (redteam,blueteam) else (blueteam,redteam) in
		let wormCycle worm =
			
		in
		List.iter wormCycle wormsA;
	done
	
let () = start_bot bot