open Hashtbl
open Definitions
open State
open Constants


type game = ((worm_id, worm_data) t * 
  (projectile_id, projectile_data) t * 
	(obstacle_id, obstacle_data) t * 
	(timer ref)) 
	
let projectile_id_counter = ref 0
let wormWaypoints = Hasthbl.create (cNUM_TEAMS * cTEAM_SIZE)
let futureProj = Hashtbl.create (cNUM_TEAMS * cTEAM_SIZE)

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
		(* wormID is positive for first team, negative for second *)
		let id = 
			if team = 0
			then !count
			else -1 * !count in
		let xpos = 
			if team = 0 
			then Random.float cRED_START
			else (Random.float (cBOARD_WIDTH -. cBLUE_START)) +. cBLUE_START in
		let lineFinder ((x1,y1),(x2,y2),b) (x3,y3) = 
			if b then ((x1,y1),(x2,y2),true) else 
				(if x2 <= xpos && x3 >= xpos
				then ((x2,y2),(x3,y3),true)
				else ((x2,y2),(x2,y2),false)) in
		let calcY (x1,y1) (x2,y2) =
			let m = (y2 -. y1) /. (x2 -. x1) in
			m *. (xpos -. x1) +. y1 in
		let ypos = 
			let (l,r,_) = List.fold_left lineFinder ((0.,0.),(0.,0.),false) cBOARD in
			calcY l r in
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
		 (let weapon = 
				(match wormtype with
					Basic -> Bomb
				| Grenader -> Grenade
				| MissileBlaster -> Missile
				| Miner -> Mine
				| PelletShooter -> Pellet
				| LazerGunner -> Lazer) in
			 (* accel needs to be figured out *)
			let accel = (0.,0.) in
			let p : projectile = (!projectile_id_counter,weapon,pos,v,accel) in
				(try 
					let relList = Hashtbl.find futureProj worm_id in
						Hashtbl.replace futureProj worm_id (relList@[p])
				with Not_found e -> 
					Hashtbl.add futureProj worm_id [p]);
				projectile_id_counter := !projectile_id_counter + 1)
	| QueueMove(v) ->
			let relList = Hashtbl.find wormWaypoints worm_id in
				Hashtbl.replace wormWaypoints worm_id (relList@[v])
	| QueueBat -> 
			let p : projectile = (!projectile_id_counter,Bat,pos,(0.,0.),(0.,0.)) in
			(try
				let relList = Hashtbl.find futureProj worm_id in
					Hashtbl.replace futureProj worm_id (relList@[p])
			with Not_found e ->
				Hashtbl.add futureProj worm_id [p]);
			projectile_id_counter := !projectile_id_counter + 1
	| ClearShoot ->
	| ClearMove ->
	| Promote(wt) ->
	| Talk(s) ->
	
let handleStatus g status = 
  failwith "Whats the difference between a worm and an apple?
  Have you ever had worm pie?!!?"

let handleTime g newt = 
  failwith "Why did the sparrow go to the library?
  To look for bookworms!"
  
