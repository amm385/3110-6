type vector = float * float (* (x,y) *)

type timer = float
type color = Red | Blue
type game_result = Winner of color | Tie
type score = int

type worm_id = int
type projectile_id = int
type obstacle_id = int
type health = int 
type position = vector
type velocity = vector
type acceleration = vector
type move = vector

module Worm_map = Map.Make(struct type t = worm_id let compare = compare end)
module Projectile_map = 
        Map.Make(struct type t = projectile_id let compare = compare end)

(*More guys?  More guys = More strategy!*)
type worm_type = Basic | Grenader | MissileBlaster | Miner | PelletShooter 
                | LazerGunner 
type weapon_type = Bomb | Grenade | Missile | Mine | Pellet | Lazer | Bat

type projectile = projectile_id * weapon_type * position * velocity * acceleration * timer option

type obstacle = Cloud of obstacle_id * position |
                Satellite of obstacle_id * position * int

(*Graphics updates*)

type update = InitGraphics of vector list |
              Countdown of int | 
              DisplayString of color * string |
              AddWorm of worm_id * position * health | (*always add basic worm*)
              MorphWorm of worm_id * worm_type |
              MoveWorm of worm_id * position |
              RemoveWorm of worm_id |              
              AddProjectile of projectile_id * weapon_type * position |
              MoveProjectile of projectile_id * position | 
              RemoveProjectile of projectile_id |
              UpdateScore of color * score | 
              GameOver of game_result |
              AddObstacle of obstacle |             
              RemoveObstacle of obstacle_id |
              DoBat of worm_id

(* Type for control messages *)
type control = GameStart |
               GameRequest |
               Team of color |
               GameEnd

(*actions for AI to do*)
(* for projectiles vector is velocity, for stand still things its the position*)
type action = QueueShoot of  vector|  
              QueueMove of  vector |
              QueueBat | 
              ClearShoot |
              ClearMove |
              Promote of worm_type |
              Talk of string 


type worm_data = worm_id * worm_type * health * position * velocity *
acceleration * timer * timer 
type projectile_data = projectile
type obstacle_data = obstacle 
type team_data = score * worm_data list
type game_data = team_data * team_data * projectile_data list * obstacle_data list * timer

type result = Success | Failed

(*type for clients to request information*)
type status = WormStatus of worm_id |  
              ProjectileStatus of projectile_id |
              TeamStatus of color | 
              ObstacleStatus |
              GameStatus
              
(*returned to clients after status request*)
type data = WormData of worm_data |
            ProjectileData of projectile_data |
            TeamData of team_data |
            GameData of game_data |
            ObstacleData of obstacle_data list |
            NoData

type command = Control of control | 
               Action of  worm_id * action |
               Status of status |
               Data of data |
               Result of worm_id * result |
               Error of string 

