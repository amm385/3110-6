(* GENERAL *)
let cNUM_TEAMS = 2
(*We'll need to decide on this*)
let cTEAM_SIZE = 5
let cUPDATE_TIME = 0.05
let cMAX_WAIT_CONNECTION_TIME = 10.
let cDEFAULT_PORT_NUMBER = 10500
let cDEFAULT_GUI_PORT_NUMBER = 10501
let cDEFAULT_BOT_PORT_NUMBER = 10502
(*We'll need to decide on this*)
let cTIME_LIMIT = 120.

let cORIGIN = (0.0, 0.0)
let cBOARD_WIDTH = 800.
let cBOARD_HEIGHT = 800.
let cBOARD_SIZE = (cBOARD_WIDTH, cBOARD_HEIGHT)

let cBOARD = [(0., 180.); (240.,200.); (600.,125.); (800., 100.)]
let cCLOUD_POSITIONS = [(700.,700.);  (520., 600.); (200., 400.)]
let cSATELLITE_POSITIONS = [(300., 500.); (500.,300.)]
let cSATELLITE_HEALTH = 2
let cRED_START = 200.
let cBLUE_START = 600.

let cGRAVITY = -9.8

let cCLOUD_RADIUS = 75.
let cCLOUD_DRAG = 0.4

let cBASIC_KILL_SCORE = 100
let cGRENADER_KILL_SCORE = 140
let cMISSILE_KILL_SCORE = 200
let cMINER_KILL_SCORE = 150
let cPELLET_KILL_SCORE = 120
let cLAZER_KILL_SCORE = 175

(*starting hitpoints*)
let cBASIC_HEALTH = 75
let cGRENADER_HEALTH = 110
let cMISSILE_BLASTER_HEALTH = 130
let cMINER_HEALTH = 100
let cPELLET_SHOOTER_HEALTH = 85
let cLAZER_GUNNER_HEALTH = 85

(*promotion times*)
let cGRENADE_PROMOTION_TIME = 5.0
let cMISSILE_PROMOTION_TIME = 10.0
let cMINER_PROMOTION_TIME = 9.0
let cPELLET_SHOOTER_PROMOTION_TIME = 6.0
let cLAZER_GUNNER_PROMOTION_TIME = 7.0

let cBOMB_DRAG = 0.2
let cGRENADE_DRAG = 0.2
let cMISSILE_DRAG = 0.6
let cPELLET_DRAG = 0.8
let cLAZER_DRAG = 0.02

let cBASIC_SPEED = 10.0
let cGRENADER_SPEED = 12.0
let cMISSILE_BLASTER_SPEED = 7.3
let cMINER_SPEED = 4.0
let cPELLET_SHOOTER_SPEED = 16.0
let cLAZER_GUNNER_SPEED = 8.0

let cBOMB_SIZE = 10.0
let cGRENADE_SIZE = 20.0
let cMISSILE_SIZE = 39.0
let cMINE_SIZE = 40.0
let cPELLET_SIZE = 80.0
let cLAZER_SIZE = 0.3
let cSATELLITE_SIZE = 20.0

let cBOMB_EXPLOSION_RADIUS = 30.0
let cGRENADE_EXPLOSION_RADIUS = 25.0
let cMISSILE_EXPLOSION_RADIUS = 50.0
let cMINE_EXPLOSION_RADIUS = 25.0
let cPELLET_EXPLOSION_RADIUS = 15.0
let cLAZER_EXPLOSION_RADIUS = 15.0

let cBASIC_RADIUS = 40.0
let cGRENADER_RADIUS = 50.0
let cMISSILE_BLASTER_RADIUS = 50.0
let cMINER_RADIUS = 60.0
let cPELLET_SHOOTER_RADIUS = 30.0
let cLAZER_GUNNER_RADIUS = 70.0

let cBOMB_DAMAGE = 10
let cGRENADE_DAMAGE = 25
let cMISSILE_DAMAGE = 40
let cMINE_DAMAGE = 13
let cPELLET_DAMAGE = 8
let cLAZER_DAMAGE = 25
let cBAT_DAMAGE = 25

let cMAX_BOMB_MAGNITUDE = 80.0
let cMAX_GRENADE_MAGNITUDE = 80.0
let cMAX_MISSILE_MAGNITUDE = 100.0
let cMAX_PELLET_MAGNITUDE = 45.0
let cMAX_LAZER_MAGNITUDE = 140.0
let cBAT_LENGTH = 80.0

let cMINE_DETONATION_TIME = 8.0
let cGRENADE_DETONATION_TIME = 4.0
let cMINE_PLANT_DISTANCE = 300.0

let cBASIC_ATTACK_COOLDOWN = 5.0
let cGRENADER_COOLDOWN = 7.0
let cMINER_COOLDOWN = 6.0
let cMISSILE_BLASTER_COOLDOWN = 9.0
let cLAZER_GUNNER_COOLDOWN = 9.0
let cPELLET_SHOOTER_COOLDOWN = 2.0

