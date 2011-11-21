open Definitions

type game

(* initGame board initializes a new game with no worms
 *)
val initGame: unit -> game

(* initWorms game initializes the worms in the game
 *)
val initWorms : game -> unit

(* startGame g tells the game that all initialization is done and
 * actual play is starting
 *)
val startGame: game -> unit

(* handleAction g id a tells the game that the worm with id id has attempted
 * to perform action a, and modifies the game state to reflect that attempt
 * Returns GameEnd if the game is over, and the appropriate
 * result command otherwise
 *)
val handleAction: game -> worm_id -> action -> color -> command

(*Teams request information with handleStatus and return Data*)

val handleStatus: game -> status -> command

(* Tick function that updates the state of the game to time t
*)
val handleTime: game -> float -> game_result option
