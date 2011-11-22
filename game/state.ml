open Constants
open Definitions


let create_wormtable () = Hashtbl.create (cNUM_TEAMS * cTEAM_SIZE)
let create_projtable () = Hashtbl.create  (cNUM_TEAMS * cTEAM_SIZE)
let create_obstable () = Hashtbl.create ((List.length cCLOUD_POSITIONS) + (List.length cSATELLITE_POSITIONS))
let create_timer ():timer ref = ref 0.0

 
  

 