open Team
open Definitions
open Constants

let my_data_structure = ref None
let _ = Random.init 9001

let get_random_index lst =
  Random.int (List.length lst)

let get_random_id lst = 
  let idify (id, _,_,_,_,_,_,_) = id in
  let ids = List.map idify lst in
  List.nth ids (get_random_index ids)

let gen_random_x () = 
  Random.float cBOARD_WIDTH

let gen_random_y () =
  Random.float cBOARD_HEIGHT

let string_of_point (x,y) =
  string_of_float x ^ " " ^ string_of_float y
  
let bot c = 
  while true do
    my_data_structure := Some(get_status (TeamStatus c));
    let (sc, wmlist) = match !my_data_structure with Some(TeamData(a)) -> a 
      |_ -> failwith "NUuuuuuuuuuuuuuuu" in
    let worm_id = get_random_id wmlist in
    let move_point = gen_random_x(), gen_random_y() in
    let move_action = QueueMove(move_point) in
    let shot_point = gen_random_x(), gen_random_y() in
    let shot_action = QueueShoot(shot_point) in
    let res = send_action move_action worm_id in
    let _ = send_action shot_action worm_id in 
    Thread.delay 1.0
  done

let () = start_bot bot
