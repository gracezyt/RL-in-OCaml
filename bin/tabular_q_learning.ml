type pos = int * int
type reward = float
type size = int * int
type reward_list = (int * int * reward) list
type env_state = (size * reward_list) 

let print_list_of_list_of_list lll =
  List.iteri (fun x row ->
    Printf.printf "Row %d:\n" x;
    List.iteri (fun y col ->
      Printf.printf "  Col %d: [" y;
      List.iteri (fun z value ->
        if z > 0 then Printf.printf "; ";
        Printf.printf "%.2f" value
      ) col;
      Printf.printf "]\n"
    ) row
  ) lll;
  Printf.printf "\n"

(* Algo/Env parameters *)
let num_actions = 4
let learning_rate = 0.2
let discount_factor = 0.95
let exploration_prob = 0.1
let num_epochs = 500
let episode_length = 50
let reward = [(2,3, 3.0);(3, 2, 3.0);(3, 3, 10.0)]
let action_list = [(1, 0); (0, 1); (-1, 0); (0, -1)]
let size = (4, 4)
let env_state = (size, reward)
let q_table_dims = (fst (size), snd (size), num_actions)
let move = Final.Env.move

exception NoElementsInList of string
(*Gets max element and index of max element*)
let get_max (xs:'a list) (leq:'a -> 'a -> bool): 'a * int = 
  let rec aux (xs: 'a list) (leq: 'a -> 'a -> bool) (curr_max: 'a * int) (curr_idx: int): 'a * int = 
    match xs with 
    | [] -> curr_max
    | hd::tl -> if leq (fst curr_max) hd then aux tl leq (hd, curr_idx) (curr_idx + 1) else aux tl leq curr_max (curr_idx + 1) in 
  match xs with 
  | [] -> raise (NoElementsInList "No elements where we expected elements in get_max func")
  | hd::tl -> aux tl leq (hd, 0) 1

(* Returns new list with element changed at new_elt index idx *)
let change_at_idx (xs:'a list) (new_elt:'a) (idx:int): 'a list = 
  let replace_func = fun i curr_elt -> if i = idx then new_elt else curr_elt in
  List.mapi (replace_func) xs 

(* Runs one epsiode of the environment *)
let rec train (ep_length:int) (env:env_state) (curr_pos:pos) (q_table) (cum_reward: float) =
   (* Takes the action specified by action index and updates the Q-Table *)
  let update (action_idx:int) (env:env_state) = 
    (* Move in the environment and collect reward *)
    let action = List.nth action_list action_idx in 
    let (x,y), reward, new_env = move (curr_pos, action) (env) in 
    let curr_x, curr_y = curr_pos in 
    (* Get current Q-value and compute its update *)
    let q_over_actions = List.nth (List.nth q_table x) y in
    let curr_q_over_actions = List.nth (List.nth q_table curr_x) curr_y in
    let curr_q = List.nth (curr_q_over_actions) action_idx in 
    let update_size = learning_rate *. (reward +. discount_factor *. fst (get_max (q_over_actions) (<=)) -. curr_q) in
    let update = curr_q +. update_size in 
    (* Update the Q-table with the new value *)
    let new_q_over_actions = change_at_idx curr_q_over_actions update action_idx in 
    let new_q1 = change_at_idx (List.nth q_table curr_x) new_q_over_actions curr_y in 
    (change_at_idx q_table new_q1 curr_x, new_env, (x,y), reward) in 
  if ep_length == 0 then q_table, cum_reward
  else 
    if Random.float 1.0 < exploration_prob then 
      let action_idx = Random.int num_actions in 
      let new_qtable, new_env, new_pos, reward = update action_idx env in 
      train (ep_length - 1) new_env new_pos new_qtable (cum_reward +. reward)
    else
      let (x, y) = curr_pos in
      let action_idx = snd (get_max (List.nth (List.nth q_table x) y) (<=)) in 
      let new_qtable, new_env, new_pos, reward = update action_idx env in 
      train (ep_length - 1) new_env new_pos new_qtable (cum_reward +. reward)

(* Trains over multiple episodes of an environment *)
let rec train_epoch (epochs:int) (env:env_state) (q_table) (total_reward)= 
  if epochs == 0 then q_table, total_reward
  else 
    let curr_pos = (1, 1) in 
    let new_qtable, cum_reward = train episode_length env curr_pos q_table 0.0 in 
    Printf.printf "The reward we got in epoch %d is %f. \n" (num_epochs - epochs) cum_reward ;
    print_list_of_list_of_list new_qtable;
    train_epoch (epochs - 1) env new_qtable (cum_reward::total_reward) 

(* Initializes the Q-Table *)
let init_qtable (x, y, z) = 
  List.init x (fun _ -> 
    List.init y (fun _ -> 
      List.init z (fun _ -> 0.0)))
let q_table = init_qtable (q_table_dims)
let _, total_reward = train_epoch num_epochs env_state q_table []
let print_as_python_list lst =
  Printf.printf "[";
  List.iteri (fun i x -> 
    if i > 0 then Printf.printf ", ";
    Printf.printf "%.2f" x
  ) lst;
  Printf.printf "]\n"
let _ = print_as_python_list (List.rev total_reward)