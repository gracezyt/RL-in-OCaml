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

(* Env parameters *)
let num_actions = 4
let learning_rate = 0.1
let discount_factor = 0.99
let exploration_prob = 0.1
let num_epochs = 50
let episode_length = 100
let reward = [(1, 2, 1.0);(2, 1, 1.0);(2, 2, 2.0);(2,3, 3.0);(3, 2, 3.0);(3, 3, 5.0)]
let action_list = [(1, 0); (0, 1); (-1, 0); (0, -1)]
let size = (4, 4)

let env_state = (size, reward)
let q_table_dims = (fst (size), snd (size), num_actions)
(*assume that the index is less than the list length, otherwise raise an exception *)
(* exception IndexOutOfBounds of int *)
exception NoElementsInList of string
(* let rec find_index (xs:'a list) (idx:int):'a = 
  match xs with 
  | [] -> raise (IndexOutOfBounds idx)
  | hd::tl -> if idx == 0 then hd else find_index tl (idx - 1) *)
let get_max (xs:'a list) (leq:'a -> 'a -> bool): 'a * int = 
  let rec aux (xs: 'a list) (leq: 'a -> 'a -> bool) (curr_max: 'a * int) (curr_idx: int): 'a * int = 
    match xs with 
    | [] -> curr_max
    | hd::tl -> if leq (fst curr_max) hd then aux tl leq (hd, curr_idx) (curr_idx + 1) else aux tl leq curr_max (curr_idx + 1) in 
  match xs with 
  | [] -> raise (NoElementsInList "No elements where we expected elements in get_max func")
  | hd::tl -> aux tl leq (hd, 0) 1
let change_at_idx (xs:'a list) (new_elt:'a) (idx:int): 'a list = 
  let replace_func = fun i curr_elt -> if i = idx then new_elt else curr_elt in
  List.mapi (replace_func) xs 

let epsilon_greedy (q_table) (curr_pos:pos) (epsilon:float) = 
  if Random.float 1. < epsilon then 
    Random.int num_actions
  else 
    let (x, y) = curr_pos in
    snd (get_max (List.nth (List.nth q_table x) y) (<=))
let rec train (ep_length:int) (env:env_state) (curr_pos:pos) (q_table) (cum_reward: float) (curr_action_idx:int)=
  let update (env:env_state) = 
    let curr_action = List.nth action_list curr_action_idx in
    let next_state, reward, new_env = Final.Env.move (curr_pos, curr_action) (env) in 
    let next_x, next_y = next_state in 
    let next_action_idx = epsilon_greedy q_table next_state exploration_prob in
    let curr_x, curr_y = curr_pos in 
    let next_q = List.nth (List.nth (List.nth q_table next_x) next_y) next_action_idx in
    let curr_q_over_actions = List.nth (List.nth q_table curr_x) curr_y in
    let curr_q = List.nth (curr_q_over_actions) curr_action_idx in 
    let update_size = learning_rate *. (reward +. discount_factor *. next_q -. curr_q) in
    let update = curr_q +. update_size in 
    let new_q_over_actions = change_at_idx curr_q_over_actions update curr_action_idx in 
    let new_q1 = change_at_idx (List.nth q_table curr_x) new_q_over_actions curr_y in 
    (change_at_idx q_table new_q1 curr_x, new_env, next_state, next_action_idx, reward) in 
  if ep_length == 0 then q_table, cum_reward
  else 
    let new_qtable, new_env, curr_state, curr_action_idx, reward = update env in
    train (ep_length - 1) new_env curr_state new_qtable (cum_reward +. reward) curr_action_idx
let rec train_epoch (epochs:int) (env:env_state) (q_table) (total_reward)= 
  if epochs == 0 then q_table, total_reward
  else 
    let curr_pos = (1, 1) in 
    let curr_action_idx = epsilon_greedy q_table curr_pos exploration_prob in
    let new_qtable, cum_reward = train episode_length env curr_pos q_table 0.0 curr_action_idx in 
    Printf.printf "The reward we got in epoch %d is %f. \n" (num_epochs - epochs) cum_reward ;
    print_list_of_list_of_list new_qtable;
    train_epoch (epochs - 1) env new_qtable (cum_reward::total_reward) 

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






