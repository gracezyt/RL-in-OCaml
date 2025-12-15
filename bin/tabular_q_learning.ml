type pos = int * int
type reward = float
type size = int * int
type reward_list = (int * int * reward) list
type env_state = (size * reward_list * reward_list) 

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
let learning_rate = 0.2
let discount_factor = 0.95
let exploration_prob = 0.1
let num_epochs = 500
let episode_length = 50
let reward = [(2,3, 3.0);(3, 2, 3.0);(3, 3, 10.0)]
(* let reward = [(2, 1, 0.5);(1,2, 0.5);(2,2, 1.0);(2,3, 3.0);(3, 2, 3.0);(3, 3, 10.0)] *)
let action_list = [(1, 0); (0, 1); (-1, 0); (0, -1)]
let size = (4, 4)

let env_state = (size, reward, reward)
let q_table_dims = (fst (size), snd (size), num_actions)

(* ENV *)
(* assume all actions are legal *)
(* let valid_pos ((x, y):pos) ((n, m):size) : bool = 
  (x < n && x >= 0 && y < m && y >= 0) *)

(* let get_reward (pos:pos) (curr_env: reward_list) : (reward * reward_list) = 
  let rec aux ((x, y):pos) (curr_env: reward_list) (acc: (reward * reward_list)): (reward * reward_list) = 
    match curr_env with 
    | [] -> acc
    | (curr_x, curr_y, reward)::tl -> if (curr_x == x) && (curr_y == y) then (fst acc +. reward, snd acc) else aux (x, y) tl (fst acc, (curr_x, curr_y, reward)::snd acc) in 
  aux pos curr_env (0.0, []) *)

(* let get_reward (pos:pos) (curr_env: reward_list) : (reward * reward_list) = 
  let rec aux ((x, y):pos) (curr_env: reward_list) (acc: (reward * reward_list)): (reward * reward_list) = 
    match curr_env with 
    | [] -> acc
    | (curr_x, curr_y, reward)::tl -> 
      if (curr_x == x) && (curr_y == y) then 
        aux (x, y) tl (fst acc +. reward, snd acc)  
      else aux (x, y) tl (fst acc, (curr_x, curr_y, reward)::snd acc) in 
  aux pos curr_env (0.0, [])

let move (position, action) (size, env, copy) : (pos * reward * env_state) = 
  let (x, y) = position in 
  let temp_pos = (x + fst action, y + snd action) in 
  let new_pos = if valid_pos temp_pos size then temp_pos else position in 
  let (reward, new_env) = get_reward new_pos env in
  (* if reward=100.0 then Printf.printf "reward is %f." reward; *)
  (new_pos, reward, (size, new_env, copy)) *)

(* let reset ((size, _, new_env):env_state) : env_state = (size, new_env, new_env) *)

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


let rec train (ep_length:int) (env:env_state) (curr_pos:pos) (q_table) (cum_reward: float) =
  let update (action_idx:int) (env:env_state) = 
    let action = List.nth action_list action_idx in 
    (* Printf.printf "Took action (%d, %d)" (fst action) (snd action); *)
    let (x,y), reward, new_env = Final.Env.move (curr_pos, action) (env) in 
    let curr_x, curr_y = curr_pos in 
    let q_over_actions = List.nth (List.nth q_table x) y in
    let curr_q_over_actions = List.nth (List.nth q_table curr_x) curr_y in
    let curr_q = List.nth (curr_q_over_actions) action_idx in 
    let update_size = learning_rate *. (reward +. discount_factor *. fst (get_max (q_over_actions) (<=)) -. curr_q) in
    let update = curr_q +. update_size in 
    let new_q_over_actions = change_at_idx curr_q_over_actions update action_idx in 
    let new_q1 = change_at_idx (List.nth q_table curr_x) new_q_over_actions curr_y in 
    (change_at_idx q_table new_q1 curr_x, new_env, (x,y), reward) in 
  if ep_length == 0 then q_table, cum_reward
  else 
    if Random.float 1.0 < exploration_prob then 
      let action_idx = Random.int num_actions in 
      let new_qtable, new_env, new_pos, reward = update action_idx env in 
      (* Printf.printf "Position is now (%d, %d) \n" (fst new_pos) (snd new_pos); *)
      train (ep_length - 1) new_env new_pos new_qtable (cum_reward +. reward)
    else
      let (x, y) = curr_pos in
      let action_idx = snd (get_max (List.nth (List.nth q_table x) y) (<=)) in 
      let new_qtable, new_env, new_pos, reward = update action_idx env in 
      (* Printf.printf "Position is now (%d, %d) \n" (fst new_pos) (snd new_pos); *)
      train (ep_length - 1) new_env new_pos new_qtable (cum_reward +. reward)
let rec train_epoch (epochs:int) (env:env_state) (q_table) (total_reward)= 
  if epochs == 0 then q_table, total_reward
  else 
    let curr_pos = (1, 1) in 
    let new_qtable, cum_reward = train episode_length env curr_pos q_table 0.0 in 
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






