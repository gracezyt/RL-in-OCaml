type pos = int * int
type reward = float
type size = int * int
type reward_list = (int * int * reward) list
type env_state = (size * reward_list * reward_list) 

(* Determines if the given state is valid or not *)
let valid_pos ((x, y):pos) ((n, m):size) : bool = 
  (x < n && x >= 0 && y < m && y >= 0)

(* Gets reward from being in the current position in the environment, removes the reward after the agent collects it *)
let get_reward (pos:pos) (curr_env: reward_list) : (reward * reward_list) = 
  let rec aux ((x, y):pos) (curr_env: reward_list) (acc: (reward * reward_list)): (reward * reward_list) = 
    match curr_env with 
    | [] -> acc
    | (curr_x, curr_y, reward)::tl -> 
      if (curr_x == x) && (curr_y == y) then 
        aux (x, y) tl (fst acc +. reward, snd acc)  
      else aux (x, y) tl (fst acc, (curr_x, curr_y, reward)::snd acc) in 
  aux pos curr_env (0.0, [])

(* Collects reward and updates position of the agent and the environment state *) 
let move (position, action) (size, env, copy) : (pos * reward * env_state) = 
  let (x, y) = position in 
  let temp_pos = (x + fst action, y + snd action) in 
  let new_pos = if valid_pos temp_pos size then temp_pos else position in 
  let (reward, new_env) = get_reward new_pos env in
  (* if reward=100.0 then Printf.printf "reward is %f." reward; *)
  (new_pos, reward, (size, new_env, copy))
