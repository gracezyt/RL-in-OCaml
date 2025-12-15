type pos = int * int
type reward = float
type size = int * int
type reward_list = (int * int * reward) list
type env_state = (size * reward_list * reward_list) 

let death_states = [(3, 0)]
let obstacles = [(2, 1); (0, 3)]

let valid_pos (curr_state:pos) (proposed_state:pos) ((n, m):size) (death_states:pos list) (obstacles:pos list): bool = 
  let new_x, new_y = proposed_state in 
  if List.mem curr_state death_states then false
  else if List.mem proposed_state obstacles then false 
  else (new_x < n && new_x >= 0 && new_y < m && new_y >= 0)

(* let get_reward (pos:pos) (curr_env: reward_list) : (reward * reward_list) = 
  let rec aux ((x, y):pos) (curr_env: reward_list) (acc: (reward * reward_list)): (reward * reward_list) = 
    match curr_env with 
    | [] -> acc
    | (curr_x, curr_y, reward)::tl -> if (curr_x == x) && (curr_y == y) then (fst acc +. reward, snd acc) else aux (x, y) tl (fst acc, (curr_x, curr_y, reward)::snd acc) in 
  aux pos curr_env (0.0, []) *)

let get_reward (pos:pos) (curr_env: reward_list) : (reward * reward_list) = 
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
  let new_pos = if valid_pos position temp_pos size death_states obstacles then temp_pos else position in 
  let (reward, new_env) = get_reward new_pos env in
  (new_pos, reward, (size, new_env, copy))
