type token_p = 
| Token_info
| Balance of key_hash
| Transfer_token of (key_hash * nat)
| Approve of (key_hash * nat)
| Transfer_token_from_approve_dict of (key_hash * key_hash * nat)
| Allowance of (key_hash * key_hash)

type token_r_variant =
| General_return
| Token_info_return of nat * nat * string * string
| Balance_return of nat

type token_r = nat * token_r_variant
type token_contract = (token_p, token_r) contract

type token = nat
type odd_index = nat
type odd_decimal = nat    (* it should be div 1000 *)

type state =
| Open
| Disabled of string
| Settled

type info = {
  name : string;
  created_date : timestamp;
  bet_time_range : timestamp * timestamp;
  report_time_range : timestamp * timestamp;
  odds_lst : string list;
}

type bookmaker = {
  key : key_hash option;
  margin : token;
  odds : (odd_index, odd_decimal) map;
}

type storage_t = {
  self_key : key_hash;
  token_contract : token_contract;

  state : state;
  info : info;
  bookmaker : bookmaker;

  all_token_amount : token;
  all_bets_token_amount: (odd_index, token) map;
  orders : (odd_index, (key_hash * odd_decimal * token) list) map;
  reports : (key_hash, odd_index) map;
  distribution: (key_hash, token) map;
}

type bet_parameter_t = 
| Check
| Override of storage_t

(* =============== All aboves are commonly used in betting contracts =============== *)

type parameter_t = (bet_parameter_t, storage_t) contract

type return_t = 
| Success
| Transfer_fail of nat
| Fail of string


let%entry main 
  (parameter : parameter_t) 
  (storage : parameter_t * storage_t) 
  [%return : return_t] =
  let bet_contract = parameter in
  let (_, bet_storage) = storage in
  let (bet_storage, storage) = Contract.call bet_contract 0tz (parameter, bet_storage) Check in
  let (parameter, _) = storage in
  let bet_contract = parameter in
  
  let bet_storage = bet_storage.state <- Settled in
  let votes_reduce (x : (key_hash * odd_index) * (odd_index, nat) map) = 
    let odd_index = x.(0).(1) in
    let map = x.(1) in
    let prev_n = match Map.find odd_index map with | None -> 0p | Some n -> n in
    Map.update odd_index (Some (1p + prev_n)) map
  in
  let odds_votes = Map.reduce votes_reduce bet_storage.reports (Map : (odd_index, nat) map) in
  let correct_reduce (x : (odd_index * nat) * (odd_index * nat)) =
    let curr_odd_index = x.(0).(0) in
    let curr_count = x.(0).(1) in
    let best_odd_index = x.(1).(0) in
    let best_count = x.(1).(1) in
    if curr_count > best_count then
      (curr_odd_index, curr_count)
    else
      (best_odd_index, best_count)
  in
  let correct_odd = Map.reduce correct_reduce odds_votes (0p, 0p) in
  let correct_odd_index = correct_odd.(0) in
  let winning_orders = match Map.find correct_odd_index bet_storage.orders with | None -> ([] : (key_hash * odd_decimal * token) list) | Some x -> x in
  (* save tokens for winners in distribution *)
  let orders_reduce (x : ((key_hash * odd_decimal * token) * (token * (key_hash, token) map))) =
    let key = x.(0).(0) in
    let odd_decimal = x.(0).(1) in
    let token = x.(0).(2) in
    let all_token = x.(1).(0) in
    let map = x.(1).(1) in
    let result = match token * odd_decimal / 1000p with
      | None -> 0p
      | Some x -> x.(0)
    in 
    (all_token + token, Map.update key (Some result) map)
  in
  let orders_reduce_result = List.reduce orders_reduce winning_orders (0p, (Map : (key_hash, token) map)) in
  let all_token_left = abs (bet_storage.all_token_amount - orders_reduce_result.(0)) in
  let bet_storage = bet_storage.distribution <- orders_reduce_result.(1) in
  (* save tokens for reporters in distribution *)
  let reports_reduce (x : ((key_hash * odd_index) * (token * (key_hash, token) map))) =
    let key = x.(0).(0) in
    let all_token_left = x.(1).(0) in
    let distribution = x.(1).(1) in
    let prev_token = match Map.find key distribution with | None -> 0p | Some x -> x in
    (abs (all_token_left - 1000p), Map.update key (Some (prev_token + 1000p)) distribution)       
  in
  let reports_reduce_result = Map.reduce reports_reduce bet_storage.reports (all_token_left, bet_storage.distribution) in
  let all_token_left = reports_reduce_result.(0) in
  let bet_storage = bet_storage.distribution <- reports_reduce_result.(1) in
  (* save all left tokens for bookmaker in distribution *)
  (* TODO: fee *)
  match bet_storage.bookmaker.key with
    | None -> 
      let bet_storage = bet_storage.state <- Disabled "no bookmaker's key found" in
      let (z, storage) = Contract.call bet_contract 0tz (parameter, bet_storage) (Override bet_storage) in
      (Fail "no bookmaker's key found", storage)
    | Some key ->
      let prev_bookmaker_token = 
        match Map.find key bet_storage.distribution with 
        | None -> 0p 
        | Some x -> x 
      in
      let bet_storage = bet_storage.distribution <- Map.update key (Some (all_token_left + prev_bookmaker_token)) bet_storage.distribution in
      let bet_storage = bet_storage.state <- Settled in
      let (z, storage) = Contract.call bet_contract 0tz (parameter, bet_storage) (Override bet_storage) in
      (Success, storage)
