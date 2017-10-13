[%%version 0.1]

type token_p = 
| Token_info
| Balance of key
| Transfer_token of (key * nat)
| Approve of (key * nat)
| Transfer_token_from_approve_dict of (key * key * nat)
| Allowance of (key * key)

type token_r_variant =
| General_return
| Token_info_return of nat * nat * string * string
| Balance_return of nat

type token_r = nat * token_r_variant
type token_contract = (token_p, token_r) contract

type token = nat
type odd_index = nat
type odd_decimal = nat    (* it should be div 1000 *)


type parameter_t = 
| Add_margin of token
| Setup_odds of (odd_index, odd_decimal) map
| Add_bet of token * odd_index * odd_decimal
| Report of odd_index
| Settle

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
  key : key option;
  margin : token;
  odds : (odd_index, odd_decimal) map;
}

type storage_t = {
  parameter : parameter_t;
  token_contract : token_contract;
  state : state;
  info : info;
  bookmaker : bookmaker;
  all_token_amount : token;
  orders : (odd_index, (key * odd_decimal * token) list) map;
  reports : (key, odd_index) map;
  distribution: (key, token) map;
}

type return_t = 
| Success
| Fail of nat
(*
  0: no enough tokens had been approved
  1: source key is not right for bookmaker
  2: storage parameter error
  3: source key has already reported
*)

let%entry main (parameter : parameter_t) (storage : storage_t) [%return : return_t] =
  (* TODO: state detection *)
  let self = Contract.manager (Current.contract ()) in
  let source = Contract.manager (Source : (unit, unit) contract) in
  let preprocess_return = 
    match parameter with
    | Add_margin token ->
      (match storage.bookmaker.key with
      | None -> 
        (Left token : (nat, return_t * storage_t) variant)
      | Some key -> 
        if key = source then 
          (Left token : (nat, return_t * storage_t) variant)
        else 
          (Right (Fail 1p, storage) : (nat, return_t * storage_t) variant))

    | Setup_odds odds_mod -> 
      let i = List.size storage.info.odds_lst in
      let result = Loop.loop (fun x -> 
        let odds_mod = x.(0) in
        let i = abs x.(1) in
        let odds_mod' = match Map.find i odds_mod with
          | None -> Map.update i (Some 1p) odds_mod
          | Some z -> odds_mod
        in
        if i = 0p then
          (false, (odds_mod', 0))
        else
          (true, (odds_mod', i - 1p))
        ) (odds_mod, i - 1p)
      in
      (Right (Success, storage.bookmaker.odds <- result.(0)) : (nat, return_t * storage_t) variant)

    | Add_bet x -> 
      (Left x.(0) : (nat, return_t * storage_t) variant)

    | Report odd_index -> 
      (match Map.find source storage.reports with
      | Some z ->
        (Right (Fail 3p, storage) : (nat, return_t * storage_t) variant)
      | None ->
        (Left 1000p : (nat, return_t * storage_t) variant))

    | Settle -> 
      let storage' = storage.state <- Settled in
      let votes_reduce (x : (key * odd_index) * (odd_index, nat) map) = 
        let key = x.(0).(0) in
        let odd_index = x.(0).(1) in
        let map = x.(1) in
        let prev_n = match Map.find odd_index map with | None -> 0p | Some n -> n in
        Map.update odd_index (Some (1p + prev_n)) map
      in
      let odds_votes = Map.reduce votes_reduce storage'.reports (Map : (odd_index, nat) map) in
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
      let winning_orders = match Map.find correct_odd_index storage'.orders with | None -> ([] : (key * odd_decimal * token) list) | Some x -> x in
      let orders_reduce (x : ((key * odd_decimal * token) * (token * (key, token) map))) =
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
      let orders_reduce_result = List.reduce orders_reduce winning_orders (0p, (Map : (key, token) map)) in
      let all_token_left = abs (storage'.all_token_amount - orders_reduce_result.(0)) in
      let storage'' = storage'.distribution <- orders_reduce_result.(1) in
      (* TODO: reporters token return *)
      (* TODO: bookmaker profit token return *)
      (Right (Success, storage'') : (nat, return_t * storage_t) variant)
  in
  match preprocess_return with
  | Right ret -> ret
  | Left token ->
    let storage' = storage.parameter <- parameter in
    let (result, storage) = Contract.call storage'.token_contract 0tz storage' (Transfer_token_from_approve_dict (source, self, token)) in
    let source = Contract.manager (Source : (unit, unit) contract) in
    if result.(0) = 0p then
      match storage.parameter with
      | Setup_odds z -> (Fail 2p, storage)
      | Settle -> (Fail 2p, storage)

      | Add_margin token -> 
        let storage' = storage.bookmaker.margin <- storage.bookmaker.margin + token in
        let storage'' = 
          match storage'.bookmaker.key with 
          | None -> storage'.bookmaker.key <- Some source
          | Some z -> storage'
        in
        let storage''' = storage''.all_token_amount <- storage''.all_token_amount + token in
        (Success, storage''')

      | Add_bet x -> 
        (* TODO: margin restriction calculation *)
        let token = x.(0) in
        let odd_index = x.(1) in
        let odd_decimal = x.(2) in
        let orders_lst = match Map.find odd_index storage.orders with
          | None -> ([] : (key * odd_decimal * token) list)
          | Some x -> x
        in
        let storage' = storage.orders <- Map.update odd_index (Some ((source, odd_decimal, token) :: orders_lst)) storage.orders in
        let storage'' = storage'.all_token_amount <- storage'.all_token_amount + token in
        (Success, storage'')

      | Report odd_index -> 
        let storage' = storage.reports <- Map.update source (Some odd_index) storage.reports in
        let storage'' = storage'.all_token_amount <- storage'.all_token_amount + 1000p in
        (Success, storage'')

    else
      (Fail (result.(0) + 100p), storage)


