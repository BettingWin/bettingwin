[%%version 0.1]

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


type parameter_t = 
| Add_margin of token
| Setup_odds of (odd_index, odd_decimal) map
| Add_bet of token * odd_index * odd_decimal
| Report of odd_index
| Settle

type state =
| Open
| Disabled of nat
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
  parameter : parameter_t;
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

type return_t = 
| Success
| Fail of nat
(*
  0: no enough tokens had been approved
  1: source key is not right for bookmaker
  2: storage parameter error
  3: source key has already reported
  4: no bookmaker key
  5: contract settled
  6: contract is disabled
  7: margin insufficient
*)

let%entry main (parameter : parameter_t) (storage : storage_t) [%return : return_t] =
  (* state detection *)
  match storage.state with
  | Settled ->
    (Fail 5p, storage)
  | Disabled n ->
    (Fail (n + 50p), storage)
  | Open ->
    let self = storage.self_key in
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
        let result = Loop.loop (fun x -> 
          let odds_mod = x.(0) in
          let i = x.(1) in
          let odds_lst = x.(2) in
          let odds_mod' = match Map.find i odds_mod with
            | None -> Map.update i (Some 1p) odds_mod
            | Some z -> odds_mod
          in
          match odds_lst with
          | [] -> 
            (false, (odds_mod', i, odds_lst))
          | z :: tl ->
            (true, (odds_mod', i + 1p, tl))

        ) (odds_mod, 0p, storage.info.odds_lst)
        in
        (Right (Success, storage.bookmaker.odds <- result.(0)) : (nat, return_t * storage_t) variant)

      | Add_bet x -> 
        let token = x.(0) in
        let odd_index = x.(1) in
        let odd_decimal = x.(2) in
        let odds_reduce (x : ((odd_index * token) * token)) =
          let curr_odd_index = x.(0).(0) in
          let token = x.(0).(1) in
          let acc_token = x.(1) in
          if curr_odd_index <> odd_index then
            acc_token + token
          else
            acc_token
        in
        let bet_tokens = Map.reduce odds_reduce storage.all_bets_token_amount 0p in
        let all_margin = storage.bookmaker.margin + bet_tokens in 
        let orders_reduce (x : ((key_hash * odd_decimal * token) * token)) =
          let odd_decimal = x.(0).(1) in
          let token = x.(0).(2) in
          let acc_token = x.(1) in
          let result = match token * odd_decimal / 1000p with
            | None -> 0p
            | Some x -> x.(0)
          in 
          acc_token + result
        in
        let bet_lst = 
          match Map.find odd_index storage.orders with
          | None -> [(source, odd_decimal, token)]
          | Some x -> (source, odd_decimal, token) :: x
        in  
        let counter_margin = List.reduce orders_reduce bet_lst 0p in        
        if all_margin >= counter_margin then
          (Left token : (nat, return_t * storage_t) variant)
        else
          (Right (Fail 7p, storage) : (nat, return_t * storage_t) variant)

      | Report z -> 
        (match Map.find source storage.reports with
        | Some z ->
          (Right (Fail 3p, storage) : (nat, return_t * storage_t) variant)
        | None ->
          (Left 1000p : (nat, return_t * storage_t) variant))

      | Settle -> 
        let storage = storage.state <- Settled in
        let votes_reduce (x : (key_hash * odd_index) * (odd_index, nat) map) = 
          let odd_index = x.(0).(1) in
          let map = x.(1) in
          let prev_n = match Map.find odd_index map with | None -> 0p | Some n -> n in
          Map.update odd_index (Some (1p + prev_n)) map
        in
        let odds_votes = Map.reduce votes_reduce storage.reports (Map : (odd_index, nat) map) in
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
        let winning_orders = match Map.find correct_odd_index storage.orders with | None -> ([] : (key_hash * odd_decimal * token) list) | Some x -> x in
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
        let all_token_left = abs (storage.all_token_amount - orders_reduce_result.(0)) in
        let storage = storage.distribution <- orders_reduce_result.(1) in
        (* save tokens for reporters in distribution *)
        let reports_reduce (x : ((key_hash * odd_index) * (token * (key_hash, token) map))) =
          let key = x.(0).(0) in
          let all_token_left = x.(1).(0) in
          let distribution = x.(1).(1) in
          let prev_token = match Map.find key distribution with | None -> 0p | Some x -> x in
          (abs (all_token_left - 1000p), Map.update key (Some (prev_token + 1000p)) distribution)       
        in
        let reports_reduce_result = Map.reduce reports_reduce storage.reports (all_token_left, storage.distribution) in
        let all_token_left = reports_reduce_result.(0) in
        let storage = storage.distribution <- reports_reduce_result.(1) in
        (* save all left tokens for bookmaker in distribution *)
        (* TODO: fee *)
        match storage.bookmaker.key with
          | None -> 
            let storage = storage.state <- Disabled 4p in
            (Right (Fail 4p, storage) : (nat, return_t * storage_t) variant)
          | Some key ->
            let prev_bookmaker_token = 
              match Map.find key storage.distribution with 
              | None -> 0p 
              | Some x -> x 
            in
            let storage = storage.distribution <- Map.update key (Some (all_token_left + prev_bookmaker_token)) storage.distribution in
            let storage = storage.state <- Settled in
            (Right (Success, storage) : (nat, return_t * storage_t) variant)
    in
    match preprocess_return with
    | Right ret -> ret
    | Left token ->
      let storage = storage.parameter <- parameter in
      let (result, storage) = Contract.call storage.token_contract 0tz storage (Transfer_token_from_approve_dict (source, self, token)) in
      let source = Contract.manager (Source : (unit, unit) contract) in
      if result.(0) = 0p then
        match storage.parameter with
        | Setup_odds z -> (Fail 2p, storage)
        | Settle -> (Fail 2p, storage)

        | Add_margin token -> 
          let storage = storage.bookmaker.margin <- storage.bookmaker.margin + token in
          let storage = 
            match storage.bookmaker.key with 
            | None -> storage.bookmaker.key <- Some source
            | Some z -> storage
          in
          let storage = storage.all_token_amount <- storage.all_token_amount + token in
          (Success, storage)

        | Add_bet x -> 
          let token = x.(0) in
          let odd_index = x.(1) in
          let odd_decimal = x.(2) in
          let orders_lst = match Map.find odd_index storage.orders with
            | None -> ([] : (key_hash * odd_decimal * token) list)
            | Some x -> x
          in
          let storage = storage.orders <- Map.update odd_index (Some ((source, odd_decimal, token) :: orders_lst)) storage.orders in
          let storage = storage.all_token_amount <- storage.all_token_amount + token in
          let prev_bet_token_amount = match Map.find odd_index storage.all_bets_token_amount with | None -> 0p | Some x -> x in
          let storage = storage.all_bets_token_amount <- Map.update odd_index (Some (prev_bet_token_amount + token)) storage.all_bets_token_amount in
          (Success, storage)

        | Report odd_index -> 
          let storage = storage.reports <- Map.update source (Some odd_index) storage.reports in
          let storage = storage.all_token_amount <- storage.all_token_amount + 1000p in
          (Success, storage)

      else
        (Fail (result.(0) + 100p), storage)


