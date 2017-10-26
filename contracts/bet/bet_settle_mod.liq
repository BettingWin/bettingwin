[%%version 0.12]

type token_p = 
| Token_info
| Balance of key_hash
| Transfer_token of (key_hash * nat)
| Approve of (key_hash * nat)
| Transfer_token_from_approve_dict of (key_hash * key_hash * nat)

type token_r =
| General_return of nat
| Token_info_return of nat * nat * string * string
| Balance_return of nat

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
| Override of (key_hash * token) option * storage_t

type bet_return_t = nat * storage_t

type parameter_t = token * storage_t * (bet_parameter_t, bet_return_t) contract

type return_t = 
| Success
| Transfer_fail of nat
| Fail of string

let%entry main (parameter : parameter_t) (storage : unit) [%return : return_t] =
  let reports_reduce (x : ((key_hash * odd_index) * (token * (key_hash, token) map))) =
    let key = x.(0).(0) in
    let all_token_left = x.(1).(0) in
    let distribution = x.(1).(1) in
    let prev_token = match Map.find key distribution with | None -> 0p | Some x -> x in
    ((match%nat (all_token_left - 1000p) with | Plus x -> x | Minus x -> x), 
      Map.update key (Some (prev_token + 1000p)) distribution)       
  in
  let bet_storage = parameter.(1) in
  let all_token_left = parameter.(0) in
  let reports_reduce_result = Map.reduce reports_reduce bet_storage.reports (all_token_left, bet_storage.distribution) in
  let all_token_left = reports_reduce_result.(0) in
  let bet_storage = bet_storage.distribution <- reports_reduce_result.(1) in
  (* save all left tokens for bookmaker in distribution *)
  (* TODO: fee *)
  match bet_storage.bookmaker.key with
    | None -> 
      let bet_storage = bet_storage.state <- Disabled "no bookmaker's key found" in
      let (z, storage) = Contract.call parameter.(2) 0tz () (Override ((None : (key_hash * token) option), bet_storage)) in
      (Fail "no bookmaker's key found", storage)
    | Some key ->
      let prev_bookmaker_token = 
        match Map.find key bet_storage.distribution with 
        | None -> 0p 
        | Some x -> x 
      in
      let bet_storage = bet_storage.distribution <- Map.update key (Some (all_token_left + prev_bookmaker_token)) bet_storage.distribution in
      let bet_storage = bet_storage.state <- Settled in
      let (_, storage) = Contract.call parameter.(2) 0tz () (Override ((None : (key_hash * token) option), bet_storage)) in
      (Success, storage)