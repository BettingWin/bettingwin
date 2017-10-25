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
(*
  init:
  (Pair None (Pair "tz1bV31HQMWMqJ8crvTZrv1LHiJPUQC9ZNyY" (Pair ${contract} (Pair (Left Unit) (Pair (Pair ${bet_name} (Pair ${created_date} (Pair (Pair ${bet_time_start} ${bet_time_end} ) (Pair (Pair ${report_time_start} ${report_time_end} ) (list ${odds} ) ) ) ) ) (Pair (Pair None (Pair 0 (Map ) ) ) (Pair 0 (Pair (Map ) (Pair (Map ) (Pair (Map ) (Map ) ) ) ) ) ) ) ) ) ) )
*)

type bet_parameter_t = 
| Check
| Override of (key_hash * token) option * storage_t

type bet_return_t = nat * storage_t

type parameter_t = token * (bet_parameter_t, bet_return_t) contract

type return_t = 
| Success
| Transfer_fail of nat
| Fail of string

let%entry main (parameter : parameter_t) (storage : parameter_t) [%return : return_t] =
  let bet_contract = parameter.(1) in
  let (result, parameter) = Contract.call bet_contract 0tz parameter Check in

  let bet_storage = result.(1) in
  if 
    match bet_storage.bookmaker.key with
    | None -> true
    | Some key -> 
      let source = Contract.manager (Source : (unit, unit) contract) in
      if key = source then 
        true
      else 
        false
  then
    let token = parameter.(0) in
    let bet_contract = parameter.(1) in

    let bet_storage = bet_storage.bookmaker.margin <- bet_storage.bookmaker.margin + token in
    let bet_storage = bet_storage.all_token_amount <- bet_storage.all_token_amount + token in
    let bet_storage = 
      match bet_storage.bookmaker.key with 
      | None -> 
        let source = Contract.manager (Source : (unit, unit) contract) in
        bet_storage.bookmaker.key <- Some source
      | Some _ -> bet_storage
    in
    let source = Contract.manager (Source : (unit, unit) contract) in
    let (result, parameter) = Contract.call bet_contract 0tz parameter (Override ((Some (source, token)), bet_storage)) in
    if result.(0) = 0p then
      (Success, parameter)
    else
      (Transfer_fail result.(0), parameter)

  else
    (Fail "Source key doesn't match the bookmaker's key", parameter)


  
