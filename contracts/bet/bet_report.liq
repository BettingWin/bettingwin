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

type parameter_t = odd_index * (bet_parameter_t, bet_return_t) contract

type return_t = 
| Success
| Transfer_fail of nat
| Fail of string

let%entry main (parameter : parameter_t) (storage : parameter_t) [%return : return_t] =
  let bet_contract = parameter.(1) in
  let (result, parameter) = Contract.call bet_contract 0tz parameter Check in

  let bet_storage = result.(1) in
  let source = Contract.manager (Source : (unit, unit) contract) in
  match Map.find source bet_storage.reports with
  | Some _ -> 
    (Fail "the source key has reported", parameter)
  | None ->
    let token = 1000p in
    let bet_storage = bet_storage.reports <- Map.update source (Some parameter.(0)) bet_storage.reports in
    let bet_storage = bet_storage.all_token_amount <- bet_storage.all_token_amount + 1000p in
    let (result, parameter) = Contract.call parameter.(1) 0tz parameter (Override ((Some (source, token)), bet_storage)) in
    if result.(0) = 0p then
      (Success, parameter)
    else
      (Transfer_fail result.(0), parameter)
