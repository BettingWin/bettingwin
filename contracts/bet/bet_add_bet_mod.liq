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

type parameter_t = key_hash * (token * odd_index * odd_decimal) * storage_t * (bet_parameter_t, bet_return_t) contract

type return_t = 
| Success
| Transfer_fail of nat
| Fail of string

let%entry main (parameter : parameter_t) (storage : key_hash) [%return : return_t] =
  let source = Contract.manager (Source : (unit, unit) contract) in
  if source = storage then
    let (origin_source, (token, odd_index, odd_decimal), bet_storage, bet_contract) = parameter in
    let orders_lst = match Map.find odd_index bet_storage.orders with
      | None -> ([] : (key_hash * odd_decimal * token) list)
      | Some x -> x
    in
    let bet_storage = bet_storage.orders <- Map.update odd_index (Some ((origin_source, odd_decimal, token) :: orders_lst)) bet_storage.orders in
    let bet_storage = bet_storage.all_token_amount <- bet_storage.all_token_amount + token in
    let prev_bet_token_amount = match Map.find odd_index bet_storage.all_bets_token_amount with | None -> 0p | Some x -> x in
    let bet_storage = bet_storage.all_bets_token_amount <- Map.update odd_index (Some (prev_bet_token_amount + token)) bet_storage.all_bets_token_amount in

    let arg = Override ((Some (origin_source, token)), bet_storage) in
    let (result, storage) = Contract.call bet_contract 0tz storage arg in
    if result.(0) = 0p then
      (Success, storage)
    else
      (Transfer_fail result.(0), storage)
  else
    (Fail "this contract can't be called by user", storage)


