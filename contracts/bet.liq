[%%version 0.1]

type token_contract =
  (
    (unit,(key,((key * nat),((key * nat),((key * (key * nat)),(key * key)) variant) variant) variant) variant) variant,
    (nat * (unit,((nat * (nat * (string * string))),nat) variant) variant)
  ) contract

type token = nat
type odd_index = nat
type odd_decimal = nat    (* it should be div 1000 *)

type parameter = 
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
  key : key;
  margin : token;
  odds : (odd_index, odd_decimal) map;
}

type storage = {
  token_contract : token_contract;
  state : state;
  info : info;
  bookmaker : bookmaker;
  orders : (key, odd_index * odd_decimal * token) map;
  reports : (odd_index, key set) map;
}

type return_variant = 
| Success
| Fail of string

let%entry main (parameter : parameter) (storage : storage) [%return : return_variant] =
  let source = Contract.manager (Source : (unit, unit) contract) in
  (Success, storage)


