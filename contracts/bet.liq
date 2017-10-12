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
  margin : token list;
  odds : (odd_index, odd_decimal) map;
}

type storage_t = {
  parameter : parameter_t;
  token_contract : token_contract;
  state : state;
  info : info;
  bookmaker : bookmaker;
  orders : (odd_index, (key * odd_decimal * token) list) map;
  reports : (odd_index, key set) map;
}

type return_t = 
| Success
| Fail of nat
(*
  0: no enough tokens had been approved
  1: the key is not right for bookmaker
  2: storage parameter error
*)

let%entry main (parameter : parameter_t) (storage : storage_t) [%return : return_t] =
  let self = Contract.manager (Current.contract ()) in
  let source = Contract.manager (Source : (unit, unit) contract) in
  match parameter with
  | Add_margin token ->

    (match storage.bookmaker.key with
    | None ->
      let storage' = storage.bookmaker.margin <- token :: storage.bookmaker.margin in
      let storage'' = storage'.bookmaker.key <- Some source in
      let (result, storage) = Contract.call storage''.token_contract 0tz storage'' (Transfer_token_from_approve_dict (source, self, token)) in
      if result.(0) = 0p then
        (Success, storage)
      else
        let margin' = match storage.bookmaker.margin with 
          | [] -> ([] : token list)
          | hd :: tl -> tl
        in
        let storage' = storage.bookmaker.margin <- margin' in
        let storage'' = storage'.bookmaker.key <- (None : key option) in
        (Fail 0p, storage'')

    | Some key ->
      if key = source then
        let storage' = storage.bookmaker.margin <- token :: storage.bookmaker.margin in
        let (result, storage) = Contract.call storage'.token_contract 0tz storage' (Transfer_token_from_approve_dict (source, self, token)) in
        if result.(0) = 0p then
          (Success, storage)
        else
          let margin' = match storage.bookmaker.margin with 
            | [] -> ([] : token list)
            | hd :: tl -> tl
          in
          let storage' = storage.bookmaker.margin <- margin' in
          (Fail 0p, storage')
      else
        (Fail 1p, storage))


  | Setup_odds odds_mod ->
    let i = List.size storage.info.odds_lst in
    let result = Loop.loop (fun x -> 
      let odds_mod = x.(0) in
      let i = abs x.(1) in
      let odds_mod' = match Map.find i odds_mod with
        | None -> Map.update i (Some 1p) odds_mod
        | Some x -> odds_mod
      in
      if i = 0p then
        (false, (odds_mod', 0))
      else
        (true, (odds_mod', i - 1p))
      ) (odds_mod, i - 1p)
    in
    (Success, storage.bookmaker.odds <- result.(0))

  | Add_bet x ->
    let token = x.(0) in
    let storage' = storage.parameter <- parameter in
    let (result, storage) = Contract.call storage'.token_contract 0tz storage' (Transfer_token_from_approve_dict (source, self, token)) in
    if result.(0) = 0p then
      match storage.parameter with
      | Add_bet x -> 
        let token = x.(0) in
        let odd_index = x.(1) in
        let odd_decimal = x.(2) in
        let orders_lst = match Map.find odd_index storage.orders with
          | None -> ([] : (key * odd_decimal * token) list)
          | Some x -> x
        in
        let source = Contract.manager (Source : (unit, unit) contract) in
        let storage' = storage.orders <- Map.update odd_index (Some ((source, odd_decimal, token) :: orders_lst)) storage.orders in
        (Success, storage')
      | Add_margin z -> (Fail 2p, storage)
      | Setup_odds z -> (Fail 2p, storage)
      | Report z -> (Fail 2p, storage)
      | Settle -> (Fail 2p, storage)
    else
      (Fail 0p, storage)

  | Report odd_index ->
    (Success, storage)

  | Settle ->
    (Success, storage)


