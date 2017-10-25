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

type parameter_t = 
| Check
| Override of (key_hash * token) option * storage_t

type return_t = nat * storage_t

let%entry main (parameter : parameter_t) (storage : storage_t option * storage_t) [%return : return_t] =
  match parameter with
  | Check ->
    ((0p, storage.(1)), storage)

  | Override input ->
    let source = Contract.manager (Source : (unit, unit) contract) in
    if source = let s = storage.(1) in s.self_key then
      match input.(0) with 
      | None ->
        let storage = storage.(1) <- input.(1) in
        let storage = storage.(0) <- (None : storage_t option) in
        ((0p, storage.(1)), storage)
      | Some x ->
        let origin_source = x.(0) in
        let token = x.(1) in
        let storage = storage.(0) <- Some input.(1) in
        let s = storage.(1) in
        let (result, storage) = Contract.call s.token_contract 0tz storage (Transfer_token_from_approve_dict (origin_source, s.self_key, token)) in
        match result with
        | General_return x ->
          let storage = 
            if x = 0p then
              match storage.(0) with
              | None -> Current.fail()
              | Some s ->
                storage.(1) <- s
            else
              storage.(0) <- (None : storage_t option)
          in
          ((0p, storage.(1)), storage)
        | Token_info_return _ ->
          Current.fail ()
        | Balance_return _ ->
          Current.fail ()
    else
      Current.fail ()





