[%%version 0.1]

type parameter_t = 
| Token_info
| Balance of key_hash
| Transfer_token of (key_hash * nat)
| Approve of (key_hash * nat)
| Transfer_token_from_approve_dict of (key_hash * key_hash * nat)
| Allowance of (key_hash * key_hash)

type storage_t = {
  approve_dict : (key_hash, (key_hash, nat) map) map;
  balance : (key_hash, nat) map;
  total_supply : nat;
  decimals : nat;
  name : string;
  symbol : string;
}

type return_variant =
| General_return
| Token_info_return of nat * nat * string * string
| Balance_return of nat

type return_t = nat * return_variant



let get_balance = fun (balance_map : (key_hash, nat) map) -> fun (k : key_hash) ->
  match Map.find k balance_map with
  | None -> 0p
  | Some x -> x

(* 
The nat of return represents:
  0: success
  1: balance insufficient
 *)  

let%entry main (parameter : parameter_t) (storage : storage_t) [%return : return_t] =
  let get_origin_balance = get_balance storage.balance in
  let source = Contract.manager (Source : (unit, unit) contract) in
  let source_balance = get_origin_balance source in
  match parameter with
    | Token_info -> 
      ((0p, Token_info_return (storage.total_supply, storage.decimals, storage.name, storage.symbol)), storage)
    
    | Balance from -> 
      let result = get_origin_balance from in
      ((0p, Balance_return result), storage)
    
    | Transfer_token x -> 
      let target = x.(0) in
      let amount = x.(1) in
      if source_balance < amount then
        ((1p, General_return), storage)
      else
        let new_source_balance = abs (source_balance - amount) in
        let new_target_balance = amount + get_origin_balance target in
        let b1 = Map.update source (Some new_source_balance) storage.balance in
        let b2 = Map.update target (Some new_target_balance) b1 in
        ((0p, General_return), storage.balance <- b2)
    
    | Approve x -> 
      let target = x.(0) in
      let amount = x.(1) in
      if source_balance < amount then
        ((1p, General_return), storage)
      else
        let new_source_balance = abs (source_balance - amount) in
        let new_balance = Map.update source (Some new_source_balance) storage.balance in
        let approve_dict_lst = match Map.find source storage.approve_dict with
          | None -> Map.update target (Some amount) (Map [] : (key_hash, nat) map)
          | Some x -> 
            let new_amount = match Map.find target x with
              | None -> amount
              | Some x -> x + amount
            in
            Map.update target (Some new_amount) x
        in
        let approve_dict = Map.update source (Some approve_dict_lst) storage.approve_dict in
        let storage1 = storage.balance <- new_balance in
        let storage2 = storage1.approve_dict <- approve_dict in
        ((0p, General_return), storage2)
    
    | Transfer_token_from_approve_dict x -> 
      let source = x.(0) in
      let target = x.(1) in
      let amount = x.(2) in 
      (match Map.find source storage.approve_dict with
        | None -> ((1p, General_return), storage)
        | Some approve_dict_lst ->
          match Map.find target approve_dict_lst with
          | None -> ((1p, General_return), storage)
          | Some ledger_amount ->
            if amount > ledger_amount then
              ((1p, General_return), storage)
            else
              let new_target_balance = amount + get_origin_balance target in
              let new_target_amount = abs (ledger_amount - amount) in
              let new_balance = Map.update target (Some new_target_balance) storage.balance in
              let storage1 = storage.balance <- new_balance in
              let new_approve_dict_lst = Map.update target (Some new_target_amount) approve_dict_lst in
              let new_approve_dict = Map.update source (Some new_approve_dict_lst) storage1.approve_dict in
              let storage2 = storage.approve_dict <- new_approve_dict in
              ((0p, General_return), storage2))

    | Allowance x ->
      let source = x.(0) in
      let target = x.(1) in
      match Map.find source storage.approve_dict with
      | None -> ((0p, Balance_return 0p), storage)
      | Some x ->
        match Map.find target x with
        | None -> ((0p, Balance_return 0p), storage)
        | Some x -> ((0p, Balance_return x), storage)

