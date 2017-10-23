[%%version 0.12]

type parameter_t = 
| Token_info
| Balance of key_hash
| Transfer_token of (key_hash * nat)
| Approve of (key_hash * nat)
| Transfer_token_from_approve_dict of (key_hash * key_hash * nat)
(* | Allowance of (key_hash * key_hash) *)

type storage_t = {
  approve_dict : (key_hash, (key_hash, nat) map) map;
  balance : (key_hash, nat) map;
  info: nat * nat * string * string;
}
(* 
  init:
  (Pair (Map ) (Pair (Map ) (Pair 100000 (Pair 2 (Pair "BettingWin" "BTW" ) ) ) ) )
*)

type return_t =
| General_return of nat
| Token_info_return of nat * nat * string * string
| Balance_return of nat

(* 
The nat of General_return represents:
  0: success
  1: balance insufficient
  2: request amount greater than approved amount
 *)  

let%entry main (parameter : parameter_t) (storage : storage_t) [%return : return_t] =
  match parameter with
    | Token_info -> 
      (Token_info_return storage.info, storage)
    
    | Balance from -> 
      let result = match Map.find from storage.balance with
        | None -> 0p
        | Some x -> x 
      in
      (Balance_return result, storage)
    
    | Transfer_token x -> 
      let source = Contract.manager (Source : (unit, unit) contract) in
      let source_balance = match Map.find source storage.balance with
        | None -> 0p
        | Some x -> x
      in
      let target = x.(0) in
      let amount = x.(1) in
      (match%nat (source_balance - amount) with
      | Minus _ ->
        (General_return 1p, storage)
      | Plus diff ->
        let new_source_balance = diff in
        let new_target_balance = amount + (match Map.find target storage.balance with
          | None -> 0p
          | Some x -> x)
        in
        let balance = Map.update source (Some new_source_balance) storage.balance in
        let balance = Map.update target (Some new_target_balance) balance in
        (General_return 0p, storage.balance <- balance))
    
    | Approve x -> 
      let source = Contract.manager (Source : (unit, unit) contract) in
      let target = x.(0) in
      let amount = x.(1) in
      let inner_approve_dict = 
        match Map.find source storage.approve_dict with
        | None -> (Map [] : (key_hash, nat) map)
        | Some x -> x
      in
      let inner_approve_dict = Map.update target (Some amount) inner_approve_dict in
      let storage = storage.approve_dict <- Map.update source (Some inner_approve_dict) storage.approve_dict in
      (General_return 0p, storage)
    
    | Transfer_token_from_approve_dict x -> 
      let source = x.(0) in
      let target = x.(1) in
      let amount = x.(2) in
      (match Map.find source storage.approve_dict with
        | None -> 
          (General_return 2p, storage)
        | Some approve_dict_lst ->
          match Map.find target approve_dict_lst with
          | None -> 
            (General_return 2p, storage)
          | Some approved_amount ->
            match%nat (approved_amount - amount) with
            | Minus _ ->
              (General_return 2p, storage)
            | Plus approved_amount_diff ->
              let origin_source_balance = match Map.find source storage.balance with
                | None -> 0p
                | Some x -> x 
              in
              match%nat (origin_source_balance - amount) with
              | Minus _ -> 
                (General_return 1p, storage)
              | Plus origin_source_balance_diff ->
                let new_target_balance = amount + (match Map.find target storage.balance with
                  | None -> 0p
                  | Some x -> x) 
                in
                let new_target_approved_amount = approved_amount_diff in
                let new_source_balance = origin_source_balance_diff in

                let balance = Map.update target (Some new_target_balance) storage.balance in
                let balance = Map.update source (Some new_source_balance) balance in
                let stroage = storage.balance <- balance in
                let new_approve_dict_lst = Map.update target (Some new_target_approved_amount) approve_dict_lst in
                let new_approve_dict = Map.update source (Some new_approve_dict_lst) stroage.approve_dict in
                let storage = storage.approve_dict <- new_approve_dict in
                (General_return 0p, storage))

(*     | Allowance x ->
      let source = x.(0) in
      let target = x.(1) in
      match Map.find source storage.approve_dict with
      | None -> (Balance_return 0p, storage)
      | Some x ->
        match Map.find target x with
        | None -> (Balance_return 0p, storage)
        | Some x -> (Balance_return x, storage) *)

