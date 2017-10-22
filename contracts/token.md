## token standard

### defination
`pos(x)` means the x-th position at a `or` type.

example
```
(or 
  nat            <- pos(0)
  (or      
    string       <- pos(1)
    key_hash))   <- pos(2)
```

### functions
| function name | parameter type | return type | description |
| --- | --- | --- | --- |
| get token info | `unit` at pos(0) | `(pair nat (pair nat (pair string string)))` at pos(1) | |
| get balance | `key_hash` at pos(1) | `nat` at pos(2) | |
| transfer token | `(pair key_hash nat)` at pos(2) | `nat` at pos(0) | see error code |
| approve | `(pair key_hash nat)` at pos(3) | `nat` at pos(0) | see error code |
| transfer from approved dict | `(pair key_hash (pair key_hash nat))` at pos(4) | `nat` at pos(0) | see error code |

### error code
```
0: success
1: balance insufficient
2: request amount greater than approved amount
```