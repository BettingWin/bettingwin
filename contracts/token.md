## token standard

### notice
`pos(x)` means the x-th position at a `or` type or a `pair` type.

### functions
| function name | parameter type | return type | description |
| --- | --- | --- | --- |
| get token info | `unit` at pos(0) | `(pair nat (pair nat (pair string string)))` at pos(1) | |
| get balance | `key_hash` at pos(1) | `nat` at pos(2) | |
| transfer token | `(pair key_hash nat)` at pos(2) | `nat` at pos(0) | the return nat is the error code, 0 means no error |
| approve | `(pair key_hash nat)` at pos(3) | `nat` at pos(0) | the return nat is the error code, 0 means no error |
| transfer from approved dict | `(pair key_hash (pair key_hash nat))` at pos(4) | `nat` at pos(0) | the return nat is the error code, 0 means no error |
