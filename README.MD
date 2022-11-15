# MintIt Alt Token Sale

This contract allows people to create an alternate token that can be used to mint on MintIt.

Steps

1. Create an alt token sale by calling:
```lisp
(defun create-mintit-alt-coin:string 
  (
    sale-id:string
    collection-name:string
    bank-account:string
    alt-coin:module{fungible-v2}
    alt-kda-ratio:decimal
    require-whitelist:bool
    guard:guard
  )
)
```
3. Fund the bank account that was created for the alt token sale. These funds will be returned to you, minus the MintIt royalties.
4. If you made the sale require whitelist, add them by calling
```lisp
(defun add-whitelisted:[string] 
  (sale-id:string whitelisted:[string])
)
```
5. Build a website that lets users call
```lisp
(defun mint-with-alt-coin:string 
  (
    buyer:string
    sale-id:string
    guard:guard
  )
)

(defun claim-nfts:[string] 
  (buyer:string guard:guard)
)
```

You can get the nfts that a person has purchased through this method using
```lisp
(defun get-owned-nfts:[object] (buyer:string))
```

You can get the revealed NFTs by calling
```lisp
(defun get-revealed-nfts:[object] (buyer:string))
```
6. Once you have finished, you can withdraw your funds from the bank by calling
```lisp
(defun withdraw-from-bank:string 
  (
    sale-id:string 
    receiver:string 
    amount:decimal
  )
)
```

## Notes

`claim-nfts` can only be called once the NFT has been revealed by MintIt.

For users to mint and claim you must pass in the following capability:
```lisp
(defcap MANAGED (buyer:string))
```

To add whitelist and to withdraw, you must provide the following capability:
```lisp
(defcap MINTIT_ALT_COIN_MANAGER (sale-id:string))
```