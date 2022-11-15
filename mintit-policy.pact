(namespace "free")

(module mintit-policy-v3 GOVERNANCE

  (use kip.token-policy-v1 [token-info])

  (implements kip.token-policy-v1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; CONSTANTS 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defconst ARTIST_ACCESS_PASS_TOKEN "t:mintit-creator-access-pass" )
  (defconst TRANSFER_PRECISION 6)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GOVERNANCE 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defschema governance 
    guard:guard 
  )

  (deftable governance-table:{governance})

  (defcap GOVERNANCE 
    () 
    (enforce-keyset "free.mintit-admin-2")
  )

  (defconst EXC_MINTIT_ADMIN_ALREADY_EXISTS "EXC_MINTIT_ADMIN_ALREADY_EXISTS")
  (defconst EXC_MINTIT_ADMIN_DOES_NOT_EXIST "EXC_MINTIT_ADMIN_DOES_NOT_EXIST")

  (defcap ADMIN_ACCESS:bool
    () 
    (enforce-admin-guard)
  )

  (defun enforce-admin-guard:bool 
    () 
    (with-default-read governance-table "admin" 
      { 'guard: "" }
      { 'guard := current-guard }
      (enforce (!= "" (format "{}" [current-guard])) EXC_MINTIT_ADMIN_DOES_NOT_EXIST)
      (enforce-guard current-guard))
    true
  )

  (defun create-admin-guard:bool 
    ( guard:guard 
    )
    (with-default-read governance-table "admin" 
      { 'guard: "" }
      { 'guard := current-guard }
      (enforce (= "" (format "{}" [current-guard])) EXC_MINTIT_ADMIN_ALREADY_EXISTS)
      (enforce-guard (read-keyset "mintit-admin"))
      (enforce-guard guard)
      (insert governance-table "admin" { 'guard: guard }))
    true
  )

  (defun update-admin-guard 
    ( guard:guard 
    )
    (with-default-read governance-table "admin" 
      { 'guard: "" }
      { 'guard := current-guard }
      (enforce (!= "" (format "{}" [current-guard])) EXC_MINTIT_ADMIN_DOES_NOT_EXIST)
      (enforce-guard current-guard)
      (enforce-guard guard)
      (update governance-table "admin" { 'guard: guard }))
    true
  )

  (defun enforce-marmalade-ledger:bool ()
    (enforce-guard (marmalade.ledger.ledger-guard))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; NFT 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;  (defconst EXC_INVALID_MIDDAY_MIDNIGHT_FLIP_NFT_SPEC "EXC_INVALID_MIDDAY_MIDNIGHT_FLIP_NFT_SPEC")
  (defconst EXC_UNKNOWN_NFT_SPEC_TYPE "EXC_UNKNOWN_NFT_SPEC_TYPE")
  (defconst EXC_NFT_NOT_REVEALED "EXC_NFT_NOT_REVEALED")
  (defconst EXC_NFT_NOT_FOUND "EXC_NFT_NOT_FOUND")
  (defconst EXC_NFT_ALREADY_EXISTS "EXC_NFT_ALREADY_EXISTS")
  (defconst EXC_INVALID_NFT_NAME "Invalid NFT name")
  (defconst EXC_INVALID_NFT_DESCRIPTION "Invalid NFT description")
  (defconst EXC_INVALID_NFT_CONTENT_HASH "NFT spec and hash don't match")

  (defconst NFT_TYPE_NORMAL "normal")
  ;  (defconst NFT_TYPE_MIDDAY_MIDNIGHT_FLIP "midday-midnight-flip")
  (defconst NFT_TYPE_MUTATING "mutating")

  (defschema nft
    name:string 
    description:string 
    content-hash:string 
    spec:object
    collection-name:string 
    content-uri:object{kip.token-manifest.mf-uri} 
    marmalade-token-id:string
    edition:integer  
    mint-index:integer  
    mint-time:time 
    creator:string ; k:account 
    current-owner:string ; k:account
    current-owner-guard:guard 
    revealed:bool
    minted:bool
  )
  
  (defschema nft-spec-value-mutating
    type:string 
    sub-specs:[object]  
  )

  (defschema nft-spec 
    type:string 
    value:object
  )

  (deftable nft-table:{nft})

  (defun get-nft-key:string (
    collection-name:string 
    content-hash:string
  )
    (concat [collection-name, ":", content-hash])
  )

  (defun enforce-nft-exists:bool 
    ( 
      collection-name:string
      content-hash:string
    )
    (with-default-read nft-table (get-nft-key collection-name content-hash) { 'minted: false } { 'minted := minted }
      (enforce minted EXC_NFT_NOT_FOUND))
  )

  (defun enforce-nft-does-not-exist:bool 
    ( content-hash:string
    )
    (with-default-read nft-table content-hash { 'minted: false } { 'minted := minted }
      (enforce (= false minted) EXC_NFT_ALREADY_EXISTS))
  )

  (defun read-nft-table (collection-name:string content-hash:string)
    (read nft-table (get-nft-key collection-name content-hash))
  )

  (defun raw-get-nft:object{nft}
    ( content-hash:string 
      collection-name:string
    )
    (enforce-nft-exists collection-name content-hash)
    (read nft-table (get-nft-key collection-name content-hash))
  )

  (defun get-nft:object{nft}
    ( collection-name:string
      content-hash:string
    )
    (enforce-nft-exists collection-name content-hash)
    (at 0 (handle-nfts-by-type [(read nft-table (get-nft-key collection-name content-hash))]))
  )

  (defun search-nfts-by-collection:[object{nft}]
    ( collection-name:string 
    )
    (handle-nfts-by-type 
      (select nft-table (where 'collection-name (= collection-name))))
  )

  (defun search-nfts-by-creator:[object{nft}]
    ( creator:string ; k:account 
    )
    (handle-nfts-by-type 
      (select nft-table (where 'creator (= creator))))
  )

  (defun search-nfts-by-owner:[object{nft}]
    ( owner:string ; k:account 
    )
    (handle-nfts-by-type 
      (select nft-table (where 'current-owner (= owner))))
  )

  (defun search-nfts-by-mint-time:[object{nft}]
    ( max-age-in-mins:integer   
    )
    (let ((search-time (add-time (at 'block-time (chain-data)) (minutes (- max-age-in-mins)))))
    (handle-nfts-by-type 
      (select nft-table (where 'mint-time (>= search-time)))))
  )

  (defun handle-nfts-by-type:[object{nft}]
    ( nfts:[object{nft}]
    )
    (map (handle-nft-by-type (at 'block-time (chain-data))) nfts)
  )

  (defun is-current-sub-spec:bool (current-time:string sub-spec:object)
    (and
      (>= current-time (at 'spec-start-time sub-spec))
      (<= current-time (at 'spec-end-time sub-spec))  
    )
  )

  (defun handle-nft-by-type:object{nft}
    ( block-time:time
      nft:object{nft}
    )
    (bind nft
      { 'name := name 
      , 'content-hash := content-hash 
      , 'spec := spec 
      , 'creator := creator
      , 'collection-name := collection-name
      , 'revealed := revealed 
      }
      
      (let ((type (try "" (at 'type spec))))

        (cond 
          (
            (= type NFT_TYPE_NORMAL)
            nft 
          )
          ( 
            ;  (= type NFT_TYPE_MIDDAY_MIDNIGHT_FLIP)
            (= type NFT_TYPE_MUTATING)
            (let* 
              (
                (time-hhmmss:string (format-time "%H:%M:%S" block-time))
                (sub-specs:[object] (at 'sub-specs spec))
                (current-spec (at 0 
                  (filter (is-current-sub-spec time-hhmmss) sub-specs
                )))
                (updated-nft {
                  'spec: {
                    'value: (at 'value current-spec),
                    'type: (at 'type spec)
                  },
                  'content-uri: (at 'content_uri current-spec)
                })
              )
              (+ updated-nft nft)
            )
          )
          (
            false 
            (enforce false EXC_UNKNOWN_NFT_SPEC_TYPE)
          )
          false
        )
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; init-nft-collection
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defconst EXC_INVALID_COLLECTION_CREATOR "EXC_INVALID_COLLECTION_CREATOR") 
  (defconst EXC_INVALID_COLLECTION_DESCRIPTION "EXC_INVALID_COLLECTION_DESCRIPTION") 
  (defconst EXC_INVALID_COLLECTION_NAME "EXC_INVALID_COLLECTION_NAME") 
  (defconst EXC_INVALID_COLLECTION_TYPE "EXC_INVALID_COLLECTION_TYPE") 
  (defconst EXC_INVALID_COLLECTION_PROVENANCE_HASH "EXC_INVALID_COLLECTION_PROVENANCE_HASH") 
  (defconst EXC_INVALID_COLLECTION_PREMINT_ENDS "EXC_INVALID_COLLECTION_PREMINT_ENDS") 
  (defconst EXC_INVALID_COLLECTION_PREMINT_WHITELIST "EXC_INVALID_COLLECTION_PREMINT_WHITELIST") 
  (defconst EXC_INVALID_COLLECTION_SIZE "EXC_INVALID_COLLECTION_SIZE") 
  (defconst EXC_INVALID_COLLECTION_MINT_PRICE "EXC_INVALID_COLLECTION_MINT_PRICE") 
  (defconst EXC_INVALID_COLLECTION_TOKEN_LIST "EXC_INVALID_COLLECTION_TOKEN_LIST") 
  (defconst EXC_TOKEN_LIST_NON_UNIQUE "Token list should be unique")
  (defconst EXC_NFT_COLLECTION_NOT_FOUND "EXC_NFT_COLLECTION_NOT_FOUND")
  (defconst EXC_NFT_COLLECTION_ALREADY_EXISTS "EXC_NFT_COLLECTION_ALREADY_EXISTS")
  (defconst EXC_NO_CREATOR_ACCESS_PASS "Creator doesn't have an access pass token")

  (defconst PRIVATE_COLLECTION_TYPE "private")
  (defconst PUBLIC_COLLECTION_TYPE "public")

  (defun creator_has_access_pass:bool (creator:string)
    (let ((details (try {} (marmalade.ledger.details ARTIST_ACCESS_PASS_TOKEN creator))))
      (if (= details {}) 
        false
        (>= (at 'balance details) 1.0)
      )
    )
  )

  (defcap INIT_NFT_COLLECTION_EVENT:bool
    ( collection:object{nft-collection}
    )
    @event 
    true 
  )

  (defschema nft-collection
    creator:string  
    description:string  
    name:string 
    type:string 
    provenance-hash:string
    mint-starts:time 
    premint-ends:time 
    premint-whitelist:[string]
    size:integer 
    mint-price:decimal 
    premint-price:decimal 
    mint-royalties:object{royalty-rates}
    sale-royalties:object{royalty-rates}
    num-minted:integer 
    num-revealed:integer 
    fungible:module{fungible-v2}
    next-index:integer
    starting-index:integer
    created:bool
    num-token-initialized:integer
  ) 

  (defun enumerate-list:[object] (in:list)
    "Returns a list of objects {'i:idx, 'v:value} where i is the index, and v the value"
    ; The enumerate should go from 0 to N-1, but since zip takes the shortest, and for clarity we go from 0 to N
    (let ((indexes (enumerate 0 (length in))))
      (zip (lambda (idx x) {'i:idx, 'v:x}) indexes in))
  )

  (deftable nft-collection-table:{nft-collection})

  (defschema nft-collection-token-hash
    collection-name:string
    hash:string
    index:integer
  )

  (deftable nft-collection-token-hashes-table:{nft-collection-token-hash})

  (defun get-nft-collection-token-hash-key:string (
    collection-name:string
    index:integer
  )
    (concat [
      collection-name,
      "-",
      (int-to-str 10 index)
    ])
  )

  (defun get-nft-collection-token-hash:string (
    collection-name:string
    index:integer
  )
    (at 'hash 
      (read nft-collection-token-hashes-table 
        (get-nft-collection-token-hash-key
          collection-name
          index
        )
      )
    )
  )

  (defun write-nft-collection-token-hash:bool (
    collection-name:string
    num-token-initialized:integer
    hash-with-index:object
  )
    (with-capability (ADMIN_ACCESS)
        (let (
          (index (+ (at 'i hash-with-index) num-token-initialized))
          (hash (at 'v hash-with-index))
        )
        (write nft-collection-token-hashes-table 
          (get-nft-collection-token-hash-key collection-name index)
          {
            'collection-name: collection-name,
            'index: index,
            'hash: hash
          }
        )
      )
    )
  )

  (defun write-nft-collection-token-hashes:bool (
    collection-name:string
    token-list:[string]
  )
    (with-capability (ADMIN_ACCESS)
      (let ((token-list-indexed (enumerate-list token-list)))
        (with-read nft-collection-table 
          collection-name
          { 'num-token-initialized := num-token-initialized }

          (map 
            (write-nft-collection-token-hash 
              collection-name
              num-token-initialized) 
            token-list-indexed
          )
          (update nft-collection-table collection-name { 'num-token-initialized : (+ num-token-initialized (length token-list))})
        )
      )
    )
  )

  (defschema init-nft-collection-params
    creator:string
    description:string 
    name:string 
    type:string 
    provenance-hash:string
    mint-starts:time 
    premint-ends:time 
    premint-whitelist:[string]
    size:integer 
    mint-price:decimal 
    premint-price:decimal
    mint-royalties:object{royalty-rates}
    sale-royalties:object{royalty-rates}
    fungible:module{fungible-v2}
    token-list:[string]
  )

  (defun init-nft-collection:bool
    ( params:object{init-nft-collection-params}
    )

    (with-capability (ADMIN_ACCESS)

    (bind params  
      { 'creator := creator
      , 'description := description
      , 'name := name
      , 'type := type 
      , 'provenance-hash := provenance-hash 
      , 'mint-starts := mint-starts
      , 'premint-ends := premint-ends
      , 'premint-whitelist := premint-whitelist
      , 'size := size
      , 'mint-price := mint-price
      , 'premint-price := premint-price
      , 'sale-royalties := sale-royalties
      , 'mint-royalties := mint-royalties
      , 'fungible := fungible
      , 'token-list := token-list
      } 

    (enforce (= (length creator) 66) EXC_INVALID_COLLECTION_CREATOR)
    (enforce (<= (length description) 1024) EXC_INVALID_COLLECTION_DESCRIPTION)
    (enforce (!= "" name) EXC_INVALID_COLLECTION_NAME)
    (enforce (<= (length name) 64) EXC_INVALID_COLLECTION_NAME)
    (enforce (or? (= PUBLIC_COLLECTION_TYPE) (= PRIVATE_COLLECTION_TYPE) type) EXC_INVALID_COLLECTION_TYPE)
    (enforce (= (length provenance-hash) 43) EXC_INVALID_COLLECTION_PROVENANCE_HASH)
    (enforce (>= premint-ends mint-starts) EXC_INVALID_COLLECTION_PREMINT_ENDS)
    (enforce (<= (length premint-whitelist) 1000) EXC_INVALID_COLLECTION_PREMINT_WHITELIST)
    (enforce (>= size 1) EXC_INVALID_COLLECTION_SIZE)
    (enforce (<= size 10000) EXC_INVALID_COLLECTION_SIZE)
    (enforce (>= mint-price 0.0) EXC_INVALID_COLLECTION_MINT_PRICE)
    (enforce (>= premint-price 0.0) EXC_INVALID_COLLECTION_MINT_PRICE)
    (enforce (= (distinct token-list) token-list) EXC_TOKEN_LIST_NON_UNIQUE)
    ;  (enforce (creator_has_access_pass creator))

    (verify-royalty-rates mint-royalties "mint" fungible)
    (verify-royalty-rates sale-royalties "sale" fungible)
    
    (let*
      (
        (starting-index (mod (random-integer) size))
        (collection 
          { 'creator: creator
          , 'description: description
          , 'name: name
          , 'type: type
          , 'provenance-hash: provenance-hash 
          , 'mint-starts: mint-starts
          , 'premint-ends: premint-ends
          , 'premint-whitelist: premint-whitelist 
          , 'size: size
          , 'mint-price: mint-price
          , 'premint-price: premint-price
          , 'sale-royalties: sale-royalties
          , 'mint-royalties: mint-royalties
          , 'fungible: fungible
          , 'created: true
          , 'num-minted: 0
          , 'num-revealed: 0
          , 'next-index: starting-index
          , 'starting-index: starting-index
          , 'num-token-initialized: 0
          })
      )

    (enforce-nft-collection-does-not-exist name)
    (write nft-collection-table name collection)
    (write-nft-collection-token-hashes name token-list)

    (emit-event (INIT_NFT_COLLECTION_EVENT collection)))))
  )

  (defun update-nft-collection-mint-price:bool (collection-name:string new-mint-price:decimal)
    (with-capability (ADMIN_ACCESS)
      (enforce (>= new-mint-price 0.0) EXC_INVALID_COLLECTION_MINT_PRICE)
      (update nft-collection-table collection-name 
        { 
          'mint-price: new-mint-price
        }
      )
    )
  )
  
  (defun update-nft-collection-premint-price:bool (collection-name:string new-premint-price:decimal)
    (with-capability (ADMIN_ACCESS)
      (enforce (>= new-premint-price 0.0) EXC_INVALID_COLLECTION_MINT_PRICE)
      (update nft-collection-table collection-name 
        { 
          'premint-price: new-premint-price
        }
      )
    )
  )

  (defun add-nft-tokens:bool (collection-name:string token-list:[string])
    (with-capability (ADMIN_ACCESS)
      (write-nft-collection-token-hashes collection-name token-list)
    )
  )

  (defun enforce-nft-collection-exists:bool 
    ( name:string 
    )
    (with-default-read nft-collection-table name { 'created: false } { 'created := created }
      (enforce created EXC_NFT_COLLECTION_NOT_FOUND))
  )

  (defun enforce-nft-collection-does-not-exist:bool 
    ( name:string 
    )
    (with-default-read nft-collection-table name { 'created: false } { 'created := created }
      (enforce (= false created) EXC_NFT_COLLECTION_ALREADY_EXISTS))
  )

  (defun get-nft-collection:object{nft-collection}
    ( name:string 
    )
    (enforce-nft-collection-exists name)
    (read nft-collection-table name)
  ) 

  (defun search-nft-collections-by-creator:[object{nft-collection}]
    ( creator:string ; k:account 
    )
    (select nft-collection-table (where 'creator (= creator)))
  )

  (defun compute-nft-collection-mint-royalties:object{royalty-payouts}
    ( collection:string 
      minter:string ; k:account
    )
    (bind (get-nft-collection collection) 
      { 'mint-royalties := rates
      , 'mint-price := mint-price 
      , 'premint-price:= premint-price
      , 'fungible := fungible
      }
      (create-royalty-payouts rates fungible minter mint-price)
    )
  )

  (defun compute-nft-collection-sale-royalties:object{royalty-payouts}
    ( collection:string 
      buyer:string ; k:account
      price:decimal 
    )
    (bind (get-nft-collection collection) 
      { 'sale-royalties := rates
      , 'fungible := fungible
      }
      (create-royalty-payouts rates fungible buyer price)
    )
  )

  (defun get-mint-price:decimal (collection-name:string account:string)
    (bind (chain-data) { "block-time" := curr-time }
      (bind (read nft-collection-table collection-name) {
        'mint-price := mint-price,
        'premint-price := premint-price,
        'premint-whitelist := premint-whitelist,
        'premint-ends := premint-ends
      }
        (if (and
            (< curr-time premint-ends)
            (contains account premint-whitelist)
          )
          premint-price
          mint-price
        )
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; mint-nft
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defconst EXC_COLLECTION_FULLY_MINTED "EXC_COLLECTION_FULLY_MINTED") 
  (defconst EXC_COLLECTION_MINT_NOT_STARTED "EXC_COLLECTION_MINT_NOT_STARTED") 
  (defconst EXC_MINTER_NOT_WHITELISTED "EXC_MINTER_NOT_WHITELISTED") 
  (defconst EXC_MINTER_NOT_CREATOR "EXC_MINTER_NOT_CREATOR") 
  (defconst EXC_MINTER_GUARD_FUNGIBLE_MISMATCH "EXC_MINTER_GUARD_FUNGIBLE_MISMATCH") 
  (defconst EXC_MINTER_FUNGIBLE_ACCOUNT_DOES_NOT_EXIST "EXC_MINTER_FUNGIBLE_ACCOUNT_DOES_NOT_EXIST") 
  
  (defcap MINT_NFT_EVENT:bool 
    ( nft:object{nft}
    )
    @event 
    true
  )

  (defcap MINT_NFT_REQUEST:bool 
    ()
    true
  )

  (defschema mint-nft-params 
    account:string 
    guard:guard 
    collection-name:string 
  )

  (defun mint-nft:bool
    ( params:object{mint-nft-params})
      
    (bind params 
      { 'account := account 
      , 'guard := guard  
      , 'collection-name := collection-name  
      }

      (enforce-nft-collection-exists collection-name)

      (bind (read nft-collection-table collection-name) 
        { 'creator := creator
        , 'type := type
        , 'mint-starts := mint-starts
        , 'premint-ends := premint-ends
        , 'premint-whitelist := premint-whitelist 
        , 'size := size
        , 'mint-royalties := mint-royalties
        ;  , 'token-list := token-list
        , 'num-minted := num-minted
        , 'next-index := next-index
        , 'fungible := fungible:module{fungible-v2}
        }

        (bind (chain-data)
          { 'block-time := curr-time 
          }

          (enforce (> size num-minted) EXC_COLLECTION_FULLY_MINTED)
          (enforce (<= mint-starts curr-time) EXC_COLLECTION_MINT_NOT_STARTED)
          (enforce (or (>= curr-time premint-ends) (contains account premint-whitelist)) EXC_MINTER_NOT_WHITELISTED)
          (enforce (or (= type PUBLIC_COLLECTION_TYPE) (= account creator)) EXC_MINTER_NOT_CREATOR)
          
          (enforce-fungible-account account guard fungible)

          (with-capability (MINT_NFT_REQUEST) (enforce-guard guard))
          
          (let*
            (
              (content-hash (get-nft-collection-token-hash collection-name next-index))
              (nft:object{nft}
                { 'name: ""
                , 'description: ""
                , 'content-hash: content-hash
                , 'spec: { 'type: NFT_TYPE_NORMAL, 'value: {} }
                , 'collection-name: collection-name 
                , 'content-uri: (kip.token-manifest.uri "" "")
                , 'marmalade-token-id: ""
                , 'edition: -1
                , 'mint-time: curr-time
                , 'mint-index: next-index 
                , 'creator: creator 
                , 'current-owner: account 
                , 'current-owner-guard: guard 
                , 'revealed: false 
                , 'minted: true 
                })
              (account-mint-price (get-mint-price collection-name account))
            )

            (enforce-nft-does-not-exist content-hash)
            (write nft-table (get-nft-key collection-name content-hash) nft) 

            (update nft-collection-table collection-name 
              { 
                'next-index: (mod (+ 1 next-index) size),
                'num-minted: (+ num-minted 1)
              }
            )

            ; TODO better error message when minter is among the stakeholders. 
            ; currently it is sender cannot be the receiver k:alice of a transfer 
            ; or ignore royalty payout in this case
            (create-execute-royalty-payouts mint-royalties fungible account account-mint-price)
          
            (emit-event (MINT_NFT_EVENT nft))
          )
        )
      )
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; reveal-nft
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defconst EXC_NFT_ALREADY_REVEALED "EXC_NFT_ALREADY_REVEALED")
  (defconst EXC_NFT_REVEAL_COLLECTION_MISMATCH "EXC_NFT_REVEAL_COLLECTION_MISMATCH")
  (defconst EXC_NFT_REVEAL_CREATOR_MISMATCH "EXC_NFT_REVEAL_CREATOR_MISMATCH")
  (defconst EXC_NFT_REVEAL_CONTENT_HASH_MISMATCH "EXC_NFT_REVEAL_CONTENT_HASH_MISMATCH")

  (defcap REVEAL_NFT_EVENT:bool 
    ( nft:object{nft}
    )
    @event 
    true
  )

  (defschema reveal-nft-params 
    name:string 
    description:string 
    content-hash:string 
    spec:object
    collection-name:string 
    content-uri:object{kip.token-manifest.mf-uri}
    marmalade-token-id:string 
    edition:integer 
    creator:string  
  )

  (defun validate-nft-name:bool (name:string)
    (and
      (>= (length name) 3)
      (<= (length name) 64)
    )
  )

  (defun validate-nft-description:bool (description:string)
    (<= (length description) 1024)
  )

  (defun validate-nft-content-hash:bool (content-hash:string spec:object)
    (= content-hash (hash spec))
  )

  (defun reveal-nft:bool
    ( params:object{reveal-nft-params}
    )

    (with-capability (ADMIN_ACCESS)

    (bind params 
      { 'name := name 
      , 'description := description 
      , 'content-hash := params-content-hash 
      , 'spec := spec 
      , 'collection-name := params-collection-name 
      , 'content-uri := content-uri 
      , 'marmalade-token-id := marmalade-token-id 
      , 'edition := edition  
      , 'creator := params-creator  
      }

    (with-default-read nft-table (get-nft-key params-collection-name params-content-hash)
      { 'collection-name: ""
      , 'creator: ""
      , 'content-hash: params-content-hash
      , 'current-owner: ""
      , 'current-owner-guard: ""
      , 'mint-index: -1
      , 'mint-time: EPOCH
      , 'revealed: false 
      , 'minted: false 
      }
      { 'collection-name := collection-name
      , 'creator := creator 
      , 'content-hash := content-hash 
      , 'current-owner := current-owner 
      , 'current-owner-guard := current-owner-guard 
      , 'mint-time := mint-time 
      , 'mint-index := mint-index
      , 'revealed := revealed  
      , 'minted := minted 
      } 

    ; TODO enforce edition EXC_INVALID_NFT_EDITION
    (enforce (validate-nft-name name) EXC_INVALID_NFT_NAME)
    (enforce (validate-nft-description description) EXC_INVALID_NFT_DESCRIPTION)
    ;  (enforce (validate-nft-content-hash content-hash spec) EXC_INVALID_NFT_CONTENT_HASH)
    (enforce minted EXC_NFT_NOT_FOUND)
    (enforce (= false revealed) EXC_NFT_ALREADY_REVEALED)
    (enforce (= collection-name params-collection-name) EXC_NFT_REVEAL_COLLECTION_MISMATCH)
    (enforce (= creator params-creator) EXC_NFT_REVEAL_CREATOR_MISMATCH)
    (enforce (= content-hash params-content-hash) EXC_NFT_REVEAL_CONTENT_HASH_MISMATCH)

    (let*
      (
        (nft 
          { 'name: name 
          , 'description: description 
          , 'content-hash: content-hash
          , 'spec: spec 
          , 'collection-name: collection-name 
          , 'content-uri: content-uri 
          , 'marmalade-token-id: marmalade-token-id 
          , 'edition: edition
          , 'mint-index: mint-index 
          , 'mint-time: mint-time
          , 'creator: creator 
          , 'current-owner: current-owner 
          , 'current-owner-guard: current-owner-guard 
          , 'revealed: true 
          , 'minted: true 
          })
      )

    (update nft-table (get-nft-key collection-name content-hash) nft)

    (emit-event (REVEAL_NFT_EVENT nft))

    (sync-with-marmalade nft)))))
  )

  (defschema nft-manifest-datum 
    name:string 
    description:string 
    content-hash:string 
    spec:object 
    creator:string  
    collection-name:string 
    content-uri:object{kip.token-manifest.mf-uri}
    edition:integer
    mint-index:integer 
    mint-time:time 
  )

  (defun sync-with-marmalade:bool
    ( nft:object{nft}
    )
    
    (require-capability (ADMIN_ACCESS))

    (bind nft 
      { 'name := name 
      , 'description := description 
      , 'content-hash := content-hash
      , 'spec := spec 
      , 'creator := creator 
      , 'collection-name := collection-name 
      , 'content-uri := content-uri 
      , 'marmalade-token-id := marmalade-token-id 
      , 'edition := edition
      , 'mint-index := mint-index 
      , 'mint-time := mint-time 
      , 'current-owner := current-owner 
      , 'current-owner-guard := current-owner-guard
      }

    (let*
      (
        (datum-object:object{nft-manifest-datum}
          { 'name: name 
          , 'description: description 
          , 'content-hash: content-hash
          , 'spec: spec
          , 'creator: creator
          , 'collection-name: collection-name 
          , 'content-uri: content-uri
          , 'edition: edition
          , 'mint-index: mint-index 
          , 'mint-time: mint-time
          })

        (datum-uri (kip.token-manifest.uri "pact:schema" "free.mintit-policy-v3.nft-manifest-datum"))
        (manifest-datum (kip.token-manifest.create-datum datum-uri datum-object)) 
        (manifest-uri content-uri) 
        (nft-manifest (kip.token-manifest.create-manifest manifest-uri [manifest-datum]))
        (token-id marmalade-token-id)
        (token-precision 0)
      )

    (marmalade.ledger.create-token token-id token-precision nft-manifest mintit-policy-v3)
    (marmalade.ledger.create-account token-id current-owner current-owner-guard)
    (install-capability (marmalade.ledger.MINT token-id current-owner 1.0))
    (marmalade.ledger.mint token-id current-owner current-owner-guard 1.0)))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; marmalade policy
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defconst EXC_INVALID_TOKEN_AMOUNT "EXC_INVALID_TOKEN_AMOUNT")

  (defun enforce-mint:bool
    ( token:object{token-info}
      account:string  
      guard:guard 
      amount:decimal
    )
    (enforce-marmalade-ledger)
    (require-capability (ADMIN_ACCESS))
  )

  (defun enforce-init:bool
    ( token:object{token-info}
    )
    (enforce-marmalade-ledger)
    (require-capability (ADMIN_ACCESS))
  )

  ;; Offer/sale/buy

  (defcap QUOTE:bool
    ( sale-id:string
      token-id:string
      amount:decimal
      price:decimal
      sale-price:decimal
      spec:object{quote-spec}
    )
    @doc "For event emission purposes"
    @event
    true
  )

  (defconst QUOTE-MSG-KEY "quote"
    @doc "Payload field for quote spec")

  (defschema quote-spec
    @doc "Quote data to include in payload"
    fungible:module{fungible-v2}
    price:decimal
    recipient:string
    recipient-guard:guard
    )

  (defschema quote-schema
    id:string
    spec:object{quote-spec})

  (deftable quotes-table:{quote-schema})

  (defun enforce-sale-pact:bool (sale:string)
    "Enforces that SALE is id for currently executing pact"
    (enforce (= sale (pact-id)) "Invalid pact/sale id")
  )

  (defun enforce-buy:bool
    ( token:object{token-info}
      seller:string
      buyer:string
      buyer-guard:guard
      amount:decimal
      sale-id:string 
    )
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-marmalade-ledger)
    (enforce-sale-pact sale-id)
    (with-read quotes-table sale-id { 'id:= qtoken, 'spec:= spec:object{quote-spec} }
      (enforce (= qtoken (at 'id token)) "incorrect sale token")
      (bind spec
        { 'fungible := fungible:module{fungible-v2}
        , 'price := price:decimal
        , 'recipient := recipient:string
        }
        (fungible::transfer buyer recipient (* amount price))
      )
    )
    true
  )

  (defun enforce-offer:bool
    ( token:object{token-info}
      seller:string ; unused 
      amount:decimal
      sale-id:string
    )
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-marmalade-ledger)
    (enforce-sale-pact sale-id)
    (let* ( (spec:object{quote-spec} (read-msg QUOTE-MSG-KEY))
            (fungible:module{fungible-v2} (at 'fungible spec) )
            (price:decimal (at 'price spec))
            (recipient:string (at 'recipient spec))
            (recipient-guard:guard (at 'recipient-guard spec))
            (recipient-details:object (fungible::details recipient))
            (sale-price:decimal (* amount price)) )
      (fungible::enforce-unit sale-price)
      (enforce (< 0.0 price) "Offer price must be positive")
      (enforce (=
        (at 'guard recipient-details) recipient-guard)
        "Recipient guard does not match")
      (insert quotes-table sale-id { 'id: (at 'id token), 'spec: spec })
      (emit-event (QUOTE sale-id (at 'id token) amount price sale-price spec))
    )
    true
  )

  (defun enforce-transfer:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      amount:decimal 
    )
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-marmalade-ledger)
    true
  )

  (defun enforce-crosschain:bool
    ( token:object{token-info}
      sender:string
      guard:guard
      receiver:string
      target-chain:string
      amount:decimal 
    )
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-marmalade-ledger)
    (enforce false "xtransfer prohibited")
  )

  (defun enforce-burn:bool
    ( token:object{token-info}
      account:string
      amount:decimal
    )
    (enforce (= 1.0 amount) EXC_INVALID_TOKEN_AMOUNT)
    (enforce-marmalade-ledger)
    (enforce false "burn not implemented") 
  )
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ROYALTY RATES & PAYOUTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defconst EXC_INVALID_ROYALTY_DESCRIPTION "EXC_INVALID_ROYALTY_DESCRIPTION")
  (defconst EXC_INVALID_ROYALTY_STAKEHOLDER "EXC_INVALID_ROYALTY_STAKEHOLDER")
  (defconst EXC_INVALID_ROYALTY_RATE "EXC_INVALID_ROYALTY_RATE")
  (defconst EXC_DUPLICATED_ROYALTY_STAKEHOLDER "EXC_DUPLICATED_ROYALTY_STAKEHOLDER")
  (defconst EXC_DUPLICATED_ROYALTY_DESCRIPTION "EXC_DUPLICATED_ROYALTY_DESCRIPTION")
  (defconst EXC_INVALID_MINT_ROYALTY_RATE_SUM "EXC_INVALID_MINT_ROYALTY_RATE_SUM")
  (defconst EXC_INVALID_SALE_ROYALTY_RATE_SUM "EXC_INVALID_SALE_ROYALTY_RATE_SUM")
  (defconst EXC_MISMATCHED_ROYALTY_STAKEHOLDER_GUARD "EXC_MISMATCHED_ROYALTY_STAKEHOLDER_GUARD")

  (defconst NO_ROYALTY_RATES { 'rates: [] })

  (defschema royalty-rate 
    description:string 
    stakeholder:string ; k:account
    stakeholder-guard:guard 
    rate:decimal 
  )

  (defschema royalty-rates
    rates:[object{royalty-rate}]
  )

  (defun create-royalty-rates:object{royalty-rates}
    ( rates:[object{royalty-rate}]
    )
    { 'rates: rates
    } 
  )

  (defun add-royalty-rate:object{royalty-rates}
    ( o:object{royalty-rates}
      rate:object{royalty-rate}
    )
    (+ { 'rates: (+ [rate] (at 'rates o)) } o)
  )

  (defun verify-royalty-rates:bool 
    ( o:object{royalty-rates}
      type:string
      fungible:module{fungible-v2}
    )
    (bind o { "rates" := rates }
      (map (verify-royalty-rate fungible) rates)
      (enforce (unique-field rates "stakeholder") EXC_DUPLICATED_ROYALTY_STAKEHOLDER)
      (enforce (unique-field rates "description") EXC_DUPLICATED_ROYALTY_DESCRIPTION)
      (if (= type "mint")
        (enforce (= (sum-field rates "rate") 1.0) EXC_INVALID_MINT_ROYALTY_RATE_SUM)
        (enforce (and (>= (sum-field rates "rate") 0.0) (<= (sum-field rates "rate") 1.0)) EXC_INVALID_SALE_ROYALTY_RATE_SUM)
      )
      true
    )
  )

  (defun verify-royalty-rate:bool 
    ( fungible:module{fungible-v2} 
      o:object{royalty-rate}
    )
    (bind o  
      { 'description := description 
      , 'stakeholder := stakeholder  
      , 'stakeholder-guard := stakeholder-guard  
      , 'rate := rate 
      }
      (enforce (!= "" description) EXC_INVALID_ROYALTY_DESCRIPTION)
      (enforce (!= "" stakeholder) EXC_INVALID_ROYALTY_STAKEHOLDER)
      (enforce (>= rate 0.0) EXC_INVALID_ROYALTY_RATE)
      (enforce (<= rate 1.0) EXC_INVALID_ROYALTY_RATE)
      (verify-royalty-stakeholder-guard fungible stakeholder stakeholder-guard)
    )
  )

  (defun verify-royalty-stakeholder-guard:bool 
    ( fungible:module{fungible-v2} 
      stakeholder:string ; k:account
      stakeholder-guard:guard
    )
    (bind (fungible::details stakeholder)  
      { 'guard := fungible-guard 
      }
      (enforce (= fungible-guard stakeholder-guard) EXC_MISMATCHED_ROYALTY_STAKEHOLDER_GUARD)
    )
  )

  (defschema royalty-payout
    stakeholder:string ; k:account
    rate:decimal 
    payout:decimal 
  )

  (defschema royalty-payouts
    payouts:[object{royalty-payout}]
    from:string ; k:account
    fungible:module{fungible-v2}
  )
  
  (defun create-execute-royalty-payouts:bool
    ( rates:object{royalty-rates}
      fungible:module{fungible-v2}
      from:string ; k:account
      amount:decimal
    )
    (execute-royalty-payouts (create-royalty-payouts rates fungible from amount))
  )

  (defun create-royalty-payouts:object{royalty-payouts}
    ( o:object{royalty-rates}
      fungible:module{fungible-v2}
      from:string ; k:account
      amount:decimal
    )
    (bind o 
      { 'rates := rates 
      }
      { 'payouts: (map (create-royalty-payout amount fungible) rates)
      , 'from: from 
      , 'fungible: fungible
      })
  )

  (defun create-royalty-payout:object{royalty-payout}  
    ( amount:decimal
      fungible:module{fungible-v2}
      o:object{royalty-rate}
    )
    (bind o 
      { 'stakeholder := to 
      , 'rate := rate 
      }
      { 'stakeholder: to  
      , 'rate: rate 
      , 'payout: (round (* amount rate) TRANSFER_PRECISION)
      }
    )
  )

  (defun execute-royalty-payouts:bool 
    ( o:object{royalty-payouts}
    )
    (bind o 
      { 'payouts := payouts:[object{royalty-payout}]
      , 'from := from:string
      , 'fungible := fungible:module{fungible-v2}
      }
      (map (execute-royalty-payout from fungible) payouts)
      true
    )
  )

  (defun execute-royalty-payout:bool
    ( from:string ; k:account
      fungible:module{fungible-v2}
      o:object{royalty-payout}
    )
    (bind o 
      { 'stakeholder := to:string 
      , 'rate := rate:decimal 
      , 'payout := payout:decimal 
      }
      (if (> payout 0.0) (fungible::transfer from to payout) "NO ROYALTY")
      true
    )
  )

  (defun total-royalty-payout:decimal
    ( o:object{royalty-payouts}
    )
    (sum-field (at 'payouts o) 'payout)
  )  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; UTILS 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defconst EPOCH:time (time "1970-01-01T00:00:00Z"))

  (defun unique-field 
    ( xs:[object] 
      k:string 
    )
    (let 
      ((ys (map (at k) xs)))
      (= (distinct ys) ys))
  )

  (defun sum-field:decimal  
    ( xs:[object] 
      k:string 
    )
    (fold (+) 0.0 (map (at k) xs))
  )

  (defun random-integer:integer
    ()
    (bind (chain-data)
      { 'block-height := block-height
      , 'block-time := block-time
      }
    (round (* block-height (diff-time block-time (time "1970-01-01T00:00:00Z")))))
  )

  (defconst EXC_FUNGIBLE_ACCOUNT_NOT_FOUND "EXC_FUNGIBLE_ACCOUNT_NOT_FOUND")
  (defconst EXC_FUNGIBLE_GUARD_MISMATCH "EXC_FUNGIBLE_GUARD_MISMATCH")

  (defun enforce-fungible-account:bool
    ( account:string 
      guard:guard
      fungible:module{fungible-v2}
    )
    (let ((details (try {} (fungible::details account))))
    (enforce (!= {} details) 
      (format "{}: account={} not found for fungible={}" 
        [EXC_FUNGIBLE_ACCOUNT_NOT_FOUND account fungible]))
    (enforce (= guard (at 'guard details)) 
      (format "{}: account={} for fungible={} has guard={} but expecting guard={}"
        [EXC_FUNGIBLE_GUARD_MISMATCH account fungible (at 'guard details) guard])))
  )

)
