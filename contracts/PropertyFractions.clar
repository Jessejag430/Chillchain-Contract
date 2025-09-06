;; PropertyFractions Contract
;; Enables fractional ownership of land title NFTs

(define-constant ERR-NOT-AUTHORIZED (err u500))
(define-constant ERR-PROPERTY-NOT-FOUND (err u501))
(define-constant ERR-FRACTION-NOT-FOUND (err u502))
(define-constant ERR-INSUFFICIENT-SHARES (err u503))
(define-constant ERR-INVALID-SHARE-AMOUNT (err u504))
(define-constant ERR-PROPERTY-NOT-FRACTIONALIZED (err u505))
(define-constant ERR-ALREADY-FRACTIONALIZED (err u506))
(define-constant ERR-MINIMUM-SHARES-NOT-MET (err u507))

;; Property fraction data
(define-map fractional-properties
  uint ;; title-id
  {
    original-owner: principal,
    total-shares: uint,
    available-shares: uint,
    share-price: uint,
    fractionalized-date: uint,
    minimum-shares: uint,
    is-active: bool
  }
)

;; Individual shareholder records
(define-map property-shareholders
  { title-id: uint, shareholder: principal }
  {
    shares-owned: uint,
    purchase-date: uint,
    total-invested: uint,
    voting-rights: bool
  }
)

;; Track shareholders per property
(define-map property-shareholder-list
  uint ;; title-id
  (list 50 principal)
)

;; Track properties owned by each shareholder
(define-map shareholder-properties
  principal
  (list 20 uint)
)

;; Dividend distribution records
(define-map dividend-distributions
  { title-id: uint, distribution-id: uint }
  {
    total-amount: uint,
    per-share-amount: uint,
    distribution-date: uint,
    distributed-by: principal,
    claimed-amount: uint
  }
)

;; Individual dividend claims
(define-map shareholder-dividends
  { title-id: uint, distribution-id: uint, shareholder: principal }
  {
    amount-due: uint,
    claimed: bool,
    claim-date: (optional uint)
  }
)

;; Voting proposals for fractional properties
(define-map property-proposals
  { title-id: uint, proposal-id: uint }
  {
    proposer: principal,
    proposal-type: (string-ascii 32),
    description: (string-ascii 256),
    voting-deadline: uint,
    votes-for: uint,
    votes-against: uint,
    total-voting-shares: uint,
    executed: bool,
    proposal-data: (optional (string-ascii 256))
  }
)

;; Individual shareholder votes
(define-map shareholder-votes
  { title-id: uint, proposal-id: uint, shareholder: principal }
  {
    vote: bool, ;; true = for, false = against
    shares-voted: uint,
    vote-date: uint
  }
)

(define-data-var next-distribution-id uint u1)
(define-data-var next-proposal-id uint u1)

;; Fractionalize a property
(define-public (fractionalize-property 
    (title-id uint) 
    (total-shares uint) 
    (share-price uint)
    (minimum-shares uint))
  (let 
    ((property-owner (unwrap! (contract-call? .landTitle get-title-owner title-id) ERR-PROPERTY-NOT-FOUND)))
    
    ;; Only property owner can fractionalize
    (asserts! (is-eq tx-sender property-owner) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? fractional-properties title-id)) ERR-ALREADY-FRACTIONALIZED)
    (asserts! (> total-shares u0) ERR-INVALID-SHARE-AMOUNT)
    (asserts! (> share-price u0) ERR-INVALID-SHARE-AMOUNT)
    (asserts! (<= minimum-shares total-shares) ERR-INVALID-SHARE-AMOUNT)
    
    (map-set fractional-properties title-id {
      original-owner: tx-sender,
      total-shares: total-shares,
      available-shares: total-shares,
      share-price: share-price,
      fractionalized-date: stacks-block-height,
      minimum-shares: minimum-shares,
      is-active: true
    })
    
    (ok true)
  )
)

;; Purchase shares in a fractionalized property
(define-public (purchase-shares (title-id uint) (shares-to-buy uint))
  (let 
    ((fraction-data (unwrap! (map-get? fractional-properties title-id) ERR-PROPERTY-NOT-FRACTIONALIZED))
     (share-price (get share-price fraction-data))
     (available-shares (get available-shares fraction-data))
     (total-cost (* shares-to-buy share-price))
     (current-shareholders (default-to (list) (map-get? property-shareholder-list title-id)))
     (shareholder-props (default-to (list) (map-get? shareholder-properties tx-sender)))
     (existing-shares (default-to 
                        {shares-owned: u0, purchase-date: u0, total-invested: u0, voting-rights: false} 
                        (map-get? property-shareholders {title-id: title-id, shareholder: tx-sender}))))
    
    (asserts! (get is-active fraction-data) ERR-PROPERTY-NOT-FRACTIONALIZED)
    (asserts! (>= shares-to-buy (get minimum-shares fraction-data)) ERR-MINIMUM-SHARES-NOT-MET)
    (asserts! (<= shares-to-buy available-shares) ERR-INSUFFICIENT-SHARES)
    (asserts! (>= (stx-get-balance tx-sender) total-cost) ERR-INSUFFICIENT-SHARES)
    
    ;; Transfer payment to original owner
    (try! (stx-transfer? total-cost tx-sender (get original-owner fraction-data)))
    
    ;; Update shareholder record
    (map-set property-shareholders {title-id: title-id, shareholder: tx-sender} {
      shares-owned: (+ (get shares-owned existing-shares) shares-to-buy),
      purchase-date: (if (is-eq (get shares-owned existing-shares) u0) 
                        stacks-block-height 
                        (get purchase-date existing-shares)),
      total-invested: (+ (get total-invested existing-shares) total-cost),
      voting-rights: true
    })
    
    ;; Add to shareholder list if new shareholder
    (if (is-eq (get shares-owned existing-shares) u0)
      (map-set property-shareholder-list title-id
        (unwrap-panic (as-max-len? 
          (append current-shareholders tx-sender) 
          u50)))
      true)
    
    ;; Add to shareholder's property list if new property
    (if (is-none (index-of shareholder-props title-id))
      (map-set shareholder-properties tx-sender
        (unwrap-panic (as-max-len?
          (append shareholder-props title-id)
          u20)))
      true)
    
    ;; Update fraction data
    (map-set fractional-properties title-id 
      (merge fraction-data {available-shares: (- available-shares shares-to-buy)}))
    
    (ok shares-to-buy)
  )
)

;; Distribute dividends to shareholders
(define-public (distribute-dividends (title-id uint) (total-amount uint))
  (let 
    ((fraction-data (unwrap! (map-get? fractional-properties title-id) ERR-PROPERTY-NOT-FRACTIONALIZED))
     (distribution-id (var-get next-distribution-id))
     (total-shares (get total-shares fraction-data))
     (sold-shares (- total-shares (get available-shares fraction-data)))
     (per-share-amount (/ total-amount sold-shares))
     (shareholders (default-to (list) (map-get? property-shareholder-list title-id))))
    
    ;; Only original owner can distribute dividends
    (asserts! (is-eq tx-sender (get original-owner fraction-data)) ERR-NOT-AUTHORIZED)
    (asserts! (> total-amount u0) ERR-INVALID-SHARE-AMOUNT)
    (asserts! (>= (stx-get-balance tx-sender) total-amount) ERR-INSUFFICIENT_SHARES)
    
    ;; Create distribution record
    (map-set dividend-distributions {title-id: title-id, distribution-id: distribution-id} {
      total-amount: total-amount,
      per-share-amount: per-share-amount,
      distribution-date: stacks-block-height,
      distributed-by: tx-sender,
      claimed-amount: u0
    })
    
    ;; Create individual dividend claims for each shareholder
    (try! (fold setup-dividend-claims shareholders {title-id: title-id, dist-id: distribution-id, per-share: per-share-amount}))
    
    (var-set next-distribution-id (+ distribution-id u1))
    (ok distribution-id)
  )
)

;; Helper function to setup dividend claims
(define-private (setup-dividend-claims 
    (shareholder principal) 
    (data {title-id: uint, dist-id: uint, per-share: uint}))
  (let 
    ((share-data (unwrap-panic (map-get? property-shareholders 
                               {title-id: (get title-id data), shareholder: shareholder})))
     (shares-owned (get shares-owned share-data))
     (dividend-amount (* shares-owned (get per-share data))))
    
    (map-set shareholder-dividends 
      {title-id: (get title-id data), distribution-id: (get dist-id data), shareholder: shareholder} {
        amount-due: dividend-amount,
        claimed: false,
        claim-date: none
      })
    
    (ok true)
  )
)

;; Claim dividend
(define-public (claim-dividend (title-id uint) (distribution-id uint))
  (let 
    ((dividend-claim (unwrap! (map-get? shareholder-dividends 
                              {title-id: title-id, distribution-id: distribution-id, shareholder: tx-sender}) 
                             ERR-FRACTION-NOT-FOUND))
     (distribution-data (unwrap! (map-get? dividend-distributions 
                                 {title-id: title-id, distribution-id: distribution-id}) 
                                ERR-FRACTION-NOT-FOUND))
     (amount-due (get amount-due dividend-claim))
     (distributor (get distributed-by distribution-data)))
    
    (asserts! (not (get claimed dividend-claim)) ERR-NOT-AUTHORIZED)
    (asserts! (> amount-due u0) ERR-INVALID-SHARE-AMOUNT)
    
    ;; Transfer dividend to shareholder
    (try! (stx-transfer? amount-due distributor tx-sender))
    
    ;; Mark as claimed
    (map-set shareholder-dividends 
      {title-id: title-id, distribution-id: distribution-id, shareholder: tx-sender}
      (merge dividend-claim {
        claimed: true,
        claim-date: (some stacks-block-height)
      }))
    
    ;; Update claimed amount in distribution
    (map-set dividend-distributions {title-id: title-id, distribution-id: distribution-id}
      (merge distribution-data {
        claimed-amount: (+ (get claimed-amount distribution-data) amount-due)
      }))
    
    (ok amount-due)
  )
)

;; Read-only functions
(define-read-only (get-property-fractions (title-id uint))
  (map-get? fractional-properties title-id)
)

(define-read-only (get-shareholder-info (title-id uint) (shareholder principal))
  (map-get? property-shareholders {title-id: title-id, shareholder: shareholder})
)

(define-read-only (get-property-shareholders (title-id uint))
  (default-to (list) (map-get? property-shareholder-list title-id))
)

(define-read-only (get-shareholder-properties (shareholder principal))
  (default-to (list) (map-get? shareholder-properties shareholder))
)

(define-read-only (calculate-ownership-percentage (title-id uint) (shareholder principal))
  (match (map-get? property-shareholders {title-id: title-id, shareholder: shareholder})
    share-data 
      (match (map-get? fractional-properties title-id)
        fraction-data
          (ok (/ (* (get shares-owned share-data) u100) (get total-shares fraction-data)))
        ERR-PROPERTY-NOT-FRACTIONALIZED)
    ERR-FRACTION-NOT-FOUND
  )
)
