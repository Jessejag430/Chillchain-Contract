(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_TEMPERATURE (err u101))
(define-constant ERR_SHIPMENT_NOT_FOUND (err u102))
(define-constant ERR_SHIPMENT_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_TIMESTAMP (err u104))
(define-constant ERR_SHIPMENT_COMPLETED (err u105))
(define-constant ERR_INVALID_THRESHOLD (err u106))
(define-constant ERR_AUCTION_NOT_FOUND (err u107))
(define-constant ERR_AUCTION_ENDED (err u108))
(define-constant ERR_AUCTION_ACTIVE (err u109))
(define-constant ERR_BID_TOO_LOW (err u110))
(define-constant ERR_INSUFFICIENT_FUNDS (err u111))
(define-constant ERR_AUCTION_NOT_ENDED (err u112))
(define-constant ERR_CANNOT_BID_OWN_AUCTION (err u113))
(define-constant ERR_AUCTION_ALREADY_EXISTS (err u114))
(define-constant ERR_POLICY_NOT_FOUND (err u115))
(define-constant ERR_POLICY_EXISTS (err u116))
(define-constant ERR_CLAIM_ALREADY_PROCESSED (err u117))
(define-constant ERR_POLICY_EXPIRED (err u118))
(define-constant ERR_INSUFFICIENT_POOL (err u119))
(define-constant ERR_INVALID_COVERAGE (err u120))
(define-constant ERR_CLAIM_NOT_ELIGIBLE (err u121))

(define-non-fungible-token shipment-nft uint)

(define-data-var next-shipment-id uint u1)

(define-map shipments
  { shipment-id: uint }
  {
    owner: principal,
    product-name: (string-ascii 50),
    origin: (string-ascii 50),
    destination: (string-ascii 50),
    min-temp: int,
    max-temp: int,
    created-at: uint,
    completed-at: (optional uint),
    status: (string-ascii 20),
    violation-count: uint
  }
)

(define-map temperature-logs
  { shipment-id: uint, log-id: uint }
  {
    temperature: int,
    timestamp: uint,
    location: (string-ascii 50),
    sensor-id: (string-ascii 30)
  }
)

(define-map shipment-log-counts
  { shipment-id: uint }
  { count: uint }
)

(define-map authorized-sensors
  { sensor-id: (string-ascii 30) }
  { authorized: bool, owner: principal }
)

(define-map shipment-auctions
  { shipment-id: uint }
  {
    owner: principal,
    min-bid: uint,
    current-bid: uint,
    current-bidder: (optional principal),
    end-height: uint,
    is-active: bool,
    description: (string-ascii 100),
    carrier-requirements: (string-ascii 200)
  }
)

(define-map auction-bids
  { shipment-id: uint, bid-id: uint }
  {
    bidder: principal,
    amount: uint,
    bid-height: uint,
    carrier-info: (string-ascii 150)
  }
)

(define-map auction-bid-counts
  { shipment-id: uint }
  { count: uint }
)

(define-map carrier-profiles
  { carrier: principal }
  {
    name: (string-ascii 50),
    license-number: (string-ascii 30),
    rating: uint,
    total-shipments: uint,
    successful-shipments: uint,
    verified: bool
  }
)

(define-map carrier-deposits
  { carrier: principal }
  { amount: uint }
)

(define-map insurance-policies
  { policy-id: uint }
  {
    shipment-id: uint,
    policyholder: principal,
    coverage-amount: uint,
    premium-paid: uint,
    policy-start: uint,
    policy-duration: uint,
    temperature-threshold: uint,
    violation-deductible: uint,
    is-active: bool
  }
)

(define-map insurance-claims
  { claim-id: uint }
  {
    policy-id: uint,
    shipment-id: uint,
    claimant: principal,
    claim-amount: uint,
    violation-count: uint,
    filed-at: uint,
    processed-at: (optional uint),
    status: (string-ascii 20),
    payout-amount: uint
  }
)

(define-map risk-assessments
  { shipment-id: uint }
  {
    base-risk-score: uint,
    route-risk-factor: uint,
    product-risk-factor: uint,
    carrier-risk-factor: uint,
    total-risk-score: uint,
    premium-multiplier: uint
  }
)

(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var insurance-pool uint u0)
(define-data-var total-claims-paid uint u0)
(define-data-var base-premium-rate uint u100)

(define-public (create-shipment 
  (product-name (string-ascii 50))
  (origin (string-ascii 50))
  (destination (string-ascii 50))
  (min-temp int)
  (max-temp int))
  (let
    (
      (shipment-id (var-get next-shipment-id))
    )
    (asserts! (> max-temp min-temp) ERR_INVALID_THRESHOLD)
    (try! (nft-mint? shipment-nft shipment-id tx-sender))
    (map-set shipments
      { shipment-id: shipment-id }
      {
        owner: tx-sender,
        product-name: product-name,
        origin: origin,
        destination: destination,
        min-temp: min-temp,
        max-temp: max-temp,
        created-at: stacks-block-height,
        completed-at: none,
        status: "in-transit",
        violation-count: u0
      }
    )
    (map-set shipment-log-counts
      { shipment-id: shipment-id }
      { count: u0 }
    )
    (var-set next-shipment-id (+ shipment-id u1))
    (ok shipment-id)
  )
)

(define-public (authorize-sensor (sensor-id (string-ascii 30)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set authorized-sensors
      { sensor-id: sensor-id }
      { authorized: true, owner: tx-sender }
    )
    (ok true)
  )
)

(define-public (revoke-sensor (sensor-id (string-ascii 30)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set authorized-sensors
      { sensor-id: sensor-id }
      { authorized: false, owner: tx-sender }
    )
    (ok true)
  )
)

(define-public (log-temperature
  (shipment-id uint)
  (temperature int)
  (location (string-ascii 50))
  (sensor-id (string-ascii 30)))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (sensor-auth (default-to { authorized: false, owner: CONTRACT_OWNER } 
                    (map-get? authorized-sensors { sensor-id: sensor-id })))
      (log-count-data (default-to { count: u0 } 
                       (map-get? shipment-log-counts { shipment-id: shipment-id })))
      (current-log-id (get count log-count-data))
      (is-violation (or (< temperature (get min-temp shipment)) 
                       (> temperature (get max-temp shipment))))
      (new-violation-count (if is-violation 
                            (+ (get violation-count shipment) u1)
                            (get violation-count shipment)))
    )
    (asserts! (get authorized sensor-auth) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status shipment) "in-transit") ERR_SHIPMENT_COMPLETED)
    
    (map-set temperature-logs
      { shipment-id: shipment-id, log-id: current-log-id }
      {
        temperature: temperature,
        timestamp: stacks-block-height,
        location: location,
        sensor-id: sensor-id
      }
    )
    
    (map-set shipment-log-counts
      { shipment-id: shipment-id }
      { count: (+ current-log-id u1) }
    )
    
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment { violation-count: new-violation-count })
    )
    
    (ok current-log-id)
  )
)

(define-public (complete-shipment (shipment-id uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (nft-owner (unwrap! (nft-get-owner? shipment-nft shipment-id) ERR_SHIPMENT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender nft-owner) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status shipment) "in-transit") ERR_SHIPMENT_COMPLETED)
    
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment { 
        status: "completed", 
        completed-at: (some stacks-block-height) 
      })
    )
    (ok true)
  )
)

(define-public (transfer-shipment (shipment-id uint) (new-owner principal))
  (let
    (
      (current-owner (unwrap! (nft-get-owner? shipment-nft shipment-id) ERR_SHIPMENT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender current-owner) ERR_NOT_AUTHORIZED)
    (try! (nft-transfer? shipment-nft shipment-id tx-sender new-owner))
    (ok true)
  )
)

(define-read-only (get-shipment (shipment-id uint))
  (map-get? shipments { shipment-id: shipment-id })
)

(define-read-only (get-temperature-log (shipment-id uint) (log-id uint))
  (map-get? temperature-logs { shipment-id: shipment-id, log-id: log-id })
)

(define-read-only (get-shipment-log-count (shipment-id uint))
  (default-to u0 (get count (map-get? shipment-log-counts { shipment-id: shipment-id })))
)

(define-read-only (get-sensor-authorization (sensor-id (string-ascii 30)))
  (map-get? authorized-sensors { sensor-id: sensor-id })
)

(define-read-only (get-next-shipment-id)
  (var-get next-shipment-id)
)

(define-read-only (is-temperature-compliant (shipment-id uint))
  (let
    (
      (shipment (map-get? shipments { shipment-id: shipment-id }))
    )
    (match shipment
      shipment-data (is-eq (get violation-count shipment-data) u0)
      false
    )
  )
)

(define-read-only (get-shipment-violations (shipment-id uint))
  (let
    (
      (shipment (map-get? shipments { shipment-id: shipment-id }))
    )
    (match shipment
      shipment-data (get violation-count shipment-data)
      u0
    )
  )
)

(define-read-only (get-all-logs-for-shipment (shipment-id uint))
  (let
    (
      (log-count (get-shipment-log-count shipment-id))
    )
    (map get-temperature-log-helper (list shipment-id shipment-id shipment-id shipment-id shipment-id 
                                         shipment-id shipment-id shipment-id shipment-id shipment-id)
         (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9))
  )
)

(define-private (get-temperature-log-helper (shipment-id uint) (log-id uint))
  (map-get? temperature-logs { shipment-id: shipment-id, log-id: log-id })
)

(define-public (register-carrier
  (name (string-ascii 50))
  (license-number (string-ascii 30)))
  (begin
    (map-set carrier-profiles
      { carrier: tx-sender }
      {
        name: name,
        license-number: license-number,
        rating: u0,
        total-shipments: u0,
        successful-shipments: u0,
        verified: false
      }
    )
    (ok true)
  )
)

(define-public (verify-carrier (carrier principal))
  (let
    (
      (profile (unwrap! (map-get? carrier-profiles { carrier: carrier }) ERR_NOT_AUTHORIZED))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set carrier-profiles
      { carrier: carrier }
      (merge profile { verified: true })
    )
    (ok true)
  )
)

(define-public (deposit-carrier-funds)
  (let
    (
      (amount (stx-get-balance tx-sender))
      (current-deposit (default-to u0 (get amount (map-get? carrier-deposits { carrier: tx-sender }))))
    )
    (asserts! (> amount u0) ERR_INSUFFICIENT_FUNDS)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set carrier-deposits
      { carrier: tx-sender }
      { amount: (+ current-deposit amount) }
    )
    (ok amount)
  )
)

(define-public (create-auction
  (shipment-id uint)
  (min-bid uint)
  (duration uint)
  (description (string-ascii 100))
  (carrier-requirements (string-ascii 200)))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (nft-owner (unwrap! (nft-get-owner? shipment-nft shipment-id) ERR_SHIPMENT_NOT_FOUND))
      (existing-auction (map-get? shipment-auctions { shipment-id: shipment-id }))
    )
    (asserts! (is-eq tx-sender nft-owner) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status shipment) "in-transit") ERR_SHIPMENT_COMPLETED)
    (asserts! (is-none existing-auction) ERR_AUCTION_ALREADY_EXISTS)
    (asserts! (> min-bid u0) ERR_BID_TOO_LOW)
    (asserts! (> duration u0) ERR_INVALID_TIMESTAMP)
    
    (map-set shipment-auctions
      { shipment-id: shipment-id }
      {
        owner: tx-sender,
        min-bid: min-bid,
        current-bid: u0,
        current-bidder: none,
        end-height: (+ stacks-block-height duration),
        is-active: true,
        description: description,
        carrier-requirements: carrier-requirements
      }
    )
    
    (map-set auction-bid-counts
      { shipment-id: shipment-id }
      { count: u0 }
    )
    
    (ok true)
  )
)

(define-public (place-bid
  (shipment-id uint)
  (bid-amount uint)
  (carrier-info (string-ascii 150)))
  (let
    (
      (auction (unwrap! (map-get? shipment-auctions { shipment-id: shipment-id }) ERR_AUCTION_NOT_FOUND))
      (carrier-profile (map-get? carrier-profiles { carrier: tx-sender }))
      (carrier-deposit (default-to u0 (get amount (map-get? carrier-deposits { carrier: tx-sender }))))
      (bid-count-data (default-to { count: u0 } (map-get? auction-bid-counts { shipment-id: shipment-id })))
      (current-bid-id (get count bid-count-data))
    )
    (asserts! (get is-active auction) ERR_AUCTION_ENDED)
    (asserts! (< stacks-block-height (get end-height auction)) ERR_AUCTION_ENDED)
    (asserts! (not (is-eq tx-sender (get owner auction))) ERR_CANNOT_BID_OWN_AUCTION)
    (asserts! (is-some carrier-profile) ERR_NOT_AUTHORIZED)
    (asserts! (> bid-amount (get current-bid auction)) ERR_BID_TOO_LOW)
    (asserts! (>= bid-amount (get min-bid auction)) ERR_BID_TOO_LOW)
    (asserts! (>= carrier-deposit bid-amount) ERR_INSUFFICIENT_FUNDS)
    
    (map-set auction-bids
      { shipment-id: shipment-id, bid-id: current-bid-id }
      {
        bidder: tx-sender,
        amount: bid-amount,
        bid-height: stacks-block-height,
        carrier-info: carrier-info
      }
    )
    
    (map-set auction-bid-counts
      { shipment-id: shipment-id }
      { count: (+ current-bid-id u1) }
    )
    
    (map-set shipment-auctions
      { shipment-id: shipment-id }
      (merge auction { 
        current-bid: bid-amount,
        current-bidder: (some tx-sender)
      })
    )
    
    (ok current-bid-id)
  )
)

(define-public (finalize-auction (shipment-id uint))
  (let
    (
      (auction (unwrap! (map-get? shipment-auctions { shipment-id: shipment-id }) ERR_AUCTION_NOT_FOUND))
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (nft-owner (unwrap! (nft-get-owner? shipment-nft shipment-id) ERR_SHIPMENT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner auction)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active auction) ERR_AUCTION_ENDED)
    (asserts! (>= stacks-block-height (get end-height auction)) ERR_AUCTION_NOT_ENDED)
    
    (map-set shipment-auctions
      { shipment-id: shipment-id }
      (merge auction { is-active: false })
    )
    
    (match (get current-bidder auction)
      winner-carrier
        (begin
          (try! (nft-transfer? shipment-nft shipment-id tx-sender winner-carrier))
          (try! (as-contract (stx-transfer? (get current-bid auction) tx-sender (get owner auction))))
          (let
            (
              (winner-profile (unwrap! (map-get? carrier-profiles { carrier: winner-carrier }) ERR_NOT_AUTHORIZED))
            )
            (map-set carrier-profiles
              { carrier: winner-carrier }
              (merge winner-profile { 
                total-shipments: (+ (get total-shipments winner-profile) u1)
              })
            )
          )
          (ok (some winner-carrier))
        )
      (ok none)
    )
  )
)

(define-public (cancel-auction (shipment-id uint))
  (let
    (
      (auction (unwrap! (map-get? shipment-auctions { shipment-id: shipment-id }) ERR_AUCTION_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner auction)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active auction) ERR_AUCTION_ENDED)
    (asserts! (is-eq (get current-bid auction) u0) ERR_AUCTION_ACTIVE)
    
    (map-set shipment-auctions
      { shipment-id: shipment-id }
      (merge auction { is-active: false })
    )
    
    (ok true)
  )
)

(define-public (complete-shipment-with-rating (shipment-id uint) (rating uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (nft-owner (unwrap! (nft-get-owner? shipment-nft shipment-id) ERR_SHIPMENT_NOT_FOUND))
      (carrier-profile (map-get? carrier-profiles { carrier: nft-owner }))
    )
    (asserts! (is-eq tx-sender (get owner shipment)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status shipment) "in-transit") ERR_SHIPMENT_COMPLETED)
    (asserts! (<= rating u5) ERR_INVALID_TEMPERATURE)
    
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment { 
        status: "completed", 
        completed-at: (some stacks-block-height) 
      })
    )
    
    (match carrier-profile
      profile
        (let
          (
            (new-total (+ (get total-shipments profile) u1))
            (new-successful (if (is-eq (get violation-count shipment) u0) 
                             (+ (get successful-shipments profile) u1)
                             (get successful-shipments profile)))
            (current-rating (get rating profile))
            (new-rating (if (is-eq current-rating u0) 
                         rating 
                         (/ (+ (* current-rating (get total-shipments profile)) rating) new-total)))
          )
          (map-set carrier-profiles
            { carrier: nft-owner }
            (merge profile { 
              rating: new-rating,
              total-shipments: new-total,
              successful-shipments: new-successful
            })
          )
        )
      true
    )
    
    (ok true)
  )
)

(define-read-only (get-auction (shipment-id uint))
  (map-get? shipment-auctions { shipment-id: shipment-id })
)

(define-read-only (get-auction-bid (shipment-id uint) (bid-id uint))
  (map-get? auction-bids { shipment-id: shipment-id, bid-id: bid-id })
)

(define-read-only (get-auction-bid-count (shipment-id uint))
  (default-to u0 (get count (map-get? auction-bid-counts { shipment-id: shipment-id })))
)

(define-read-only (get-carrier-profile (carrier principal))
  (map-get? carrier-profiles { carrier: carrier })
)

(define-read-only (get-carrier-deposit (carrier principal))
  (default-to u0 (get amount (map-get? carrier-deposits { carrier: carrier })))
)

(define-read-only (is-auction-active (shipment-id uint))
  (let
    (
      (auction (map-get? shipment-auctions { shipment-id: shipment-id }))
    )
    (match auction
      auction-data (and (get is-active auction-data) 
                       (< stacks-block-height (get end-height auction-data)))
      false
    )
  )
)

(define-read-only (get-auction-winner (shipment-id uint))
  (let
    (
      (auction (map-get? shipment-auctions { shipment-id: shipment-id }))
    )
    (match auction
      auction-data (if (and (not (get is-active auction-data))
                           (>= stacks-block-height (get end-height auction-data)))
                      (get current-bidder auction-data)
                      none)
      none
    )
  )
)

(define-public (fund-insurance-pool)
  (let
    (
      (amount (stx-get-balance tx-sender))
    )
    (asserts! (> amount u0) ERR_INSUFFICIENT_FUNDS)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) amount))
    (ok amount)
  )
)

(define-public (calculate-risk-assessment
  (shipment-id uint)
  (route-risk uint)
  (product-risk uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (nft-owner (unwrap! (nft-get-owner? shipment-nft shipment-id) ERR_SHIPMENT_NOT_FOUND))
      (carrier-profile (map-get? carrier-profiles { carrier: nft-owner }))
      (carrier-risk (match carrier-profile
                      profile (if (get verified profile)
                               (- u100 (* (get rating profile) u10))
                               u80)
                      u100))
      (base-risk u50)
      (total-risk (/ (+ base-risk route-risk product-risk carrier-risk) u4))
      (premium-multiplier (+ u100 (/ total-risk u2)))
    )
    (asserts! (is-eq tx-sender (get owner shipment)) ERR_NOT_AUTHORIZED)
    (asserts! (<= route-risk u100) ERR_INVALID_TEMPERATURE)
    (asserts! (<= product-risk u100) ERR_INVALID_TEMPERATURE)
    
    (map-set risk-assessments
      { shipment-id: shipment-id }
      {
        base-risk-score: base-risk,
        route-risk-factor: route-risk,
        product-risk-factor: product-risk,
        carrier-risk-factor: carrier-risk,
        total-risk-score: total-risk,
        premium-multiplier: premium-multiplier
      }
    )
    (ok total-risk)
  )
)

(define-public (create-insurance-policy
  (shipment-id uint)
  (coverage-amount uint)
  (policy-duration uint)
  (temperature-threshold uint)
  (deductible uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (nft-owner (unwrap! (nft-get-owner? shipment-nft shipment-id) ERR_SHIPMENT_NOT_FOUND))
      (risk-data (map-get? risk-assessments { shipment-id: shipment-id }))
      (policy-id (var-get next-policy-id))
      (base-rate (var-get base-premium-rate))
      (risk-multiplier (match risk-data
                         data (get premium-multiplier data)
                         u150))
      (premium (/ (* coverage-amount base-rate risk-multiplier) u10000))
    )
    (asserts! (is-eq tx-sender (get owner shipment)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status shipment) "in-transit") ERR_SHIPMENT_COMPLETED)
    (asserts! (> coverage-amount u0) ERR_INVALID_COVERAGE)
    (asserts! (> policy-duration u0) ERR_INVALID_TIMESTAMP)
    (asserts! (> temperature-threshold u0) ERR_INVALID_TEMPERATURE)
    (asserts! (>= (stx-get-balance tx-sender) premium) ERR_INSUFFICIENT_FUNDS)
    (asserts! (is-none (map-get? insurance-policies { policy-id: policy-id })) ERR_POLICY_EXISTS)
    
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) premium))
    
    (map-set insurance-policies
      { policy-id: policy-id }
      {
        shipment-id: shipment-id,
        policyholder: tx-sender,
        coverage-amount: coverage-amount,
        premium-paid: premium,
        policy-start: stacks-block-height,
        policy-duration: policy-duration,
        temperature-threshold: temperature-threshold,
        violation-deductible: deductible,
        is-active: true
      }
    )
    
    (var-set next-policy-id (+ policy-id u1))
    (ok policy-id)
  )
)

(define-public (file-insurance-claim (policy-id uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) ERR_POLICY_NOT_FOUND))
      (shipment (unwrap! (map-get? shipments { shipment-id: (get shipment-id policy) }) ERR_SHIPMENT_NOT_FOUND))
      (claim-id (var-get next-claim-id))
      (violation-count (get violation-count shipment))
      (is-eligible (and (> violation-count (get temperature-threshold policy))
                       (get is-active policy)
                       (< stacks-block-height (+ (get policy-start policy) (get policy-duration policy)))))
      (base-claim (* violation-count u1000))
      (claim-amount (if (<= base-claim (get coverage-amount policy)) 
                       base-claim 
                       (get coverage-amount policy)))
    )
    (asserts! (is-eq tx-sender (get policyholder policy)) ERR_NOT_AUTHORIZED)
    (asserts! is-eligible ERR_CLAIM_NOT_ELIGIBLE)
    (asserts! (or (is-eq (get status shipment) "completed") 
                 (is-eq (get status shipment) "in-transit")) ERR_SHIPMENT_NOT_FOUND)
    
    (map-set insurance-claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        shipment-id: (get shipment-id policy),
        claimant: tx-sender,
        claim-amount: claim-amount,
        violation-count: violation-count,
        filed-at: stacks-block-height,
        processed-at: none,
        status: "pending",
        payout-amount: u0
      }
    )
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

(define-public (process-insurance-claim (claim-id uint))
  (let
    (
      (claim (unwrap! (map-get? insurance-claims { claim-id: claim-id }) ERR_POLICY_NOT_FOUND))
      (policy (unwrap! (map-get? insurance-policies { policy-id: (get policy-id claim) }) ERR_POLICY_NOT_FOUND))
      (shipment (unwrap! (map-get? shipments { shipment-id: (get shipment-id claim) }) ERR_SHIPMENT_NOT_FOUND))
      (current-pool (var-get insurance-pool))
      (claim-amount (get claim-amount claim))
      (deductible (get violation-deductible policy))
      (final-payout (if (> claim-amount deductible) (- claim-amount deductible) u0))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status claim) "pending") ERR_CLAIM_ALREADY_PROCESSED)
    (asserts! (>= current-pool final-payout) ERR_INSUFFICIENT_POOL)
    (asserts! (is-eq (get status shipment) "completed") ERR_SHIPMENT_COMPLETED)
    
    (if (> final-payout u0)
      (begin
        (try! (as-contract (stx-transfer? final-payout tx-sender (get claimant claim))))
        (var-set insurance-pool (- current-pool final-payout))
        (var-set total-claims-paid (+ (var-get total-claims-paid) final-payout))
      )
      true
    )
    
    (map-set insurance-claims
      { claim-id: claim-id }
      (merge claim {
        processed-at: (some stacks-block-height),
        status: "processed",
        payout-amount: final-payout
      })
    )
    
    (map-set insurance-policies
      { policy-id: (get policy-id claim) }
      (merge policy { is-active: false })
    )
    
    (ok final-payout)
  )
)

(define-public (auto-process-claim-on-completion (shipment-id uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (nft-owner (unwrap! (nft-get-owner? shipment-nft shipment-id) ERR_SHIPMENT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender nft-owner) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status shipment) "completed") ERR_SHIPMENT_COMPLETED)
    (asserts! (> (get violation-count shipment) u0) ERR_CLAIM_NOT_ELIGIBLE)
    
    (ok true)
  )
)

(define-public (update-premium-rates (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> new-rate u0) ERR_INVALID_COVERAGE)
    (var-set base-premium-rate new-rate)
    (ok new-rate)
  )
)

(define-read-only (get-insurance-policy (policy-id uint))
  (map-get? insurance-policies { policy-id: policy-id })
)

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? insurance-claims { claim-id: claim-id })
)

(define-read-only (get-risk-assessment (shipment-id uint))
  (map-get? risk-assessments { shipment-id: shipment-id })
)

(define-read-only (get-insurance-pool-balance)
  (var-get insurance-pool)
)

(define-read-only (get-total-claims-paid)
  (var-get total-claims-paid)
)

(define-read-only (calculate-premium-quote
  (coverage-amount uint)
  (shipment-id uint))
  (let
    (
      (risk-data (map-get? risk-assessments { shipment-id: shipment-id }))
      (base-rate (var-get base-premium-rate))
      (risk-multiplier (match risk-data
                         data (get premium-multiplier data)
                         u150))
    )
    (/ (* coverage-amount base-rate risk-multiplier) u10000)
  )
)

(define-read-only (get-policy-status (policy-id uint))
  (let
    (
      (policy (map-get? insurance-policies { policy-id: policy-id }))
    )
    (match policy
      policy-data (and (get is-active policy-data)
                      (< stacks-block-height (+ (get policy-start policy-data) 
                                               (get policy-duration policy-data))))
      false
    )
  )
)

(define-read-only (estimate-claim-payout
  (policy-id uint)
  (violation-count uint))
  (let
    (
      (policy (map-get? insurance-policies { policy-id: policy-id }))
    )
    (match policy
      policy-data (let
                    (
                      (base-claim (* violation-count u1000))
                      (claim-amount (if (<= base-claim (get coverage-amount policy-data)) 
                                       base-claim 
                                       (get coverage-amount policy-data)))
                      (deductible (get violation-deductible policy-data))
                    )
                    (if (> claim-amount deductible) (- claim-amount deductible) u0))
      u0
    )
  )
)



