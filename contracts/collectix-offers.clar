;; Collectix Offers System
;; Allows users to make and manage private offers on collectibles

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u118))
(define-constant ERR_OFFER_NOT_FOUND (err u119))
(define-constant ERR_OFFER_EXPIRED (err u120))
(define-constant ERR_INVALID_OFFER_AMOUNT (err u121))
(define-constant ERR_OFFER_ALREADY_EXISTS (err u122))
(define-constant ERR_CANNOT_OFFER_OWN_ITEM (err u123))
(define-constant ERR_COLLECTIBLE_NOT_FOUND (err u124))
(define-constant ERR_INSUFFICIENT_BALANCE (err u125))
(define-constant ERR_OFFER_ALREADY_ACCEPTED (err u126))

;; Data variables
(define-data-var next-offer-id uint u1)

;; Data maps
(define-map offers
  { offer-id: uint }
  {
    collectible-id: uint,
    offerer: principal,
    owner: principal,
    amount: uint,
    expiry-block: uint,
    status: (string-ascii 16),
    created-at: uint
  }
)

(define-map collectible-offers
  { collectible-id: uint, offerer: principal }
  { offer-id: uint }
)

(define-map user-offers-made
  { user: principal }
  { count: uint }
)

(define-map user-offers-received
  { user: principal }
  { count: uint }
)

(define-map offer-counter-history
  { user: principal }
  { total-made: uint, total-received: uint, accepted: uint, rejected: uint }
)

;; Public functions

;; Make an offer on a collectible
(define-public (make-offer (collectible-id uint) (amount uint) (duration-blocks uint))
  (let (
    (offer-id (var-get next-offer-id))
    (offerer tx-sender)
    (collectible (unwrap! (contract-call? .Collectix get-collectible collectible-id) ERR_COLLECTIBLE_NOT_FOUND))
    (owner (get owner collectible))
    (existing-offer (map-get? collectible-offers { collectible-id: collectible-id, offerer: offerer }))
    (offerer-balance (contract-call? .Collectix get-user-balance offerer))
    (expiry-block (+ stacks-block-height duration-blocks))
  )
    ;; Validations
    (asserts! (> amount u0) ERR_INVALID_OFFER_AMOUNT)
    (asserts! (> duration-blocks u0) ERR_INVALID_OFFER_AMOUNT)
    (asserts! (is-none existing-offer) ERR_OFFER_ALREADY_EXISTS)
    (asserts! (not (is-eq offerer owner)) ERR_CANNOT_OFFER_OWN_ITEM)
    (asserts! (>= offerer-balance amount) ERR_INSUFFICIENT_BALANCE)

    ;; Create offer
    (map-set offers
      { offer-id: offer-id }
      {
        collectible-id: collectible-id,
        offerer: offerer,
        owner: owner,
        amount: amount,
        expiry-block: expiry-block,
        status: "pending",
        created-at: stacks-block-height
      }
    )

    ;; Create reference mapping
    (map-set collectible-offers
      { collectible-id: collectible-id, offerer: offerer }
      { offer-id: offer-id }
    )

    ;; Update user counters
    (update-user-offer-count offerer "made" 1)
    (update-user-offer-count owner "received" 1)
    
    ;; Update offer history
    (update-offer-history offerer "made")
    (update-offer-history owner "received")

    (var-set next-offer-id (+ offer-id u1))
    (ok offer-id)
  )
)

;; Accept an offer (owner only)
(define-public (accept-offer (offer-id uint))
  (let (
    (offer (unwrap! (map-get? offers { offer-id: offer-id }) ERR_OFFER_NOT_FOUND))
    (caller tx-sender)
    (collectible-id (get collectible-id offer))
    (offerer (get offerer offer))
    (owner (get owner offer))
    (amount (get amount offer))
    (expiry-block (get expiry-block offer))
    (status (get status offer))
    (collectible (unwrap! (contract-call? .Collectix get-collectible collectible-id) ERR_COLLECTIBLE_NOT_FOUND))
    (current-owner (get owner collectible))
  )
    ;; Validations
    (asserts! (is-eq caller owner) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq current-owner owner) ERR_NOT_AUTHORIZED)
    (asserts! (<= stacks-block-height expiry-block) ERR_OFFER_EXPIRED)
    (asserts! (is-eq status "pending") ERR_OFFER_ALREADY_ACCEPTED)

    ;; Check if offerer still has sufficient balance
    (let ((offerer-balance (contract-call? .Collectix get-user-balance offerer)))
      (asserts! (>= offerer-balance amount) ERR_INSUFFICIENT_BALANCE)
    )

    ;; Execute the trade through Collectix contract
    (try! (contract-call? .Collectix set-collectible-for-sale collectible-id true amount))
    (try! (as-contract (contract-call? .Collectix buy-collectible collectible-id)))

    ;; Update offer status
    (map-set offers
      { offer-id: offer-id }
      (merge offer { status: "accepted" })
    )

    ;; Update history
    (update-offer-history owner "accepted")
    (update-offer-history offerer "accepted")

    (ok true)
  )
)

;; Reject an offer (owner only)
(define-public (reject-offer (offer-id uint))
  (let (
    (offer (unwrap! (map-get? offers { offer-id: offer-id }) ERR_OFFER_NOT_FOUND))
    (caller tx-sender)
    (owner (get owner offer))
    (offerer (get offerer offer))
    (status (get status offer))
  )
    ;; Validations
    (asserts! (is-eq caller owner) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq status "pending") ERR_OFFER_ALREADY_ACCEPTED)

    ;; Update offer status
    (map-set offers
      { offer-id: offer-id }
      (merge offer { status: "rejected" })
    )

    ;; Update history
    (update-offer-history owner "rejected")
    (update-offer-history offerer "rejected")

    (ok true)
  )
)

;; Cancel own offer (offerer only)
(define-public (cancel-offer (offer-id uint))
  (let (
    (offer (unwrap! (map-get? offers { offer-id: offer-id }) ERR_OFFER_NOT_FOUND))
    (caller tx-sender)
    (offerer (get offerer offer))
    (status (get status offer))
  )
    ;; Validations
    (asserts! (is-eq caller offerer) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq status "pending") ERR_OFFER_ALREADY_ACCEPTED)

    ;; Update offer status
    (map-set offers
      { offer-id: offer-id }
      (merge offer { status: "cancelled" })
    )

    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-offer (offer-id uint))
  (map-get? offers { offer-id: offer-id })
)

(define-read-only (get-user-offer-count (user principal) (offer-type (string-ascii 16)))
  (if (is-eq offer-type "made")
    (default-to u0 (get count (map-get? user-offers-made { user: user })))
    (default-to u0 (get count (map-get? user-offers-received { user: user })))
  )
)

(define-read-only (get-user-offer-history (user principal))
  (default-to 
    { total-made: u0, total-received: u0, accepted: u0, rejected: u0 }
    (map-get? offer-counter-history { user: user })
  )
)

(define-read-only (get-collectible-offer (collectible-id uint) (offerer principal))
  (match (map-get? collectible-offers { collectible-id: collectible-id, offerer: offerer })
    ref (map-get? offers { offer-id: (get offer-id ref) })
    none
  )
)

(define-read-only (is-offer-valid (offer-id uint))
  (match (map-get? offers { offer-id: offer-id })
    offer (and 
           (is-eq (get status offer) "pending")
           (<= stacks-block-height (get expiry-block offer)))
    false
  )
)

(define-read-only (get-next-offer-id)
  (var-get next-offer-id)
)

;; Private functions

(define-private (update-user-offer-count (user principal) (offer-type (string-ascii 16)) (change int))
  (if (is-eq offer-type "made")
    (let ((current-count (get-user-offer-count user "made")))
      (map-set user-offers-made
        { user: user }
        { count: (+ current-count (to-uint change)) }
      )
    )
    (let ((current-count (get-user-offer-count user "received")))
      (map-set user-offers-received
        { user: user }
        { count: (+ current-count (to-uint change)) }
      )
    )
  )
)

(define-private (update-offer-history (user principal) (action (string-ascii 16)))
  (let (
    (current-history (get-user-offer-history user))
    (total-made (get total-made current-history))
    (total-received (get total-received current-history))
    (accepted (get accepted current-history))
    (rejected (get rejected current-history))
  )
    (map-set offer-counter-history
      { user: user }
      (if (is-eq action "made")
        { total-made: (+ total-made u1), total-received: total-received, accepted: accepted, rejected: rejected }
        (if (is-eq action "received")
          { total-made: total-made, total-received: (+ total-received u1), accepted: accepted, rejected: rejected }
          (if (is-eq action "accepted")
            { total-made: total-made, total-received: total-received, accepted: (+ accepted u1), rejected: rejected }
            { total-made: total-made, total-received: total-received, accepted: accepted, rejected: (+ rejected u1) }
          )
        )
      )
    )
  )
)
