;; Farmer Cooperative Insurance System
;; Enables farmers to form mutual insurance cooperatives for shared risk management

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-coop-not-found (err u301))
(define-constant err-already-member (err u302))
(define-constant err-not-member (err u303))
(define-constant err-insufficient-balance (err u304))
(define-constant err-coop-full (err u305))
(define-constant err-invalid-parameters (err u306))
(define-constant err-voting-period-ended (err u307))
(define-constant err-already-voted (err u308))
(define-constant err-minimum-members-required (err u309))
(define-constant err-proposal-not-found (err u310))

;; Cooperative system constants
(define-constant min-coop-members u3)
(define-constant max-coop-members u25)
(define-constant voting-period-blocks u720) ;; ~5 days
(define-constant min-stake-amount u100000) ;; 0.1 STX minimum stake

;; Data variables
(define-data-var next-coop-id uint u1)
(define-data-var next-proposal-id uint u1)
(define-data-var total-cooperatives uint u0)

;; Cooperative data structure
(define-map farmer-cooperatives
  { coop-id: uint }
  {
    coop-name: (string-ascii 50),
    founder: principal,
    member-count: uint,
    total-stake: uint,
    shared-premium-discount: uint, ;; Percentage discount (0-30)
    risk-sharing-ratio: uint, ;; Percentage of losses shared (50-100)
    governance-threshold: uint, ;; Votes needed for decisions (51-80)
    created-block: uint,
    is-active: bool
  }
)

;; Member information within cooperatives
(define-map coop-members
  { coop-id: uint, member: principal }
  {
    stake-amount: uint,
    join-block: uint,
    vote-weight: uint,
    policies-covered: uint,
    total-contributions: uint,
    benefits-received: uint,
    is-active: bool
  }
)

;; Track member's cooperative memberships
(define-map member-cooperatives
  { member: principal }
  { coop-ids: (list 10 uint) }
)

;; Governance proposals for cooperatives
(define-map coop-proposals
  { proposal-id: uint }
  {
    coop-id: uint,
    proposer: principal,
    proposal-type: (string-ascii 30),
    description: (string-ascii 200),
    votes-for: uint,
    votes-against: uint,
    voting-ends-block: uint,
    executed: bool,
    target-member: (optional principal),
    parameter-value: uint
  }
)

;; Member votes on proposals
(define-map member-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, vote-weight: uint }
)

;; Create new farmer cooperative
(define-public (create-cooperative
  (coop-name (string-ascii 50))
  (premium-discount uint)
  (risk-sharing-ratio uint)
  (governance-threshold uint)
  (initial-stake uint))
  (let (
    (coop-id (var-get next-coop-id))
  )
    (asserts! (>= initial-stake min-stake-amount) err-insufficient-balance)
    (asserts! (<= premium-discount u30) err-invalid-parameters)
    (asserts! (and (>= risk-sharing-ratio u50) (<= risk-sharing-ratio u100)) err-invalid-parameters)
    (asserts! (and (>= governance-threshold u51) (<= governance-threshold u80)) err-invalid-parameters)
    (asserts! (>= (stx-get-balance tx-sender) initial-stake) err-insufficient-balance)

    ;; Transfer initial stake to contract
    (try! (stx-transfer? initial-stake tx-sender (as-contract tx-sender)))

    ;; Create cooperative
    (map-set farmer-cooperatives
      { coop-id: coop-id }
      {
        coop-name: coop-name,
        founder: tx-sender,
        member-count: u1,
        total-stake: initial-stake,
        shared-premium-discount: premium-discount,
        risk-sharing-ratio: risk-sharing-ratio,
        governance-threshold: governance-threshold,
        created-block: stacks-block-height,
        is-active: true
      }
    )

    ;; Add founder as first member
    (map-set coop-members
      { coop-id: coop-id, member: tx-sender }
      {
        stake-amount: initial-stake,
        join-block: stacks-block-height,
        vote-weight: u100, ;; Founder gets full vote weight initially
        policies-covered: u0,
        total-contributions: initial-stake,
        benefits-received: u0,
        is-active: true
      }
    )

    ;; Track member's cooperatives
    (map-set member-cooperatives
      { member: tx-sender }
      { coop-ids: (list coop-id) }
    )

    (var-set next-coop-id (+ coop-id u1))
    (var-set total-cooperatives (+ (var-get total-cooperatives) u1))

    (ok coop-id)
  )
)

;; Join existing cooperative
(define-public (join-cooperative (coop-id uint) (stake-amount uint))
  (let (
    (coop (unwrap! (map-get? farmer-cooperatives { coop-id: coop-id }) err-coop-not-found))
    (existing-member (map-get? coop-members { coop-id: coop-id, member: tx-sender }))
  )
    (asserts! (get is-active coop) err-coop-not-found)
    (asserts! (is-none existing-member) err-already-member)
    (asserts! (< (get member-count coop) max-coop-members) err-coop-full)
    (asserts! (>= stake-amount min-stake-amount) err-insufficient-balance)
    (asserts! (>= (stx-get-balance tx-sender) stake-amount) err-insufficient-balance)

    ;; Transfer stake to contract
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))

    ;; Add member to cooperative
    (map-set coop-members
      { coop-id: coop-id, member: tx-sender }
      {
        stake-amount: stake-amount,
        join-block: stacks-block-height,
        vote-weight: (calculate-vote-weight stake-amount (get total-stake coop)),
        policies-covered: u0,
        total-contributions: stake-amount,
        benefits-received: u0,
        is-active: true
      }
    )

    ;; Update cooperative stats
    (map-set farmer-cooperatives
      { coop-id: coop-id }
      (merge coop {
        member-count: (+ (get member-count coop) u1),
        total-stake: (+ (get total-stake coop) stake-amount)
      })
    )

    ;; Update member's cooperative list
    (let ((current-coops (default-to (list) (get coop-ids (map-get? member-cooperatives { member: tx-sender })))))
      (map-set member-cooperatives
        { member: tx-sender }
        { coop-ids: (unwrap! (as-max-len? (append current-coops coop-id) u10) err-invalid-parameters) }
      )
    )

    (ok true)
  )
)

;; Create proposal for cooperative decision
(define-public (create-proposal
  (coop-id uint)
  (proposal-type (string-ascii 30))
  (description (string-ascii 200))
  (target-member (optional principal))
  (parameter-value uint))
  (let (
    (coop (unwrap! (map-get? farmer-cooperatives { coop-id: coop-id }) err-coop-not-found))
    (member (unwrap! (map-get? coop-members { coop-id: coop-id, member: tx-sender }) err-not-member))
    (proposal-id (var-get next-proposal-id))
  )
    (asserts! (get is-active coop) err-coop-not-found)
    (asserts! (get is-active member) err-not-member)

    (map-set coop-proposals
      { proposal-id: proposal-id }
      {
        coop-id: coop-id,
        proposer: tx-sender,
        proposal-type: proposal-type,
        description: description,
        votes-for: u0,
        votes-against: u0,
        voting-ends-block: (+ stacks-block-height voting-period-blocks),
        executed: false,
        target-member: target-member,
        parameter-value: parameter-value
      }
    )

    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

;; Vote on cooperative proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let (
    (proposal (unwrap! (map-get? coop-proposals { proposal-id: proposal-id }) err-proposal-not-found))
    (member (unwrap! (map-get? coop-members { coop-id: (get coop-id proposal), member: tx-sender }) err-not-member))
    (existing-vote (map-get? member-votes { proposal-id: proposal-id, voter: tx-sender }))
  )
    (asserts! (< stacks-block-height (get voting-ends-block proposal)) err-voting-period-ended)
    (asserts! (is-none existing-vote) err-already-voted)
    (asserts! (get is-active member) err-not-member)

    (let ((vote-weight (get vote-weight member)))
      ;; Record vote
      (map-set member-votes
        { proposal-id: proposal-id, voter: tx-sender }
        { vote: vote-for, vote-weight: vote-weight }
      )

      ;; Update proposal vote counts
      (map-set coop-proposals
        { proposal-id: proposal-id }
        (merge proposal {
          votes-for: (if vote-for (+ (get votes-for proposal) vote-weight) (get votes-for proposal)),
          votes-against: (if vote-for (get votes-against proposal) (+ (get votes-against proposal) vote-weight))
        })
      )
    )

    (ok true)
  )
)

;; Calculate cooperative premium discount
(define-public (calculate-coop-premium-discount
  (coop-id uint)
  (base-premium uint))
  (let (
    (coop (unwrap! (map-get? farmer-cooperatives { coop-id: coop-id }) err-coop-not-found))
    (member (unwrap! (map-get? coop-members { coop-id: coop-id, member: tx-sender }) err-not-member))
  )
    (asserts! (get is-active coop) err-coop-not-found)
    (asserts! (get is-active member) err-not-member)
    (asserts! (>= (get member-count coop) min-coop-members) err-minimum-members-required)

    (let (
      (discount-percentage (get shared-premium-discount coop))
      (discount-amount (/ (* base-premium discount-percentage) u100))
    )
      (ok (- base-premium discount-amount))
    )
  )
)

;; Private helper functions
(define-private (calculate-vote-weight (member-stake uint) (total-stake uint))
  (if (> total-stake u0)
    (/ (* member-stake u100) total-stake)
    u100
  )
)

;; Read-only functions
(define-read-only (get-cooperative (coop-id uint))
  (map-get? farmer-cooperatives { coop-id: coop-id })
)

(define-read-only (get-member-info (coop-id uint) (member principal))
  (map-get? coop-members { coop-id: coop-id, member: member })
)

(define-read-only (get-member-cooperatives (member principal))
  (map-get? member-cooperatives { member: member })
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? coop-proposals { proposal-id: proposal-id })
)

(define-read-only (get-member-vote (proposal-id uint) (voter principal))
  (map-get? member-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-system-stats)
  {
    total-cooperatives: (var-get total-cooperatives),
    next-coop-id: (var-get next-coop-id),
    next-proposal-id: (var-get next-proposal-id)
  }
)
