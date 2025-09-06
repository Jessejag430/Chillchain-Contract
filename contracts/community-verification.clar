;; Community Verification System
;; Enables contributors to vote on emergency claims before payouts are processed

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u600))
(define-constant ERR-INVALID-CLAIM (err u601))
(define-constant ERR-ALREADY-VOTED (err u602))
(define-constant ERR-VOTING-CLOSED (err u603))
(define-constant ERR-INSUFFICIENT-CONTRIBUTION (err u604))
(define-constant ERR-VERIFICATION-IN-PROGRESS (err u605))

;; Verification status constants
(define-constant PENDING-VERIFICATION u1)
(define-constant APPROVED u2)
(define-constant REJECTED u3)

;; Voting thresholds
(define-constant MIN-VOTES-REQUIRED u5)
(define-constant APPROVAL-THRESHOLD u60) ;; 60% approval required
(define-constant MIN-CONTRIBUTOR-AMOUNT u1000000) ;; 1 STX minimum to vote
(define-constant VOTING-PERIOD u144) ;; ~24 hours in blocks

;; Data structures
(define-map verification-claims
  { claim-id: uint }
  {
    beneficiary: principal,
    amount: uint,
    description: (string-ascii 200),
    evidence-hash: (string-ascii 64),
    submission-block: uint,
    voting-end-block: uint,
    status: uint,
    votes-for: uint,
    votes-against: uint,
    total-votes: uint,
    submitter: principal
  }
)

(define-map claim-votes
  { claim-id: uint, voter: principal }
  {
    vote: bool,
    contribution-weight: uint,
    vote-block: uint,
    reasoning: (string-ascii 100)
  }
)

(define-map voter-eligibility
  principal
  {
    total-contribution: uint,
    verification-reputation: uint,
    accurate-votes: uint,
    total-votes-cast: uint,
    last-contribution-block: uint
  }
)

;; Data variables
(define-data-var claim-id-counter uint u0)
(define-data-var contract-owner principal tx-sender)

;; Public functions

;; Submit claim for community verification
(define-public (submit-claim-for-verification 
    (beneficiary principal)
    (amount uint)
    (description (string-ascii 200))
    (evidence-hash (string-ascii 64)))
  (let ((claim-id (var-get claim-id-counter))
        (voting-end (+ stacks-block-height VOTING-PERIOD)))
    (begin
      (asserts! (> amount u0) ERR-INVALID-CLAIM)
      (map-set verification-claims
        { claim-id: claim-id }
        {
          beneficiary: beneficiary,
          amount: amount,
          description: description,
          evidence-hash: evidence-hash,
          submission-block: stacks-block-height,
          voting-end-block: voting-end,
          status: PENDING-VERIFICATION,
          votes-for: u0,
          votes-against: u0,
          total-votes: u0,
          submitter: tx-sender
        })
      (var-set claim-id-counter (+ claim-id u1))
      (ok claim-id))))

;; Update voter eligibility based on contribution
(define-public (update-voter-eligibility (contributor principal) (contribution-amount uint))
  (let ((current-eligibility (get-voter-eligibility contributor)))
    (begin
      (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
      (map-set voter-eligibility contributor
        {
          total-contribution: (+ (get total-contribution current-eligibility) contribution-amount),
          verification-reputation: (get verification-reputation current-eligibility),
          accurate-votes: (get accurate-votes current-eligibility),
          total-votes-cast: (get total-votes-cast current-eligibility),
          last-contribution-block: stacks-block-height
        })
      (ok true))))

;; Vote on claim verification
(define-public (vote-on-claim 
    (claim-id uint)
    (approve bool)
    (reasoning (string-ascii 100)))
  (let ((claim (unwrap! (map-get? verification-claims { claim-id: claim-id }) ERR-INVALID-CLAIM))
        (voter-data (get-voter-eligibility tx-sender))
        (existing-vote (map-get? claim-votes { claim-id: claim-id, voter: tx-sender })))
    (begin
      (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
      (asserts! (>= (get total-contribution voter-data) MIN-CONTRIBUTOR-AMOUNT) ERR-INSUFFICIENT-CONTRIBUTION)
      (asserts! (<= stacks-block-height (get voting-end-block claim)) ERR-VOTING-CLOSED)
      (asserts! (is-eq (get status claim) PENDING-VERIFICATION) ERR-VERIFICATION-IN-PROGRESS)

      ;; Calculate vote weight based on contribution
      (let ((vote-weight (/ (get total-contribution voter-data) u1000000)))
        
        ;; Record the vote
        (map-set claim-votes
          { claim-id: claim-id, voter: tx-sender }
          {
            vote: approve,
            contribution-weight: vote-weight,
            vote-block: stacks-block-height,
            reasoning: reasoning
          })

        ;; Update claim vote counts
        (map-set verification-claims
          { claim-id: claim-id }
          (merge claim
            {
              votes-for: (if approve 
                           (+ (get votes-for claim) vote-weight) 
                           (get votes-for claim)),
              votes-against: (if approve 
                              (get votes-against claim) 
                              (+ (get votes-against claim) vote-weight)),
              total-votes: (+ (get total-votes claim) vote-weight)
            }))

        ;; Update voter's voting history
        (map-set voter-eligibility tx-sender
          (merge voter-data
            {
              total-votes-cast: (+ (get total-votes-cast voter-data) u1)
            }))

        ;; Check if voting threshold is reached
        (let ((total-votes (+ (get total-votes claim) vote-weight)))
          (if (>= total-votes MIN-VOTES-REQUIRED)
            (finalize-claim-verification claim-id)
            (ok true)))))))

;; Finalize claim verification based on votes
(define-private (finalize-claim-verification (claim-id uint))
  (let ((claim (unwrap-panic (map-get? verification-claims { claim-id: claim-id })))
        (total-votes (get total-votes claim))
        (approval-percentage (if (> total-votes u0)
                               (/ (* (get votes-for claim) u100) total-votes)
                               u0)))
    (begin
      (if (>= approval-percentage APPROVAL-THRESHOLD)
        (map-set verification-claims
          { claim-id: claim-id }
          (merge claim { status: APPROVED }))
        (map-set verification-claims
          { claim-id: claim-id }
          (merge claim { status: REJECTED })))
      (ok true))))

;; Read-only functions

;; Get claim verification details
(define-read-only (get-claim-verification (claim-id uint))
  (map-get? verification-claims { claim-id: claim-id }))

;; Get voter eligibility information
(define-read-only (get-voter-eligibility (voter principal))
  (default-to
    {
      total-contribution: u0,
      verification-reputation: u100,
      accurate-votes: u0,
      total-votes-cast: u0,
      last-contribution-block: u0
    }
    (map-get? voter-eligibility voter)))

;; Get vote details for specific claim and voter
(define-read-only (get-vote-details (claim-id uint) (voter principal))
  (map-get? claim-votes { claim-id: claim-id, voter: voter }))

;; Check if voter is eligible to vote on claims
(define-read-only (is-eligible-voter (voter principal))
  (let ((eligibility (get-voter-eligibility voter)))
    (>= (get total-contribution eligibility) MIN-CONTRIBUTOR-AMOUNT)))

;; Get voting statistics for claim
(define-read-only (get-voting-stats (claim-id uint))
  (let ((claim (map-get? verification-claims { claim-id: claim-id })))
    (if (is-some claim)
      (some {
        total-votes: (get total-votes (unwrap-panic claim)),
        votes-for: (get votes-for (unwrap-panic claim)),
        votes-against: (get votes-against (unwrap-panic claim)),
        approval-percentage: (if (> (get total-votes (unwrap-panic claim)) u0)
                               (/ (* (get votes-for (unwrap-panic claim)) u100) 
                                  (get total-votes (unwrap-panic claim)))
                               u0),
        voting-end-block: (get voting-end-block (unwrap-panic claim)),
        status: (get status (unwrap-panic claim))
      })
      none)))
