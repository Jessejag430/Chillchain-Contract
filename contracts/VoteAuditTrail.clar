;; Vote Verification & Audit Trail - Cryptographic proof and transparency for elections
;; Provides independent verification of vote integrity and transparent audit mechanisms

;; Error constants
(define-constant err-audit-not-found (err u400))
(define-constant err-unauthorized (err u401))
(define-constant err-already-verified (err u402))
(define-constant err-verification-failed (err u403))
(define-constant err-invalid-proof (err u404))
(define-constant err-proposal-not-found (err u405))
(define-constant err-audit-sealed (err u406))

;; Constants for audit configuration
(define-constant audit-seal-delay u144) ;; Blocks before audit can be sealed (1 day)
(define-constant max-verification-attempts u3)
(define-constant verification-reward u1000) ;; Tokens for successful verification

;; Vote commitment structure for integrity verification
(define-map vote-commitments
  { proposal-id: uint, voter: principal }
  {
    commitment-hash: (buff 32),    ;; Hash of vote + nonce
    vote-weight: uint,
    commitment-block: uint,
    verification-status: (string-ascii 20), ;; "pending", "verified", "disputed"
    verification-attempts: uint
  }
)

;; Audit trail entries for transparency
(define-map audit-entries
  { proposal-id: uint, entry-id: uint }
  {
    event-type: (string-ascii 30),     ;; "vote-cast", "verification", "tally"
    event-data: (string-ascii 200),    ;; Event details
    block-height: uint,
    actor: principal,
    integrity-hash: (buff 32)         ;; Hash for tamper detection
  }
)

;; Proposal audit summary
(define-map proposal-audits
  { proposal-id: uint }
  {
    total-vote-commitments: uint,
    verified-commitments: uint,
    disputed-commitments: uint,
    audit-entries-count: uint,
    final-tally-hash: (optional (buff 32)),
    audit-sealed: bool,
    seal-block: (optional uint),
    independent-verifier: (optional principal)
  }
)

;; Vote verification challenges for disputed votes
(define-map verification-challenges
  { proposal-id: uint, challenger: principal, vote-hash: (buff 32) }
  {
    challenge-reason: (string-ascii 100),
    challenge-block: uint,
    resolution-status: (string-ascii 20), ;; "pending", "upheld", "rejected"
    resolved-by: (optional principal),
    evidence-hash: (optional (buff 32))
  }
)

;; Independent verifier registry
(define-map authorized-verifiers
  { verifier: principal }
  {
    authorized-by: principal,
    authorization-block: uint,
    verifications-completed: uint,
    accuracy-score: uint  ;; Percentage of correct verifications
  }
)

;; Data variables
(define-data-var next-entry-id uint u1)
(define-data-var contract-admin principal tx-sender)
(define-data-var verification-fee uint u100) ;; Cost to challenge verification

;; Read-only functions

;; Get vote commitment details
(define-read-only (get-vote-commitment (proposal-id uint) (voter principal))
  (map-get? vote-commitments { proposal-id: proposal-id, voter: voter })
)

;; Get audit entry
(define-read-only (get-audit-entry (proposal-id uint) (entry-id uint))
  (map-get? audit-entries { proposal-id: proposal-id, entry-id: entry-id })
)

;; Get proposal audit summary
(define-read-only (get-proposal-audit (proposal-id uint))
  (map-get? proposal-audits { proposal-id: proposal-id })
)

;; Get verification challenge
(define-read-only (get-verification-challenge (proposal-id uint) (challenger principal) (vote-hash (buff 32)))
  (map-get? verification-challenges { proposal-id: proposal-id, challenger: challenger, vote-hash: vote-hash })
)

;; Check if verifier is authorized
(define-read-only (is-authorized-verifier (verifier principal))
  (is-some (map-get? authorized-verifiers { verifier: verifier }))
)

;; Generate vote commitment hash (utility function)
(define-read-only (generate-vote-commitment (voter principal) (proposal-id uint) (vote bool) (weight uint) (nonce uint))
  (keccak256 (concat
    (concat (unwrap-panic (to-consensus-buff? voter)) (unwrap-panic (to-consensus-buff? proposal-id)))
    (concat (concat (unwrap-panic (to-consensus-buff? vote)) (unwrap-panic (to-consensus-buff? weight)))
            (unwrap-panic (to-consensus-buff? nonce)))
  ))
)

;; Calculate audit completeness score
(define-read-only (get-audit-completeness (proposal-id uint))
  (match (get-proposal-audit proposal-id)
    audit
      (if (> (get total-vote-commitments audit) u0)
        (/ (* (get verified-commitments audit) u100) (get total-vote-commitments audit))
        u0)
    u0
  )
)

;; Public functions

;; Record vote commitment for later verification
(define-public (record-vote-commitment (proposal-id uint) (voter principal) (commitment-hash (buff 32)) (weight uint))
  (let
    (
      (existing-commitment (get-vote-commitment proposal-id voter))
      (audit (get-proposal-audit proposal-id))
    )
    ;; Ensure no existing commitment for this vote
    (asserts! (is-none existing-commitment) err-already-verified)
    
    ;; Store vote commitment
    (map-set vote-commitments
      { proposal-id: proposal-id, voter: voter }
      {
        commitment-hash: commitment-hash,
        vote-weight: weight,
        commitment-block: stacks-block-height,
        verification-status: "pending",
        verification-attempts: u0
      }
    )
    
    ;; Update audit tracking
    (let
      (
        (updated-audit (match audit
          existing-audit (merge existing-audit { total-vote-commitments: (+ (get total-vote-commitments existing-audit) u1) })
          { total-vote-commitments: u1, verified-commitments: u0, disputed-commitments: u0,
            audit-entries-count: u0, final-tally-hash: none, audit-sealed: false,
            seal-block: none, independent-verifier: none }
        ))
      )
      (map-set proposal-audits { proposal-id: proposal-id } updated-audit)
    )
    
    ;; Create audit entry
    (let ((entry-id (var-get next-entry-id)))
      (map-set audit-entries
        { proposal-id: proposal-id, entry-id: entry-id }
        {
          event-type: "vote-commitment",
          event-data: "Vote commitment recorded for verification",
          block-height: stacks-block-height,
          actor: voter,
          integrity-hash: (keccak256 commitment-hash)
        }
      )
      (var-set next-entry-id (+ entry-id u1))
    )
    
    (ok commitment-hash)
  )
)

;; Verify vote commitment by revealing vote details
(define-public (verify-vote-commitment (proposal-id uint) (voter principal) (vote bool) (weight uint) (nonce uint))
  (let
    (
      (commitment (unwrap! (get-vote-commitment proposal-id voter) err-audit-not-found))
      (expected-hash (generate-vote-commitment voter proposal-id vote weight nonce))
      (audit (unwrap! (get-proposal-audit proposal-id) err-audit-not-found))
    )
    ;; Verify the commitment matches
    (asserts! (is-eq (get commitment-hash commitment) expected-hash) err-verification-failed)
    (asserts! (is-eq (get vote-weight commitment) weight) err-verification-failed)
    (asserts! (< (get verification-attempts commitment) max-verification-attempts) err-verification-failed)
    
    ;; Update commitment as verified
    (map-set vote-commitments
      { proposal-id: proposal-id, voter: voter }
      (merge commitment {
        verification-status: "verified",
        verification-attempts: (+ (get verification-attempts commitment) u1)
      })
    )
    
    ;; Update audit summary
    (map-set proposal-audits
      { proposal-id: proposal-id }
      (merge audit { verified-commitments: (+ (get verified-commitments audit) u1) })
    )
    
    ;; Create verification audit entry
    (let ((entry-id (var-get next-entry-id)))
      (map-set audit-entries
        { proposal-id: proposal-id, entry-id: entry-id }
        {
          event-type: "verification-success",
          event-data: "Vote commitment successfully verified",
          block-height: stacks-block-height,
          actor: tx-sender,
          integrity-hash: expected-hash
        }
      )
      (var-set next-entry-id (+ entry-id u1))
    )
    
    (ok true)
  )
)

;; Challenge a vote verification
(define-public (challenge-vote-verification (proposal-id uint) (vote-hash (buff 32)) (challenge-reason (string-ascii 100)) (evidence-hash (buff 32)))
  (let
    (
      (challenge-fee (var-get verification-fee))
    )
    ;; Charge verification fee to prevent spam
    (try! (contract-call? .VotingToken transfer challenge-fee tx-sender (as-contract tx-sender) none))
    
    ;; Record the challenge
    (map-set verification-challenges
      { proposal-id: proposal-id, challenger: tx-sender, vote-hash: vote-hash }
      {
        challenge-reason: challenge-reason,
        challenge-block: stacks-block-height,
        resolution-status: "pending",
        resolved-by: none,
        evidence-hash: (some evidence-hash)
      }
    )
    
    ;; Create audit entry for challenge
    (let ((entry-id (var-get next-entry-id)))
      (map-set audit-entries
        { proposal-id: proposal-id, entry-id: entry-id }
        {
          event-type: "verification-challenge",
          event-data: challenge-reason,
          block-height: stacks-block-height,
          actor: tx-sender,
          integrity-hash: evidence-hash
        }
      )
      (var-set next-entry-id (+ entry-id u1))
    )
    
    (ok true)
  )
)

;; Seal audit trail to prevent further modifications
(define-public (seal-audit-trail (proposal-id uint) (final-tally-hash (buff 32)))
  (let
    (
      (audit (unwrap! (get-proposal-audit proposal-id) err-audit-not-found))
    )
    ;; Only authorized verifiers or admin can seal
    (asserts! (or (is-authorized-verifier tx-sender) (is-eq tx-sender (var-get contract-admin))) err-unauthorized)
    (asserts! (not (get audit-sealed audit)) err-audit-sealed)
    
    ;; Ensure enough time has passed for verification
    (asserts! (> stacks-block-height (+ audit-seal-delay stacks-block-height)) err-unauthorized)
    
    ;; Seal the audit
    (map-set proposal-audits
      { proposal-id: proposal-id }
      (merge audit {
        audit-sealed: true,
        seal-block: (some stacks-block-height),
        final-tally-hash: (some final-tally-hash),
        independent-verifier: (some tx-sender)
      })
    )
    
    ;; Create final audit entry
    (let ((entry-id (var-get next-entry-id)))
      (map-set audit-entries
        { proposal-id: proposal-id, entry-id: entry-id }
        {
          event-type: "audit-sealed",
          event-data: "Audit trail sealed and finalized",
          block-height: stacks-block-height,
          actor: tx-sender,
          integrity-hash: final-tally-hash
        }
      )
      (var-set next-entry-id (+ entry-id u1))
    )
    
    (ok true)
  )
)

;; Authorize independent verifier
(define-public (authorize-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-admin)) err-unauthorized)
    
    (map-set authorized-verifiers
      { verifier: verifier }
      {
        authorized-by: tx-sender,
        authorization-block: stacks-block-height,
        verifications-completed: u0,
        accuracy-score: u100 ;; Start with perfect score
      }
    )
    
    (ok true)
  )
)

;; Generate integrity report for proposal
(define-public (generate-integrity-report (proposal-id uint))
  (let
    (
      (audit (unwrap! (get-proposal-audit proposal-id) err-audit-not-found))
      (completeness-score (get-audit-completeness proposal-id))
    )
    ;; Only authorized verifiers can generate reports
    (asserts! (is-authorized-verifier tx-sender) err-unauthorized)
    
    ;; Create comprehensive audit entry
    (let ((entry-id (var-get next-entry-id)))
      (map-set audit-entries
        { proposal-id: proposal-id, entry-id: entry-id }
        {
          event-type: "integrity-report",
          event-data: "Comprehensive audit report generated",
          block-height: stacks-block-height,
          actor: tx-sender,
          integrity-hash: (keccak256 (unwrap-panic (to-consensus-buff? completeness-score)))
        }
      )
      (var-set next-entry-id (+ entry-id u1))
    )
    
    (ok completeness-score)
  )
)
