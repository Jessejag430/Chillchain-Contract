;; Voting Pool & Incentive Distribution System
;; This contract manages incentive pools and reward distribution for voting participation

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-POOL-NOT-FOUND (err u101))
(define-constant ERR-POOL-ALREADY-EXISTS (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-NO-PARTICIPATION (err u105))
(define-constant ERR-ALREADY-CLAIMED (err u106))
(define-constant ERR-POOL-INACTIVE (err u107))
(define-constant ERR-INVALID-DISTRIBUTION-METHOD (err u108))

;; Data Maps
(define-map incentive-pools 
    uint 
    {
        creator: principal,
        total-pool: uint,
        distributed-amount: uint,
        distribution-method: (string-ascii 20),
        min-participation: uint,
        active: bool,
        created-at: uint,
        distribution-completed: bool
    }
)

(define-map voter-participation
    {pool-id: uint, voter: principal}
    {
        votes-cast: uint,
        quality-score: uint,
        participation-start: uint,
        eligible: bool
    }
)

(define-map pool-contributions
    {pool-id: uint, contributor: principal}
    uint
)

(define-map reward-distributions
    {pool-id: uint, voter: principal}
    {
        reward-amount: uint,
        claimed: bool,
        distribution-date: uint
    }
)

;; Global Settings
(define-data-var next-pool-id uint u1)
(define-data-var min-pool-amount uint u1000000) ;; 1 STX minimum
(define-data-var participation-threshold uint u5) ;; Minimum votes to be eligible
(define-data-var quality-weight-factor uint u100) ;; Factor for quality score weighting

;; Create Incentive Pool
(define-public (create-pool (distribution-method (string-ascii 20)) (min-participation uint))
    (let 
        (
            (pool-id (var-get next-pool-id))
        )
        (asserts! (or (is-eq distribution-method "equal") 
                     (is-eq distribution-method "weighted") 
                     (is-eq distribution-method "performance")) ERR-INVALID-DISTRIBUTION-METHOD)
        (map-set incentive-pools pool-id
            {
                creator: tx-sender,
                total-pool: u0,
                distributed-amount: u0,
                distribution-method: distribution-method,
                min-participation: min-participation,
                active: true,
                created-at: block-height,
                distribution-completed: false
            }
        )
        (var-set next-pool-id (+ pool-id u1))
        (ok pool-id)
    )
)

;; Contribute to Pool
(define-public (contribute-to-pool (pool-id uint) (amount uint))
    (let 
        (
            (pool-data (unwrap! (map-get? incentive-pools pool-id) ERR-POOL-NOT-FOUND))
            (current-contribution (default-to u0 (map-get? pool-contributions {pool-id: pool-id, contributor: tx-sender})))
        )
        (asserts! (get active pool-data) ERR-POOL-INACTIVE)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update pool total
        (map-set incentive-pools pool-id
            (merge pool-data {total-pool: (+ (get total-pool pool-data) amount)})
        )
        
        ;; Update contributor's total
        (map-set pool-contributions {pool-id: pool-id, contributor: tx-sender}
            (+ current-contribution amount)
        )
        
        (ok true)
    )
)

;; Record Voter Participation
(define-public (record-participation (pool-id uint) (voter principal) (quality-score uint))
    (let 
        (
            (pool-data (unwrap! (map-get? incentive-pools pool-id) ERR-POOL-NOT-FOUND))
            (current-participation (default-to {votes-cast: u0, quality-score: u0, participation-start: block-height, eligible: false} 
                                             (map-get? voter-participation {pool-id: pool-id, voter: voter})))
        )
        (asserts! (get active pool-data) ERR-POOL-INACTIVE)
        
        ;; Update participation data
        (map-set voter-participation {pool-id: pool-id, voter: voter}
            {
                votes-cast: (+ (get votes-cast current-participation) u1),
                quality-score: (+ (get quality-score current-participation) quality-score),
                participation-start: (get participation-start current-participation),
                eligible: (>= (+ (get votes-cast current-participation) u1) (get min-participation pool-data))
            }
        )
        
        (ok true)
    )
)

;; Compute and Distribute Rewards
(define-public (compute-rewards (pool-id uint) (voters (list 100 principal)))
    (let 
        (
            (pool-data (unwrap! (map-get? incentive-pools pool-id) ERR-POOL-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get creator pool-data)) ERR-UNAUTHORIZED)
        (asserts! (get active pool-data) ERR-POOL-INACTIVE)
        (asserts! (not (get distribution-completed pool-data)) ERR-ALREADY-CLAIMED)
        
        ;; Process each voter for reward calculation
        (fold compute-individual-reward voters (ok pool-id))
    )
)

;; Helper function to compute individual rewards
(define-private (compute-individual-reward (voter principal) (result (response uint uint)))
    (match result
        success-pool-id
        (let 
            (
                (pool-data (unwrap! (map-get? incentive-pools success-pool-id) ERR-POOL-NOT-FOUND))
                (participation-data (map-get? voter-participation {pool-id: success-pool-id, voter: voter}))
            )
            (match participation-data
                some-participation
                (if (get eligible some-participation)
                    (let 
                        (
                            (reward-amount (calculate-reward-amount success-pool-id voter))
                        )
                        (map-set reward-distributions {pool-id: success-pool-id, voter: voter}
                            {
                                reward-amount: reward-amount,
                                claimed: false,
                                distribution-date: block-height
                            }
                        )
                        (ok success-pool-id)
                    )
                    (ok success-pool-id)
                )
                (ok success-pool-id)
            )
        )
        error-code (err error-code)
    )
)

;; Calculate reward amount based on distribution method
(define-read-only (calculate-reward-amount (pool-id uint) (voter principal))
    (let 
        (
            (pool-data (unwrap! (map-get? incentive-pools pool-id) u0))
            (participation (unwrap! (map-get? voter-participation {pool-id: pool-id, voter: voter}) u0))
            (distribution-method (get distribution-method pool-data))
            (total-pool (get total-pool pool-data))
        )
        (if (is-eq distribution-method "equal")
            ;; Equal distribution - divide pool equally among eligible voters
            (/ total-pool u10) ;; Simplified - would need to count eligible voters
            (if (is-eq distribution-method "weighted")
                ;; Weighted by votes cast
                (* (/ total-pool u100) (get votes-cast participation))
                ;; Performance-based (votes + quality score)
                (* (/ total-pool u100) (+ (get votes-cast participation) (get quality-score participation)))
            )
        )
    )
)

;; Claim Rewards
(define-public (claim-rewards (pool-id uint))
    (let 
        (
            (distribution-data (unwrap! (map-get? reward-distributions {pool-id: pool-id, voter: tx-sender}) ERR-NO-PARTICIPATION))
        )
        (asserts! (not (get claimed distribution-data)) ERR-ALREADY-CLAIMED)
        (asserts! (> (get reward-amount distribution-data) u0) ERR-INVALID-AMOUNT)
        
        ;; Transfer reward to voter
        (try! (as-contract (stx-transfer? (get reward-amount distribution-data) tx-sender tx-sender)))
        
        ;; Mark as claimed
        (map-set reward-distributions {pool-id: pool-id, voter: tx-sender}
            (merge distribution-data {claimed: true})
        )
        
        (ok (get reward-amount distribution-data))
    )
)

;; Deactivate Pool
(define-public (deactivate-pool (pool-id uint))
    (let 
        (
            (pool-data (unwrap! (map-get? incentive-pools pool-id) ERR-POOL-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get creator pool-data)) ERR-UNAUTHORIZED)
        (map-set incentive-pools pool-id
            (merge pool-data {active: false})
        )
        (ok true)
    )
)

;; Read-only functions

(define-read-only (get-pool-info (pool-id uint))
    (map-get? incentive-pools pool-id)
)

(define-read-only (get-voter-participation (pool-id uint) (voter principal))
    (map-get? voter-participation {pool-id: pool-id, voter: voter})
)

(define-read-only (get-pool-contribution (pool-id uint) (contributor principal))
    (default-to u0 (map-get? pool-contributions {pool-id: pool-id, contributor: contributor}))
)

(define-read-only (get-reward-info (pool-id uint) (voter principal))
    (map-get? reward-distributions {pool-id: pool-id, voter: voter})
)

(define-read-only (get-next-pool-id)
    (var-get next-pool-id)
)

(define-read-only (is-voter-eligible (pool-id uint) (voter principal))
    (match (map-get? voter-participation {pool-id: pool-id, voter: voter})
        some-participation (get eligible some-participation)
        false
    )
)