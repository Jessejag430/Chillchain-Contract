;; Warranty Insurance - Enhanced protection for warranty holders
;; Provides additional coverage beyond standard warranty terms

;; Error constants
(define-constant err-not-authorized (err u300))
(define-constant err-policy-not-found (err u301))
(define-constant err-insufficient-funds (err u302))
(define-constant err-policy-expired (err u303))
(define-constant err-claim-limit-exceeded (err u304))
(define-constant err-invalid-coverage-type (err u305))

;; Data variables
(define-data-var policy-counter uint u0)
(define-data-var admin principal tx-sender)

;; Coverage types
(define-constant coverage-basic u1)
(define-constant coverage-premium u2)
(define-constant coverage-ultimate u3)

;; Policy status
(define-constant status-active "active")
(define-constant status-expired "expired")
(define-constant status-suspended "suspended")

;; Insurance policies
(define-map insurance-policies
    uint ;; policy-id
    {
        warranty-id: uint,
        policy-holder: principal,
        coverage-type: uint,
        premium-paid: uint,
        coverage-amount: uint,
        deductible: uint,
        start-date: uint,
        expiry-date: uint,
        claims-made: uint,
        max-claims: uint,
        status: (string-ascii 20),
        benefits: (list 5 (string-ascii 50))
    }
)

;; Coverage plans with different tiers
(define-map coverage-plans
    uint ;; coverage-type
    {
        name: (string-ascii 30),
        base-premium: uint,
        coverage-multiplier: uint,
        max-claims: uint,
        deductible-percentage: uint,
        benefits: (list 5 (string-ascii 50)),
        active: bool
    }
)

;; Premium calculation factors
(define-map premium-factors
    {warranty-id: uint, coverage-type: uint}
    {
        risk-score: uint,
        age-factor: uint,
        usage-factor: uint,
        final-premium: uint
    }
)

;; Insurance claims
(define-map insurance-claims
    uint ;; claim-id generated from policy-id + claim-count
    {
        policy-id: uint,
        claim-amount: uint,
        incident-date: uint,
        description: (string-ascii 200),
        status: (string-ascii 20),
        payout-amount: uint,
        processed-date: (optional uint)
    }
)

;; Policy holder tracking
(define-map holder-policies
    principal
    (list 10 uint)
)

;; Read-only functions
(define-read-only (get-policy (policy-id uint))
    (map-get? insurance-policies policy-id))

(define-read-only (get-coverage-plan (coverage-type uint))
    (map-get? coverage-plans coverage-type))

(define-read-only (get-premium-factors (warranty-id uint) (coverage-type uint))
    (map-get? premium-factors {warranty-id: warranty-id, coverage-type: coverage-type}))

(define-read-only (get-holder-policies (holder principal))
    (default-to (list) (map-get? holder-policies holder)))

(define-read-only (calculate-premium (warranty-id uint) (coverage-type uint))
    (match (get-coverage-plan coverage-type)
        plan (let (
            (base-premium (get base-premium plan))
            (risk-score u100) ;; Default risk score
            (premium (* base-premium (/ risk-score u100)))
        )
            (some premium)
        )
        none
    )
)

;; Initialize coverage plans
(define-public (initialize-coverage-plans)
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) err-not-authorized)
        
        ;; Basic coverage
        (map-set coverage-plans coverage-basic
            {
                name: "Basic Protection",
                base-premium: u50000,
                coverage-multiplier: u150,
                max-claims: u2,
                deductible-percentage: u10,
                benefits: (list "Extended Support" "Accident Coverage"),
                active: true
            }
        )
        
        ;; Premium coverage
        (map-set coverage-plans coverage-premium
            {
                name: "Premium Shield",
                base-premium: u100000,
                coverage-multiplier: u300,
                max-claims: u5,
                deductible-percentage: u5,
                benefits: (list "24/7 Support" "Replacement Guarantee" "Theft Protection"),
                active: true
            }
        )
        
        ;; Ultimate coverage
        (map-set coverage-plans coverage-ultimate
            {
                name: "Ultimate Care",
                base-premium: u200000,
                coverage-multiplier: u500,
                max-claims: u10,
                deductible-percentage: u2,
                benefits: (list "Concierge Service" "Global Coverage" "Instant Replacement" "No Questions Asked"),
                active: true
            }
        )
        (ok true)
    )
)

;; Purchase insurance policy
(define-public (purchase-policy (warranty-id uint) (coverage-type uint))
    (let (
        (plan (unwrap! (get-coverage-plan coverage-type) err-policy-not-found))
        (premium (unwrap! (calculate-premium warranty-id coverage-type) err-policy-not-found))
        (policy-id (+ (var-get policy-counter) u1))
        (current-policies (get-holder-policies tx-sender))
        (coverage-amount (* premium (get coverage-multiplier plan)))
        (deductible (/ (* coverage-amount (get deductible-percentage plan)) u100))
    )
        (asserts! (get active plan) err-policy-not-found)
        (asserts! (<= coverage-type coverage-ultimate) err-invalid-coverage-type)
        
        ;; Transfer premium payment
        (try! (stx-transfer? premium tx-sender (var-get admin)))
        
        ;; Create policy
        (map-set insurance-policies policy-id
            {
                warranty-id: warranty-id,
                policy-holder: tx-sender,
                coverage-type: coverage-type,
                premium-paid: premium,
                coverage-amount: coverage-amount,
                deductible: deductible,
                start-date: stacks-block-height,
                expiry-date: (+ stacks-block-height u52560), ;; ~1 year
                claims-made: u0,
                max-claims: (get max-claims plan),
                status: status-active,
                benefits: (get benefits plan)
            }
        )
        
        ;; Update holder's policy list
        (map-set holder-policies tx-sender
            (unwrap! (as-max-len? (append current-policies policy-id) u10) err-policy-not-found)
        )
        
        (var-set policy-counter policy-id)
        (ok policy-id)
    )
)

;; File insurance claim
(define-public (file-insurance-claim (policy-id uint) (claim-amount uint) (description (string-ascii 200)))
    (let (
        (policy (unwrap! (get-policy policy-id) err-policy-not-found))
        (claim-id (+ (* policy-id u1000) (get claims-made policy)))
    )
        (asserts! (is-eq tx-sender (get policy-holder policy)) err-not-authorized)
        (asserts! (is-eq (get status policy) status-active) err-policy-expired)
        (asserts! (< stacks-block-height (get expiry-date policy)) err-policy-expired)
        (asserts! (< (get claims-made policy) (get max-claims policy)) err-claim-limit-exceeded)
        
        ;; Create claim
        (map-set insurance-claims claim-id
            {
                policy-id: policy-id,
                claim-amount: claim-amount,
                incident-date: stacks-block-height,
                description: description,
                status: "pending",
                payout-amount: u0,
                processed-date: none
            }
        )
        
        ;; Update policy claims count
        (map-set insurance-policies policy-id
            (merge policy {claims-made: (+ (get claims-made policy) u1)})
        )
        
        (ok claim-id)
    )
)

;; Process insurance claim (admin only)
(define-public (process-claim (claim-id uint) (approved bool) (payout-amount uint))
    (let (
        (claim (unwrap! (map-get? insurance-claims claim-id) err-policy-not-found))
        (policy (unwrap! (get-policy (get policy-id claim)) err-policy-not-found))
        (final-payout (if approved 
            (if (> payout-amount (get deductible policy))
                (- payout-amount (get deductible policy))
                u0
            )
            u0
        ))
    )
        (asserts! (is-eq tx-sender (var-get admin)) err-not-authorized)
        
        ;; Update claim status
        (map-set insurance-claims claim-id
            (merge claim {
                status: (if approved "approved" "denied"),
                payout-amount: final-payout,
                processed-date: (some stacks-block-height)
            })
        )
        
        ;; Transfer payout if approved
        (if (and approved (> final-payout u0))
            (try! (stx-transfer? final-payout (var-get admin) (get policy-holder policy)))
            true
        )
        
        (ok final-payout)
    )
)

;; Renew policy
(define-public (renew-policy (policy-id uint))
    (let (
        (policy (unwrap! (get-policy policy-id) err-policy-not-found))
        (plan (unwrap! (get-coverage-plan (get coverage-type policy)) err-policy-not-found))
        (renewal-premium (get base-premium plan))
    )
        (asserts! (is-eq tx-sender (get policy-holder policy)) err-not-authorized)
        
        ;; Transfer renewal premium
        (try! (stx-transfer? renewal-premium tx-sender (var-get admin)))
        
        ;; Extend policy
        (map-set insurance-policies policy-id
            (merge policy {
                expiry-date: (+ stacks-block-height u52560),
                claims-made: u0,
                status: status-active
            })
        )
        
        (ok true)
    )
)

;; Check policy status
(define-read-only (is-policy-active (policy-id uint))
    (match (get-policy policy-id)
        policy (and 
            (is-eq (get status policy) status-active)
            (< stacks-block-height (get expiry-date policy))
        )
        false
    )
)

;; Get policy benefits
(define-read-only (get-policy-benefits (policy-id uint))
    (match (get-policy policy-id)
        policy (some (get benefits policy))
        none
    )
)
