;; Tenant Screening & Background Verification Contract
;; Enables landlords to verify tenant eligibility through on-chain references and credit history

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u600))
(define-constant ERR-SCREENING-NOT-FOUND (err u601))
(define-constant ERR-ALREADY-SUBMITTED (err u602))
(define-constant ERR-INSUFFICIENT-SCORE (err u603))
(define-constant ERR-EXPIRED-REFERENCE (err u604))
(define-constant ERR-INVALID-RATING (err u605))

;; Data variables
(define-data-var screening-counter uint u0)
(define-data-var min-credit-score uint u650)
(define-data-var reference-validity-blocks uint u14400) ;; ~100 days

;; Tenant screening applications
(define-map screening-applications
    uint ;; screening-id
    {
        applicant: principal,
        property: principal,
        employment-verified: bool,
        income-amount: uint,
        credit-score: uint,
        references-count: uint,
        application-date: uint,
        status: (string-ascii 20), ;; \"pending\", \"approved\", \"rejected\"
        landlord-notes: (string-ascii 256)
    }
)

;; Reference verification from previous landlords
(define-map tenant-references
    { applicant: principal, reference-id: uint }
    {
        previous-landlord: principal,
        rental-period-months: uint,
        payment-timeliness: uint, ;; 1-100 scale
        property-care: uint, ;; 1-100 scale
        overall-rating: uint, ;; 1-100 scale
        would-rent-again: bool,
        reference-date: uint,
        verified: bool
    }
)

;; Employment verification records
(define-map employment-records
    principal ;; applicant
    {
        employer-verified: bool,
        monthly-income: uint,
        employment-length: uint, ;; in months
        income-stability: uint, ;; 1-100 scale
        verification-date: uint,
        verifier: principal
    }
)

;; Credit history summary (privacy-preserving)
(define-map credit-summaries
    principal ;; applicant
    {
        credit-score: uint,
        payment-history-score: uint, ;; 1-100 scale
        debt-to-income-ratio: uint, ;; percentage
        has-evictions: bool,
        bankruptcy-history: bool,
        verified-by: principal,
        last-updated: uint
    }
)

;; Submit tenant screening application
(define-public (submit-screening-application 
    (property principal)
    (employment-verified bool)
    (income-amount uint)
    (credit-score uint))
    (let
        (
            (screening-id (+ (var-get screening-counter) u1))
        )
        ;; Check if applicant already has pending application for this property
        (asserts! (is-none (get-pending-application tx-sender property)) ERR-ALREADY-SUBMITTED)
        (asserts! (>= credit-score (var-get min-credit-score)) ERR-INSUFFICIENT-SCORE)
        
        ;; Create screening application
        (map-set screening-applications screening-id
            {
                applicant: tx-sender,
                property: property,
                employment-verified: employment-verified,
                income-amount: income-amount,
                credit-score: credit-score,
                references-count: u0,
                application-date: stacks-block-height,
                status: \"pending\",
                landlord-notes: \"\"
            }
        )
        
        (var-set screening-counter screening-id)
        (ok screening-id)
    )
)

;; Add reference from previous landlord
(define-public (add-tenant-reference
    (applicant principal)
    (reference-id uint)
    (rental-period-months uint)
    (payment-timeliness uint)
    (property-care uint)
    (overall-rating uint)
    (would-rent-again bool))
    (begin
        ;; Validate ratings are within range
        (asserts! (and (<= payment-timeliness u100) (>= payment-timeliness u1)) ERR-INVALID-RATING)
        (asserts! (and (<= property-care u100) (>= property-care u1)) ERR-INVALID-RATING)
        (asserts! (and (<= overall-rating u100) (>= overall-rating u1)) ERR-INVALID-RATING)
        
        ;; Store reference
        (map-set tenant-references { applicant: applicant, reference-id: reference-id }
            {
                previous-landlord: tx-sender,
                rental-period-months: rental-period-months,
                payment-timeliness: payment-timeliness,
                property-care: property-care,
                overall-rating: overall-rating,
                would-rent-again: would-rent-again,
                reference-date: stacks-block-height,
                verified: true
            }
        )
        
        (ok true)
    )
)

;; Verify employment information
(define-public (verify-employment
    (applicant principal)
    (monthly-income uint)
    (employment-length uint)
    (income-stability uint))
    (begin
        (asserts! (and (<= income-stability u100) (>= income-stability u1)) ERR-INVALID-RATING)
        
        (ok (map-set employment-records applicant
            {
                employer-verified: true,
                monthly-income: monthly-income,
                employment-length: employment-length,
                income-stability: income-stability,
                verification-date: stacks-block-height,
                verifier: tx-sender
            }
        ))
    )
)

;; Submit credit summary (by authorized credit agency)
(define-public (submit-credit-summary
    (applicant principal)
    (credit-score uint)
    (payment-history-score uint)
    (debt-to-income-ratio uint)
    (has-evictions bool)
    (bankruptcy-history bool))
    (begin
        (asserts! (and (<= payment-history-score u100) (>= payment-history-score u1)) ERR-INVALID-RATING)
        (asserts! (<= debt-to-income-ratio u100) ERR-INVALID-RATING)
        
        (ok (map-set credit-summaries applicant
            {
                credit-score: credit-score,
                payment-history-score: payment-history-score,
                debt-to-income-ratio: debt-to-income-ratio,
                has-evictions: has-evictions,
                bankruptcy-history: bankruptcy-history,
                verified-by: tx-sender,
                last-updated: stacks-block-height
            }
        ))
    )
)

;; Landlord reviews and approves/rejects application
(define-public (review-screening-application
    (screening-id uint)
    (approve bool)
    (notes (string-ascii 256)))
    (let
        (
            (application (unwrap! (map-get? screening-applications screening-id) ERR-SCREENING-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get property application)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status application) \"pending\") ERR-NOT-AUTHORIZED)
        
        (ok (map-set screening-applications screening-id
            (merge application {
                status: (if approve \"approved\" \"rejected\"),
                landlord-notes: notes
            })
        ))
    )
)

;; Calculate overall tenant score
(define-public (calculate-tenant-score (applicant principal))
    (let
        (
            (credit-info (map-get? credit-summaries applicant))
            (employment-info (map-get? employment-records applicant))
            (base-score u50)
        )
        (let
            (
                (credit-component (match credit-info
                    some-credit (/ (get credit-score some-credit) u10) ;; Scale down credit score
                    u0))
                (employment-component (match employment-info
                    some-employment (/ (get income-stability some-employment) u4) ;; Scale down stability
                    u0))
            )
            (ok (+ base-score credit-component employment-component))
        )
    )
)

;; Helper function to check for pending applications
(define-private (get-pending-application (applicant principal) (property principal))
    ;; Simplified: would check if applicant has pending application for property
    none
)

;; Read-only functions
(define-read-only (get-screening-application (screening-id uint))
    (map-get? screening-applications screening-id)
)

(define-read-only (get-tenant-reference (applicant principal) (reference-id uint))
    (map-get? tenant-references { applicant: applicant, reference-id: reference-id })
)

(define-read-only (get-employment-record (applicant principal))
    (map-get? employment-records applicant)
)

(define-read-only (get-credit-summary (applicant principal))
    (map-get? credit-summaries applicant)
)

(define-read-only (get-screening-stats)
    {
        total-applications: (var-get screening-counter),
        min-credit-score: (var-get min-credit-score),
        reference-validity-blocks: (var-get reference-validity-blocks)
    }
)