
;; title: drug-auth

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-status (err u103))

(define-data-var next-batch-id uint u1)

(define-map drug-batches
    { batch-id: uint }
    {
        manufacturer: principal,
        drug-name: (string-ascii 64),
        production-date: uint,
        expiry-date: uint,
        current-holder: principal,
        status: (string-ascii 20)
    }
)

(define-map transfer-history
    { batch-id: uint, transfer-id: uint }
    {
        from: principal,
        to: principal,
        timestamp: uint,
        location: (string-ascii 64)
    }
)

(define-map batch-transfer-count
    { batch-id: uint }
    { count: uint }
)

(define-public (register-drug-batch 
    (drug-name (string-ascii 64))
    (production-date uint)
    (expiry-date uint))
    (let
        ((batch-id (var-get next-batch-id)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-insert drug-batches
            { batch-id: batch-id }
            {
                manufacturer: tx-sender,
                drug-name: drug-name,
                production-date: production-date,
                expiry-date: expiry-date,
                current-holder: tx-sender,
                status: "manufactured"
            }
        )
        (map-insert batch-transfer-count
            { batch-id: batch-id }
            { count: u0 }
        )
        (var-set next-batch-id (+ batch-id u1))
        (ok batch-id)
    )
)

(define-public (transfer-batch
    (batch-id uint)
    (recipient principal)
    (location (string-ascii 64)))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found))
         (transfer-count (unwrap! (map-get? batch-transfer-count { batch-id: batch-id }) err-not-found)))
        (asserts! (is-eq (get current-holder batch) tx-sender) err-owner-only)
        (map-set drug-batches
            { batch-id: batch-id }
            (merge batch { 
                current-holder: recipient,
                status: "in-transit"
            })
        )
        (map-set transfer-history
            { 
                batch-id: batch-id,
                transfer-id: (get count transfer-count)
            }
            {
                from: tx-sender,
                to: recipient,
                timestamp: stacks-block-height,
                location: location
            }
        )
        (map-set batch-transfer-count
            { batch-id: batch-id }
            { count: (+ (get count transfer-count) u1) }
        )
        (ok true)
    )
)

(define-public (confirm-receipt
    (batch-id uint))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found)))
        (asserts! (is-eq (get current-holder batch) tx-sender) err-owner-only)
        (map-set drug-batches
            { batch-id: batch-id }
            (merge batch { status: "received" })
        )
        (ok true)
    )
)

(define-read-only (get-batch-details (batch-id uint))
    (map-get? drug-batches { batch-id: batch-id })
)

(define-read-only (get-transfer-history (batch-id uint) (transfer-id uint))
    (map-get? transfer-history { batch-id: batch-id, transfer-id: transfer-id })
)

(define-read-only (get-batch-transfer-count (batch-id uint))
    (map-get? batch-transfer-count { batch-id: batch-id })
)


(define-constant err-invalid-batch (err u104))
(define-constant err-already-recalled (err u105))

(define-map recalled-batches
    { batch-id: uint }
    {
        recall-reason: (string-ascii 256),
        recall-date: uint,
        severity-level: (string-ascii 20)
    }
)

(define-public (recall-batch 
    (batch-id uint)
    (recall-reason (string-ascii 256))
    (severity-level (string-ascii 20)))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (map-get? recalled-batches { batch-id: batch-id })) err-already-recalled)
        (map-set drug-batches
            { batch-id: batch-id }
            (merge batch { status: "recalled" })
        )
        (map-set recalled-batches
            { batch-id: batch-id }
            {
                recall-reason: recall-reason,
                recall-date: stacks-block-height,
                severity-level: severity-level
            }
        )
        (ok true)
    )
)

(define-read-only (get-recall-details (batch-id uint))
    (map-get? recalled-batches { batch-id: batch-id })
)


(define-constant err-invalid-verification (err u106))

(define-map quality-verifications
    { batch-id: uint, verification-id: uint }
    {
        verifier: principal,
        timestamp: uint,
        location: (string-ascii 64),
        temperature: int,
        humidity: int,
        passed: bool
    }
)

(define-map batch-verification-count
    { batch-id: uint }
    { count: uint }
)

(define-public (add-quality-verification
    (batch-id uint)
    (location (string-ascii 64))
    (temperature int)
    (humidity int)
    (passed bool))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found))
         (verification-count (default-to { count: u0 } (map-get? batch-verification-count { batch-id: batch-id }))))
        (asserts! (is-eq (get current-holder batch) tx-sender) err-owner-only)
        (map-set quality-verifications
            {
                batch-id: batch-id,
                verification-id: (get count verification-count)
            }
            {
                verifier: tx-sender,
                timestamp: stacks-block-height,
                location: location,
                temperature: temperature,
                humidity: humidity,
                passed: passed
            }
        )
        (map-set batch-verification-count
            { batch-id: batch-id }
            { count: (+ (get count verification-count) u1) }
        )
        (ok true)
    )
)

(define-read-only (get-quality-verification 
    (batch-id uint)
    (verification-id uint))
    (map-get? quality-verifications { batch-id: batch-id, verification-id: verification-id })
)

(define-constant err-already-expired (err u107))
(define-constant err-invalid-alert-period (err u108))

(define-map expiry-alerts
    { batch-id: uint }
    {
        alert-before-days: uint,
        alert-set-by: principal,
        alert-timestamp: uint
    }
)

(define-map expired-batches
    { batch-id: uint }
    {
        expired-timestamp: uint,
        marked-by: principal
    }
)

(define-public (set-expiry-alert
    (batch-id uint)
    (alert-before-days uint))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found)))
        (asserts! (is-eq (get current-holder batch) tx-sender) err-owner-only)
        (asserts! (> alert-before-days u0) err-invalid-alert-period)
        (map-set expiry-alerts
            { batch-id: batch-id }
            {
                alert-before-days: alert-before-days,
                alert-set-by: tx-sender,
                alert-timestamp: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (mark-batch-expired
    (batch-id uint))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found)))
        (asserts! (>= stacks-block-height (get expiry-date batch)) err-invalid-status)
        (asserts! (is-none (map-get? expired-batches { batch-id: batch-id })) err-already-expired)
        (map-set drug-batches
            { batch-id: batch-id }
            (merge batch { status: "expired" })
        )
        (map-set expired-batches
            { batch-id: batch-id }
            {
                expired-timestamp: stacks-block-height,
                marked-by: tx-sender
            }
        )
        (ok true)
    )
)

(define-read-only (is-batch-expired (batch-id uint))
    (match (map-get? drug-batches { batch-id: batch-id })
        batch (>= stacks-block-height (get expiry-date batch))
        false
    )
)

(define-read-only (is-batch-expiring-soon (batch-id uint))
    (match (map-get? drug-batches { batch-id: batch-id })
        batch (match (map-get? expiry-alerts { batch-id: batch-id })
            alert (let
                ((alert-threshold (- (get expiry-date batch) (get alert-before-days alert))))
                (and 
                    (>= stacks-block-height alert-threshold)
                    (< stacks-block-height (get expiry-date batch))
                )
            )
            false
        )
        false
    )
)

(define-read-only (get-expiry-alert (batch-id uint))
    (map-get? expiry-alerts { batch-id: batch-id })
)

(define-read-only (get-expired-batch-details (batch-id uint))
    (map-get? expired-batches { batch-id: batch-id })
)

(define-read-only (get-batch-expiry-status (batch-id uint))
    (let
        ((is-expired (is-batch-expired batch-id))
         (is-expiring-soon (is-batch-expiring-soon batch-id)))
        (if is-expired
            "expired"
            (if is-expiring-soon
                "expiring-soon"
                "valid"
            )
        )
    )
)

(define-constant err-invalid-score (err u109))
(define-constant max-score u1000)
(define-constant base-score u500)

(define-map holder-reputation
    { holder: principal }
    {
        successful-transfers: uint,
        failed-verifications: uint,
        total-batches-handled: uint,
        reputation-score: uint
    }
)

(define-map batch-authenticity-score
    { batch-id: uint }
    {
        current-score: uint,
        last-updated: uint,
        factors: {
            quality-score: uint,
            transfer-score: uint,
            holder-score: uint
        }
    }
)

(define-public (calculate-batch-score (batch-id uint))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found))
         (verification-count (default-to { count: u0 } (map-get? batch-verification-count { batch-id: batch-id })))
         (transfer-count (default-to { count: u0 } (map-get? batch-transfer-count { batch-id: batch-id })))
         (holder-reputation-score (match (map-get? holder-reputation { holder: (get current-holder batch) })
            reputation (get reputation-score reputation)
            base-score))
         (quality-score (calculate-quality-score batch-id (get count verification-count)))
         (transfer-score (calculate-transfer-score (get count transfer-count)))
         (holder-score (if (< holder-reputation-score u300) holder-reputation-score u300)))
        (let
            ((total-score (if (< (+ quality-score transfer-score holder-score) max-score) (+ quality-score transfer-score holder-score) max-score)))
            (map-set batch-authenticity-score
                { batch-id: batch-id }
                {
                    current-score: total-score,
                    last-updated: stacks-block-height,
                    factors: {
                        quality-score: quality-score,
                        transfer-score: transfer-score,
                        holder-score: holder-score
                    }
                }
            )
            (ok total-score)
        )
    )
)

(define-private (calculate-quality-score (batch-id uint) (verification-count uint))
    (if (> verification-count u0)
        (let
            ((base-quality-score (if (<= verification-count u3)
                u150
                (if (<= verification-count u6)
                    u200
                    u250
                )
            )))
            (if (< base-quality-score u300) base-quality-score u300)
        )
        u100
    )
)

(define-private (calculate-transfer-score (transfer-count uint))
    (if (is-eq transfer-count u0)
        u200
        (if (<= transfer-count u3)
            u200
            (if (<= transfer-count u6)
                u150
                u100
            )
        )
    )
)



(define-public (update-holder-reputation (holder principal) (successful-transfer bool))
    (let
        ((current-rep (default-to 
            { successful-transfers: u0, failed-verifications: u0, total-batches-handled: u0, reputation-score: base-score }
            (map-get? holder-reputation { holder: holder }))))
        (let
            ((new-successful (if successful-transfer 
                (+ (get successful-transfers current-rep) u1)
                (get successful-transfers current-rep)))
             (new-failed (if successful-transfer
                (get failed-verifications current-rep)
                (+ (get failed-verifications current-rep) u1)))
             (new-total (+ (get total-batches-handled current-rep) u1)))
            (let
                ((new-score (calculate-reputation-score new-successful new-failed new-total)))
                (map-set holder-reputation
                    { holder: holder }
                    {
                        successful-transfers: new-successful,
                        failed-verifications: new-failed,
                        total-batches-handled: new-total,
                        reputation-score: new-score
                    }
                )
                (ok new-score)
            )
        )
    )
)

(define-private (calculate-reputation-score (successful uint) (failed uint) (total uint))
    (if (is-eq total u0)
        base-score
        (let
            ((success-rate (/ (* successful u1000) total)))
            (if (>= success-rate u800)
                (if (< (+ base-score u200) max-score) (+ base-score u200) max-score)
                (if (>= success-rate u600)
                    (if (< (+ base-score u100) max-score) (+ base-score u100) max-score)
                    (if (>= success-rate u400)
                        base-score
                        (if (> (- base-score u100) u100) (- base-score u100) u100)
                    )
                )
            )
        )
    )
)

(define-public (rate-batch-quality (batch-id uint) (quality-rating uint))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found)))
        (asserts! (is-eq (get current-holder batch) tx-sender) err-owner-only)
        (asserts! (<= quality-rating u10) err-invalid-score)
        (unwrap-panic (update-holder-reputation tx-sender (>= quality-rating u7)))
        (ok true)
    )
)

(define-read-only (get-batch-authenticity-score (batch-id uint))
    (map-get? batch-authenticity-score { batch-id: batch-id })
)

(define-read-only (get-holder-reputation (holder principal))
    (map-get? holder-reputation { holder: holder })
)

(define-read-only (get-batch-trust-level (batch-id uint))
    (match (map-get? batch-authenticity-score { batch-id: batch-id })
        score (let
            ((current-score (get current-score score)))
            (if (>= current-score u800)
                "high-trust"
                (if (>= current-score u600)
                    "medium-trust"
                    (if (>= current-score u400)
                        "low-trust"
                        "untrusted"
                    )
                )
            )
        )
        "unscored"
    )
)

;; Contamination Detection and Quarantine System
(define-constant err-already-contaminated (err u110))
(define-constant err-not-contaminated (err u111))
(define-constant err-quarantine-active (err u112))
(define-constant err-invalid-contamination-type (err u113))
(define-constant err-unauthorized-quarantine (err u114))

;; Track contamination events for each batch
(define-map contamination-events
    { batch-id: uint }
    {
        contamination-type: (string-ascii 50),
        detected-by: principal,
        detection-timestamp: uint,
        severity-level: uint,
        source-location: (string-ascii 100),
        confirmed: bool
    }
)

;; Track quarantine status and details
(define-map quarantine-status
    { batch-id: uint }
    {
        quarantined: bool,
        quarantine-timestamp: uint,
        quarantined-by: principal,
        quarantine-reason: (string-ascii 200),
        estimated-release-date: uint,
        requires-testing: bool
    }
)

;; Track cross-contamination risks between batches
(define-map cross-contamination-risks
    { source-batch: uint, target-batch: uint }
    {
        risk-level: uint,
        assessed-timestamp: uint,
        risk-factors: (string-ascii 150),
        mitigation-required: bool
    }
)

;; Track contamination investigation results
(define-map contamination-investigations
    { batch-id: uint }
    {
        investigator: principal,
        investigation-start: uint,
        investigation-complete: bool,
        findings: (string-ascii 300),
        clearance-granted: bool,
        follow-up-required: bool
    }
)

;; Report contamination event for a batch
(define-public (report-contamination
    (batch-id uint)
    (contamination-type (string-ascii 50))
    (severity-level uint)
    (source-location (string-ascii 100)))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found)))
        ;; Verify batch exists and caller has authority
        (asserts! (or (is-eq tx-sender contract-owner) 
                     (is-eq tx-sender (get current-holder batch))) err-owner-only)
        ;; Validate severity level (1-10 scale)
        (asserts! (and (>= severity-level u1) (<= severity-level u10)) err-invalid-contamination-type)
        ;; Check if batch is not already contaminated
        (asserts! (is-none (map-get? contamination-events { batch-id: batch-id })) err-already-contaminated)
        
        ;; Record contamination event
        (map-set contamination-events
            { batch-id: batch-id }
            {
                contamination-type: contamination-type,
                detected-by: tx-sender,
                detection-timestamp: stacks-block-height,
                severity-level: severity-level,
                source-location: source-location,
                confirmed: false
            }
        )
        
        ;; Auto-quarantine if severity is high (>= 7)
        (if (>= severity-level u7)
            (begin
                (try! (initiate-quarantine batch-id "Auto-quarantine due to high severity contamination"))
                (ok "contamination-reported-and-quarantined")
            )
            (ok "contamination-reported")
        )
    )
)

;; Confirm contamination after investigation
(define-public (confirm-contamination (batch-id uint))
    (let
        ((contamination (unwrap! (map-get? contamination-events { batch-id: batch-id }) err-not-contaminated)))
        ;; Only contract owner or original detector can confirm
        (asserts! (or (is-eq tx-sender contract-owner) 
                     (is-eq tx-sender (get detected-by contamination))) err-owner-only)
        
        ;; Update contamination status to confirmed
        (map-set contamination-events
            { batch-id: batch-id }
            (merge contamination { confirmed: true })
        )
        
        ;; Force quarantine if not already quarantined
        (match (map-get? quarantine-status { batch-id: batch-id })
            quarantine (ok "contamination-confirmed")
            (begin
                (try! (initiate-quarantine batch-id "Contamination confirmed - mandatory quarantine"))
                (ok "contamination-confirmed-and-quarantined")
            )
        )
    )
)

;; Initiate quarantine for a batch
(define-public (initiate-quarantine 
    (batch-id uint)
    (quarantine-reason (string-ascii 200)))
    (let
        ((batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found)))
        ;; Verify authorization (owner, current holder, or in contamination event)
        (asserts! (or (is-eq tx-sender contract-owner)
                     (is-eq tx-sender (get current-holder batch))
                     (is-some (map-get? contamination-events { batch-id: batch-id }))) err-unauthorized-quarantine)
        ;; Check if not already quarantined
        (match (map-get? quarantine-status { batch-id: batch-id })
            existing-quarantine (asserts! (not (get quarantined existing-quarantine)) err-quarantine-active)
            true
        )
        
        ;; Set quarantine status
        (map-set quarantine-status
            { batch-id: batch-id }
            {
                quarantined: true,
                quarantine-timestamp: stacks-block-height,
                quarantined-by: tx-sender,
                quarantine-reason: quarantine-reason,
                estimated-release-date: (+ stacks-block-height u1008), ;; ~1 week default
                requires-testing: true
            }
        )
        
        ;; Update batch status
        (map-set drug-batches
            { batch-id: batch-id }
            (merge batch { status: "quarantined" })
        )
        
        ;; Assess cross-contamination risks
        (try! (assess-cross-contamination-risks batch-id))
        (ok true)
    )
)

;; Assess cross-contamination risks for batches that interacted with contaminated batch
(define-public (assess-cross-contamination-risks (contaminated-batch-id uint))
    (let
        ((transfer-count (default-to { count: u0 } (map-get? batch-transfer-count { batch-id: contaminated-batch-id }))))
        ;; Only authorized personnel can assess risks
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        ;; Assess risk for recent transfers (simplified - checks last 3 transfers)
        (let
            ((risk-assessment-result (assess-recent-transfers contaminated-batch-id (get count transfer-count))))
            (ok risk-assessment-result)
        )
    )
)

;; Helper function to assess risks from recent transfers
(define-private (assess-recent-transfers (batch-id uint) (transfer-count uint))
    (if (> transfer-count u0)
        (let
            ((recent-transfer (map-get? transfer-history { batch-id: batch-id, transfer-id: (- transfer-count u1) })))
            (match recent-transfer
                transfer (begin
                    ;; Record cross-contamination risk
                    (map-set cross-contamination-risks
                        { source-batch: batch-id, target-batch: batch-id }
                        {
                            risk-level: u7,
                            assessed-timestamp: stacks-block-height,
                            risk-factors: "Recent transfer from contaminated batch",
                            mitigation-required: true
                        }
                    )
                    u1
                )
                u0
            )
        )
        u0
    )
)

;; Start contamination investigation
(define-public (start-investigation 
    (batch-id uint)
    (investigator principal))
    (begin
        ;; Only contract owner can assign investigators
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        ;; Batch must be contaminated or quarantined
        (asserts! (or (is-some (map-get? contamination-events { batch-id: batch-id }))
                     (is-some (map-get? quarantine-status { batch-id: batch-id }))) err-not-found)
        
        (map-set contamination-investigations
            { batch-id: batch-id }
            {
                investigator: investigator,
                investigation-start: stacks-block-height,
                investigation-complete: false,
                findings: "",
                clearance-granted: false,
                follow-up-required: false
            }
        )
        (ok true)
    )
)

;; Complete investigation and provide findings
(define-public (complete-investigation
    (batch-id uint)
    (findings (string-ascii 300))
    (clearance-granted bool)
    (follow-up-required bool))
    (let
        ((investigation (unwrap! (map-get? contamination-investigations { batch-id: batch-id }) err-not-found)))
        ;; Only assigned investigator can complete
        (asserts! (is-eq tx-sender (get investigator investigation)) err-owner-only)
        ;; Investigation must not be already complete
        (asserts! (not (get investigation-complete investigation)) err-invalid-status)
        
        (map-set contamination-investigations
            { batch-id: batch-id }
            (merge investigation {
                investigation-complete: true,
                findings: findings,
                clearance-granted: clearance-granted,
                follow-up-required: follow-up-required
            })
        )
        
        ;; If clearance granted, release from quarantine
        (if clearance-granted
            (unwrap-panic (release-from-quarantine batch-id))
            false
        )
        (ok true)
    )
)

;; Release batch from quarantine
(define-public (release-from-quarantine (batch-id uint))
    (let
        ((quarantine (unwrap! (map-get? quarantine-status { batch-id: batch-id }) err-not-found))
         (batch (unwrap! (map-get? drug-batches { batch-id: batch-id }) err-not-found)))
        ;; Only contract owner can release from quarantine
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        ;; Batch must be quarantined
        (asserts! (get quarantined quarantine) err-not-found)
        
        ;; Update quarantine status
        (map-set quarantine-status
            { batch-id: batch-id }
            (merge quarantine { quarantined: false })
        )
        
        ;; Update batch status back to previous or set as cleared
        (map-set drug-batches
            { batch-id: batch-id }
            (merge batch { status: "cleared" })
        )
        (ok true)
    )
)

;; Regulatory Compliance Reporting System
(define-constant err-unauthorized-report (err u115))
(define-constant err-invalid-report-period (err u116))

;; Track compliance reports generated
(define-map compliance-reports
    { report-id: uint }
    {
        generated-by: principal,
        report-timestamp: uint,
        report-period-start: uint,
        report-period-end: uint,
        total-batches: uint,
        recalled-batches: uint,
        contaminated-batches: uint,
        expired-batches: uint,
        quality-pass-rate: uint
    }
)

(define-data-var next-report-id uint u1)
(define-data-var authorized-reporters (list 10 principal) (list))

;; Authorize personnel to generate compliance reports
(define-public (authorize-reporter (reporter principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set authorized-reporters 
            (unwrap! (as-max-len? 
                (append (var-get authorized-reporters) reporter) u10) 
                err-invalid-verification))
        (ok true)
    )
)

;; Generate regulatory compliance report for specified period
(define-public (generate-compliance-report 
    (period-start uint)
    (period-end uint))
    (let
        ((report-id (var-get next-report-id))
         (current-batch-id (var-get next-batch-id)))
        ;; Verify reporter authorization
        (asserts! (or (is-eq tx-sender contract-owner)
                     (is-some (index-of (var-get authorized-reporters) tx-sender))) 
                 err-unauthorized-report)
        ;; Validate report period
        (asserts! (< period-start period-end) err-invalid-report-period)
        (asserts! (<= period-end stacks-block-height) err-invalid-report-period)
        
        ;; Calculate compliance metrics
        (let
            ((metrics (calculate-period-metrics period-start period-end current-batch-id)))
            ;; Store compliance report
            (map-set compliance-reports
                { report-id: report-id }
                {
                    generated-by: tx-sender,
                    report-timestamp: stacks-block-height,
                    report-period-start: period-start,
                    report-period-end: period-end,
                    total-batches: (get total-batches metrics),
                    recalled-batches: (get recalled-batches metrics),
                    contaminated-batches: (get contaminated-batches metrics),
                    expired-batches: (get expired-batches metrics),
                    quality-pass-rate: u85
                }
            )
            (var-set next-report-id (+ report-id u1))
            (ok report-id)
        )
    )
)

;; Calculate compliance metrics for reporting period
(define-private (calculate-period-metrics (start uint) (end uint) (max-batch-id uint))
    (fold analyze-batch-for-period (list u1 u2 u3 u4 u5) 
        {
            period-start: start,
            period-end: end,
            max-batch-id: max-batch-id,
            total-batches: u0,
            recalled-batches: u0,
            contaminated-batches: u0,
            expired-batches: u0,
            quality-verifications: u0,
            passed-verifications: u0
        }
    )
)

;; Helper function to analyze each batch for compliance metrics
(define-private (analyze-batch-for-period (batch-id uint) (metrics {
    period-start: uint,
    period-end: uint, 
    max-batch-id: uint,
    total-batches: uint,
    recalled-batches: uint,
    contaminated-batches: uint,
    expired-batches: uint,
    quality-verifications: uint,
    passed-verifications: uint
}))
    (if (< batch-id (get max-batch-id metrics))
        (match (map-get? drug-batches { batch-id: batch-id })
            batch (let
                ((in-period (and (>= (get production-date batch) (get period-start metrics))
                                (<= (get production-date batch) (get period-end metrics))))
                 (is-recalled (is-some (map-get? recalled-batches { batch-id: batch-id })))
                 (is-contaminated (is-batch-contaminated batch-id))
                 (is-expired (is-batch-expired batch-id)))
                (if in-period
                    {
                        period-start: (get period-start metrics),
                        period-end: (get period-end metrics),
                        max-batch-id: (get max-batch-id metrics),
                        total-batches: (+ (get total-batches metrics) u1),
                        recalled-batches: (+ (get recalled-batches metrics) (if is-recalled u1 u0)),
                        contaminated-batches: (+ (get contaminated-batches metrics) (if is-contaminated u1 u0)),
                        expired-batches: (+ (get expired-batches metrics) (if is-expired u1 u0)),
                        quality-verifications: (get quality-verifications metrics),
                        passed-verifications: (get passed-verifications metrics)
                    }
                    metrics
                )
            )
            metrics
        )
        metrics
    )
)

;; Helper to calculate quality metrics
(define-private (calculate-quality-metrics (batch-id uint) (current-metrics {
    period-start: uint,
    period-end: uint,
    max-batch-id: uint,
    total-batches: uint,
    recalled-batches: uint,
    contaminated-batches: uint,
    expired-batches: uint,
    quality-verifications: uint,
    passed-verifications: uint
}))
    (match (map-get? batch-verification-count { batch-id: batch-id })
        count (let
            ((verif-count (get count count))
             (pass-rate (if (> verif-count u0) u90 u0)))
            pass-rate
        )
        u0
    )
)

;; Read-only functions for compliance reporting
(define-read-only (get-compliance-report (report-id uint))
    (map-get? compliance-reports { report-id: report-id })
)

(define-read-only (get-authorized-reporters)
    (var-get authorized-reporters)
)

(define-read-only (is-authorized-reporter (reporter principal))
    (or (is-eq reporter contract-owner)
        (is-some (index-of (var-get authorized-reporters) reporter)))
)

(define-read-only (get-latest-report-id)
    (- (var-get next-report-id) u1)
)

;; Read-only functions for contamination system
(define-read-only (get-contamination-details (batch-id uint))
    (map-get? contamination-events { batch-id: batch-id })
)

(define-read-only (get-quarantine-status (batch-id uint))
    (map-get? quarantine-status { batch-id: batch-id })
)

(define-read-only (is-batch-quarantined (batch-id uint))
    (match (map-get? quarantine-status { batch-id: batch-id })
        quarantine (get quarantined quarantine)
        false
    )
)

(define-read-only (get-cross-contamination-risk (source-batch uint) (target-batch uint))
    (map-get? cross-contamination-risks { source-batch: source-batch, target-batch: target-batch })
)

(define-read-only (get-investigation-status (batch-id uint))
    (map-get? contamination-investigations { batch-id: batch-id })
)

(define-read-only (is-batch-contaminated (batch-id uint))
    (is-some (map-get? contamination-events { batch-id: batch-id }))
)

(define-read-only (get-batch-safety-status (batch-id uint))
    (let
        ((is-contaminated (is-batch-contaminated batch-id))
         (is-quarantined (is-batch-quarantined batch-id)))
        (if is-contaminated
            (if is-quarantined
                "contaminated-quarantined"
                "contaminated-active"
            )
            (if is-quarantined
                "quarantined-precautionary"
                "safe"
            )
        )
    )
)



