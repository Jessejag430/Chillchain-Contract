;; Credential Lifecycle Management System
;; Manages credential expiry, renewals, and validity periods

(define-constant ERR-NOT-AUTHORIZED (err u500))
(define-constant ERR-CREDENTIAL-NOT-FOUND (err u501))
(define-constant ERR-RENEWAL-NOT-AVAILABLE (err u502))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u503))
(define-constant ERR-RENEWAL-EXPIRED (err u504))
(define-constant ERR-INVALID-RENEWAL-POLICY (err u505))
(define-constant ERR-CREDENTIAL-SUSPENDED (err u506))

;; Renewal policy constants
(define-constant EARLY-RENEWAL-PERIOD u2160) ;; 15 days before expiry
(define-constant GRACE-PERIOD u1440) ;; 10 days after expiry
(define-constant DEFAULT-RENEWAL-FEE u100) ;; Base renewal fee
(define-constant LATE-RENEWAL-PENALTY u50) ;; Additional fee for late renewal

(define-data-var contract-owner principal tx-sender)
(define-data-var renewal-counter uint u0)
(define-data-var system-renewal-fee uint DEFAULT-RENEWAL-FEE)

;; Track credential lifecycle information
(define-map credential-lifecycle
    {student: principal, credential-id: uint}
    {
        issue-date: uint,
        expiry-date: uint,
        last-renewal-date: uint,
        renewal-count: uint,
        renewal-policy: (string-ascii 20), ;; "manual", "automatic", "conditional"
        grace-period-used: bool,
        status: (string-ascii 15), ;; "active", "expired", "suspended", "revoked"
        auto-renewal-enabled: bool
    }
)

;; University renewal policies
(define-map university-renewal-policies
    {university: principal, credential-type: (string-ascii 50)}
    {
        validity-period: uint, ;; blocks until expiry
        renewal-fee: uint,
        max-renewals: uint,
        early-renewal-allowed: bool,
        grace-period-duration: uint,
        auto-renewal-default: bool,
        renewal-requirements: (string-ascii 200)
    }
)

;; Renewal requests and processing
(define-map renewal-requests
    {renewal-id: uint}
    {
        student: principal,
        credential-id: uint,
        university: principal,
        request-date: uint,
        payment-amount: uint,
        renewal-type: (string-ascii 15), ;; "regular", "early", "late"
        status: (string-ascii 15), ;; "pending", "approved", "rejected"
        processed-by: (optional principal),
        processing-date: (optional uint)
    }
)

;; Automatic renewal subscriptions
(define-map auto-renewal-subscriptions
    {student: principal, credential-id: uint}
    {
        enabled: bool,
        payment-source: (string-ascii 20), ;; "balance", "external"
        max-renewal-fee: uint,
        notification-preference: (string-ascii 15), ;; "email", "blockchain"
        subscription-date: uint,
        last-renewal-attempt: uint
    }
)

;; Renewal notifications queue
(define-map renewal-notifications
    {notification-id: uint}
    {
        student: principal,
        credential-id: uint,
        notification-type: (string-ascii 20), ;; "expiry-warning", "renewal-due", "expired"
        scheduled-date: uint,
        sent: bool,
        sent-date: (optional uint)
    }
)

(define-data-var notification-counter uint u0)

;; Set university renewal policy for credential types
(define-public (set-renewal-policy 
    (credential-type (string-ascii 50))
    (validity-period uint)
    (renewal-fee uint)
    (max-renewals uint)
    (early-renewal-allowed bool)
    (grace-period uint)
    (auto-renewal-default bool)
    (requirements (string-ascii 200))
)
    (let ((university tx-sender))
        ;; Basic validation
        (asserts! (> validity-period u0) ERR-INVALID-RENEWAL-POLICY)
        (asserts! (<= max-renewals u50) ERR-INVALID-RENEWAL-POLICY)
        
        (map-set university-renewal-policies
            {university: university, credential-type: credential-type}
            {
                validity-period: validity-period,
                renewal-fee: renewal-fee,
                max-renewals: max-renewals,
                early-renewal-allowed: early-renewal-allowed,
                grace-period-duration: grace-period,
                auto-renewal-default: auto-renewal-default,
                renewal-requirements: requirements
            }
        )
        (ok true)
    )
)

;; Initialize credential lifecycle when credential is issued
(define-public (initialize-credential-lifecycle 
    (student principal)
    (credential-id uint)
    (university principal)
    (credential-type (string-ascii 50))
)
    (let (
        (policy (default-to
            {
                validity-period: u52560, ;; 1 year default
                renewal-fee: DEFAULT-RENEWAL-FEE,
                max-renewals: u10,
                early-renewal-allowed: true,
                grace-period-duration: GRACE-PERIOD,
                auto-renewal-default: false,
                renewal-requirements: "Standard renewal process"
            }
            (map-get? university-renewal-policies {university: university, credential-type: credential-type})
        ))
        (expiry-date (+ stacks-block-height (get validity-period policy)))
    )
        (map-set credential-lifecycle
            {student: student, credential-id: credential-id}
            {
                issue-date: stacks-block-height,
                expiry-date: expiry-date,
                last-renewal-date: u0,
                renewal-count: u0,
                renewal-policy: (if (get auto-renewal-default policy) "automatic" "manual"),
                grace-period-used: false,
                status: "active",
                auto-renewal-enabled: (get auto-renewal-default policy)
            }
        )
        
        ;; Schedule expiry warning notification
        (let ((notification-id (+ (var-get notification-counter) u1)))
            (var-set notification-counter notification-id)
            (map-set renewal-notifications
                {notification-id: notification-id}
                {
                    student: student,
                    credential-id: credential-id,
                    notification-type: "expiry-warning",
                    scheduled-date: (- expiry-date EARLY-RENEWAL-PERIOD),
                    sent: false,
                    sent-date: none
                }
            )
        )
        (ok expiry-date)
    )
)

;; Request credential renewal
(define-public (request-renewal (credential-id uint))
    (let (
        (student tx-sender)
        (lifecycle-data (unwrap! (map-get? credential-lifecycle {student: student, credential-id: credential-id}) ERR-CREDENTIAL-NOT-FOUND))
        (renewal-id (+ (var-get renewal-counter) u1))
        (current-block stacks-block-height)
        (expiry-date (get expiry-date lifecycle-data))
        (is-late (> current-block expiry-date))
        (is-early (< current-block (- expiry-date EARLY-RENEWAL-PERIOD)))
    )
        ;; Check if renewal is allowed
        (asserts! (not (is-eq (get status lifecycle-data) "suspended")) ERR-CREDENTIAL-SUSPENDED)
        (asserts! (not (is-eq (get status lifecycle-data) "revoked")) ERR-CREDENTIAL-NOT-FOUND)
        
        (let (
            (renewal-type (if is-late "late" (if is-early "early" "regular")))
            (base-fee (var-get system-renewal-fee))
            (total-fee (if is-late (+ base-fee LATE-RENEWAL-PENALTY) base-fee))
        )
            ;; Check if within grace period for late renewals
            (if is-late
                (asserts! (<= current-block (+ expiry-date GRACE-PERIOD)) ERR-RENEWAL-EXPIRED)
                true
            )
            
            (var-set renewal-counter renewal-id)
            
            ;; Transfer renewal fee
            (try! (stx-transfer? total-fee student (var-get contract-owner)))
            
            ;; Create renewal request
            (map-set renewal-requests
                {renewal-id: renewal-id}
                {
                    student: student,
                    credential-id: credential-id,
                    university: (var-get contract-owner), ;; Simplified - would get from credential data
                    request-date: current-block,
                    payment-amount: total-fee,
                    renewal-type: renewal-type,
                    status: "pending",
                    processed-by: none,
                    processing-date: none
                }
            )
            (ok renewal-id)
        )
    )
)

;; Process renewal request (university/admin function)
(define-public (process-renewal-request (renewal-id uint) (approved bool))
    (let (
        (request-data (unwrap! (map-get? renewal-requests {renewal-id: renewal-id}) ERR-CREDENTIAL-NOT-FOUND))
        (student (get student request-data))
        (credential-id (get credential-id request-data))
        (lifecycle-data (unwrap! (map-get? credential-lifecycle {student: student, credential-id: credential-id}) ERR-CREDENTIAL-NOT-FOUND))
    )
        ;; Update request status
        (map-set renewal-requests
            {renewal-id: renewal-id}
            (merge request-data {
                status: (if approved "approved" "rejected"),
                processed-by: (some tx-sender),
                processing-date: (some stacks-block-height)
            })
        )
        
        ;; If approved, extend credential validity
        (if approved
            (let (
                (new-expiry-date (+ stacks-block-height u52560)) ;; Extend by 1 year
                (new-renewal-count (+ (get renewal-count lifecycle-data) u1))
            )
                (map-set credential-lifecycle
                    {student: student, credential-id: credential-id}
                    (merge lifecycle-data {
                        expiry-date: new-expiry-date,
                        last-renewal-date: stacks-block-height,
                        renewal-count: new-renewal-count,
                        status: "active"
                    })
                )
                (ok new-expiry-date)
            )
            (ok u0)
        )
    )
)

;; Enable/disable auto-renewal for a credential
(define-public (set-auto-renewal 
    (credential-id uint)
    (enabled bool)
    (max-fee uint)
    (notification-preference (string-ascii 15))
)
    (let ((student tx-sender))
        (map-set auto-renewal-subscriptions
            {student: student, credential-id: credential-id}
            {
                enabled: enabled,
                payment-source: "balance",
                max-renewal-fee: max-fee,
                notification-preference: notification-preference,
                subscription-date: stacks-block-height,
                last-renewal-attempt: u0
            }
        )
        (ok enabled)
    )
)

;; Check credential expiry status
(define-public (check-credential-expiry (student principal) (credential-id uint))
    (let (
        (lifecycle-data (unwrap! (map-get? credential-lifecycle {student: student, credential-id: credential-id}) ERR-CREDENTIAL-NOT-FOUND))
        (current-block stacks-block-height)
        (expiry-date (get expiry-date lifecycle-data))
        (is-expired (> current-block expiry-date))
        (is-expiring-soon (and (not is-expired) (<= (- expiry-date current-block) EARLY-RENEWAL-PERIOD)))
    )
        ;; Update status if expired
        (if is-expired
            (map-set credential-lifecycle
                {student: student, credential-id: credential-id}
                (merge lifecycle-data {status: "expired"})
            )
            true
        )
        
        (ok {
            status: (get status lifecycle-data),
            expiry-date: expiry-date,
            is-expired: is-expired,
            is-expiring-soon: is-expiring-soon,
            days-until-expiry: (if is-expired u0 (/ (- expiry-date current-block) u144))
        })
    )
)

;; Read-only functions
(define-read-only (get-credential-lifecycle (student principal) (credential-id uint))
    (ok (map-get? credential-lifecycle {student: student, credential-id: credential-id}))
)

(define-read-only (get-renewal-policy (university principal) (credential-type (string-ascii 50)))
    (ok (map-get? university-renewal-policies {university: university, credential-type: credential-type}))
)

(define-read-only (get-renewal-request (renewal-id uint))
    (ok (map-get? renewal-requests {renewal-id: renewal-id}))
)

(define-read-only (get-auto-renewal-settings (student principal) (credential-id uint))
    (ok (map-get? auto-renewal-subscriptions {student: student, credential-id: credential-id}))
)

(define-read-only (calculate-renewal-fee (student principal) (credential-id uint))
    (match (map-get? credential-lifecycle {student: student, credential-id: credential-id})
        lifecycle-data 
            (let (
                (current-block stacks-block-height)
                (expiry-date (get expiry-date lifecycle-data))
                (is-late (> current-block expiry-date))
                (base-fee (var-get system-renewal-fee))
            )
                (ok (if is-late (+ base-fee LATE-RENEWAL-PENALTY) base-fee))
            )
        ERR-CREDENTIAL-NOT-FOUND
    )
)

;; Admin functions
(define-public (set-system-renewal-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set system-renewal-fee new-fee)
        (ok true)
    )
)

(define-public (suspend-credential (student principal) (credential-id uint))
    (let (
        (lifecycle-data (unwrap! (map-get? credential-lifecycle {student: student, credential-id: credential-id}) ERR-CREDENTIAL-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (map-set credential-lifecycle
            {student: student, credential-id: credential-id}
            (merge lifecycle-data {status: "suspended"})
        )
        (ok true)
    )
)
