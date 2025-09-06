;; Patient Consent Audit Trail System
;; Tracks all consent decisions and access attempts for regulatory compliance

(define-constant err-not-authorized (err u400))
(define-constant err-not-found (err u401))
(define-constant err-consent-expired (err u402))
(define-constant err-consent-withdrawn (err u403))
(define-constant err-invalid-purpose (err u404))

;; Consent purposes for data access
(define-constant purpose-treatment u1)
(define-constant purpose-research u2)
(define-constant purpose-insurance u3)
(define-constant purpose-marketing u4)
(define-constant purpose-emergency u5)

;; Consent records with detailed information
(define-map consent-records
    {patient: principal, record-id: uint, accessor: principal, purpose: uint}
    {
        consent-given: bool,
        granted-at: uint,
        expires-at: (optional uint),
        specific-permissions: (string-ascii 100),
        data-categories: (string-ascii 200),
        withdrawal-allowed: bool,
        audit-trail-id: uint
    }
)

;; Detailed audit trail for every consent action
(define-map consent-audit-trail
    uint
    {
        patient: principal,
        record-id: uint,
        accessor: principal,
        action-type: (string-ascii 20),
        purpose: uint,
        consent-decision: bool,
        timestamp: uint,
        ip-hash: (optional (string-ascii 64)),
        user-agent-hash: (optional (string-ascii 64)),
        reason: (optional (string-ascii 200))
    }
)

;; Access attempt logs for compliance monitoring
(define-map access-attempts
    uint
    {
        patient: principal,
        record-id: uint,
        accessor: principal,
        purpose: uint,
        access-granted: bool,
        attempt-timestamp: uint,
        consent-valid: bool,
        access-duration: (optional uint),
        data-accessed: (optional (string-ascii 100))
    }
)

;; Consent expiration notifications
(define-map consent-notifications
    {patient: principal, notification-id: uint}
    {
        record-id: uint,
        accessor: principal,
        purpose: uint,
        notification-type: (string-ascii 30),
        expiration-date: uint,
        reminder-sent: bool,
        acknowledged: bool,
        created-at: uint
    }
)

(define-data-var audit-trail-nonce uint u0)
(define-data-var access-attempt-nonce uint u0)
(define-data-var notification-nonce uint u0)

;; Grant consent with detailed tracking
(define-public (grant-consent
    (record-id uint)
    (accessor principal)
    (purpose uint)
    (expires-at (optional uint))
    (specific-permissions (string-ascii 100))
    (data-categories (string-ascii 200))
    (ip-hash (optional (string-ascii 64)))
    (user-agent-hash (optional (string-ascii 64)))
)
    (let ((audit-id (+ (var-get audit-trail-nonce) u1)))
        ;; Validate purpose
        (asserts! (and (>= purpose u1) (<= purpose u5)) err-invalid-purpose)
        
        ;; Store consent record
        (map-set consent-records
            {patient: tx-sender, record-id: record-id, accessor: accessor, purpose: purpose}
            {
                consent-given: true,
                granted-at: stacks-block-height,
                expires-at: expires-at,
                specific-permissions: specific-permissions,
                data-categories: data-categories,
                withdrawal-allowed: true,
                audit-trail-id: audit-id
            }
        )
        
        ;; Create audit trail entry
        (map-set consent-audit-trail
            audit-id
            {
                patient: tx-sender,
                record-id: record-id,
                accessor: accessor,
                action-type: "consent-granted",
                purpose: purpose,
                consent-decision: true,
                timestamp: stacks-block-height,
                ip-hash: ip-hash,
                user-agent-hash: user-agent-hash,
                reason: none
            }
        )
        
        ;; Create expiration notification if applicable
        (match expires-at
            expiry (let ((notif-id (+ (var-get notification-nonce) u1)))
                (map-set consent-notifications
                    {patient: tx-sender, notification-id: notif-id}
                    {
                        record-id: record-id,
                        accessor: accessor,
                        purpose: purpose,
                        notification-type: "expiration-reminder",
                        expiration-date: expiry,
                        reminder-sent: false,
                        acknowledged: false,
                        created-at: stacks-block-height
                    }
                )
                (var-set notification-nonce notif-id)
            )
            u1
        )
        
        (var-set audit-trail-nonce audit-id)
        (ok audit-id)
    )
)

;; Withdraw consent with reason tracking
(define-public (withdraw-consent
    (record-id uint)
    (accessor principal)
    (purpose uint)
    (reason (string-ascii 200))
    (ip-hash (optional (string-ascii 64)))
)
    (let (
        (consent-key {patient: tx-sender, record-id: record-id, accessor: accessor, purpose: purpose})
        (existing-consent (unwrap! (map-get? consent-records consent-key) err-not-found))
        (audit-id (+ (var-get audit-trail-nonce) u1))
    )
        ;; Verify consent exists and withdrawal is allowed
        (asserts! (get consent-given existing-consent) err-not-found)
        (asserts! (get withdrawal-allowed existing-consent) err-not-authorized)
        
        ;; Update consent record
        (map-set consent-records
            consent-key
            (merge existing-consent {
                consent-given: false,
                withdrawal-allowed: false
            })
        )
        
        ;; Create audit trail entry
        (map-set consent-audit-trail
            audit-id
            {
                patient: tx-sender,
                record-id: record-id,
                accessor: accessor,
                action-type: "consent-withdrawn",
                purpose: purpose,
                consent-decision: false,
                timestamp: stacks-block-height,
                ip-hash: ip-hash,
                user-agent-hash: none,
                reason: (some reason)
            }
        )
        
        (var-set audit-trail-nonce audit-id)
        (ok audit-id)
    )
)

;; Log access attempt for monitoring
(define-public (log-access-attempt
    (record-id uint)
    (accessor principal)
    (purpose uint)
    (access-granted bool)
    (access-duration (optional uint))
    (data-accessed (optional (string-ascii 100)))
)
    (let (
        (attempt-id (+ (var-get access-attempt-nonce) u1))
        (consent-key {patient: tx-sender, record-id: record-id, accessor: accessor, purpose: purpose})
        (consent-valid (is-consent-valid record-id accessor purpose tx-sender))
    )
        (map-set access-attempts
            attempt-id
            {
                patient: tx-sender,
                record-id: record-id,
                accessor: accessor,
                purpose: purpose,
                access-granted: access-granted,
                attempt-timestamp: stacks-block-height,
                consent-valid: consent-valid,
                access-duration: access-duration,
                data-accessed: data-accessed
            }
        )
        
        (var-set access-attempt-nonce attempt-id)
        (ok attempt-id)
    )
)

;; Check if consent is valid and not expired
(define-read-only (is-consent-valid (record-id uint) (accessor principal) (purpose uint) (patient principal))
    (match (map-get? consent-records {patient: patient, record-id: record-id, accessor: accessor, purpose: purpose})
        consent (and
            (get consent-given consent)
            (match (get expires-at consent)
                expiry (< stacks-block-height expiry)
                true
            )
        )
        false
    )
)

;; Get consent details
(define-read-only (get-consent-details (patient principal) (record-id uint) (accessor principal) (purpose uint))
    (map-get? consent-records {patient: patient, record-id: record-id, accessor: accessor, purpose: purpose})
)

;; Get audit trail entry
(define-read-only (get-audit-trail-entry (audit-id uint))
    (map-get? consent-audit-trail audit-id)
)

;; Get patient's consent summary for a record
(define-read-only (get-patient-consent-summary (patient principal) (record-id uint))
    ;; Returns basic info - full implementation would aggregate all consents
    (ok {
        total-consents: u1,
        active-consents: u1,
        withdrawn-consents: u0,
        last-activity: stacks-block-height
    })
)

;; Acknowledge notification
(define-public (acknowledge-notification (notification-id uint))
    (let (
        (notif-key {patient: tx-sender, notification-id: notification-id})
        (notification (unwrap! (map-get? consent-notifications notif-key) err-not-found))
    )
        (map-set consent-notifications
            notif-key
            (merge notification {acknowledged: true})
        )
        (ok true)
    )
)

;; Get compliance report data
(define-read-only (get-compliance-report (patient principal) (start-block uint) (end-block uint))
    (ok {
        patient: patient,
        report-period-start: start-block,
        report-period-end: end-block,
        total-consent-actions: u0,
        access-attempts: u0,
        compliance-score: u100
    })
)
