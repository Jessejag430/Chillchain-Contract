;; Identity Privacy Score System
;; Calculates privacy risk scores based on identity exposure patterns and attribute sharing
;; Unique feature: Quantifies and manages privacy risk through smart analytics

(define-constant contract-owner tx-sender)

;; Error constants
(define-constant err-not-authorized (err u600))
(define-constant err-invalid-score (err u601))
(define-constant err-not-found (err u602))
(define-constant err-already-exists (err u603))
(define-constant err-insufficient-privacy (err u604))
(define-constant err-invalid-threshold (err u605))

;; Privacy scoring constants
(define-constant max-privacy-score u1000)
(define-constant base-privacy-score u800)
(define-constant sharing-penalty u50)
(define-constant verification-penalty u30)
(define-constant marketplace-penalty u40)
(define-constant cross-reference-penalty u25)
(define-constant privacy-decay-rate u5)
(define-constant privacy-recovery-rate u10)

;; Privacy risk levels
(define-constant risk-level-minimal u0)
(define-constant risk-level-low u1)
(define-constant risk-level-medium u2)
(define-constant risk-level-high u3)
(define-constant risk-level-critical u4)

;; Data variables
(define-data-var total-assessments uint u0)
(define-data-var global-average-score uint base-privacy-score)

;; Core privacy scoring maps
(define-map privacy-scores
  principal
  {
    current-score: uint,
    risk-level: uint,
    last-updated: uint,
    assessment-count: uint,
    trend-direction: int
  }
)

;; Track identity exposure events
(define-map exposure-events
  { user: principal, event-id: uint }
  {
    event-type: (string-ascii 32),
    severity: uint,
    timestamp: uint,
    data-shared: (string-ascii 128),
    recovery-time: uint
  }
)

;; Privacy improvement actions tracking
(define-map privacy-actions
  { user: principal, action-type: (string-ascii 32) }
  {
    action-count: uint,
    last-performed: uint,
    effectiveness: uint
  }
)

;; Attribute visibility settings
(define-map attribute-visibility
  { user: principal, attribute: (string-ascii 64) }
  {
    visibility-level: uint,
    share-count: uint,
    last-accessed: uint
  }
)

;; Privacy alerts for users
(define-map privacy-alerts
  { user: principal, alert-id: uint }
  {
    alert-type: (string-ascii 48),
    severity: uint,
    message: (string-ascii 256),
    triggered-at: uint,
    resolved: bool
  }
)

;; Calculate initial privacy score for a user
(define-public (initialize-privacy-score)
  (let 
    (
      (user tx-sender)
      (initial-score {
        current-score: base-privacy-score,
        risk-level: risk-level-low,
        last-updated: stacks-block-height,
        assessment-count: u0,
        trend-direction: 0
      })
    )
    (asserts! (is-none (map-get? privacy-scores user)) err-already-exists)
    (map-set privacy-scores user initial-score)
    (ok base-privacy-score)
  )
)

;; Record an identity exposure event
(define-public (record-exposure-event 
  (event-type (string-ascii 32))
  (severity uint)
  (data-shared (string-ascii 128)))
  (let 
    (
      (user tx-sender)
      (event-id (+ (var-get total-assessments) u1))
      (current-score-data (unwrap! (map-get? privacy-scores user) err-not-found))
      (penalty-amount (* severity sharing-penalty))
      (new-score (if (> (get current-score current-score-data) penalty-amount)
        (- (get current-score current-score-data) penalty-amount)
        u0))
      (new-risk-level (calculate-risk-level new-score))
    )
    (asserts! (<= severity u5) err-invalid-score)
    
    ;; Record the exposure event
    (map-set exposure-events
      { user: user, event-id: event-id }
      {
        event-type: event-type,
        severity: severity,
        timestamp: stacks-block-height,
        data-shared: data-shared,
        recovery-time: (+ stacks-block-height (* severity u100))
      }
    )
    
    ;; Update privacy score
    (map-set privacy-scores user
      {
        current-score: new-score,
        risk-level: new-risk-level,
        last-updated: stacks-block-height,
        assessment-count: (+ (get assessment-count current-score-data) u1),
        trend-direction: -1
      }
    )
    
    ;; Generate privacy alert if score drops significantly
    (if (>= penalty-amount u100)
      (try! (generate-privacy-alert user "high-exposure" u3 "Significant privacy exposure detected"))
      true
    )
    
    (var-set total-assessments event-id)
    (ok new-score)
  )
)

;; Perform privacy-enhancing action
(define-public (perform-privacy-action (action-type (string-ascii 32)))
  (let 
    (
      (user tx-sender)
      (current-score-data (unwrap! (map-get? privacy-scores user) err-not-found))
      (action-key { user: user, action-type: action-type })
      (existing-action (default-to 
        { action-count: u0, last-performed: u0, effectiveness: u100 }
        (map-get? privacy-actions action-key)))
      (recovery-amount (calculate-recovery-amount action-type (get action-count existing-action)))
      (new-score (min (+ (get current-score current-score-data) recovery-amount) max-privacy-score))
      (new-risk-level (calculate-risk-level new-score))
    )
    
    ;; Update action tracking
    (map-set privacy-actions action-key
      {
        action-count: (+ (get action-count existing-action) u1),
        last-performed: stacks-block-height,
        effectiveness: (get effectiveness existing-action)
      }
    )
    
    ;; Update privacy score
    (map-set privacy-scores user
      {
        current-score: new-score,
        risk-level: new-risk-level,
        last-updated: stacks-block-height,
        assessment-count: (+ (get assessment-count current-score-data) u1),
        trend-direction: 1
      }
    )
    
    (ok new-score)
  )
)

;; Set attribute visibility preference
(define-public (set-attribute-visibility 
  (attribute (string-ascii 64))
  (visibility-level uint))
  (let 
    (
      (user tx-sender)
      (visibility-key { user: user, attribute: attribute })
    )
    (asserts! (<= visibility-level u4) err-invalid-threshold)
    
    (map-set attribute-visibility visibility-key
      {
        visibility-level: visibility-level,
        share-count: u0,
        last-accessed: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Generate privacy improvement recommendations
(define-public (get-privacy-recommendations)
  (let 
    (
      (user tx-sender)
      (score-data (unwrap! (map-get? privacy-scores user) err-not-found))
      (current-score (get current-score score-data))
      (risk-level (get risk-level score-data))
    )
    
    (if (>= risk-level risk-level-high)
      (ok (list "reduce-sharing" "enable-privacy-mode" "audit-permissions"))
      (if (>= risk-level risk-level-medium)
        (ok (list "review-visibility" "strengthen-encryption"))
        (ok (list "maintain-practices"))))
  )
)

;; Calculate privacy score with weighted factors
(define-public (calculate-comprehensive-score)
  (let 
    (
      (user tx-sender)
      (base-score base-privacy-score)
      ;; Factor in sharing frequency
      (sharing-factor (calculate-sharing-impact user))
      ;; Factor in attribute visibility
      (visibility-factor (calculate-visibility-impact user))
      ;; Factor in recovery actions
      (recovery-factor (calculate-recovery-impact user))
      ;; Calculate final score
      (final-score (min (+ (- base-score sharing-factor visibility-factor) recovery-factor) max-privacy-score))
    )
    
    (ok final-score)
  )
)

;; Private helper functions

(define-private (calculate-risk-level (score uint))
  (if (< score u200) risk-level-critical
    (if (< score u400) risk-level-high
      (if (< score u600) risk-level-medium
        (if (< score u800) risk-level-low
          risk-level-minimal))))
)

(define-private (calculate-recovery-amount (action-type (string-ascii 32)) (action-count uint))
  (let 
    (
      (base-recovery (if (is-eq action-type "enable-encryption") u40
        (if (is-eq action-type "reduce-sharing") u30
          (if (is-eq action-type "audit-permissions") u25
            (if (is-eq action-type "privacy-review") u20 u10)))))
      ;; Diminishing returns for repeated actions
      (diminished-recovery (/ base-recovery (+ u1 (/ action-count u3))))
    )
    diminished-recovery
  )
)

(define-private (calculate-sharing-impact (user principal))
  (let ((base-penalty u0))
    ;; Simplified calculation - would integrate with actual sharing data
    base-penalty
  )
)

(define-private (calculate-visibility-impact (user principal))
  (let ((base-penalty u0))
    ;; Simplified calculation - would check attribute visibility settings
    base-penalty
  )
)

(define-private (calculate-recovery-impact (user principal))
  (let ((base-recovery u0))
    ;; Simplified calculation - would check privacy actions performed
    base-recovery
  )
)

(define-private (generate-privacy-alert 
  (user principal) 
  (alert-type (string-ascii 48)) 
  (severity uint) 
  (message (string-ascii 256)))
  (let ((alert-id (+ (get assessment-count (unwrap! (map-get? privacy-scores user) err-not-found)) u1)))
    (map-set privacy-alerts
      { user: user, alert-id: alert-id }
      {
        alert-type: alert-type,
        severity: severity,
        message: message,
        triggered-at: stacks-block-height,
        resolved: false
      }
    )
    (ok alert-id)
  )
)

(define-private (min (a uint) (b uint))
  (if (< a b) a b)
)

;; Read-only functions

(define-read-only (get-privacy-score (user principal))
  (map-get? privacy-scores user)
)

(define-read-only (get-privacy-risk-level (user principal))
  (match (map-get? privacy-scores user)
    score-data (get risk-level score-data)
    risk-level-critical
  )
)

(define-read-only (get-exposure-events (user principal) (limit uint))
  ;; Return recent exposure events (simplified)
  (map-get? exposure-events { user: user, event-id: u1 })
)

(define-read-only (get-privacy-actions (user principal) (action-type (string-ascii 32)))
  (map-get? privacy-actions { user: user, action-type: action-type })
)

(define-read-only (get-attribute-visibility (user principal) (attribute (string-ascii 64)))
  (map-get? attribute-visibility { user: user, attribute: attribute })
)

(define-read-only (get-privacy-alerts (user principal) (alert-id uint))
  (map-get? privacy-alerts { user: user, alert-id: alert-id })
)

(define-read-only (get-global-privacy-stats)
  {
    average-score: (var-get global-average-score),
    total-assessments: (var-get total-assessments),
    max-score: max-privacy-score
  }
)

(define-read-only (is-privacy-compliant (user principal) (threshold uint))
  (match (map-get? privacy-scores user)
    score-data (>= (get current-score score-data) threshold)
    false
  )
)
