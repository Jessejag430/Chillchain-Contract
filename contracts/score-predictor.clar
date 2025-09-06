;; Credit Score Impact Predictor
;; Allows users to simulate financial actions and predict impact

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-invalid-scenario (err u201))
(define-constant err-scenario-not-found (err u202))

;; Scenario impact constants
(define-constant max-scenarios-per-user u10)
(define-constant prediction-validity-period u144)

;; Data variables
(define-data-var total-predictions uint u0)

;; Financial action impacts
(define-map action-impacts
  { action-type: (string-ascii 30) }
  {
    base-impact: int,
    weight-factor: uint,
    description: (string-ascii 100)
  }
)

;; User prediction scenarios
(define-map prediction-scenarios
  { user: principal, scenario-id: uint }
  {
    scenario-name: (string-ascii 50),
    actions: (list 5 (string-ascii 30)),
    amounts: (list 5 uint),
    predicted-impact: int,
    predicted-new-score: uint,
    confidence-level: uint,
    created-at: uint
  }
)

;; User scenario tracking
(define-map user-scenario-counts
  { user: principal }
  { active-scenarios: uint, total-scenarios: uint }
)

;; Read-only functions
(define-read-only (get-prediction-scenario (user principal) (scenario-id uint))
  (match (map-get? prediction-scenarios { user: user, scenario-id: scenario-id })
    scenario (ok scenario)
    (err err-scenario-not-found)
  )
)

(define-read-only (get-user-scenario-count (user principal))
  (default-to { active-scenarios: u0, total-scenarios: u0 }
    (map-get? user-scenario-counts { user: user })
  )
)

;; Setup action impacts (owner only)
(define-public (setup-action-impact
  (action-type (string-ascii 30))
  (base-impact int)
  (weight-factor uint)
  (description (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-owner-only))
    (asserts! (and (>= base-impact (- 100)) (<= base-impact 100)) (err err-invalid-scenario))
    (asserts! (and (>= weight-factor u1) (<= weight-factor u10)) (err err-invalid-scenario))

    (map-set action-impacts
      { action-type: action-type }
      {
        base-impact: base-impact,
        weight-factor: weight-factor,
        description: description
      }
    )
    (ok true)
  )
)

;; Create prediction scenario
(define-public (create-prediction-scenario
  (scenario-name (string-ascii 50))
  (actions (list 5 (string-ascii 30)))
  (amounts (list 5 uint)))
  (let (
    (current-score (unwrap! (contract-call? .credit-scoring get-credit-score tx-sender) (err err-scenario-not-found)))
    (user-counts (get-user-scenario-count tx-sender))
    (scenario-id (+ (get active-scenarios user-counts) u1))
    (predicted-impact (calculate-scenario-impact actions amounts current-score))
    (new-predicted-score (+ current-score (to-uint (if (>= predicted-impact 0) predicted-impact (- predicted-impact)))))
  )
    (asserts! (< (get active-scenarios user-counts) max-scenarios-per-user) (err err-invalid-scenario))
    (asserts! (<= (len actions) u5) (err err-invalid-scenario))

    (map-set prediction-scenarios
      { user: tx-sender, scenario-id: scenario-id }
      {
        scenario-name: scenario-name,
        actions: actions,
        amounts: amounts,
        predicted-impact: predicted-impact,
        predicted-new-score: new-predicted-score,
        confidence-level: (calculate-confidence actions current-score),
        created-at: stacks-block-height
      }
    )

    (map-set user-scenario-counts
      { user: tx-sender }
      {
        active-scenarios: (+ (get active-scenarios user-counts) u1),
        total-scenarios: (+ (get total-scenarios user-counts) u1)
      }
    )

    (var-set total-predictions (+ (var-get total-predictions) u1))
    (ok { scenario-id: scenario-id, predicted-impact: predicted-impact, predicted-score: new-predicted-score })
  )
)

;; Calculate scenario impact
(define-private (calculate-scenario-impact
  (actions (list 5 (string-ascii 30)))
  (amounts (list 5 uint))
  (current-score uint))
  (fold calculate-single-impact (zip actions amounts) 0)
)

;; Calculate single action impact
(define-private (calculate-single-impact
  (action-amount { action: (string-ascii 30), amount: uint })
  (current-total int))
  (let (
    (action-data (map-get? action-impacts { action-type: (get action action-amount) }))
    (amount (get amount action-amount))
  )
    (match action-data
      impact-info
      (let (
        (base-impact (get base-impact impact-info))
        (weight (get weight-factor impact-info))
        (adjusted-impact (* base-impact (/ (+ amount u1) weight)))
      )
        (+ current-total adjusted-impact)
      )
      current-total
    )
  )
)

;; Calculate confidence level
(define-private (calculate-confidence (actions (list 5 (string-ascii 30))) (current-score uint))
  (let (
    (action-count (len actions))
    (score-stability (if (and (>= current-score u600) (<= current-score u750)) u20 u10))
    (base-confidence u70)
  )
    (+ base-confidence score-stability (- u10 (* action-count u2)))
  )
)

;; Get prediction summary
(define-read-only (get-prediction-summary (user principal))
  (let (
    (scenario-counts (get-user-scenario-count user))
  )
    (ok {
      active-scenarios: (get active-scenarios scenario-counts),
      total-scenarios: (get total-scenarios scenario-counts)
    })
  )
)
