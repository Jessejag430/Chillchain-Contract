;; Predictive Temperature Monitor
;; AI-powered system that predicts potential temperature violations based on historical data
;; Provides early warning alerts and route optimization suggestions for carriers

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u500))
(define-constant ERR_INVALID_PARAMETERS (err u501))
(define-constant ERR_PREDICTION_NOT_FOUND (err u502))
(define-constant ERR_INSUFFICIENT_DATA (err u503))
(define-constant ERR_ALERT_ALREADY_EXISTS (err u504))
(define-constant ERR_ROUTE_NOT_FOUND (err u505))

;; Risk levels for predictions
(define-constant RISK_LOW u1)
(define-constant RISK_MODERATE u2)
(define-constant RISK_HIGH u3)
(define-constant RISK_CRITICAL u4)

;; Historical data aggregation
(define-map route-patterns
  { route-hash: (buff 32) }
  {
    origin: (string-ascii 50),
    destination: (string-ascii 50),
    average-temp-variance: int,
    violation-frequency: uint,
    total-shipments: uint,
    last-updated: uint,
    seasonal-adjustment: int
  }
)

;; Temperature predictions for active shipments
(define-map temperature-predictions
  { shipment-id: uint }
  {
    predicted-risk-level: uint,
    violation-probability: uint, ;; Percentage 0-100
    critical-timeframe: uint, ;; Blocks when violation likely
    predicted-temp-range: { min: int, max: int },
    confidence-level: uint, ;; Percentage 0-100
    prediction-timestamp: uint,
    model-version: uint
  }
)

;; Early warning alerts
(define-map active-alerts
  { alert-id: uint }
  {
    shipment-id: uint,
    alert-type: (string-ascii 30), ;; "temperature", "route", "equipment", "weather"
    severity: uint,
    message: (string-ascii 200),
    triggered-at: uint,
    expires-at: uint,
    acknowledged: bool,
    action-taken: (optional (string-ascii 100))
  }
)

;; Route optimization suggestions
(define-map route-optimizations
  { shipment-id: uint }
  {
    suggested-route: (string-ascii 300),
    estimated-improvement: uint, ;; Percentage improvement
    additional-cost: uint,
    time-difference: int, ;; Positive = longer, negative = shorter
    risk-reduction: uint,
    generated-at: uint
  }
)

;; Environmental factors affecting predictions
(define-map environmental-factors
  { factor-id: uint }
  {
    location: (string-ascii 50),
    weather-condition: (string-ascii 30),
    temperature: int,
    humidity: uint,
    impact-score: int, ;; Can be positive or negative
    valid-until: uint
  }
)

;; Carrier performance patterns
(define-map carrier-risk-profiles
  { carrier: principal }
  {
    average-violation-rate: uint,
    temperature-consistency-score: uint,
    response-time-score: uint,
    equipment-reliability: uint,
    risk-multiplier: uint, ;; Applied to predictions
    last-assessment: uint
  }
)

(define-data-var prediction-counter uint u0)
(define-data-var alert-counter uint u0)
(define-data-var model-version uint u1)
(define-data-var prediction-accuracy uint u85) ;; Overall model accuracy

;; Generate temperature prediction for a shipment
(define-public (generate-temperature-prediction
  (shipment-id uint)
  (route-origin (string-ascii 50))
  (route-destination (string-ascii 50))
  (carrier principal)
  (duration-hours uint)
)
  (let
    (
      (route-hash (hash (concat (unwrap-panic (to-consensus-buff? route-origin)) (unwrap-panic (to-consensus-buff? route-destination)))))
      (route-data (map-get? route-patterns { route-hash: route-hash }))
      (carrier-profile (map-get? carrier-risk-profiles { carrier: carrier }))
      (base-risk (calculate-base-risk duration-hours))
    )
    (asserts! (> duration-hours u0) ERR_INVALID_PARAMETERS)
    (asserts! (<= duration-hours u720) ERR_INVALID_PARAMETERS) ;; Max 30 days
    
    ;; Calculate prediction based on available data
    (let
      (
        (route-risk (match route-data
          data (if (> (get total-shipments data) u5)
                  (+ base-risk (/ (get violation-frequency data) u10))
                  base-risk)
          base-risk))
        (carrier-risk (match carrier-profile
          profile (+ route-risk (/ (get average-violation-rate profile) u20))
          (+ route-risk u10))) ;; Unknown carrier penalty
        (final-risk-level (if (>= carrier-risk u75) RISK_CRITICAL
                           (if (>= carrier-risk u50) RISK_HIGH
                             (if (>= carrier-risk u25) RISK_MODERATE
                               RISK_LOW))))
        (violation-probability (min carrier-risk u99))
        (confidence (calculate-confidence route-data carrier-profile))
      )
      
      ;; Store prediction
      (map-set temperature-predictions
        { shipment-id: shipment-id }
        {
          predicted-risk-level: final-risk-level,
          violation-probability: violation-probability,
          critical-timeframe: (+ stacks-block-height (* duration-hours u6)), ;; ~6 blocks per hour
          predicted-temp-range: { min: -5, max: 8 }, ;; Simplified range
          confidence-level: confidence,
          prediction-timestamp: stacks-block-height,
          model-version: (var-get model-version)
        }
      )
      
      ;; Generate alert if high risk
      (if (>= final-risk-level RISK_HIGH)
        (try! (create-temperature-alert shipment-id final-risk-level))
        true)
      
      (ok final-risk-level)
    )
  )
)

;; Calculate base risk based on duration
(define-private (calculate-base-risk (duration-hours uint))
  (if (> duration-hours u168) u40 ;; > 1 week
    (if (> duration-hours u72) u25  ;; > 3 days
      (if (> duration-hours u24) u15 ;; > 1 day
        u5)))                       ;; <= 1 day

;; Calculate prediction confidence based on available data
(define-private (calculate-confidence (route-data (optional { origin: (string-ascii 50), destination: (string-ascii 50), average-temp-variance: int, violation-frequency: uint, total-shipments: uint, last-updated: uint, seasonal-adjustment: int })) (carrier-profile (optional { average-violation-rate: uint, temperature-consistency-score: uint, response-time-score: uint, equipment-reliability: uint, risk-multiplier: uint, last-assessment: uint })))
  (let
    (
      (route-confidence (match route-data
        data (min (+ u50 (* (get total-shipments data) u5)) u90)
        u30))
      (carrier-confidence (match carrier-profile
        profile u80
        u40))
    )
    (/ (+ route-confidence carrier-confidence) u2)
  )
)

;; Create temperature alert
(define-private (create-temperature-alert (shipment-id uint) (severity uint))
  (let ((alert-id (+ (var-get alert-counter) u1)))
    (map-set active-alerts
      { alert-id: alert-id }
      {
        shipment-id: shipment-id,
        alert-type: "temperature",
        severity: severity,
        message: "High risk of temperature violation predicted",
        triggered-at: stacks-block-height,
        expires-at: (+ stacks-block-height u144), ;; ~24 hours
        acknowledged: false,
        action-taken: none
      }
    )
    (var-set alert-counter alert-id)
    (ok alert-id)
  )
)

;; Update route historical data
(define-public (update-route-data
  (route-origin (string-ascii 50))
  (route-destination (string-ascii 50))
  (had-violation bool)
  (temp-variance int)
)
  (let
    (
      (route-hash (hash (concat (unwrap-panic (to-consensus-buff? route-origin)) (unwrap-panic (to-consensus-buff? route-destination)))))
      (current-data (default-to
        {
          origin: route-origin,
          destination: route-destination,
          average-temp-variance: 0,
          violation-frequency: u0,
          total-shipments: u0,
          last-updated: u0,
          seasonal-adjustment: 0
        }
        (map-get? route-patterns { route-hash: route-hash })
      ))
      (new-total (+ (get total-shipments current-data) u1))
      (new-violations (if had-violation 
                        (+ (get violation-frequency current-data) u1)
                        (get violation-frequency current-data)))
      (new-avg-variance (/ (+ (* (get average-temp-variance current-data) (get total-shipments current-data)) temp-variance) new-total))
    )
    
    (map-set route-patterns
      { route-hash: route-hash }
      (merge current-data {
        average-temp-variance: new-avg-variance,
        violation-frequency: new-violations,
        total-shipments: new-total,
        last-updated: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Generate route optimization suggestion
(define-public (generate-route-optimization 
  (shipment-id uint)
  (current-risk-level uint)
)
  (begin
    (asserts! (>= current-risk-level RISK_MODERATE) ERR_INVALID_PARAMETERS)
    
    ;; Simple optimization logic
    (let
      (
        (improvement-percent (if (is-eq current-risk-level RISK_CRITICAL) u40
                              (if (is-eq current-risk-level RISK_HIGH) u25
                                u15)))
        (additional-cost (* improvement-percent u1000))
      )
      
      (map-set route-optimizations
        { shipment-id: shipment-id }
        {
          suggested-route: "Optimized route via refrigerated hubs",
          estimated-improvement: improvement-percent,
          additional-cost: additional-cost,
          time-difference: 2, ;; 2 hours longer
          risk-reduction: improvement-percent,
          generated-at: stacks-block-height
        }
      )
      (ok improvement-percent)
    )
  )
)

;; Update carrier risk profile
(define-public (update-carrier-risk-profile
  (carrier principal)
  (violation-rate uint)
  (consistency-score uint)
  (response-score uint)
  (equipment-reliability uint)
)
  (begin
    (asserts! (<= violation-rate u100) ERR_INVALID_PARAMETERS)
    (asserts! (<= consistency-score u100) ERR_INVALID_PARAMETERS)
    (asserts! (<= response-score u100) ERR_INVALID_PARAMETERS)
    (asserts! (<= equipment-reliability u100) ERR_INVALID_PARAMETERS)
    
    (let ((risk-multiplier (/ (+ violation-rate (- u100 consistency-score) (- u100 response-score) (- u100 equipment-reliability)) u4)))
      (map-set carrier-risk-profiles
        { carrier: carrier }
        {
          average-violation-rate: violation-rate,
          temperature-consistency-score: consistency-score,
          response-time-score: response-score,
          equipment-reliability: equipment-reliability,
          risk-multiplier: risk-multiplier,
          last-assessment: stacks-block-height
        }
      )
    )
    (ok true)
  )
)

;; Acknowledge alert
(define-public (acknowledge-alert (alert-id uint) (action-taken (string-ascii 100)))
  (let ((alert (unwrap! (map-get? active-alerts { alert-id: alert-id }) ERR_PREDICTION_NOT_FOUND)))
    (map-set active-alerts
      { alert-id: alert-id }
      (merge alert {
        acknowledged: true,
        action-taken: (some action-taken)
      })
    )
    (ok true)
  )
)

;; Update model accuracy based on actual outcomes
(define-public (update-model-accuracy (new-accuracy uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-accuracy u100) ERR_INVALID_PARAMETERS)
    (var-set prediction-accuracy new-accuracy)
    (ok new-accuracy)
  )
)

;; Read-only functions
(define-read-only (get-temperature-prediction (shipment-id uint))
  (map-get? temperature-predictions { shipment-id: shipment-id })
)

(define-read-only (get-route-pattern (route-origin (string-ascii 50)) (route-destination (string-ascii 50)))
  (let ((route-hash (hash (concat (unwrap-panic (to-consensus-buff? route-origin)) (unwrap-panic (to-consensus-buff? route-destination))))))
    (map-get? route-patterns { route-hash: route-hash })
  )
)

(define-read-only (get-active-alert (alert-id uint))
  (map-get? active-alerts { alert-id: alert-id })
)

(define-read-only (get-route-optimization (shipment-id uint))
  (map-get? route-optimizations { shipment-id: shipment-id })
)

(define-read-only (get-carrier-risk-profile (carrier principal))
  (map-get? carrier-risk-profiles { carrier: carrier })
)

(define-read-only (get-model-accuracy)
  (var-get prediction-accuracy)
)

(define-read-only (calculate-shipment-risk-score
  (shipment-id uint)
  (duration-hours uint)
  (carrier principal)
)
  (let
    (
      (prediction (map-get? temperature-predictions { shipment-id: shipment-id }))
      (carrier-profile (map-get? carrier-risk-profiles { carrier: carrier }))
    )
    (match prediction
      pred (get violation-probability pred)
      (match carrier-profile
        profile (min (+ (calculate-base-risk duration-hours) (get risk-multiplier profile)) u99)
        (calculate-base-risk duration-hours)
      )
    )
  )
)

(define-read-only (get-risk-level-description (risk-level uint))
  (if (is-eq risk-level RISK_CRITICAL) "Critical - Immediate action required"
    (if (is-eq risk-level RISK_HIGH) "High - Monitor closely and prepare contingencies"
      (if (is-eq risk-level RISK_MODERATE) "Moderate - Standard monitoring protocols"
        "Low - Normal operations")))
)
