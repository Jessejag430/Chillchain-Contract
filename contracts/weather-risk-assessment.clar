;; Weather Risk Assessment and Alert System
;; This contract provides weather monitoring, risk assessment, and alert functionality

;; Error constants
(define-constant err-owner-only (err u400))
(define-constant err-not-found (err u401))
(define-constant err-invalid-data (err u402))
(define-constant err-unauthorized (err u403))

;; Contract owner
(define-constant contract-owner tx-sender)

;; Data variables
(define-data-var next-weather-record-id uint u1)
(define-data-var alert-threshold-score uint u70)

;; Weather risk levels
(define-constant risk-low u1)
(define-constant risk-medium u2)
(define-constant risk-high u3)
(define-constant risk-critical u4)

;; Weather event types
(define-constant event-drought u1)
(define-constant event-flood u2)
(define-constant event-frost u3)
(define-constant event-hail u4)
(define-constant event-storm u5)

;; Weather records
(define-map weather-records
  { record-id: uint }
  {
    location: (string-ascii 50),
    date: uint,
    temperature-high: uint,
    humidity: uint,
    precipitation: uint,
    weather-event: uint,
    risk-score: uint,
    data-source: principal
  }
)

;; Location risk profiles
(define-map location-risk-profiles
  { location: (string-ascii 50) }
  {
    current-risk-level: uint,
    last-updated: uint,
    total-weather-events: uint,
    drought-frequency: uint,
    flood-frequency: uint,
    avg-temperature: uint
  }
)

;; Token weather alerts
(define-map token-weather-alerts
  { token-id: uint }
  {
    location: (string-ascii 50),
    alert-level: uint,
    weather-event: uint,
    alert-date: uint,
    estimated-impact: uint,
    active: bool
  }
)

;; Alert subscriptions
(define-map alert-subscriptions
  { farmer: principal, location: (string-ascii 50) }
  { subscribed: bool, notification-threshold: uint }
)

;; Read-only functions
(define-read-only (get-weather-record (record-id uint))
  (map-get? weather-records { record-id: record-id })
)

(define-read-only (get-location-risk-profile (location (string-ascii 50)))
  (default-to 
    { current-risk-level: risk-low, last-updated: u0, total-weather-events: u0, 
      drought-frequency: u0, flood-frequency: u0, avg-temperature: u0 }
    (map-get? location-risk-profiles { location: location })
  )
)

(define-read-only (get-token-weather-alert (token-id uint))
  (map-get? token-weather-alerts { token-id: token-id })
)

;; Calculate weather risk score
(define-read-only (calculate-weather-risk-score (temperature-high uint) (precipitation uint) (humidity uint) (weather-event uint))
  (let
    (
      (temp-risk (if (or (> temperature-high u35) (< temperature-high u5)) u25 u0))
      (precip-risk (if (> precipitation u50) u20 (if (< precipitation u5) u15 u0)))
      (humidity-risk (if (or (> humidity u90) (< humidity u30)) u15 u0))
      (event-risk (cond 
        ((is-eq weather-event event-drought) u30)
        ((is-eq weather-event event-flood) u35)
        ((is-eq weather-event event-frost) u25)
        ((is-eq weather-event event-hail) u40)
        ((is-eq weather-event event-storm) u30)
        u0))
    )
    (+ temp-risk precip-risk humidity-risk event-risk)
  )
)

;; Submit weather data
(define-public (submit-weather-data (location (string-ascii 50)) (temperature-high uint) (humidity uint) (precipitation uint) (weather-event uint))
  (let
    (
      (record-id (var-get next-weather-record-id))
      (current-time (unwrap-panic (get-stacks-block-info? time u0)))
      (risk-score (calculate-weather-risk-score temperature-high precipitation humidity weather-event))
      (risk-level (cond 
        ((>= risk-score u80) risk-critical)
        ((>= risk-score u60) risk-high)
        ((>= risk-score u40) risk-medium)
        risk-low))
    )
    (asserts! (and (>= weather-event u0) (<= weather-event event-storm)) err-invalid-data)
    (asserts! (and (<= humidity u100) (<= precipitation u200)) err-invalid-data)
    
    (map-set weather-records
      { record-id: record-id }
      {
        location: location,
        date: current-time,
        temperature-high: temperature-high,
        humidity: humidity,
        precipitation: precipitation,
        weather-event: weather-event,
        risk-score: risk-score,
        data-source: tx-sender
      }
    )
    
    (update-location-risk-profile location risk-level weather-event temperature-high)
    (var-set next-weather-record-id (+ record-id u1))
    (ok record-id)
  )
)

;; Update location risk profile
(define-private (update-location-risk-profile (location (string-ascii 50)) (risk-level uint) (weather-event uint) (temperature uint))
  (let
    (
      (current-profile (get-location-risk-profile location))
      (current-time (unwrap-panic (get-stacks-block-info? time u0)))
      (new-event-count (+ (get total-weather-events current-profile) u1))
      (new-drought-freq (if (is-eq weather-event event-drought) (+ (get drought-frequency current-profile) u1) (get drought-frequency current-profile)))
      (new-flood-freq (if (is-eq weather-event event-flood) (+ (get flood-frequency current-profile) u1) (get flood-frequency current-profile)))
      (new-avg-temp (if (> (get total-weather-events current-profile) u0)
                      (/ (+ (* (get avg-temperature current-profile) (get total-weather-events current-profile)) temperature) new-event-count)
                      temperature))
    )
    (map-set location-risk-profiles
      { location: location }
      {
        current-risk-level: risk-level,
        last-updated: current-time,
        total-weather-events: new-event-count,
        drought-frequency: new-drought-freq,
        flood-frequency: new-flood-freq,
        avg-temperature: new-avg-temp
      }
    )
  )
)

;; Subscribe to weather alerts
(define-public (subscribe-to-weather-alerts (location (string-ascii 50)) (notification-threshold uint))
  (begin
    (asserts! (and (>= notification-threshold u0) (<= notification-threshold u100)) err-invalid-data)
    (map-set alert-subscriptions
      { farmer: tx-sender, location: location }
      { subscribed: true, notification-threshold: notification-threshold }
    )
    (ok true)
  )
)

;; Create weather alert for token
(define-public (create-weather-alert (token-id uint) (location (string-ascii 50)) (weather-event uint) (estimated-impact uint))
  (let
    (
      (location-profile (get-location-risk-profile location))
      (current-time (unwrap-panic (get-stacks-block-info? time u0)))
      (alert-level (get current-risk-level location-profile))
    )
    (asserts! (and (>= weather-event event-drought) (<= weather-event event-storm)) err-invalid-data)
    
    (map-set token-weather-alerts
      { token-id: token-id }
      {
        location: location,
        alert-level: alert-level,
        weather-event: weather-event,
        alert-date: current-time,
        estimated-impact: estimated-impact,
        active: true
      }
    )
    (ok true)
  )
)

;; Deactivate weather alert
(define-public (deactivate-weather-alert (token-id uint))
  (let
    (
      (alert (unwrap! (get-token-weather-alert token-id) err-not-found))
    )
    (map-set token-weather-alerts
      { token-id: token-id }
      (merge alert { active: false })
    )
    (ok true)
  )
)
