(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_TEMPERATURE (err u101))
(define-constant ERR_SHIPMENT_NOT_FOUND (err u102))
(define-constant ERR_SHIPMENT_ALREADY_EXISTS (err u103))
(define-constant ERR_INVALID_TIMESTAMP (err u104))
(define-constant ERR_SHIPMENT_COMPLETED (err u105))
(define-constant ERR_INVALID_THRESHOLD (err u106))

(define-non-fungible-token shipment-nft uint)

(define-data-var next-shipment-id uint u1)

(define-map shipments
  { shipment-id: uint }
  {
    owner: principal,
    product-name: (string-ascii 50),
    origin: (string-ascii 50),
    destination: (string-ascii 50),
    min-temp: int,
    max-temp: int,
    created-at: uint,
    completed-at: (optional uint),
    status: (string-ascii 20),
    violation-count: uint
  }
)

(define-map temperature-logs
  { shipment-id: uint, log-id: uint }
  {
    temperature: int,
    timestamp: uint,
    location: (string-ascii 50),
    sensor-id: (string-ascii 30)
  }
)

(define-map shipment-log-counts
  { shipment-id: uint }
  { count: uint }
)

(define-map authorized-sensors
  { sensor-id: (string-ascii 30) }
  { authorized: bool, owner: principal }
)

(define-public (create-shipment 
  (product-name (string-ascii 50))
  (origin (string-ascii 50))
  (destination (string-ascii 50))
  (min-temp int)
  (max-temp int))
  (let
    (
      (shipment-id (var-get next-shipment-id))
    )
    (asserts! (> max-temp min-temp) ERR_INVALID_THRESHOLD)
    (try! (nft-mint? shipment-nft shipment-id tx-sender))
    (map-set shipments
      { shipment-id: shipment-id }
      {
        owner: tx-sender,
        product-name: product-name,
        origin: origin,
        destination: destination,
        min-temp: min-temp,
        max-temp: max-temp,
        created-at: stacks-block-height,
        completed-at: none,
        status: "in-transit",
        violation-count: u0
      }
    )
    (map-set shipment-log-counts
      { shipment-id: shipment-id }
      { count: u0 }
    )
    (var-set next-shipment-id (+ shipment-id u1))
    (ok shipment-id)
  )
)

(define-public (authorize-sensor (sensor-id (string-ascii 30)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set authorized-sensors
      { sensor-id: sensor-id }
      { authorized: true, owner: tx-sender }
    )
    (ok true)
  )
)

(define-public (revoke-sensor (sensor-id (string-ascii 30)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (map-set authorized-sensors
      { sensor-id: sensor-id }
      { authorized: false, owner: tx-sender }
    )
    (ok true)
  )
)

(define-public (log-temperature
  (shipment-id uint)
  (temperature int)
  (location (string-ascii 50))
  (sensor-id (string-ascii 30)))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (sensor-auth (default-to { authorized: false, owner: CONTRACT_OWNER } 
                    (map-get? authorized-sensors { sensor-id: sensor-id })))
      (log-count-data (default-to { count: u0 } 
                       (map-get? shipment-log-counts { shipment-id: shipment-id })))
      (current-log-id (get count log-count-data))
      (is-violation (or (< temperature (get min-temp shipment)) 
                       (> temperature (get max-temp shipment))))
      (new-violation-count (if is-violation 
                            (+ (get violation-count shipment) u1)
                            (get violation-count shipment)))
    )
    (asserts! (get authorized sensor-auth) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status shipment) "in-transit") ERR_SHIPMENT_COMPLETED)
    
    (map-set temperature-logs
      { shipment-id: shipment-id, log-id: current-log-id }
      {
        temperature: temperature,
        timestamp: stacks-block-height,
        location: location,
        sensor-id: sensor-id
      }
    )
    
    (map-set shipment-log-counts
      { shipment-id: shipment-id }
      { count: (+ current-log-id u1) }
    )
    
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment { violation-count: new-violation-count })
    )
    
    (ok current-log-id)
  )
)

(define-public (complete-shipment (shipment-id uint))
  (let
    (
      (shipment (unwrap! (map-get? shipments { shipment-id: shipment-id }) ERR_SHIPMENT_NOT_FOUND))
      (nft-owner (unwrap! (nft-get-owner? shipment-nft shipment-id) ERR_SHIPMENT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender nft-owner) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status shipment) "in-transit") ERR_SHIPMENT_COMPLETED)
    
    (map-set shipments
      { shipment-id: shipment-id }
      (merge shipment { 
        status: "completed", 
        completed-at: (some stacks-block-height) 
      })
    )
    (ok true)
  )
)

(define-public (transfer-shipment (shipment-id uint) (new-owner principal))
  (let
    (
      (current-owner (unwrap! (nft-get-owner? shipment-nft shipment-id) ERR_SHIPMENT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender current-owner) ERR_NOT_AUTHORIZED)
    (try! (nft-transfer? shipment-nft shipment-id tx-sender new-owner))
    (ok true)
  )
)

(define-read-only (get-shipment (shipment-id uint))
  (map-get? shipments { shipment-id: shipment-id })
)

(define-read-only (get-temperature-log (shipment-id uint) (log-id uint))
  (map-get? temperature-logs { shipment-id: shipment-id, log-id: log-id })
)

(define-read-only (get-shipment-log-count (shipment-id uint))
  (default-to u0 (get count (map-get? shipment-log-counts { shipment-id: shipment-id })))
)

(define-read-only (get-sensor-authorization (sensor-id (string-ascii 30)))
  (map-get? authorized-sensors { sensor-id: sensor-id })
)

(define-read-only (get-next-shipment-id)
  (var-get next-shipment-id)
)

(define-read-only (is-temperature-compliant (shipment-id uint))
  (let
    (
      (shipment (map-get? shipments { shipment-id: shipment-id }))
    )
    (match shipment
      shipment-data (is-eq (get violation-count shipment-data) u0)
      false
    )
  )
)

(define-read-only (get-shipment-violations (shipment-id uint))
  (let
    (
      (shipment (map-get? shipments { shipment-id: shipment-id }))
    )
    (match shipment
      shipment-data (get violation-count shipment-data)
      u0
    )
  )
)

(define-read-only (get-all-logs-for-shipment (shipment-id uint))
  (let
    (
      (log-count (get-shipment-log-count shipment-id))
    )
    (map get-temperature-log-helper (list shipment-id shipment-id shipment-id shipment-id shipment-id 
                                         shipment-id shipment-id shipment-id shipment-id shipment-id)
         (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9))
  )
)

(define-private (get-temperature-log-helper (shipment-id uint) (log-id uint))
  (map-get? temperature-logs { shipment-id: shipment-id, log-id: log-id })
)