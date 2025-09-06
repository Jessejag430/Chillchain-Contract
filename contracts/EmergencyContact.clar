;; Emergency Contact System for Lastwish
;; Allows trusted contacts to send emergency heartbeats for testators

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_CONTACT_NOT_FOUND (err u201))
(define-constant ERR_CONTACT_ALREADY_EXISTS (err u202))
(define-constant ERR_INVALID_CONTACT (err u203))
(define-constant ERR_EMERGENCY_PERIOD_EXPIRED (err u204))
(define-constant ERR_EMERGENCY_LIMIT_REACHED (err u205))
(define-constant ERR_INSUFFICIENT_CONFIRMATIONS (err u206))
(define-constant ERR_WILL_NOT_FOUND (err u207))

;; Constants
(define-constant EMERGENCY_HEARTBEAT_DURATION u1440) ;; 24 hours in blocks
(define-constant MAX_EMERGENCY_CONTACTS u3)
(define-constant MIN_CONFIRMATIONS u2)
(define-constant MAX_EMERGENCY_EXTENSIONS u5)

;; Data variables
(define-data-var next-emergency-id uint u1)

;; Emergency contacts for each will
(define-map emergency-contacts
  { will-id: uint, contact: principal }
  {
    added-by: principal,
    added-at: uint,
    active: bool,
    trust-level: uint
  }
)

;; Emergency heartbeat requests
(define-map emergency-requests
  { emergency-id: uint }
  {
    will-id: uint,
    requesting-contact: principal,
    requested-at: uint,
    confirmations: uint,
    executed: bool,
    expires-at: uint
  }
)

;; Track emergency contact count per will
(define-map will-emergency-counts
  { will-id: uint }
  { contact-count: uint, emergency-extensions: uint }
)

;; Emergency contact confirmations
(define-map emergency-confirmations
  { emergency-id: uint, contact: principal }
  { confirmed-at: uint }
)

;; Add emergency contact to a will
(define-public (add-emergency-contact 
  (will-id uint)
  (contact principal)
  (trust-level uint)
)
  (let
    (
      (will-data (unwrap! (contract-call? .Lastwish get-will will-id) ERR_WILL_NOT_FOUND))
      (current-count (default-to u0 (get contact-count (map-get? will-emergency-counts { will-id: will-id }))))
    )
    (asserts! (is-eq (get testator will-data) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (get executed will-data)) ERR_UNAUTHORIZED)
    (asserts! (not (is-eq contact tx-sender)) ERR_INVALID_CONTACT)
    (asserts! (and (>= trust-level u1) (<= trust-level u3)) ERR_INVALID_CONTACT)
    (asserts! (< current-count MAX_EMERGENCY_CONTACTS) ERR_EMERGENCY_LIMIT_REACHED)
    (asserts! (is-none (map-get? emergency-contacts { will-id: will-id, contact: contact })) ERR_CONTACT_ALREADY_EXISTS)
    
    (map-set emergency-contacts
      { will-id: will-id, contact: contact }
      {
        added-by: tx-sender,
        added-at: stacks-block-height,
        active: true,
        trust-level: trust-level
      }
    )
    
    (map-set will-emergency-counts
      { will-id: will-id }
      { 
        contact-count: (+ current-count u1),
        emergency-extensions: (default-to u0 (get emergency-extensions (map-get? will-emergency-counts { will-id: will-id })))
      }
    )
    
    (ok true)
  )
)

;; Remove emergency contact
(define-public (remove-emergency-contact 
  (will-id uint)
  (contact principal)
)
  (let
    (
      (will-data (unwrap! (contract-call? .Lastwish get-will will-id) ERR_WILL_NOT_FOUND))
      (contact-data (unwrap! (map-get? emergency-contacts { will-id: will-id, contact: contact }) ERR_CONTACT_NOT_FOUND))
      (current-count (default-to u1 (get contact-count (map-get? will-emergency-counts { will-id: will-id }))))
    )
    (asserts! (is-eq (get testator will-data) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (not (get executed will-data)) ERR_UNAUTHORIZED)
    
    (map-delete emergency-contacts { will-id: will-id, contact: contact })
    (map-set will-emergency-counts { will-id: will-id } { contact-count: (- current-count u1), emergency-extensions: (default-to u0 (get emergency-extensions (map-get? will-emergency-counts { will-id: will-id }))) })
    (ok true)
  )
)

;; Request emergency heartbeat extension
(define-public (request-emergency-heartbeat (will-id uint))
  (let
    (
      (will-data (unwrap! (contract-call? .Lastwish get-will will-id) ERR_WILL_NOT_FOUND))
      (contact-data (unwrap! (map-get? emergency-contacts { will-id: will-id, contact: tx-sender }) ERR_CONTACT_NOT_FOUND))
      (emergency-id (var-get next-emergency-id))
      (expires-at (+ stacks-block-height EMERGENCY_HEARTBEAT_DURATION))
      (extensions-used (default-to u0 (get emergency-extensions (map-get? will-emergency-counts { will-id: will-id }))))
    )
    (asserts! (get active contact-data) ERR_CONTACT_NOT_FOUND)
    (asserts! (not (get executed will-data)) ERR_UNAUTHORIZED)
    (asserts! (< extensions-used MAX_EMERGENCY_EXTENSIONS) ERR_EMERGENCY_LIMIT_REACHED)
    (asserts! 
      (> (* (get heartbeat-interval will-data) u2) 
         (- stacks-block-height (get last-heartbeat will-data))) 
      ERR_EMERGENCY_PERIOD_EXPIRED
    )
    
    (map-set emergency-requests
      { emergency-id: emergency-id }
      {
        will-id: will-id,
        requesting-contact: tx-sender,
        requested-at: stacks-block-height,
        confirmations: u1,
        executed: false,
        expires-at: expires-at
      }
    )
    
    (map-set emergency-confirmations { emergency-id: emergency-id, contact: tx-sender } { confirmed-at: stacks-block-height })
    (var-set next-emergency-id (+ emergency-id u1))
    (ok emergency-id)
  )
)

;; Confirm emergency heartbeat request
(define-public (confirm-emergency-request (emergency-id uint))
  (let
    (
      (request-data (unwrap! (map-get? emergency-requests { emergency-id: emergency-id }) ERR_CONTACT_NOT_FOUND))
      (contact-data (unwrap! (map-get? emergency-contacts { will-id: (get will-id request-data), contact: tx-sender }) ERR_CONTACT_NOT_FOUND))
      (existing-confirmation (map-get? emergency-confirmations { emergency-id: emergency-id, contact: tx-sender }))
    )
    (asserts! (get active contact-data) ERR_CONTACT_NOT_FOUND)
    (asserts! (not (get executed request-data)) ERR_UNAUTHORIZED)
    (asserts! (< stacks-block-height (get expires-at request-data)) ERR_EMERGENCY_PERIOD_EXPIRED)
    (asserts! (is-none existing-confirmation) ERR_CONTACT_ALREADY_EXISTS)
    
    (map-set emergency-confirmations { emergency-id: emergency-id, contact: tx-sender } { confirmed-at: stacks-block-height })
    (map-set emergency-requests { emergency-id: emergency-id } (merge request-data { confirmations: (+ (get confirmations request-data) u1) }))
    (ok (+ (get confirmations request-data) u1))
  )
)

;; Execute emergency heartbeat (requires sufficient confirmations)
(define-public (execute-emergency-heartbeat (emergency-id uint))
  (let
    (
      (request-data (unwrap! (map-get? emergency-requests { emergency-id: emergency-id }) ERR_CONTACT_NOT_FOUND))
      (will-data (unwrap! (contract-call? .Lastwish get-will (get will-id request-data)) ERR_WILL_NOT_FOUND))
      (extensions-data (default-to { contact-count: u0, emergency-extensions: u0 } (map-get? will-emergency-counts { will-id: (get will-id request-data) })))
    )
    (asserts! (not (get executed request-data)) ERR_UNAUTHORIZED)
    (asserts! (< stacks-block-height (get expires-at request-data)) ERR_EMERGENCY_PERIOD_EXPIRED)
    (asserts! (>= (get confirmations request-data) MIN_CONFIRMATIONS) ERR_INSUFFICIENT_CONFIRMATIONS)
    (asserts! (not (get executed will-data)) ERR_UNAUTHORIZED)
    
    (map-set emergency-requests { emergency-id: emergency-id } (merge request-data { executed: true }))
    (map-set will-emergency-counts { will-id: (get will-id request-data) } (merge extensions-data { emergency-extensions: (+ (get emergency-extensions extensions-data) u1) }))
    (contract-call? .Lastwish send-heartbeat (get will-id request-data))
  )
)

;; Read-only functions
(define-read-only (get-emergency-contact (will-id uint) (contact principal))
  (map-get? emergency-contacts { will-id: will-id, contact: contact })
)

(define-read-only (get-emergency-request (emergency-id uint))
  (map-get? emergency-requests { emergency-id: emergency-id })
)

(define-read-only (get-will-emergency-info (will-id uint))
  (map-get? will-emergency-counts { will-id: will-id })
)

(define-read-only (is-emergency-contact (will-id uint) (contact principal))
  (match (map-get? emergency-contacts { will-id: will-id, contact: contact })
    contact-data (get active contact-data)
    false
  )
)
