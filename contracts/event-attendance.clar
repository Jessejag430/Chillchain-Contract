;; Event Attendance Verification System
;; Allows event organizers to verify attendance and reward community engagement

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_ALREADY_EXISTS (err u409))
(define-constant ERR_INVALID_STATUS (err u410))
(define-constant ERR_NOT_PARTICIPANT (err u412))
(define-constant ERR_ALREADY_VERIFIED (err u413))

;; Data Variables
(define-data-var next-verification-id uint u1)

;; Data Maps
(define-map event-attendance
  {event-id: uint, participant: principal}
  {
    verified: bool,
    verification-time: uint,
    verifier: principal,
    attendance-score: uint,
    bonus-awarded: bool
  }
)

(define-map attendance-verifications
  uint
  {
    event-id: uint,
    participant: principal,
    verifier: principal,
    verification-method: (string-ascii 50),
    notes: (string-ascii 200),
    created-at: uint
  }
)

(define-map event-attendance-summary
  uint
  {
    total-participants: uint,
    verified-attendees: uint,
    verification-rate: uint,
    completed-verification: bool
  }
)

(define-map member-attendance-history
  principal
  {
    events-joined: uint,
    events-attended: uint,
    attendance-rate: uint,
    total-reputation-earned: uint
  }
)

;; Private Functions
(define-private (calculate-attendance-bonus (attendance-score uint))
  (if (<= attendance-score u3)
    u1
    (if (<= attendance-score u5)
      u2
      u3)
  )
)

(define-private (update-member-attendance-stats (member principal) (attended bool))
  (let ((current-stats (default-to 
                         {events-joined: u0, events-attended: u0, attendance-rate: u0, total-reputation-earned: u0}
                         (map-get? member-attendance-history member))))
    (let (
      (new-joined (+ (get events-joined current-stats) u1))
      (new-attended (if attended (+ (get events-attended current-stats) u1) (get events-attended current-stats)))
      (new-rate (if (> new-joined u0) (/ (* new-attended u100) new-joined) u0))
    )
      (map-set member-attendance-history member {
        events-joined: new-joined,
        events-attended: new-attended,
        attendance-rate: new-rate,
        total-reputation-earned: (get total-reputation-earned current-stats)
      })
    )
  )
)

;; Public Functions

;; Verify attendance for a specific participant at an event
(define-public (verify-attendance 
  (event-id uint) 
  (participant principal) 
  (attendance-score uint)
  (verification-method (string-ascii 50))
  (notes (string-ascii 200)))
  (let (
    (verification-id (var-get next-verification-id))
    (existing-attendance (map-get? event-attendance {event-id: event-id, participant: participant}))
  )
    ;; Check attendance score bounds
    (asserts! (<= attendance-score u5) ERR_INVALID_STATUS)
    (asserts! (> attendance-score u0) ERR_INVALID_STATUS)
    
    ;; Check if already verified
    (match existing-attendance
      attendance-data (asserts! (not (get verified attendance-data)) ERR_ALREADY_VERIFIED)
      (asserts! false ERR_NOT_PARTICIPANT)
    )
    
    ;; Create verification record
    (map-set attendance-verifications verification-id {
      event-id: event-id,
      participant: participant,
      verifier: tx-sender,
      verification-method: verification-method,
      notes: notes,
      created-at: stacks-block-height
    })
    
    ;; Update attendance record
    (map-set event-attendance {event-id: event-id, participant: participant} {
      verified: true,
      verification-time: stacks-block-height,
      verifier: tx-sender,
      attendance-score: attendance-score,
      bonus-awarded: false
    })
    
    ;; Update member stats
    (update-member-attendance-stats participant true)
    
    ;; Update event summary
    (let ((summary (default-to 
                     {total-participants: u1, verified-attendees: u0, verification-rate: u0, completed-verification: false}
                     (map-get? event-attendance-summary event-id))))
      (map-set event-attendance-summary event-id
        (merge summary {
          verified-attendees: (+ (get verified-attendees summary) u1),
          verification-rate: (/ (* (+ (get verified-attendees summary) u1) u100) (get total-participants summary))
        }))
    )
    
    (var-set next-verification-id (+ verification-id u1))
    (ok verification-id)
  )
)

;; Award reputation bonus for verified attendance
(define-public (award-attendance-bonus (event-id uint) (participant principal))
  (let ((attendance-data (unwrap! (map-get? event-attendance {event-id: event-id, participant: participant}) ERR_NOT_FOUND)))
    (asserts! (get verified attendance-data) ERR_NOT_FOUND)
    (asserts! (not (get bonus-awarded attendance-data)) ERR_ALREADY_EXISTS)
    
    (let ((bonus-amount (calculate-attendance-bonus (get attendance-score attendance-data))))
      ;; Update attendance record
      (map-set event-attendance {event-id: event-id, participant: participant}
        (merge attendance-data {bonus-awarded: true}))
      
      ;; Update member attendance history
      (let ((current-stats (unwrap-panic (map-get? member-attendance-history participant))))
        (map-set member-attendance-history participant
          (merge current-stats {
            total-reputation-earned: (+ (get total-reputation-earned current-stats) bonus-amount)
          }))
      )
      
      (ok bonus-amount)
    )
  )
)

;; Register participant attendance (called when joining event)
(define-public (register-event-participant (event-id uint) (participant principal))
  (begin
    (asserts! (is-none (map-get? event-attendance {event-id: event-id, participant: participant})) ERR_ALREADY_EXISTS)
    
    (map-set event-attendance {event-id: event-id, participant: participant} {
      verified: false,
      verification-time: u0,
      verifier: tx-sender,
      attendance-score: u0,
      bonus-awarded: false
    })
    
    ;; Update event summary
    (let ((current-summary (default-to 
                             {total-participants: u0, verified-attendees: u0, verification-rate: u0, completed-verification: false}
                             (map-get? event-attendance-summary event-id))))
      (map-set event-attendance-summary event-id
        (merge current-summary {
          total-participants: (+ (get total-participants current-summary) u1)
        }))
    )
    
    ;; Initialize member stats if first event
    (if (is-none (map-get? member-attendance-history participant))
      (map-set member-attendance-history participant {
        events-joined: u1,
        events-attended: u0,
        attendance-rate: u0,
        total-reputation-earned: u0
      })
      (update-member-attendance-stats participant false)
    )
    
    (ok true)
  )
)

;; Complete verification process for an event
(define-public (complete-event-verification (event-id uint))
  (let ((summary (unwrap! (map-get? event-attendance-summary event-id) ERR_NOT_FOUND)))
    (asserts! (not (get completed-verification summary)) ERR_ALREADY_EXISTS)
    
    (let ((verification-rate (if (> (get total-participants summary) u0)
                               (/ (* (get verified-attendees summary) u100) (get total-participants summary))
                               u0)))
      (map-set event-attendance-summary event-id
        (merge summary {
          verification-rate: verification-rate,
          completed-verification: true
        }))
    )
    
    (ok true)
  )
)

;; Read-only functions

;; Get attendance record for a participant
(define-read-only (get-attendance-record (event-id uint) (participant principal))
  (map-get? event-attendance {event-id: event-id, participant: participant})
)

;; Get verification details
(define-read-only (get-verification-details (verification-id uint))
  (map-get? attendance-verifications verification-id)
)

;; Get event attendance summary
(define-read-only (get-event-attendance-summary (event-id uint))
  (map-get? event-attendance-summary event-id)
)

;; Get member attendance history
(define-read-only (get-member-attendance-history (member principal))
  (map-get? member-attendance-history member)
)

;; Get member attendance rate
(define-read-only (get-member-attendance-rate (member principal))
  (match (map-get? member-attendance-history member)
    stats (ok (get attendance-rate stats))
    (ok u0)
  )
)

;; Check if participant attended event
(define-read-only (did-attend-event (event-id uint) (participant principal))
  (match (map-get? event-attendance {event-id: event-id, participant: participant})
    attendance-record (get verified attendance-record)
    false
  )
)

;; Get next verification ID
(define-read-only (get-next-verification-id)
  (var-get next-verification-id)
)
