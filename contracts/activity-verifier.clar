;; Human Activity Verification System
;; Tracks periodic check-ins for registered humans to maintain active status

(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-NOT-REGISTERED (err u201))
(define-constant ERR-CHALLENGE-NOT-FOUND (err u202))
(define-constant ERR-CHALLENGE-EXPIRED (err u203))
(define-constant ERR-ALREADY-RESPONDED (err u204))

;; Activity verification constants
(define-constant ACTIVITY_CHECK_INTERVAL u1008) ;; ~1 week
(define-constant CHALLENGE_DURATION u144) ;; ~1 day
(define-constant MAX_INACTIVE_PERIOD u4032) ;; ~4 weeks
(define-constant ACTIVITY_REWARD_POINTS u15)

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var total-challenges uint u0)
(define-data-var active-challenges uint u0)

;; Human activity tracking
(define-map human-activity principal
  {
    last-checkin: uint,
    total-checkins: uint,
    consecutive-checkins: uint,
    longest-streak: uint,
    activity-score: uint
  }
)

;; Activity challenges for verification
(define-map activity-challenges uint
  {
    human: principal,
    challenge-type: uint,
    challenge-data: uint,
    correct-answer: uint,
    created-at: uint,
    expires-at: uint,
    completed: bool,
    response: (optional uint)
  }
)

;; Activity statistics
(define-map activity-stats principal
  {
    challenges-completed: uint,
    challenges-failed: uint,
    last-warning-sent: uint,
    status: uint
  }
)

;; Check-in for human activity
(define-public (check-in)
  (let
    (
      (sender tx-sender)
      (current-block stacks-block-height)
      (activity-data (default-to 
        {
          last-checkin: u0,
          total-checkins: u0,
          consecutive-checkins: u0,
          longest-streak: u0,
          activity-score: u0
        }
        (map-get? human-activity sender)
      ))
      (blocks-since-last (- current-block (get last-checkin activity-data)))
      (is-consecutive (<= blocks-since-last ACTIVITY_CHECK_INTERVAL))
      (new-consecutive (if is-consecutive (+ (get consecutive-checkins activity-data) u1) u1))
      (new-longest (if (> new-consecutive (get longest-streak activity-data)) new-consecutive (get longest-streak activity-data)))
    )
    ;; Verify sender is registered in main Humanchain contract
    (asserts! (contract-call? .Humanchain is-registered sender) ERR-NOT-REGISTERED)
    
    ;; Update activity data
    (map-set human-activity sender
      (merge activity-data
        {
          last-checkin: current-block,
          total-checkins: (+ (get total-checkins activity-data) u1),
          consecutive-checkins: new-consecutive,
          longest-streak: new-longest,
          activity-score: (+ (get activity-score activity-data) (calculate-checkin-points new-consecutive))
        }
      )
    )
    (ok true)
  )
)

;; Create activity challenge
(define-public (create-activity-challenge (challenge-type uint))
  (let
    (
      (sender tx-sender)
      (challenge-id (+ (var-get total-challenges) u1))
      (challenge-data (generate-challenge-data challenge-type))
      (correct-answer (calculate-correct-answer challenge-type challenge-data))
    )
    (asserts! (contract-call? .Humanchain is-registered sender) ERR-NOT-REGISTERED)
    (asserts! (is-valid-challenge-type challenge-type) ERR-NOT-AUTHORIZED)
    
    (map-set activity-challenges challenge-id
      {
        human: sender,
        challenge-type: challenge-type,
        challenge-data: challenge-data,
        correct-answer: correct-answer,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height CHALLENGE_DURATION),
        completed: false,
        response: none
      }
    )
    
    (var-set total-challenges challenge-id)
    (var-set active-challenges (+ (var-get active-challenges) u1))
    (ok challenge-id)
  )
)

;; Respond to activity challenge
(define-public (respond-to-challenge (challenge-id uint) (answer uint))
  (let
    (
      (sender tx-sender)
      (challenge-data (unwrap! (map-get? activity-challenges challenge-id) ERR-CHALLENGE-NOT-FOUND))
      (is-correct (is-eq answer (get correct-answer challenge-data)))
    )
    (asserts! (is-eq sender (get human challenge-data)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get completed challenge-data)) ERR-ALREADY-RESPONDED)
    (asserts! (< stacks-block-height (get expires-at challenge-data)) ERR-CHALLENGE-EXPIRED)
    
    ;; Update challenge status
    (map-set activity-challenges challenge-id
      (merge challenge-data
        {
          completed: true,
          response: (some answer)
        }
      )
    )
    
    ;; Update stats
    (unwrap-panic (update-challenge-stats sender is-correct))
    (var-set active-challenges (- (var-get active-challenges) u1))
    
    (ok is-correct)
  )
)

;; Check inactivity status
(define-public (check-inactivity (human principal))
  (let
    (
      (activity-data (map-get? human-activity human))
      (stats-data (default-to
        {
          challenges-completed: u0,
          challenges-failed: u0,
          last-warning-sent: u0,
          status: u0
        }
        (map-get? activity-stats human)
      ))
    )
    (asserts! (contract-call? .Humanchain is-registered human) ERR-NOT-REGISTERED)
    
    (match activity-data
      data
      (let
        (
          (blocks-since-checkin (- stacks-block-height (get last-checkin data)))
          (warning-needed (> blocks-since-checkin ACTIVITY_CHECK_INTERVAL))
          (flagged-needed (> blocks-since-checkin MAX_INACTIVE_PERIOD))
        )
        (if flagged-needed
          (begin
            (map-set activity-stats human
              (merge stats-data { status: u2 })
            )
            (ok u2)
          )
          (if warning-needed
            (begin
              (map-set activity-stats human
                (merge stats-data { status: u1, last-warning-sent: stacks-block-height })
              )
              (ok u1)
            )
            (ok u0)
          )
        )
      )
      ERR-NOT-REGISTERED
    )
  )
)

;; Private helper functions
(define-private (calculate-checkin-points (consecutive uint))
  (if (> consecutive u30) u20
    (if (> consecutive u7) u15
      (if (> consecutive u3) u10
        u5
      )
    )
  )
)

(define-private (generate-challenge-data (challenge-type uint))
  (if (is-eq challenge-type u1)
    (+ (mod stacks-block-height u50) u10)
    (if (is-eq challenge-type u2)
      (mod stacks-block-height u100)
      (mod stacks-block-height u20)
    )
  )
)

(define-private (calculate-correct-answer (challenge-type uint) (data uint))
  (if (is-eq challenge-type u1)
    (+ data u10)
    (if (is-eq challenge-type u2)
      (* data u2)
      (+ data u1)
    )
  )
)

(define-private (is-valid-challenge-type (challenge-type uint))
  (and (>= challenge-type u1) (<= challenge-type u3))
)

(define-private (update-challenge-stats (human principal) (correct bool))
  (let
    (
      (stats-data (default-to
        {
          challenges-completed: u0,
          challenges-failed: u0,
          last-warning-sent: u0,
          status: u0
        }
        (map-get? activity-stats human)
      ))
    )
    (map-set activity-stats human
      (merge stats-data
        {
          challenges-completed: (if correct (+ (get challenges-completed stats-data) u1) (get challenges-completed stats-data)),
          challenges-failed: (if correct (get challenges-failed stats-data) (+ (get challenges-failed stats-data) u1)),
          status: u0
        }
      )
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-human-activity (human principal))
  (map-get? human-activity human)
)

(define-read-only (get-activity-stats (human principal))
  (map-get? activity-stats human)
)

(define-read-only (get-challenge (challenge-id uint))
  (map-get? activity-challenges challenge-id)
)

(define-read-only (get-system-stats)
  (ok {
    total-challenges: (var-get total-challenges),
    active-challenges: (var-get active-challenges)
  })
)

(define-read-only (is-human-active (human principal))
  (match (map-get? human-activity human)
    data
    (let
      (
        (blocks-since-checkin (- stacks-block-height (get last-checkin data)))
      )
      (<= blocks-since-checkin ACTIVITY_CHECK_INTERVAL)
    )
    false
  )
)
