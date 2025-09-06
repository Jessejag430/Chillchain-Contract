;; Juror Availability Scheduling System
;; Allows jurors to set availability windows for case assignments

(define-constant ERR-UNAUTHORIZED (err u200))
(define-constant ERR-NOT-REGISTERED (err u201))
(define-constant ERR-INVALID-TIME-RANGE (err u202))
(define-constant ERR-AVAILABILITY-EXISTS (err u203))
(define-constant ERR-AVAILABILITY-NOT-FOUND (err u204))
(define-constant ERR-MAX-AVAILABILITY-REACHED (err u205))

;; Maximum availability windows per juror
(define-constant MAX-AVAILABILITY-WINDOWS u10)

;; Track juror availability windows
(define-map juror-availability {juror: principal, window-id: uint}
  {
    start-block: uint,
    end-block: uint,
    days-of-week: uint, ;; Bitmask: Mon=1, Tue=2, Wed=4, Thu=8, Fri=16, Sat=32, Sun=64
    is-active: bool
  }
)

;; Track availability window counts per juror
(define-map availability-counts principal uint)

;; Track next window ID per juror
(define-map next-window-id principal uint)

;; Set availability window for a juror
(define-public (set-availability (start-block uint) (end-block uint) (days-of-week uint))
  (let (
    (caller tx-sender)
    (current-count (default-to u0 (map-get? availability-counts caller)))
    (next-id (default-to u1 (map-get? next-window-id caller)))
  )
    ;; Check if caller is registered as juror (reference to main contract)
    (asserts! (is-some (contract-call? .Jurybit get-juror caller)) ERR-NOT-REGISTERED)
    
    ;; Validate time range
    (asserts! (< start-block end-block) ERR-INVALID-TIME-RANGE)
    (asserts! (>= start-block stacks-block-height) ERR-INVALID-TIME-RANGE)
    
    ;; Check availability limit
    (asserts! (< current-count MAX-AVAILABILITY-WINDOWS) ERR-MAX-AVAILABILITY-REACHED)
    
    ;; Validate days of week (must be between 1-127, representing Mon-Sun)
    (asserts! (and (> days-of-week u0) (<= days-of-week u127)) ERR-INVALID-TIME-RANGE)
    
    ;; Add availability window
    (map-set juror-availability {juror: caller, window-id: next-id}
      {
        start-block: start-block,
        end-block: end-block,
        days-of-week: days-of-week,
        is-active: true
      }
    )
    
    ;; Update counters
    (map-set availability-counts caller (+ current-count u1))
    (map-set next-window-id caller (+ next-id u1))
    
    (ok next-id)
  )
)

;; Remove availability window
(define-public (remove-availability (window-id uint))
  (let (
    (caller tx-sender)
    (availability-key {juror: caller, window-id: window-id})
    (availability-data (unwrap! (map-get? juror-availability availability-key) ERR-AVAILABILITY-NOT-FOUND))
    (current-count (default-to u0 (map-get? availability-counts caller)))
  )
    ;; Remove availability window
    (map-delete juror-availability availability-key)
    
    ;; Update counter
    (if (> current-count u0)
      (map-set availability-counts caller (- current-count u1))
      true
    )
    
    (ok true)
  )
)

;; Toggle availability window active status
(define-public (toggle-availability (window-id uint) (active bool))
  (let (
    (caller tx-sender)
    (availability-key {juror: caller, window-id: window-id})
    (availability-data (unwrap! (map-get? juror-availability availability-key) ERR-AVAILABILITY-NOT-FOUND))
  )
    ;; Update availability status
    (map-set juror-availability availability-key
      (merge availability-data {is-active: active})
    )
    
    (ok true)
  )
)

;; Check if juror is available at current time
(define-read-only (is-juror-available (juror principal))
  (let (
    (current-block stacks-block-height)
    (current-day (mod (/ current-block u144) u7)) ;; Approximate day of week (0-6)
    (day-bitmask (pow u2 current-day))
    (availability-count (default-to u0 (map-get? availability-counts juror)))
  )
    ;; If no availability windows set, assume always available
    (if (is-eq availability-count u0)
      true
      (check-availability-windows juror u1 (+ availability-count u1) current-block day-bitmask)
    )
  )
)

;; Helper function to check all availability windows
(define-private (check-availability-windows (juror principal) (window-id uint) (max-window uint) (current-block uint) (day-bitmask uint))
  (if (>= window-id max-window)
    false
    (let (
      (availability-data (map-get? juror-availability {juror: juror, window-id: window-id}))
    )
      (match availability-data
        window
        (if (and 
            (get is-active window)
            (>= current-block (get start-block window))
            (<= current-block (get end-block window))
            (> (bit-and (get days-of-week window) day-bitmask) u0))
          true
          (check-availability-windows juror (+ window-id u1) max-window current-block day-bitmask)
        )
        (check-availability-windows juror (+ window-id u1) max-window current-block day-bitmask)
      )
    )
  )
)

;; Get all availability windows for a juror
(define-read-only (get-juror-availability (juror principal))
  (let (
    (availability-count (default-to u0 (map-get? availability-counts juror)))
  )
    (if (is-eq availability-count u0)
      (list )
      (get-availability-list juror u1 (+ availability-count u1))
    )
  )
)

;; Helper to build availability list
(define-private (get-availability-list (juror principal) (window-id uint) (max-window uint))
  (if (>= window-id max-window)
    (list )
    (let (
      (availability-data (map-get? juror-availability {juror: juror, window-id: window-id}))
      (rest-of-list (get-availability-list juror (+ window-id u1) max-window))
    )
      (match availability-data
        window
        (unwrap-panic (as-max-len? (append (list window) rest-of-list) u10))
        rest-of-list
      )
    )
  )
)

;; Get availability window count
(define-read-only (get-availability-count (juror principal))
  (default-to u0 (map-get? availability-counts juror))
)

;; Check if specific window exists
(define-read-only (get-availability-window (juror principal) (window-id uint))
  (map-get? juror-availability {juror: juror, window-id: window-id})
)

;; Get list of currently available jurors (up to 20 for efficiency)
(define-read-only (get-available-jurors-list (max-jurors uint))
  (filter-available-jurors u0 (if (> max-jurors u20) u20 max-jurors))
)

;; Helper to filter available jurors
(define-private (filter-available-jurors (start-index uint) (max-count uint))
  (let (
    (total-jurors (contract-call? .Jurybit get-total-jurors))
  )
    (if (or (>= start-index total-jurors) (is-eq max-count u0))
      (list )
      (let (
        (juror (contract-call? .Jurybit get-juror-by-index start-index))
        (rest-of-list (filter-available-jurors (+ start-index u1) (- max-count u1)))
      )
        (if (is-juror-available juror)
          (unwrap-panic (as-max-len? (append (list juror) rest-of-list) u20))
          rest-of-list
        )
      )
    )
  )
)

;; Simple power function for day bitmask calculation
(define-private (pow (base uint) (exp uint))
  (if (is-eq exp u0)
    u1
    (if (is-eq exp u1)
      base
      (* base (pow base (- exp u1)))
    )
  )
)

;; Simple bitwise AND operation
(define-private (bit-and (a uint) (b uint))
  (mod (/ (* (mod a u2) (mod b u2)) u1) u2)
)
