;; Reader Achievement System  
;; Gamifies news platform engagement with badges, streaks, and milestone rewards
;; Unique feature: Encourages consistent reading habits through achievement mechanics

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u150))
(define-constant err-not-found (err u151))
(define-constant err-invalid-action (err u152))
(define-constant err-badge-not-earned (err u153))
(define-constant err-already-claimed (err u154))
(define-constant err-invalid-streak (err u155))
(define-constant err-insufficient-activity (err u156))

;; Achievement thresholds
(define-constant reader-badge-threshold u10)     ;; 10 articles read
(define-constant commentor-badge-threshold u25)  ;; 25 comments posted
(define-constant critic-badge-threshold u50)     ;; 50 ratings given
(define-constant sharer-badge-threshold u20)     ;; 20 shares made
(define-constant streak-badge-threshold u7)      ;; 7 consecutive days
(define-constant power-user-threshold u100)      ;; 100 total actions
(define-constant early-bird-threshold u5)        ;; Read 5 articles before 8 AM

;; Action type constants
(define-constant action-read "read")
(define-constant action-comment "comment")
(define-constant action-rate "rate")
(define-constant action-share "share")
(define-constant action-bookmark "bookmark")

;; Badge name constants
(define-constant badge-reader "avid-reader")
(define-constant badge-commentor "active-commentor")  
(define-constant badge-critic "thoughtful-critic")
(define-constant badge-sharer "content-sharer")
(define-constant badge-streak "streak-keeper")
(define-constant badge-power-user "power-user")
(define-constant badge-early-bird "early-bird")

;; Data variables
(define-data-var total-users uint u0)

;; Track user activity statistics
(define-map user-stats
  principal
  {
    reads: uint,
    comments: uint,
    ratings: uint,
    shares: uint,
    bookmarks: uint,
    total-actions: uint,
    last-activity-day: uint,
    current-streak: uint,
    longest-streak: uint,
    early-bird-count: uint,
    joined-date: uint
  }
)

;; Track earned badges for each user
(define-map user-badges
  { user: principal, badge: (string-ascii 30) }
  { 
    earned-at: uint,
    claimed: bool
  }
)

;; Track daily activity for streak calculation
(define-map daily-activity
  { user: principal, day: uint }
  { 
    actions-today: uint,
    first-read-block: uint
  }
)

;; Badge definitions with requirements
(define-map badge-definitions
  (string-ascii 30)
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    requirement-value: uint,
    badge-type: (string-ascii 20)
  }
)

;; Initialize badge definitions
(map-set badge-definitions badge-reader
  { name: "Avid Reader", description: "Read 10 or more articles", requirement-value: reader-badge-threshold, badge-type: "reads" })

(map-set badge-definitions badge-commentor  
  { name: "Active Commentor", description: "Posted 25 or more comments", requirement-value: commentor-badge-threshold, badge-type: "comments" })

(map-set badge-definitions badge-critic
  { name: "Thoughtful Critic", description: "Rated 50 or more articles", requirement-value: critic-badge-threshold, badge-type: "ratings" })

(map-set badge-definitions badge-sharer
  { name: "Content Sharer", description: "Shared 20 or more articles", requirement-value: sharer-badge-threshold, badge-type: "shares" })

(map-set badge-definitions badge-streak
  { name: "Streak Keeper", description: "Read for 7 consecutive days", requirement-value: streak-badge-threshold, badge-type: "streak" })

(map-set badge-definitions badge-power-user
  { name: "Power User", description: "Completed 100 total platform actions", requirement-value: power-user-threshold, badge-type: "total" })

(map-set badge-definitions badge-early-bird
  { name: "Early Bird", description: "Read 5 articles before 8 AM", requirement-value: early-bird-threshold, badge-type: "early" })

;; Record user action and update statistics
(define-public (record-action (action-type (string-ascii 20)))
  (let 
    (
      (user tx-sender)
      (current-day (/ stacks-block-height u144)) ;; ~144 blocks per day
      (current-stats (get-or-create-user-stats user))
      (today-activity (default-to { actions-today: u0, first-read-block: u0 }
        (map-get? daily-activity { user: user, day: current-day })))
      (is-early-bird (and (is-eq action-type action-read) (is-early-morning)))
    )
    
    (asserts! (is-valid-action action-type) err-invalid-action)
    
    ;; Update daily activity
    (map-set daily-activity
      { user: user, day: current-day }
      { 
        actions-today: (+ (get actions-today today-activity) u1),
        first-read-block: (if (and (is-eq action-type action-read) (is-eq (get first-read-block today-activity) u0))
          stacks-block-height
          (get first-read-block today-activity))
      })
    
    ;; Calculate new streak
    (let ((new-streak (calculate-new-streak user current-day current-stats)))
      
      ;; Update user statistics
      (map-set user-stats user
        {
          reads: (+ (get reads current-stats) (if (is-eq action-type action-read) u1 u0)),
          comments: (+ (get comments current-stats) (if (is-eq action-type action-comment) u1 u0)),
          ratings: (+ (get ratings current-stats) (if (is-eq action-type action-rate) u1 u0)),
          shares: (+ (get shares current-stats) (if (is-eq action-type action-share) u1 u0)),
          bookmarks: (+ (get bookmarks current-stats) (if (is-eq action-type action-bookmark) u1 u0)),
          total-actions: (+ (get total-actions current-stats) u1),
          last-activity-day: current-day,
          current-streak: new-streak,
          longest-streak: (max new-streak (get longest-streak current-stats)),
          early-bird-count: (+ (get early-bird-count current-stats) (if is-early-bird u1 u0)),
          joined-date: (get joined-date current-stats)
        })
      
      ;; Auto-award badges if thresholds are met
      (try! (auto-award-badges user))
      (ok true)
    )
  )
)

;; Claim earned badge
(define-public (claim-badge (badge-name (string-ascii 30)))
  (let 
    (
      (user tx-sender)
      (badge-key { user: user, badge: badge-name })
      (badge-info (unwrap! (map-get? user-badges badge-key) err-not-found))
    )
    
    (asserts! (not (get claimed badge-info)) err-already-claimed)
    
    ;; Mark badge as claimed
    (map-set user-badges badge-key
      (merge badge-info { claimed: true }))
    
    (ok badge-name)
  )
)

;; Get user statistics
(define-read-only (get-user-stats (user principal))
  (map-get? user-stats user)
)

;; Get user's earned badges
(define-read-only (get-user-badges (user principal))
  (let 
    (
      (reader-badge (map-get? user-badges { user: user, badge: badge-reader }))
      (commentor-badge (map-get? user-badges { user: user, badge: badge-commentor }))
      (critic-badge (map-get? user-badges { user: user, badge: badge-critic }))
      (sharer-badge (map-get? user-badges { user: user, badge: badge-sharer }))
      (streak-badge (map-get? user-badges { user: user, badge: badge-streak }))
      (power-badge (map-get? user-badges { user: user, badge: badge-power-user }))
      (early-badge (map-get? user-badges { user: user, badge: badge-early-bird }))
    )
    {
      avid-reader: reader-badge,
      active-commentor: commentor-badge,
      thoughtful-critic: critic-badge,
      content-sharer: sharer-badge,
      streak-keeper: streak-badge,
      power-user: power-badge,
      early-bird: early-badge
    }
  )
)

;; Check if user has specific badge
(define-read-only (has-badge (user principal) (badge-name (string-ascii 30)))
  (is-some (map-get? user-badges { user: user, badge: badge-name }))
)

;; Get badge progress for user
(define-read-only (get-badge-progress (user principal))
  (let ((stats (get-or-create-user-stats user)))
    {
      reader-progress: (min (get reads stats) reader-badge-threshold),
      commentor-progress: (min (get comments stats) commentor-badge-threshold),
      critic-progress: (min (get ratings stats) critic-badge-threshold),
      sharer-progress: (min (get shares stats) sharer-badge-threshold),
      streak-progress: (min (get current-streak stats) streak-badge-threshold),
      power-user-progress: (min (get total-actions stats) power-user-threshold),
      early-bird-progress: (min (get early-bird-count stats) early-bird-threshold)
    }
  )
)

;; Get leaderboard (top users by total actions)
(define-read-only (get-user-rank (user principal))
  (let ((stats (get-or-create-user-stats user)))
    {
      total-actions: (get total-actions stats),
      current-streak: (get current-streak stats),
      longest-streak: (get longest-streak stats)
    }
  )
)

;; Get badge definition
(define-read-only (get-badge-info (badge-name (string-ascii 30)))
  (map-get? badge-definitions badge-name)
)

;; Private helper functions

(define-private (get-or-create-user-stats (user principal))
  (default-to 
    {
      reads: u0, comments: u0, ratings: u0, shares: u0, bookmarks: u0,
      total-actions: u0, last-activity-day: u0, current-streak: u0,
      longest-streak: u0, early-bird-count: u0, joined-date: stacks-block-height
    }
    (map-get? user-stats user)
  )
)

(define-private (is-valid-action (action-type (string-ascii 20)))
  (or (is-eq action-type action-read)
      (is-eq action-type action-comment)
      (is-eq action-type action-rate)
      (is-eq action-type action-share)
      (is-eq action-type action-bookmark))
)

(define-private (calculate-new-streak (user principal) (current-day uint) (stats (tuple (reads uint) (comments uint) (ratings uint) (shares uint) (bookmarks uint) (total-actions uint) (last-activity-day uint) (current-streak uint) (longest-streak uint) (early-bird-count uint) (joined-date uint))))
  (let ((last-day (get last-activity-day stats)))
    (if (is-eq last-day u0)
      u1  ;; First day
      (if (is-eq current-day (+ last-day u1))
        (+ (get current-streak stats) u1)  ;; Consecutive day
        (if (is-eq current-day last-day)
          (get current-streak stats)  ;; Same day
          u1))))  ;; Streak broken, start new
)

(define-private (is-early-morning)
  ;; Simplified: consider blocks 0-30 of each day as "early morning"
  (let ((block-of-day (mod stacks-block-height u144)))
    (< block-of-day u30))
)

(define-private (auto-award-badges (user principal))
  (let ((stats (unwrap-panic (map-get? user-stats user))))
    (begin
      ;; Award reader badge
      (if (and (>= (get reads stats) reader-badge-threshold) 
               (is-none (map-get? user-badges { user: user, badge: badge-reader })))
        (map-set user-badges { user: user, badge: badge-reader } 
          { earned-at: stacks-block-height, claimed: false })
        true)
      
      ;; Award commentor badge
      (if (and (>= (get comments stats) commentor-badge-threshold)
               (is-none (map-get? user-badges { user: user, badge: badge-commentor })))
        (map-set user-badges { user: user, badge: badge-commentor } 
          { earned-at: stacks-block-height, claimed: false })
        true)
      
      ;; Award critic badge
      (if (and (>= (get ratings stats) critic-badge-threshold)
               (is-none (map-get? user-badges { user: user, badge: badge-critic })))
        (map-set user-badges { user: user, badge: badge-critic } 
          { earned-at: stacks-block-height, claimed: false })
        true)
      
      ;; Award sharer badge  
      (if (and (>= (get shares stats) sharer-badge-threshold)
               (is-none (map-get? user-badges { user: user, badge: badge-sharer })))
        (map-set user-badges { user: user, badge: badge-sharer } 
          { earned-at: stacks-block-height, claimed: false })
        true)
      
      ;; Award streak badge
      (if (and (>= (get current-streak stats) streak-badge-threshold)
               (is-none (map-get? user-badges { user: user, badge: badge-streak })))
        (map-set user-badges { user: user, badge: badge-streak } 
          { earned-at: stacks-block-height, claimed: false })
        true)
      
      ;; Award power user badge
      (if (and (>= (get total-actions stats) power-user-threshold)
               (is-none (map-get? user-badges { user: user, badge: badge-power-user })))
        (map-set user-badges { user: user, badge: badge-power-user } 
          { earned-at: stacks-block-height, claimed: false })
        true)
      
      ;; Award early bird badge
      (if (and (>= (get early-bird-count stats) early-bird-threshold)
               (is-none (map-get? user-badges { user: user, badge: badge-early-bird })))
        (map-set user-badges { user: user, badge: badge-early-bird } 
          { earned-at: stacks-block-height, claimed: false })
        true)
      
      (ok true)
    )
  )
)

(define-private (max (a uint) (b uint))
  (if (> a b) a b))

(define-private (min (a uint) (b uint))
  (if (< a b) a b))
