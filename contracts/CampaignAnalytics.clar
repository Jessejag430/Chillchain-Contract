;; Campaign Performance Analytics - Real-time metrics and success prediction for campaigns
;; Provides performance tracking, engagement analytics, and predictive insights

;; Error constants
(define-constant err-campaign-not-found (err u300))
(define-constant err-unauthorized (err u301))
(define-constant err-invalid-period (err u302))
(define-constant err-analytics-not-found (err u303))
(define-constant err-invalid-metric (err u304))

;; Constants for analytics calculations
(define-constant blocks-per-day u144)  ;; Approximate blocks per day
(define-constant performance-window-blocks u1008)  ;; 7 days for trending analysis
(define-constant success-threshold-percentage u75)  ;; 75% target reached = likely success

;; Campaign performance metrics structure
(define-map campaign-performance
  { campaign-id: uint }
  {
    daily-donation-velocity: uint,    ;; Average donations per day
    peak-donation-day: uint,          ;; Highest single-day donations
    donor-retention-score: uint,      ;; Percentage of repeat donors
    engagement-score: uint,           ;; Comments + updates activity score
    momentum-indicator: uint,         ;; Trending up/down/stable (1/2/3)
    success-probability: uint,        ;; Predicted success percentage (0-100)
    last-updated: uint
  }
)

;; Daily performance snapshots for trend analysis
(define-map daily-snapshots
  { campaign-id: uint, day: uint }
  {
    donations-amount: uint,
    new-donors-count: uint,
    total-donors: uint,
    comments-count: uint,
    updates-count: uint,
    cumulative-raised: uint
  }
)

;; Campaign engagement tracking
(define-map engagement-metrics
  { campaign-id: uint }
  {
    total-unique-donors: uint,
    average-donation-size: uint,
    comment-engagement-rate: uint,    ;; Comments per donor percentage
    update-frequency: uint,           ;; Updates per week
    social-virality-score: uint,      ;; Based on donor growth pattern
    creator-responsiveness: uint      ;; Response rate to comments/questions
  }
)

;; Comparative benchmarks for similar campaigns
(define-map benchmark-categories
  { category: (string-ascii 50) }
  {
    average-success-rate: uint,
    median-funding-time: uint,
    typical-donor-count: uint,
    average-donation-size: uint
  }
)

;; Performance alerts and recommendations
(define-map campaign-insights
  { campaign-id: uint }
  {
    performance-trend: (string-ascii 20),     ;; "improving", "declining", "stable"
    key-recommendations: (string-ascii 200),   ;; Top 3 actionable insights
    risk-factors: (string-ascii 200),          ;; Potential issues to address
    optimization-score: uint,                  ;; Overall optimization rating (0-100)
    next-review-block: uint
  }
)

;; Track milestone prediction accuracy for learning
(define-map prediction-history
  { campaign-id: uint, prediction-block: uint }
  {
    predicted-success: uint,
    actual-outcome: (optional bool),
    accuracy-score: (optional uint)
  }
)

;; Analytics data variables
(define-data-var total-campaigns-analyzed uint u0)
(define-data-var analytics-update-frequency uint u144)  ;; Update daily

;; Read-only functions

;; Get campaign performance metrics
(define-read-only (get-campaign-performance (campaign-id uint))
  (map-get? campaign-performance { campaign-id: campaign-id })
)

;; Get daily snapshot for specific day
(define-read-only (get-daily-snapshot (campaign-id uint) (day uint))
  (map-get? daily-snapshots { campaign-id: campaign-id, day: day })
)

;; Get engagement metrics
(define-read-only (get-engagement-metrics (campaign-id uint))
  (map-get? engagement-metrics { campaign-id: campaign-id })
)

;; Get campaign insights and recommendations
(define-read-only (get-campaign-insights (campaign-id uint))
  (map-get? campaign-insights { campaign-id: campaign-id })
)

;; Get benchmark data for category
(define-read-only (get-benchmark-category (category (string-ascii 50)))
  (map-get? benchmark-categories { category: category })
)

;; Calculate current success probability
(define-read-only (calculate-success-probability (campaign-id uint) (current-raised uint) (target-amount uint) (days-elapsed uint))
  (let
    (
      (progress-percentage (/ (* current-raised u100) target-amount))
      (time-factor (if (> days-elapsed u0) (/ u100 days-elapsed) u100))
      (velocity-score (min u40 (* progress-percentage time-factor)))
      (engagement-bonus (get-engagement-bonus campaign-id))
      (total-probability (min u100 (+ velocity-score engagement-bonus)))
    )
    total-probability
  )
)

;; Helper function to calculate engagement bonus
(define-read-only (get-engagement-bonus (campaign-id uint))
  (match (get-engagement-metrics campaign-id)
    metrics
      (let
        (
          (comment-score (min u15 (/ (get comment-engagement-rate metrics) u5)))
          (update-score (min u15 (get update-frequency metrics)))
          (virality-score (min u20 (/ (get social-virality-score metrics) u5)))
        )
        (+ comment-score update-score virality-score)
      )
    u0
  )
)

;; Get performance trend analysis
(define-read-only (analyze-performance-trend (campaign-id uint))
  (let
    (
      (current-snapshot (get-current-day-snapshot campaign-id))
      (previous-snapshot (get-daily-snapshot campaign-id (- (get-current-day) u1)))
    )
    (match current-snapshot
      current
        (match previous-snapshot
          previous
            (let
              (
                (donation-change (- (get donations-amount current) (get donations-amount previous)))
                (donor-change (- (get new-donors-count current) (get new-donors-count previous)))
              )
              (if (and (> donation-change u0) (> donor-change u0))
                "improving"
                (if (and (< donation-change u0) (< donor-change u0))
                  "declining"
                  "stable"
                )
              )
            )
          "insufficient-data"
        )
      "no-data"
    )
  )
)

;; Get current day number (simplified)
(define-read-only (get-current-day)
  (/ stacks-block-height blocks-per-day)
)

;; Get current day snapshot (mock function - would integrate with main contract)
(define-read-only (get-current-day-snapshot (campaign-id uint))
  (get-daily-snapshot campaign-id (get-current-day))
)

;; Public functions

;; Initialize analytics tracking for a campaign
(define-public (initialize-campaign-analytics (campaign-id uint) (target-amount uint) (category (string-ascii 50)))
  (let
    (
      (current-day (get-current-day))
    )
    ;; Initialize performance tracking
    (map-set campaign-performance
      { campaign-id: campaign-id }
      {
        daily-donation-velocity: u0,
        peak-donation-day: u0,
        donor-retention-score: u0,
        engagement-score: u0,
        momentum-indicator: u3, ;; Start as stable
        success-probability: u50, ;; Start with neutral probability
        last-updated: stacks-block-height
      }
    )
    
    ;; Initialize engagement metrics
    (map-set engagement-metrics
      { campaign-id: campaign-id }
      {
        total-unique-donors: u0,
        average-donation-size: u0,
        comment-engagement-rate: u0,
        update-frequency: u0,
        social-virality-score: u0,
        creator-responsiveness: u0
      }
    )
    
    ;; Initialize first daily snapshot
    (map-set daily-snapshots
      { campaign-id: campaign-id, day: current-day }
      {
        donations-amount: u0,
        new-donors-count: u0,
        total-donors: u0,
        comments-count: u0,
        updates-count: u0,
        cumulative-raised: u0
      }
    )
    
    ;; Update global counter
    (var-set total-campaigns-analyzed (+ (var-get total-campaigns-analyzed) u1))
    
    (ok true)
  )
)

;; Update daily performance snapshot
(define-public (update-daily-snapshot (campaign-id uint) (donations-amount uint) (new-donors uint) (total-donors uint) (comments-count uint) (updates-count uint) (cumulative-raised uint))
  (let
    (
      (current-day (get-current-day))
      (performance (unwrap! (get-campaign-performance campaign-id) err-analytics-not-found))
    )
    ;; Update daily snapshot
    (map-set daily-snapshots
      { campaign-id: campaign-id, day: current-day }
      {
        donations-amount: donations-amount,
        new-donors-count: new-donors,
        total-donors: total-donors,
        comments-count: comments-count,
        updates-count: updates-count,
        cumulative-raised: cumulative-raised
      }
    )
    
    ;; Update performance metrics
    (let
      (
        (new-velocity (/ donations-amount blocks-per-day))
        (new-peak (if (> donations-amount (get peak-donation-day performance)) 
                    donations-amount 
                    (get peak-donation-day performance)))
      )
      (map-set campaign-performance
        { campaign-id: campaign-id }
        (merge performance {
          daily-donation-velocity: new-velocity,
          peak-donation-day: new-peak,
          last-updated: stacks-block-height
        })
      )
    )
    
    (ok true)
  )
)

;; Update engagement metrics
(define-public (update-engagement-metrics (campaign-id uint) (unique-donors uint) (avg-donation uint) (comment-rate uint) (update-freq uint) (virality uint) (responsiveness uint))
  (begin
    (map-set engagement-metrics
      { campaign-id: campaign-id }
      {
        total-unique-donors: unique-donors,
        average-donation-size: avg-donation,
        comment-engagement-rate: comment-rate,
        update-frequency: update-freq,
        social-virality-score: virality,
        creator-responsiveness: responsiveness
      }
    )
    (ok true)
  )
)

;; Generate campaign insights and recommendations
(define-public (generate-campaign-insights (campaign-id uint) (target-amount uint))
  (let
    (
      (performance (unwrap! (get-campaign-performance campaign-id) err-analytics-not-found))
      (engagement (unwrap! (get-engagement-metrics campaign-id) err-analytics-not-found))
      (trend (analyze-performance-trend campaign-id))
      (current-snapshot (unwrap! (get-current-day-snapshot campaign-id) err-analytics-not-found))
      (success-prob (calculate-success-probability campaign-id (get cumulative-raised current-snapshot) target-amount u7))
    )
    
    ;; Generate recommendations based on performance
    (let
      (
        (recommendations (generate-recommendations performance engagement trend))
        (risk-factors (identify-risk-factors performance engagement success-prob))
        (optimization-score (calculate-optimization-score performance engagement))
      )
      (map-set campaign-insights
        { campaign-id: campaign-id }
        {
          performance-trend: trend,
          key-recommendations: recommendations,
          risk-factors: risk-factors,
          optimization-score: optimization-score,
          next-review-block: (+ stacks-block-height (var-get analytics-update-frequency))
        }
      )
    )
    
    ;; Update success probability
    (map-set campaign-performance
      { campaign-id: campaign-id }
      (merge performance { success-probability: success-prob })
    )
    
    (ok success-prob)
  )
)

;; Helper function to generate recommendations
(define-private (generate-recommendations (performance { daily-donation-velocity: uint, peak-donation-day: uint, donor-retention-score: uint, engagement-score: uint, momentum-indicator: uint, success-probability: uint, last-updated: uint }) (engagement { total-unique-donors: uint, average-donation-size: uint, comment-engagement-rate: uint, update-frequency: uint, social-virality-score: uint, creator-responsiveness: uint }) (trend (string-ascii 20)))
  (if (< (get update-frequency engagement) u2)
    "Increase update frequency, engage with donors, optimize social sharing"
    (if (< (get comment-engagement-rate engagement) u10)
      "Respond to comments, create engaging content, share progress updates"
      "Maintain momentum, leverage peak performance, expand outreach efforts"
    )
  )
)

;; Helper function to identify risk factors
(define-private (identify-risk-factors (performance { daily-donation-velocity: uint, peak-donation-day: uint, donor-retention-score: uint, engagement-score: uint, momentum-indicator: uint, success-probability: uint, last-updated: uint }) (engagement { total-unique-donors: uint, average-donation-size: uint, comment-engagement-rate: uint, update-frequency: uint, social-virality-score: uint, creator-responsiveness: uint }) (success-prob uint))
  (if (< success-prob u30)
    "Low success probability, declining engagement, insufficient outreach"
    (if (< (get social-virality-score engagement) u20)
      "Limited social reach, low donor diversity, engagement plateau"
      "Minimal risks identified, maintain current strategy"
    )
  )
)

;; Helper function to calculate optimization score
(define-private (calculate-optimization-score (performance { daily-donation-velocity: uint, peak-donation-day: uint, donor-retention-score: uint, engagement-score: uint, momentum-indicator: uint, success-probability: uint, last-updated: uint }) (engagement { total-unique-donors: uint, average-donation-size: uint, comment-engagement-rate: uint, update-frequency: uint, social-virality-score: uint, creator-responsiveness: uint }))
  (let
    (
      (velocity-score (min u25 (/ (get daily-donation-velocity performance) u10)))
      (engagement-score (min u25 (/ (get comment-engagement-rate engagement) u4)))
      (update-score (min u25 (* (get update-frequency engagement) u5)))
      (responsiveness-score (min u25 (/ (get creator-responsiveness engagement) u4)))
    )
    (+ velocity-score engagement-score update-score responsiveness-score)
  )
)
