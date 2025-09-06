;; Pool Analytics Dashboard Contract
;; Provides comprehensive analytics and insights for crowdfunding pools

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u400))
(define-constant ERR_INVALID_PERIOD (err u401))
(define-constant ERR_NO_DATA (err u402))
(define-constant ERR_INVALID_POOL (err u403))

;; Contract owner
(define-constant CONTRACT_OWNER tx-sender)

;; Analytics tracking variables
(define-data-var analytics-enabled bool true)
(define-data-var tracking-period uint u1440) ;; ~10 days in blocks

;; Pool performance metrics
(define-map pool-analytics
    uint ;; pool-id
    {
        total-contributions: uint,
        unique-contributors: uint,
        avg-contribution: uint,
        funding-velocity: uint,
        success-probability: uint,
        peak-activity-block: uint,
        engagement-score: uint
    }
)

;; Contributor behavior patterns
(define-map contributor-analytics
    principal
    {
        total-pools-contributed: uint,
        avg-contribution-amount: uint,
        success-rate: uint,
        preferred-pool-size: uint,
        contribution-pattern: (string-ascii 20),
        loyalty-score: uint
    }
)

;; Daily platform statistics
(define-map daily-stats
    uint ;; day (block-height / 1440)
    {
        pools-created: uint,
        total-contributions: uint,
        successful-pools: uint,
        total-contributors: uint,
        platform-volume: uint
    }
)

;; Pool category performance
(define-map category-analytics
    (string-ascii 50) ;; category based on pool description
    {
        total-pools: uint,
        success-rate: uint,
        avg-target-amount: uint,
        avg-funding-time: uint,
        top-performer-pool: uint
    }
)

;; Time-based trending data
(define-map trending-pools
    uint ;; time-period
    {
        trending-pool-ids: (list 10 uint),
        momentum-scores: (list 10 uint),
        period-start: uint,
        period-end: uint
    }
)

;; Record pool creation analytics
(define-public (record-pool-creation (pool-id uint) (creator principal) (target-amount uint))
    (let ((current-day (/ stacks-block-height u1440)))
        (asserts! (var-get analytics-enabled) ERR_NOT_AUTHORIZED)
        
        ;; Update daily stats
        (let ((daily-data (default-to 
            {pools-created: u0, total-contributions: u0, successful-pools: u0, 
             total-contributors: u0, platform-volume: u0}
            (map-get? daily-stats current-day))))
            (map-set daily-stats current-day
                (merge daily-data {pools-created: (+ (get pools-created daily-data) u1)})
            )
        )
        
        ;; Initialize pool analytics
        (map-set pool-analytics pool-id
            {
                total-contributions: u0,
                unique-contributors: u0,
                avg-contribution: u0,
                funding-velocity: u0,
                success-probability: u50, ;; Default 50%
                peak-activity-block: stacks-block-height,
                engagement-score: u0
            }
        )
        (ok true)
    )
)

;; Record contribution analytics
(define-public (record-contribution (pool-id uint) (contributor principal) (amount uint))
    (let (
        (current-day (/ stacks-block-height u1440))
        (pool-data (default-to 
            {total-contributions: u0, unique-contributors: u0, avg-contribution: u0,
             funding-velocity: u0, success-probability: u50, peak-activity-block: u0, engagement-score: u0}
            (map-get? pool-analytics pool-id)))
        (contributor-data (default-to 
            {total-pools-contributed: u0, avg-contribution-amount: u0, success-rate: u50,
             preferred-pool-size: u0, contribution-pattern: "regular", loyalty-score: u0}
            (map-get? contributor-analytics contributor)))
    )
        (asserts! (var-get analytics-enabled) ERR_NOT_AUTHORIZED)
        
        ;; Update pool analytics
        (let (
            (new-total-contributions (+ (get total-contributions pool-data) u1))
            (new-avg-contribution (/ (+ (* (get avg-contribution pool-data) (get total-contributions pool-data)) amount) 
                                   new-total-contributions))
        )
            (map-set pool-analytics pool-id
                (merge pool-data {
                    total-contributions: new-total-contributions,
                    unique-contributors: (+ (get unique-contributors pool-data) u1),
                    avg-contribution: new-avg-contribution,
                    funding-velocity: (calculate-funding-velocity pool-id),
                    peak-activity-block: stacks-block-height,
                    engagement-score: (+ (get engagement-score pool-data) u10)
                })
            )
        )
        
        ;; Update contributor analytics
        (let (
            (new-pools-contributed (+ (get total-pools-contributed contributor-data) u1))
            (new-avg-amount (/ (+ (* (get avg-contribution-amount contributor-data) (get total-pools-contributed contributor-data)) amount)
                             new-pools-contributed))
        )
            (map-set contributor-analytics contributor
                (merge contributor-data {
                    total-pools-contributed: new-pools-contributed,
                    avg-contribution-amount: new-avg-amount,
                    loyalty-score: (+ (get loyalty-score contributor-data) u5)
                })
            )
        )
        
        ;; Update daily stats
        (let ((daily-data (default-to 
            {pools-created: u0, total-contributions: u0, successful-pools: u0,
             total-contributors: u0, platform-volume: u0}
            (map-get? daily-stats current-day))))
            (map-set daily-stats current-day
                (merge daily-data {
                    total-contributions: (+ (get total-contributions daily-data) u1),
                    platform-volume: (+ (get platform-volume daily-data) amount)
                })
            )
        )
        (ok true)
    )
)

;; Record successful pool completion
(define-public (record-pool-success (pool-id uint) (final-amount uint))
    (let (
        (current-day (/ stacks-block-height u1440))
        (pool-data (unwrap! (map-get? pool-analytics pool-id) ERR_INVALID_POOL))
    )
        (asserts! (var-get analytics-enabled) ERR_NOT_AUTHORIZED)
        
        ;; Update pool success probability for similar pools
        (map-set pool-analytics pool-id
            (merge pool-data {success-probability: u100})
        )
        
        ;; Update daily stats
        (let ((daily-data (default-to 
            {pools-created: u0, total-contributions: u0, successful-pools: u0,
             total-contributors: u0, platform-volume: u0}
            (map-get? daily-stats current-day))))
            (map-set daily-stats current-day
                (merge daily-data {successful-pools: (+ (get successful-pools daily-data) u1)})
            )
        )
        (ok true)
    )
)

;; Calculate funding velocity for a pool
(define-private (calculate-funding-velocity (pool-id uint))
    (let ((pool-data (unwrap-panic (map-get? pool-analytics pool-id))))
        (if (> (get total-contributions pool-data) u0)
            (/ (* (get total-contributions pool-data) u144) ;; contributions per day
               (+ (- stacks-block-height (get peak-activity-block pool-data)) u1))
            u0
        )
    )
)

;; Get pool performance insights
(define-read-only (get-pool-insights (pool-id uint))
    (match (map-get? pool-analytics pool-id)
        analytics (some {
            performance-grade: (if (>= (get success-probability analytics) u80) "A"
                              (if (>= (get success-probability analytics) u60) "B"
                               (if (>= (get success-probability analytics) u40) "C" "D"))),
            funding-momentum: (if (>= (get funding-velocity analytics) u10) "High"
                             (if (>= (get funding-velocity analytics) u5) "Medium" "Low")),
            engagement-level: (if (>= (get engagement-score analytics) u100) "Very High"
                             (if (>= (get engagement-score analytics) u50) "High"
                              (if (>= (get engagement-score analytics) u20) "Medium" "Low"))),
            contributor-diversity: (get unique-contributors analytics),
            avg-contribution: (get avg-contribution analytics)
        })
        none
    )
)

;; Get contributor behavior profile
(define-read-only (get-contributor-profile (contributor principal))
    (match (map-get? contributor-analytics contributor)
        profile (some {
            investment-style: (if (>= (get avg-contribution-amount profile) u1000000) "High-Value"
                             (if (>= (get avg-contribution-amount profile) u100000) "Medium-Value" "Micro-Investor")),
            experience-level: (if (>= (get total-pools-contributed profile) u20) "Expert"
                             (if (>= (get total-pools-contributed profile) u10) "Experienced"
                              (if (>= (get total-pools-contributed profile) u3) "Intermediate" "Beginner"))),
            success-rate: (get success-rate profile),
            loyalty-tier: (if (>= (get loyalty-score profile) u100) "Gold"
                         (if (>= (get loyalty-score profile) u50) "Silver" "Bronze")),
            contribution-pattern: (get contribution-pattern profile)
        })
        none
    )
)

;; Get platform statistics for a specific day
(define-read-only (get-daily-platform-stats (day uint))
    (map-get? daily-stats day)
)

;; Calculate platform success rate for a period
(define-read-only (calculate-success-rate (start-day uint) (end-day uint))
    (let (
        (period-stats (fold sum-period-stats 
            (generate-day-range start-day end-day)
            {total-pools: u0, successful-pools: u0}))
    )
        (if (> (get total-pools period-stats) u0)
            (/ (* (get successful-pools period-stats) u100) (get total-pools period-stats))
            u0
        )
    )
)

;; Helper function to generate day range
(define-private (generate-day-range (start uint) (end uint))
    (if (<= start end)
        (list start)
        (list)
    )
)

;; Helper function to sum period statistics
(define-private (sum-period-stats (day uint) (acc {total-pools: uint, successful-pools: uint}))
    (let ((day-stats (default-to 
        {pools-created: u0, total-contributions: u0, successful-pools: u0,
         total-contributors: u0, platform-volume: u0}
        (map-get? daily-stats day))))
        {
            total-pools: (+ (get total-pools acc) (get pools-created day-stats)),
            successful-pools: (+ (get successful-pools acc) (get successful-pools day-stats))
        }
    )
)

;; Get trending pools for current period
(define-read-only (get-trending-analysis)
    (let ((current-period (/ stacks-block-height (var-get tracking-period))))
        (match (map-get? trending-pools current-period)
            trending (some {
                trending-pools: (get trending-pool-ids trending),
                momentum-scores: (get momentum-scores trending),
                analysis-period: (get period-start trending)
            })
            none
        )
    )
)

;; Admin function to toggle analytics
(define-public (toggle-analytics (enabled bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (var-set analytics-enabled enabled)
        (ok enabled)
    )
)

;; Admin function to set tracking period
(define-public (set-tracking-period (period uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (asserts! (> period u0) ERR_INVALID_PERIOD)
        (var-set tracking-period period)
        (ok period)
    )
)

;; Get comprehensive platform analytics
(define-read-only (get-platform-overview)
    (let ((current-day (/ stacks-block-height u1440)))
        (match (map-get? daily-stats current-day)
            today-stats (some {
                todays-pools: (get pools-created today-stats),
                todays-volume: (get platform-volume today-stats),
                todays-contributions: (get total-contributions today-stats),
                analytics-enabled: (var-get analytics-enabled),
                tracking-period: (var-get tracking-period)
            })
            none
        )
    )
)

;; Get analytics status
(define-read-only (get-analytics-config)
    {
        enabled: (var-get analytics-enabled),
        tracking-period: (var-get tracking-period),
        current-block: stacks-block-height,
        current-day: (/ stacks-block-height u1440)
    }
)