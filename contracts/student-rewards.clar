;; Student Performance Rewards System
;; Incentivizes academic achievement through loan benefits and rewards

(define-constant ERR-NOT-AUTHORIZED (err u400))
(define-constant ERR-STUDENT-NOT-FOUND (err u401))
(define-constant ERR-PERFORMANCE-EXISTS (err u402))
(define-constant ERR-INVALID-GPA (err u403))
(define-constant ERR-INSUFFICIENT-BALANCE (err u404))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u405))
(define-constant ERR-MILESTONE-NOT-ACHIEVED (err u406))

;; Performance thresholds for rewards
(define-constant GPA-EXCELLENT u375) ;; 3.75 GPA (scaled by 100)
(define-constant GPA-GOOD u325) ;; 3.25 GPA (scaled by 100)
(define-constant GPA-SATISFACTORY u275) ;; 2.75 GPA (scaled by 100)

;; Reward rates and discounts
(define-constant EXCELLENT-DISCOUNT u15) ;; 15% interest discount
(define-constant GOOD-DISCOUNT u10) ;; 10% interest discount
(define-constant SATISFACTORY-DISCOUNT u5) ;; 5% interest discount
(define-constant GRADUATION-BONUS u1000) ;; Graduation bonus amount
(define-constant SEMESTER-COMPLETION-REWARD u200) ;; Per semester reward

(define-data-var contract-owner principal tx-sender)
(define-data-var total-rewards-pool uint u50000) ;; Total rewards available
(define-data-var rewards-distributed uint u0)

;; Track student academic performance
(define-map student-performance
    principal
    {
        current-gpa: uint, ;; GPA scaled by 100 (375 = 3.75)
        completed-semesters: uint,
        total-semesters: uint,
        graduation-status: (string-ascii 20), ;; "enrolled", "graduated", "dropped"
        last-update: uint,
        performance-level: (string-ascii 15), ;; "excellent", "good", "satisfactory", "poor"
        total-earned-rewards: uint
    }
)

;; Track milestone achievements
(define-map achievement-milestones
    { student: principal, milestone: (string-ascii 30) }
    {
        achieved-date: uint,
        reward-amount: uint,
        claimed: bool
    }
)

;; Track reward claims
(define-map reward-claims
    principal
    {
        total-claimed: uint,
        last-claim: uint,
        interest-discount-active: bool,
        discount-percentage: uint,
        discount-expires: uint
    }
)

;; Calculate performance level based on GPA
(define-private (calculate-performance-level (gpa uint))
    (if (>= gpa GPA-EXCELLENT)
        "excellent"
        (if (>= gpa GPA-GOOD)
            "good"
            (if (>= gpa GPA-SATISFACTORY)
                "satisfactory"
                "poor"))))

;; Calculate interest discount based on performance
(define-private (calculate-interest-discount (performance-level (string-ascii 15)))
    (if (is-eq performance-level "excellent")
        EXCELLENT-DISCOUNT
        (if (is-eq performance-level "good")
            GOOD-DISCOUNT
            (if (is-eq performance-level "satisfactory")
                SATISFACTORY-DISCOUNT
                u0))))

;; Register student for performance tracking
(define-public (register-student-performance (total-semesters uint))
    (let ((student tx-sender))
        (asserts! (is-none (map-get? student-performance student)) ERR-PERFORMANCE-EXISTS)
        (asserts! (> total-semesters u0) ERR-INVALID-GPA)
        
        (map-set student-performance student {
            current-gpa: u0,
            completed-semesters: u0,
            total-semesters: total-semesters,
            graduation-status: "enrolled",
            last-update: stacks-block-height,
            performance-level: "poor",
            total-earned-rewards: u0
        })
        
        (map-set reward-claims student {
            total-claimed: u0,
            last-claim: u0,
            interest-discount-active: false,
            discount-percentage: u0,
            discount-expires: u0
        })
        (ok true)))

;; Update student GPA and semester completion
(define-public (update-student-performance (student principal) (gpa uint) (completed-semesters uint))
    (let (
        (performance-data (unwrap! (map-get? student-performance student) ERR-STUDENT-NOT-FOUND))
        (performance-level (calculate-performance-level gpa))
        (semester-reward (if (> completed-semesters (get completed-semesters performance-data))
            SEMESTER-COMPLETION-REWARD u0))
    )
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (<= gpa u400) ERR-INVALID-GPA) ;; Max GPA 4.0 (scaled)
        (asserts! (<= completed-semesters (get total-semesters performance-data)) ERR-INVALID-GPA)
        
        ;; Update performance data
        (map-set student-performance student {
            current-gpa: gpa,
            completed-semesters: completed-semesters,
            total-semesters: (get total-semesters performance-data),
            graduation-status: (if (is-eq completed-semesters (get total-semesters performance-data))
                "graduated" "enrolled"),
            last-update: stacks-block-height,
            performance-level: performance-level,
            total-earned-rewards: (+ (get total-earned-rewards performance-data) semester-reward)
        })
        
        ;; Award semester completion if applicable
        (if (> semester-reward u0)
            (map-set achievement-milestones 
                { student: student, milestone: "semester-completion" }
                {
                    achieved-date: stacks-block-height,
                    reward-amount: semester-reward,
                    claimed: false
                })
            true)
        
        ;; Check for graduation milestone
        (if (is-eq completed-semesters (get total-semesters performance-data))
            (map-set achievement-milestones 
                { student: student, milestone: "graduation" }
                {
                    achieved-date: stacks-block-height,
                    reward-amount: GRADUATION-BONUS,
                    claimed: false
                })
            true)
        
        (ok performance-level)))

;; Apply for performance-based interest discount
(define-public (apply-performance-discount)
    (let (
        (student tx-sender)
        (performance-data (unwrap! (map-get? student-performance student) ERR-STUDENT-NOT-FOUND))
        (current-claims (unwrap! (map-get? reward-claims student) ERR-STUDENT-NOT-FOUND))
        (discount-rate (calculate-interest-discount (get performance-level performance-data)))
    )
        (asserts! (> discount-rate u0) ERR-MILESTONE-NOT-ACHIEVED)
        (asserts! (not (get interest-discount-active current-claims)) ERR-REWARD-ALREADY-CLAIMED)
        
        (map-set reward-claims student {
            total-claimed: (get total-claimed current-claims),
            last-claim: stacks-block-height,
            interest-discount-active: true,
            discount-percentage: discount-rate,
            discount-expires: (+ stacks-block-height u2160) ;; Valid for ~15 days
        })
        (ok discount-rate)))

;; Claim achievement milestone reward
(define-public (claim-milestone-reward (milestone (string-ascii 30)))
    (let (
        (student tx-sender)
        (milestone-data (unwrap! (map-get? achievement-milestones 
            { student: student, milestone: milestone }) ERR-MILESTONE-NOT-ACHIEVED))
        (reward-amount (get reward-amount milestone-data))
        (current-claims (unwrap! (map-get? reward-claims student) ERR-STUDENT-NOT-FOUND))
    )
        (asserts! (not (get claimed milestone-data)) ERR-REWARD-ALREADY-CLAIMED)
        (asserts! (<= (+ (var-get rewards-distributed) reward-amount) 
            (var-get total-rewards-pool)) ERR-INSUFFICIENT-BALANCE)
        
        ;; Transfer reward to student
        (try! (as-contract (stx-transfer? reward-amount tx-sender student)))
        
        ;; Mark milestone as claimed
        (map-set achievement-milestones 
            { student: student, milestone: milestone }
            {
                achieved-date: (get achieved-date milestone-data),
                reward-amount: reward-amount,
                claimed: true
            })
        
        ;; Update claim tracking
        (map-set reward-claims student {
            total-claimed: (+ (get total-claimed current-claims) reward-amount),
            last-claim: stacks-block-height,
            interest-discount-active: (get interest-discount-active current-claims),
            discount-percentage: (get discount-percentage current-claims),
            discount-expires: (get discount-expires current-claims)
        })
        
        (var-set rewards-distributed (+ (var-get rewards-distributed) reward-amount))
        (ok reward-amount)))

;; Read-only functions
(define-read-only (get-student-performance (student principal))
    (ok (map-get? student-performance student)))

(define-read-only (get-reward-status (student principal))
    (ok (map-get? reward-claims student)))

(define-read-only (get-milestone-status (student principal) (milestone (string-ascii 30)))
    (ok (map-get? achievement-milestones { student: student, milestone: milestone })))

(define-read-only (calculate-potential-discount (student principal))
    (match (map-get? student-performance student)
        performance-data (ok (calculate-interest-discount (get performance-level performance-data)))
        ERR-STUDENT-NOT-FOUND))

(define-read-only (get-rewards-pool-status)
    (ok {
        total-pool: (var-get total-rewards-pool),
        distributed: (var-get rewards-distributed),
        remaining: (- (var-get total-rewards-pool) (var-get rewards-distributed))
    }))

;; Admin functions
(define-public (add-rewards-to-pool (amount uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-rewards-pool (+ (var-get total-rewards-pool) amount))
        (ok true)))

(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)))
