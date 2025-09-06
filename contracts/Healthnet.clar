(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_FUNDS (err u101))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_VOTED (err u103))
(define-constant ERR_VOTING_ENDED (err u104))
(define-constant ERR_PROPOSAL_NOT_PASSED (err u105))
(define-constant ERR_ALREADY_EXECUTED (err u106))
(define-constant ERR_INVALID_AMOUNT (err u107))
(define-constant ERR_NOT_MEMBER (err u108))
(define-constant ERR_SHIPMENT_NOT_FOUND (err u109))
(define-constant ERR_INVALID_STATUS (err u110))
(define-constant ERR_ALREADY_VERIFIED (err u111))
(define-constant ERR_INSUFFICIENT_INVENTORY (err u112))
(define-constant ERR_SUPPLIER_NOT_REGISTERED (err u113))
(define-constant ERR_INVALID_QUANTITY (err u114))
(define-constant ERR_INVALID_METRIC_VALUE (err u115))
(define-constant ERR_REGION_NOT_FOUND (err u116))
(define-constant ERR_METRIC_NOT_FOUND (err u117))
(define-constant ERR_ALREADY_CLAIMED (err u118))
(define-constant ERR_TARGET_NOT_MET (err u119))
(define-constant ERR_INVALID_TIMEFRAME (err u120))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u121))
(define-constant ERR_REPUTATION_COOLDOWN (err u122))

(define-data-var next-proposal-id uint u1)
(define-data-var treasury-balance uint u0)
(define-data-var min-membership-stake uint u1000000)
(define-data-var voting-period uint u1440)
(define-data-var next-shipment-id uint u1)
(define-data-var next-supply-item-id uint u1)
(define-data-var next-metric-report-id uint u1)
(define-data-var reward-per-improvement uint u50000)
(define-data-var current-reporting-period uint u1)

(define-map members principal uint)
(define-map proposals uint {
    id: uint,
    proposer: principal,
    recipient: principal,
    amount: uint,
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposal-type: (string-ascii 20),
    votes-for: uint,
    votes-against: uint,
    start-block: uint,
    end-block: uint,
    executed: bool,
    passed: bool
})
(define-map votes {proposal-id: uint, voter: principal} bool)
(define-map health-workers principal {
    certified: bool,
    specialization: (string-ascii 50),
    training-completed: uint,
    equipment-received: uint
})
(define-map suppliers principal {
    registered: bool,
    name: (string-ascii 100),
    location: (string-ascii 100),
    reputation-score: uint
})
(define-map supply-items uint {
    id: uint,
    name: (string-ascii 100),
    category: (string-ascii 50),
    unit-price: uint,
    supplier: principal,
    created-at: uint
})
(define-map shipments uint {
    id: uint,
    supplier: principal,
    item-id: uint,
    quantity: uint,
    destination: principal,
    status: (string-ascii 20),
    quality-verified: bool,
    shipped-at: uint,
    delivered-at: uint,
    notes: (string-ascii 200)
})
(define-map inventory {item-id: uint, holder: principal} uint)
(define-map community-regions (string-ascii 50) {
    name: (string-ascii 50),
    population: uint,
    active: bool,
    registered-at: uint
})
(define-map health-metrics {region: (string-ascii 50), metric-type: (string-ascii 30), period: uint} {
    value: uint,
    target: uint,
    reporter: principal,
    verified: bool,
    reported-at: uint
})
(define-map metric-reports uint {
    id: uint,
    region: (string-ascii 50),
    metric-type: (string-ascii 30),
    value: uint,
    period: uint,
    reporter: principal,
    reported-at: uint,
    verified: bool
})
(define-map improvement-rewards {region: (string-ascii 50), period: uint} {
    claimed: bool,
    reward-amount: uint,
    improvement-score: uint,
    claimer: principal
})
(define-map metric-targets (string-ascii 30) uint)
(define-map worker-reputation principal {
    reputation-score: uint,
    metrics-submitted: uint,
    verifications-completed: uint,
    contributions-score: uint,
    last-activity: uint,
    reputation-level: (string-ascii 20)
})

(define-public (join-dao (stake-amount uint))
    (begin
        (asserts! (>= stake-amount (var-get min-membership-stake)) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        (var-set treasury-balance (+ (var-get treasury-balance) stake-amount))
        (map-set members tx-sender stake-amount)
        (ok true)
    )
)

(define-public (create-proposal 
    (recipient principal)
    (amount uint)
    (title (string-ascii 100))
    (description (string-ascii 500))
    (proposal-type (string-ascii 20))
)
    (let (
        (proposal-id (var-get next-proposal-id))
        (current-block stacks-block-height)
    )
        (asserts! (is-some (map-get? members tx-sender)) ERR_NOT_MEMBER)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (<= amount (var-get treasury-balance)) ERR_INSUFFICIENT_FUNDS)
        
        (map-set proposals proposal-id {
            id: proposal-id,
            proposer: tx-sender,
            recipient: recipient,
            amount: amount,
            title: title,
            description: description,
            proposal-type: proposal-type,
            votes-for: u0,
            votes-against: u0,
            start-block: current-block,
            end-block: (+ current-block (var-get voting-period)),
            executed: false,
            passed: false
        })
        
        (var-set next-proposal-id (+ proposal-id u1))
        (ok proposal-id)
    )
)

(define-public (vote (proposal-id uint) (support bool))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
        (voter-stake (unwrap! (map-get? members tx-sender) ERR_NOT_MEMBER))
        (current-block stacks-block-height)
    )
        (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: tx-sender})) ERR_ALREADY_VOTED)
        (asserts! (<= current-block (get end-block proposal)) ERR_VOTING_ENDED)
        
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} support)
        
        (if support
            (map-set proposals proposal-id (merge proposal {votes-for: (+ (get votes-for proposal) voter-stake)}))
            (map-set proposals proposal-id (merge proposal {votes-against: (+ (get votes-against proposal) voter-stake)}))
        )
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
        (current-block stacks-block-height)
        (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
        (passed (and (> total-votes u0) (> (get votes-for proposal) (get votes-against proposal))))
    )
        (asserts! (> current-block (get end-block proposal)) ERR_VOTING_ENDED)
        (asserts! (not (get executed proposal)) ERR_ALREADY_EXECUTED)
        (asserts! passed ERR_PROPOSAL_NOT_PASSED)
        
        (try! (stx-transfer? (get amount proposal) tx-sender (get recipient proposal)))
        (var-set treasury-balance (- (var-get treasury-balance) (get amount proposal)))
        
        (map-set proposals proposal-id (merge proposal {executed: true, passed: true}))
        
        (if (is-eq (get proposal-type proposal) "certification")
            (unwrap! (update-health-worker-certification (get recipient proposal)) (err u200))
            (if (is-eq (get proposal-type proposal) "training")
                (unwrap! (update-health-worker-training (get recipient proposal)) (err u201))
                (unwrap! (update-health-worker-equipment (get recipient proposal)) (err u202))
            )
        )
        (ok true)
    )
)

(define-public (register-health-worker (specialization (string-ascii 50)))
    (begin
        (map-set health-workers tx-sender {
            certified: false,
            specialization: specialization,
            training-completed: u0,
            equipment-received: u0
        })
        (ok true)
    )
)

(define-public (add-funds (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set treasury-balance (+ (var-get treasury-balance) amount))
        (ok true)
    )
)

(define-public (update-voting-period (new-period uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set voting-period new-period)
        (ok true)
    )
)

(define-public (update-min-stake (new-stake uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set min-membership-stake new-stake)
        (ok true)
    )
)

(define-private (update-health-worker-certification (worker principal))
    (let (
        (current-data (default-to {certified: false, specialization: "", training-completed: u0, equipment-received: u0} 
                                 (map-get? health-workers worker)))
    )
        (map-set health-workers worker (merge current-data {certified: true}))
        (ok true)
    )
)

(define-private (update-health-worker-training (worker principal))
    (let (
        (current-data (default-to {certified: false, specialization: "", training-completed: u0, equipment-received: u0} 
                                 (map-get? health-workers worker)))
    )
        (map-set health-workers worker (merge current-data {training-completed: (+ (get training-completed current-data) u1)}))
        (ok true)
    )
)

(define-private (update-health-worker-equipment (worker principal))
    (let (
        (current-data (default-to {certified: false, specialization: "", training-completed: u0, equipment-received: u0} 
                                 (map-get? health-workers worker)))
    )
        (map-set health-workers worker (merge current-data {equipment-received: (+ (get equipment-received current-data) u1)}))
        (ok true)
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-member-stake (member principal))
    (map-get? members member)
)

(define-read-only (get-health-worker (worker principal))
    (map-get? health-workers worker)
)

(define-read-only (get-treasury-balance)
    (var-get treasury-balance)
)

(define-read-only (get-voting-period)
    (var-get voting-period)
)

(define-read-only (get-min-membership-stake)
    (var-get min-membership-stake)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-next-proposal-id)
    (var-get next-proposal-id)
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
    (is-some (map-get? votes {proposal-id: proposal-id, voter: voter}))
)

(define-read-only (is-proposal-active (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal (and (<= stacks-block-height (get end-block proposal)) (not (get executed proposal)))
        false
    )
)

(define-public (register-supplier (name (string-ascii 100)) (location (string-ascii 100)))
    (begin
        (map-set suppliers tx-sender {
            registered: true,
            name: name,
            location: location,
            reputation-score: u100
        })
        (ok true)
    )
)

(define-public (create-supply-item 
    (name (string-ascii 100))
    (category (string-ascii 50))
    (unit-price uint)
)
    (let (
        (item-id (var-get next-supply-item-id))
        (supplier-data (unwrap! (map-get? suppliers tx-sender) ERR_SUPPLIER_NOT_REGISTERED))
    )
        (asserts! (> unit-price u0) ERR_INVALID_AMOUNT)
        (map-set supply-items item-id {
            id: item-id,
            name: name,
            category: category,
            unit-price: unit-price,
            supplier: tx-sender,
            created-at: stacks-block-height
        })
        (var-set next-supply-item-id (+ item-id u1))
        (ok item-id)
    )
)

(define-public (create-shipment 
    (item-id uint)
    (quantity uint)
    (destination principal)
    (notes (string-ascii 200))
)
    (let (
        (shipment-id (var-get next-shipment-id))
        (item (unwrap! (map-get? supply-items item-id) ERR_SHIPMENT_NOT_FOUND))
        (supplier-data (unwrap! (map-get? suppliers tx-sender) ERR_SUPPLIER_NOT_REGISTERED))
    )
        (asserts! (> quantity u0) ERR_INVALID_QUANTITY)
        (asserts! (is-eq tx-sender (get supplier item)) ERR_UNAUTHORIZED)
        (map-set shipments shipment-id {
            id: shipment-id,
            supplier: tx-sender,
            item-id: item-id,
            quantity: quantity,
            destination: destination,
            status: "shipped",
            quality-verified: false,
            shipped-at: stacks-block-height,
            delivered-at: u0,
            notes: notes
        })
        (var-set next-shipment-id (+ shipment-id u1))
        (ok shipment-id)
    )
)

(define-public (update-shipment-status (shipment-id uint) (new-status (string-ascii 20)))
    (let (
        (shipment (unwrap! (map-get? shipments shipment-id) ERR_SHIPMENT_NOT_FOUND))
    )
        (asserts! (or (is-eq tx-sender (get supplier shipment)) 
                     (is-eq tx-sender (get destination shipment))) ERR_UNAUTHORIZED)
        (map-set shipments shipment-id (merge shipment {
            status: new-status,
            delivered-at: (if (is-eq new-status "delivered") stacks-block-height (get delivered-at shipment))
        }))
        (if (is-eq new-status "delivered")
            (try! (update-inventory-on-delivery shipment-id))
            true
        )
        (ok true)
    )
)

(define-public (verify-quality (shipment-id uint) (verified bool))
    (let (
        (shipment (unwrap! (map-get? shipments shipment-id) ERR_SHIPMENT_NOT_FOUND))
        (verifier-member (unwrap! (map-get? members tx-sender) ERR_NOT_MEMBER))
    )
        (asserts! (not (get quality-verified shipment)) ERR_ALREADY_VERIFIED)
        (map-set shipments shipment-id (merge shipment {quality-verified: verified}))
        (if verified
            (try! (update-supplier-reputation (get supplier shipment) true))
            (try! (update-supplier-reputation (get supplier shipment) false))
        )
        (ok true)
    )
)

(define-public (transfer-inventory 
    (item-id uint)
    (quantity uint)
    (to principal)
)
    (let (
        (current-inventory (default-to u0 (map-get? inventory {item-id: item-id, holder: tx-sender})))
    )
        (asserts! (>= current-inventory quantity) ERR_INSUFFICIENT_INVENTORY)
        (asserts! (> quantity u0) ERR_INVALID_QUANTITY)
        (map-set inventory {item-id: item-id, holder: tx-sender} (- current-inventory quantity))
        (map-set inventory {item-id: item-id, holder: to} 
                 (+ (default-to u0 (map-get? inventory {item-id: item-id, holder: to})) quantity))
        (ok true)
    )
)

(define-public (request-emergency-supplies 
    (item-id uint)
    (quantity uint)
    (justification (string-ascii 200))
)
    (let (
        (worker-data (unwrap! (map-get? health-workers tx-sender) ERR_UNAUTHORIZED))
        (item (unwrap! (map-get? supply-items item-id) ERR_SHIPMENT_NOT_FOUND))
    )
        (asserts! (get certified worker-data) ERR_UNAUTHORIZED)
        (asserts! (> quantity u0) ERR_INVALID_QUANTITY)
        (create-proposal 
            tx-sender 
            (* (get unit-price item) quantity)
            "Emergency Supply Request"
            justification
            "emergency-supply"
        )
    )
)

(define-private (update-inventory-on-delivery (shipment-id uint))
    (let (
        (shipment (unwrap! (map-get? shipments shipment-id) ERR_SHIPMENT_NOT_FOUND))
        (destination (get destination shipment))
        (item-id (get item-id shipment))
        (quantity (get quantity shipment))
    )
        (map-set inventory {item-id: item-id, holder: destination}
                 (+ (default-to u0 (map-get? inventory {item-id: item-id, holder: destination})) quantity))
        (ok true)
    )
)

(define-private (update-supplier-reputation (supplier principal) (positive bool))
    (let (
        (supplier-data (unwrap! (map-get? suppliers supplier) ERR_SUPPLIER_NOT_REGISTERED))
        (current-score (get reputation-score supplier-data))
    )
        (map-set suppliers supplier (merge supplier-data {
            reputation-score: (if positive 
                                 (+ current-score u10)
                                 (if (>= current-score u10) (- current-score u10) u0))
        }))
        (ok true)
    )
)

(define-read-only (get-supplier (supplier principal))
    (map-get? suppliers supplier)
)

(define-read-only (get-supply-item (item-id uint))
    (map-get? supply-items item-id)
)

(define-read-only (get-shipment (shipment-id uint))
    (map-get? shipments shipment-id)
)

(define-read-only (get-inventory (item-id uint) (holder principal))
    (default-to u0 (map-get? inventory {item-id: item-id, holder: holder}))
)

(define-read-only (get-next-shipment-id)
    (var-get next-shipment-id)
)

(define-read-only (get-next-supply-item-id)
    (var-get next-supply-item-id)
)

(define-public (register-community-region 
    (region-id (string-ascii 50))
    (population uint)
)
    (let (
        (member-data (unwrap! (map-get? members tx-sender) ERR_NOT_MEMBER))
    )
        (asserts! (> population u0) ERR_INVALID_AMOUNT)
        (map-set community-regions region-id {
            name: region-id,
            population: population,
            active: true,
            registered-at: stacks-block-height
        })
        (ok true)
    )
)

(define-public (set-metric-target 
    (metric-type (string-ascii 30))
    (target-value uint)
)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> target-value u0) ERR_INVALID_METRIC_VALUE)
        (map-set metric-targets metric-type target-value)
        (ok true)
    )
)

(define-public (submit-health-metric 
    (region (string-ascii 50))
    (metric-type (string-ascii 30))
    (value uint)
)
    (let (
        (report-id (var-get next-metric-report-id))
        (current-period (var-get current-reporting-period))
        (worker-data (unwrap! (map-get? health-workers tx-sender) ERR_UNAUTHORIZED))
        (region-data (unwrap! (map-get? community-regions region) ERR_REGION_NOT_FOUND))
        (target (default-to u0 (map-get? metric-targets metric-type)))
    )
        (asserts! (get certified worker-data) ERR_UNAUTHORIZED)
        (asserts! (> value u0) ERR_INVALID_METRIC_VALUE)
        (map-set metric-reports report-id {
            id: report-id,
            region: region,
            metric-type: metric-type,
            value: value,
            period: current-period,
            reporter: tx-sender,
            reported-at: stacks-block-height,
            verified: false
        })
        (map-set health-metrics {region: region, metric-type: metric-type, period: current-period} {
            value: value,
            target: target,
            reporter: tx-sender,
            verified: false,
            reported-at: stacks-block-height
        })
        (try! (update-worker-reputation-on-metric-submission tx-sender))
        (var-set next-metric-report-id (+ report-id u1))
        (ok report-id)
    )
)

(define-public (verify-health-metric (report-id uint))
    (let (
        (report (unwrap! (map-get? metric-reports report-id) ERR_METRIC_NOT_FOUND))
        (verifier-member (unwrap! (map-get? members tx-sender) ERR_NOT_MEMBER))
    )
        (asserts! (not (get verified report)) ERR_ALREADY_VERIFIED)
        (map-set metric-reports report-id (merge report {verified: true}))
        (map-set health-metrics 
            {region: (get region report), metric-type: (get metric-type report), period: (get period report)}
            (merge (unwrap! (map-get? health-metrics 
                {region: (get region report), metric-type: (get metric-type report), period: (get period report)}) 
                ERR_METRIC_NOT_FOUND) {verified: true}))
        (ok true)
    )
)

(define-public (calculate-improvement-score 
    (region (string-ascii 50))
    (period uint)
)
    (let (
        (previous-period (- period u1))
        (current-vaccination-data (map-get? health-metrics {region: region, metric-type: "vaccination-rate", period: period}))
        (previous-vaccination-data (map-get? health-metrics {region: region, metric-type: "vaccination-rate", period: previous-period}))
        (current-mortality-data (map-get? health-metrics {region: region, metric-type: "infant-mortality", period: period}))
        (previous-mortality-data (map-get? health-metrics {region: region, metric-type: "infant-mortality", period: previous-period}))
        (current-vaccination (match current-vaccination-data some-data (get value some-data) u0))
        (previous-vaccination (match previous-vaccination-data some-data (get value some-data) u0))
        (current-mortality (match current-mortality-data some-data (get value some-data) u0))
        (previous-mortality (match previous-mortality-data some-data (get value some-data) u0))
        (vaccination-improvement (if (> current-vaccination previous-vaccination) 
                                   (- current-vaccination previous-vaccination) u0))
        (mortality-improvement (if (< current-mortality previous-mortality) 
                                 (- previous-mortality current-mortality) u0))
        (total-score (+ vaccination-improvement mortality-improvement))
    )
        (asserts! (> period u1) ERR_INVALID_TIMEFRAME)
        (map-set improvement-rewards {region: region, period: period} {
            claimed: false,
            reward-amount: (* total-score (var-get reward-per-improvement)),
            improvement-score: total-score,
            claimer: tx-sender
        })
        (ok total-score)
    )
)

(define-public (claim-improvement-reward 
    (region (string-ascii 50))
    (period uint)
)
    (let (
        (reward-data (unwrap! (map-get? improvement-rewards {region: region, period: period}) ERR_METRIC_NOT_FOUND))
        (worker-data (unwrap! (map-get? health-workers tx-sender) ERR_UNAUTHORIZED))
    )
        (asserts! (not (get claimed reward-data)) ERR_ALREADY_CLAIMED)
        (asserts! (> (get improvement-score reward-data) u0) ERR_TARGET_NOT_MET)
        (asserts! (get certified worker-data) ERR_UNAUTHORIZED)
        (asserts! (<= (get reward-amount reward-data) (var-get treasury-balance)) ERR_INSUFFICIENT_FUNDS)
        (try! (stx-transfer? (get reward-amount reward-data) (as-contract tx-sender) tx-sender))
        (var-set treasury-balance (- (var-get treasury-balance) (get reward-amount reward-data)))
        (map-set improvement-rewards {region: region, period: period} 
                 (merge reward-data {claimed: true}))
        (ok (get reward-amount reward-data))
    )
)

(define-public (advance-reporting-period)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set current-reporting-period (+ (var-get current-reporting-period) u1))
        (ok (var-get current-reporting-period))
    )
)

(define-public (update-reward-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> new-rate u0) ERR_INVALID_AMOUNT)
        (var-set reward-per-improvement new-rate)
        (ok true)
    )
)

(define-read-only (get-community-region (region-id (string-ascii 50)))
    (map-get? community-regions region-id)
)

(define-read-only (get-health-metric 
    (region (string-ascii 50))
    (metric-type (string-ascii 30))
    (period uint)
)
    (map-get? health-metrics {region: region, metric-type: metric-type, period: period})
)

(define-read-only (get-metric-report (report-id uint))
    (map-get? metric-reports report-id)
)

(define-read-only (get-improvement-reward 
    (region (string-ascii 50))
    (period uint)
)
    (map-get? improvement-rewards {region: region, period: period})
)

(define-read-only (get-metric-target (metric-type (string-ascii 30)))
    (map-get? metric-targets metric-type)
)

(define-read-only (get-current-reporting-period)
    (var-get current-reporting-period)
)

(define-read-only (get-reward-per-improvement)
    (var-get reward-per-improvement)
)

(define-read-only (get-next-metric-report-id)
    (var-get next-metric-report-id)
)

;; === HEALTH WORKER REPUTATION SYSTEM ===

(define-private (update-worker-reputation-on-metric-submission (worker principal))
    (let (
        (current-rep (default-to {reputation-score: u50, metrics-submitted: u0, verifications-completed: u0, contributions-score: u0, last-activity: u0, reputation-level: "beginner"} 
                                 (map-get? worker-reputation worker)))
        (new-metrics-count (+ (get metrics-submitted current-rep) u1))
        (reputation-boost u5)
        (new-reputation (+ (get reputation-score current-rep) reputation-boost))
        (new-level (calculate-reputation-level new-reputation))
    )
        (map-set worker-reputation worker {
            reputation-score: new-reputation,
            metrics-submitted: new-metrics-count,
            verifications-completed: (get verifications-completed current-rep),
            contributions-score: (+ (get contributions-score current-rep) reputation-boost),
            last-activity: stacks-block-height,
            reputation-level: new-level
        })
        (ok true)
    )
)

(define-private (update-worker-reputation-on-verification (verifier principal))
    (let (
        (current-rep (default-to {reputation-score: u50, metrics-submitted: u0, verifications-completed: u0, contributions-score: u0, last-activity: u0, reputation-level: "beginner"} 
                                 (map-get? worker-reputation verifier)))
        (new-verifications-count (+ (get verifications-completed current-rep) u1))
        (reputation-boost u3)
        (new-reputation (+ (get reputation-score current-rep) reputation-boost))
        (new-level (calculate-reputation-level new-reputation))
    )
        (map-set worker-reputation verifier {
            reputation-score: new-reputation,
            metrics-submitted: (get metrics-submitted current-rep),
            verifications-completed: new-verifications-count,
            contributions-score: (+ (get contributions-score current-rep) reputation-boost),
            last-activity: stacks-block-height,
            reputation-level: new-level
        })
        (ok true)
    )
)

(define-private (calculate-reputation-level (reputation-score uint))
    (if (<= reputation-score u25) "beginner"
        (if (<= reputation-score u75) "contributor"
            (if (<= reputation-score u150) "expert"
                (if (<= reputation-score u300) "master"
                    "champion"
                )
            )
        )
    )
)

(define-public (claim-reputation-bonus)
    (let (
        (worker-data (unwrap! (map-get? health-workers tx-sender) ERR_UNAUTHORIZED))
        (reputation-data (unwrap! (map-get? worker-reputation tx-sender) ERR_UNAUTHORIZED))
        (reputation-score (get reputation-score reputation-data))
        (last-activity (get last-activity reputation-data))
        (blocks-since-activity (- stacks-block-height last-activity))
        (bonus-amount (* reputation-score u1000))
    )
        (asserts! (get certified worker-data) ERR_UNAUTHORIZED)
        (asserts! (>= reputation-score u100) ERR_INSUFFICIENT_REPUTATION)
        (asserts! (>= blocks-since-activity u1440) ERR_REPUTATION_COOLDOWN) ;; 10 days cooldown
        (asserts! (<= bonus-amount (var-get treasury-balance)) ERR_INSUFFICIENT_FUNDS)
        (try! (stx-transfer? bonus-amount (as-contract tx-sender) tx-sender))
        (var-set treasury-balance (- (var-get treasury-balance) bonus-amount))
        (map-set worker-reputation tx-sender (merge reputation-data {last-activity: stacks-block-height}))
        (ok bonus-amount)
    )
)

(define-public (verify-health-metric-with-reputation (report-id uint))
    (let (
        (report (unwrap! (map-get? metric-reports report-id) ERR_METRIC_NOT_FOUND))
        (verifier-member (unwrap! (map-get? members tx-sender) ERR_NOT_MEMBER))
    )
        (asserts! (not (get verified report)) ERR_ALREADY_VERIFIED)
        (map-set metric-reports report-id (merge report {verified: true}))
        (map-set health-metrics 
            {region: (get region report), metric-type: (get metric-type report), period: (get period report)}
            (merge (unwrap! (map-get? health-metrics 
                {region: (get region report), metric-type: (get metric-type report), period: (get period report)}) 
                ERR_METRIC_NOT_FOUND) {verified: true}))
        (try! (update-worker-reputation-on-verification tx-sender))
        (ok true)
    )
)

(define-read-only (get-worker-reputation (worker principal))
    (map-get? worker-reputation worker)
)

(define-read-only (get-reputation-level (worker principal))
    (match (map-get? worker-reputation worker)
        rep-data (get reputation-level rep-data)
        "unranked"
    )
)

(define-read-only (calculate-reputation-bonus (worker principal))
    (match (map-get? worker-reputation worker)
        rep-data (* (get reputation-score rep-data) u1000)
        u0
    )
)

