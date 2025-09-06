;; Policy Renewal Automation Contract
;; Enables automatic policy renewal with grace periods and flexible payment options

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u300))
(define-constant ERR_POLICY_NOT_FOUND (err u301))
(define-constant ERR_INSUFFICIENT_BALANCE (err u302))
(define-constant ERR_AUTO_RENEWAL_DISABLED (err u303))
(define-constant ERR_INVALID_GRACE_PERIOD (err u304))
(define-constant ERR_RENEWAL_NOT_DUE (err u305))

;; Contract owner and configuration
(define-constant CONTRACT_OWNER tx-sender)
(define-data-var default-grace-period uint u2160) ;; ~15 days in blocks
(define-data-var max-grace-period uint u4320) ;; ~30 days in blocks
(define-data-var renewal-fee-rate uint u2) ;; 0.2% fee for automation service

;; Auto-renewal settings per user
(define-map auto-renewal-settings
    principal
    {
        enabled: bool,
        grace-period: uint,
        max-renewal-count: uint,
        payment-source: (string-ascii 10), ;; "WALLET" or "STAKED"
        last-renewal: uint,
        renewal-count: uint
    }
)

;; Renewal queue for processing
(define-map renewal-queue
    principal
    {
        policy-expiry: uint,
        renewal-due: uint,
        processed: bool,
        coverage-amount: uint,
        duration: uint
    }
)

;; Renewal history tracking
(define-map renewal-history
    {user: principal, renewal-id: uint}
    {
        renewed-at: uint,
        premium-paid: uint,
        payment-source: (string-ascii 10),
        new-expiry: uint
    }
)

;; Global renewal statistics
(define-data-var total-auto-renewals uint u0)
(define-data-var total-renewal-fees uint u0)
(define-data-var next-renewal-id uint u1)

;; Enable auto-renewal for user
(define-public (enable-auto-renewal (grace-period uint) (max-renewals uint) (payment-source (string-ascii 10)))
    (begin
        (asserts! (<= grace-period (var-get max-grace-period)) ERR_INVALID_GRACE_PERIOD)
        (asserts! (or (is-eq payment-source "WALLET") (is-eq payment-source "STAKED")) ERR_NOT_AUTHORIZED)
        
        (map-set auto-renewal-settings tx-sender
            {
                enabled: true,
                grace-period: grace-period,
                max-renewal-count: max-renewals,
                payment-source: payment-source,
                last-renewal: u0,
                renewal-count: u0
            }
        )
        (ok true)
    )
)

;; Disable auto-renewal
(define-public (disable-auto-renewal)
    (let ((settings (unwrap! (map-get? auto-renewal-settings tx-sender) ERR_POLICY_NOT_FOUND)))
        (map-set auto-renewal-settings tx-sender
            (merge settings {enabled: false})
        )
        (ok true)
    )
)

;; Queue policy for renewal (called by main insurance contract)
(define-public (queue-for-renewal (user principal) (policy-expiry uint) (coverage-amount uint) (duration uint))
    (let ((settings (map-get? auto-renewal-settings user)))
        (match settings
            user-settings
            (if (get enabled user-settings)
                (begin
                    (map-set renewal-queue user
                        {
                            policy-expiry: policy-expiry,
                            renewal-due: (- policy-expiry (get grace-period user-settings)),
                            processed: false,
                            coverage-amount: coverage-amount,
                            duration: duration
                        }
                    )
                    (ok true)
                )
                (ok false)
            )
            (ok false)
        )
    )
)

;; Process automatic renewal
(define-public (process-auto-renewal (user principal))
    (let 
        (
            (settings (unwrap! (map-get? auto-renewal-settings user) ERR_AUTO_RENEWAL_DISABLED))
            (queue-item (unwrap! (map-get? renewal-queue user) ERR_POLICY_NOT_FOUND))
        )
        
        ;; Verify conditions for renewal
        (asserts! (get enabled settings) ERR_AUTO_RENEWAL_DISABLED)
        (asserts! (not (get processed queue-item)) ERR_POLICY_NOT_FOUND)
        (asserts! (>= stacks-block-height (get renewal-due queue-item)) ERR_RENEWAL_NOT_DUE)
        (asserts! (< (get renewal-count settings) (get max-renewal-count settings)) ERR_NOT_AUTHORIZED)
        
        ;; Calculate renewal premium and fees
        (let 
            (
                (coverage-amount (get coverage-amount queue-item))
                (base-premium (/ (* coverage-amount u5) u1000)) ;; 0.5% base rate
                (renewal-fee (/ (* base-premium (var-get renewal-fee-rate)) u1000))
                (total-payment (+ base-premium renewal-fee))
                (duration (get duration queue-item))
                (new-expiry (+ stacks-block-height (* duration u4320)))
            )
            
            ;; Process payment based on payment source
            (if (is-eq (get payment-source settings) "WALLET")
                (try! (stx-transfer? total-payment user (as-contract tx-sender)))
                ;; For staked payments, assume integration with staking contract
                (try! (stx-transfer? total-payment user (as-contract tx-sender)))
            )
            
            ;; Record renewal in history
            (let ((renewal-id (var-get next-renewal-id)))
                (map-set renewal-history {user: user, renewal-id: renewal-id}
                    {
                        renewed-at: stacks-block-height,
                        premium-paid: base-premium,
                        payment-source: (get payment-source settings),
                        new-expiry: new-expiry
                    }
                )
                (var-set next-renewal-id (+ renewal-id u1))
            )
            
            ;; Update user settings
            (map-set auto-renewal-settings user
                (merge settings 
                    {
                        last-renewal: stacks-block-height,
                        renewal-count: (+ (get renewal-count settings) u1)
                    }
                )
            )
            
            ;; Mark queue item as processed
            (map-set renewal-queue user
                (merge queue-item {processed: true})
            )
            
            ;; Update global statistics
            (var-set total-auto-renewals (+ (var-get total-auto-renewals) u1))
            (var-set total-renewal-fees (+ (var-get total-renewal-fees) renewal-fee))
            
            (ok new-expiry)
        )
    )
)

;; Check if renewal is due for a user
(define-read-only (is-renewal-due (user principal))
    (match (map-get? renewal-queue user)
        queue-item 
        (and 
            (not (get processed queue-item))
            (>= stacks-block-height (get renewal-due queue-item))
        )
        false
    )
)

;; Get renewal settings for user
(define-read-only (get-renewal-settings (user principal))
    (map-get? auto-renewal-settings user)
)

;; Get renewal queue status
(define-read-only (get-renewal-queue-status (user principal))
    (map-get? renewal-queue user)
)

;; Calculate renewal cost preview
(define-read-only (calculate-renewal-cost (coverage-amount uint))
    (let 
        (
            (base-premium (/ (* coverage-amount u5) u1000))
            (renewal-fee (/ (* base-premium (var-get renewal-fee-rate)) u1000))
        )
        {
            base-premium: base-premium,
            renewal-fee: renewal-fee,
            total-cost: (+ base-premium renewal-fee)
        }
    )
)

;; Admin function to update grace period limits
(define-public (set-grace-period-limits (default-period uint) (max-period uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (asserts! (<= default-period max-period) ERR_INVALID_GRACE_PERIOD)
        (var-set default-grace-period default-period)
        (var-set max-grace-period max-period)
        (ok true)
    )
)

;; Admin function to set renewal fee rate
(define-public (set-renewal-fee-rate (rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
        (var-set renewal-fee-rate rate)
        (ok true)
    )
)

;; Get contract statistics
(define-read-only (get-renewal-stats)
    {
        total-auto-renewals: (var-get total-auto-renewals),
        total-renewal-fees: (var-get total-renewal-fees),
        default-grace-period: (var-get default-grace-period),
        max-grace-period: (var-get max-grace-period),
        renewal-fee-rate: (var-get renewal-fee-rate)
    }
)

;; Get user renewal history
(define-read-only (get-user-renewal-history (user principal) (renewal-id uint))
    (map-get? renewal-history {user: user, renewal-id: renewal-id})
)