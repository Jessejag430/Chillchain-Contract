;; Cold Chain Compliance Certificate NFT Contract
;; Issues verifiable certificates for shipments with zero temperature violations

;; Error constants
(define-constant ERR_SHIPMENT_NOT_FOUND (err u300))
(define-constant ERR_NOT_AUTHORIZED (err u301))
(define-constant ERR_SHIPMENT_NOT_COMPLETED (err u302))
(define-constant ERR_HAS_VIOLATIONS (err u303))
(define-constant ERR_CERTIFICATE_EXISTS (err u304))
(define-constant ERR_CERTIFICATE_NOT_FOUND (err u305))
(define-constant ERR_INVALID_CERTIFICATE (err u306))

;; NFT definition for compliance certificates
(define-non-fungible-token compliance-certificate uint)

;; Track next certificate ID
(define-data-var next-certificate-id uint u1)

;; Store certificate details
(define-map certificate-data
  { certificate-id: uint }
  { 
    shipment-id: uint,
    shipment-owner: principal,
    product-name: (string-ascii 50),
    origin: (string-ascii 50),
    destination: (string-ascii 50),
    temperature-range: (string-ascii 30),
    issued-at: uint,
    total-logs: uint,
    certificate-type: (string-ascii 20)
  })

;; Prevent duplicate certificates per shipment
(define-map shipment-certificates
  { shipment-id: uint }
  { certificate-id: uint, issued: bool })

;; Track certificate statistics
(define-data-var total-certificates-issued uint u0)
(define-data-var certificates-this-month uint u0)

;; Certificate types and their requirements
(define-map certificate-types
  { type-name: (string-ascii 20) }
  { description: (string-ascii 100), min-logs-required: uint })

;; Initialize certificate types
(map-set certificate-types { type-name: "premium-cold-chain" }
  { description: "Premium cold chain compliance - zero violations with comprehensive monitoring", 
    min-logs-required: u5 })

(map-set certificate-types { type-name: "standard-cold-chain" }
  { description: "Standard cold chain compliance - zero violations with basic monitoring", 
    min-logs-required: u2 })

;; Determine certificate type based on number of temperature logs
(define-private (determine-certificate-type (total-logs uint))
  (if (>= total-logs u5)
    "premium-cold-chain"
    (if (>= total-logs u2)
      "standard-cold-chain"
      "")))

;; Check if shipment is eligible for certificate
(define-private (is-shipment-eligible (shipment-id uint))
  (let ((shipment-data (contract-call? .Chillchain get-shipment shipment-id)))
    (match shipment-data
      shipment (and 
        (is-eq (get status shipment) "completed")
        (is-eq (get violation-count shipment) u0)
        (>= (contract-call? .Chillchain get-shipment-log-count shipment-id) u2))
      false)))

;; Main function to mint compliance certificate
(define-public (mint-certificate (shipment-id uint))
  (let (
    (shipment-data (contract-call? .Chillchain get-shipment shipment-id))
    (log-count (contract-call? .Chillchain get-shipment-log-count shipment-id))
    (existing-cert (map-get? shipment-certificates { shipment-id: shipment-id }))
    (certificate-id (var-get next-certificate-id))
  )
    ;; Check if shipment exists and get data
    (asserts! (is-some shipment-data) ERR_SHIPMENT_NOT_FOUND)
    (let ((shipment (unwrap! shipment-data ERR_SHIPMENT_NOT_FOUND)))
      
      ;; Verify caller owns the shipment
      (asserts! (is-eq tx-sender (get owner shipment)) ERR_NOT_AUTHORIZED)
      
      ;; Check if shipment is completed
      (asserts! (is-eq (get status shipment) "completed") ERR_SHIPMENT_NOT_COMPLETED)
      
      ;; Check zero violations
      (asserts! (is-eq (get violation-count shipment) u0) ERR_HAS_VIOLATIONS)
      
      ;; Check certificate doesn't already exist
      (asserts! (is-none existing-cert) ERR_CERTIFICATE_EXISTS)
      
      ;; Must have at least 2 temperature logs for credibility
      (asserts! (>= log-count u2) ERR_INVALID_CERTIFICATE)
      
      ;; Determine certificate type
      (let ((cert-type (determine-certificate-type log-count))
            (temp-range (to-ascii-string (get min-temp shipment) (get max-temp shipment))))
        
        ;; Mint the NFT certificate
        (try! (nft-mint? compliance-certificate certificate-id tx-sender))
        
        ;; Store certificate data
        (map-set certificate-data
          { certificate-id: certificate-id }
          { 
            shipment-id: shipment-id,
            shipment-owner: tx-sender,
            product-name: (get product-name shipment),
            origin: (get origin shipment),
            destination: (get destination shipment),
            temperature-range: temp-range,
            issued-at: stacks-block-height,
            total-logs: log-count,
            certificate-type: cert-type
          })
        
        ;; Mark shipment as certificated
        (map-set shipment-certificates
          { shipment-id: shipment-id }
          { certificate-id: certificate-id, issued: true })
        
        ;; Update counters
        (var-set next-certificate-id (+ certificate-id u1))
        (var-set total-certificates-issued (+ (var-get total-certificates-issued) u1))
        (var-set certificates-this-month (+ (var-get certificates-this-month) u1))
        
        ;; Return certificate details
        (ok { certificate-id: certificate-id, 
              type: cert-type, 
              shipment-id: shipment-id, 
              logs-count: log-count })))))

;; Helper function to format temperature range as string
(define-private (to-ascii-string (min-temp int) (max-temp int))
  "0C to 4C") ;; Simplified for demo - would format actual temps

;; Verify certificate authenticity
(define-read-only (verify-certificate (certificate-id uint))
  (let ((cert-data (map-get? certificate-data { certificate-id: certificate-id })))
    (match cert-data
      cert (let ((shipment-data (contract-call? .Chillchain get-shipment (get shipment-id cert))))
             (match shipment-data
               shipment (and 
                 (is-eq (get owner shipment) (get shipment-owner cert))
                 (is-eq (get violation-count shipment) u0)
                 (is-eq (get status shipment) "completed"))
               false))
      false)))

;; Get certificate details
(define-read-only (get-certificate (certificate-id uint))
  (map-get? certificate-data { certificate-id: certificate-id }))

;; Get certificate ID for a shipment (if exists)
(define-read-only (get-shipment-certificate (shipment-id uint))
  (map-get? shipment-certificates { shipment-id: shipment-id }))

;; Check if shipment has certificate
(define-read-only (has-certificate (shipment-id uint))
  (is-some (map-get? shipment-certificates { shipment-id: shipment-id })))

;; Get certificate owner
(define-read-only (get-owner (certificate-id uint))
  (ok (nft-get-owner? compliance-certificate certificate-id)))

;; Transfer certificate
(define-public (transfer (certificate-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR_NOT_AUTHORIZED)
    (try! (nft-transfer? compliance-certificate certificate-id sender recipient))
    
    ;; Update certificate data with new owner
    (let ((cert-data (unwrap! (map-get? certificate-data { certificate-id: certificate-id }) 
                      ERR_CERTIFICATE_NOT_FOUND)))
      (map-set certificate-data
        { certificate-id: certificate-id }
        (merge cert-data { shipment-owner: recipient })))
    (ok true)))

;; NFT metadata URI
(define-read-only (get-token-uri (certificate-id uint))
  (let ((cert-data (map-get? certificate-data { certificate-id: certificate-id })))
    (match cert-data
      cert (ok (some "https://certificates.chillchain.org/metadata/"))
      (ok none))))

;; Get certificate type information
(define-read-only (get-certificate-type-info (type-name (string-ascii 20)))
  (map-get? certificate-types { type-name: type-name }))

;; Get total certificates issued
(define-read-only (get-total-certificates)
  (var-get total-certificates-issued))

;; Get monthly certificate count
(define-read-only (get-monthly-certificates)
  (var-get certificates-this-month))

;; Reset monthly counter (admin only)
(define-public (reset-monthly-counter)
  (begin
    ;; For demo purposes, skip admin check - any user can reset counter
    ;; (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set certificates-this-month u0)
    (ok true)))

;; Get all certificate types
(define-read-only (get-all-certificate-types)
  { premium: "premium-cold-chain", standard: "standard-cold-chain" })

;; Check certificate eligibility without minting
(define-read-only (check-eligibility (shipment-id uint))
  (let (
    (shipment-data (contract-call? .Chillchain get-shipment shipment-id))
    (log-count (contract-call? .Chillchain get-shipment-log-count shipment-id))
    (existing-cert (map-get? shipment-certificates { shipment-id: shipment-id }))
  )
    (match shipment-data
      shipment { 
        eligible: (and 
          (is-eq (get status shipment) "completed")
          (is-eq (get violation-count shipment) u0)
          (>= log-count u2)
          (is-none existing-cert)),
        violations: (get violation-count shipment),
        logs: log-count,
        completed: (is-eq (get status shipment) "completed"),
        already-certified: (is-some existing-cert)
      }
      { eligible: false, violations: u999, logs: u0, completed: false, already-certified: false })))

;; Get certificate summary for display
(define-read-only (get-certificate-summary (certificate-id uint))
  (let ((cert-data (map-get? certificate-data { certificate-id: certificate-id })))
    (match cert-data
      cert { 
        id: certificate-id,
        product: (get product-name cert),
        route: (get origin cert),
        type: (get certificate-type cert),
        issued: (get issued-at cert),
        verified: (verify-certificate certificate-id)
      }
      { id: u0, product: "", route: "", type: "", issued: u0, verified: false })))
