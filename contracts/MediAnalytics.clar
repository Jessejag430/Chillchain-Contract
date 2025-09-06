;; MediAnalytics - Health insights for MediVault

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-NO-PATIENT (err u201))
(define-constant ERR-INVALID-METRIC (err u202))
(define-constant ERR-NOT-DOCTOR (err u203))

;; Data variables
(define-data-var analytics-counter uint u0)

;; Patient health score tracking
(define-map PatientHealthScore
  { patient-id: principal }
  {
    current-score: uint,
    risk-level: uint,
    chronic-conditions: uint,
    calculated-at: uint
  }
)

;; Health condition statistics
(define-map HealthConditions
  { condition: (string-ascii 64) }
  {
    total-cases: uint,
    avg-age: uint,
    most-common-treatment: (string-ascii 128),
    last-updated: uint
  }
)

;; Doctor analytics
(define-map DoctorAnalytics
  { doctor-id: principal }
  {
    total-patients-treated: uint,
    avg-patient-satisfaction: uint,
    diagnostic-accuracy: uint,
    last-updated: uint
  }
)

;; Calculate patient health score
(define-public (calculate-health-score (patient-id principal))
  (let ((patient-data (contract-call? .MediVault get-patient-info patient-id)))
    (asserts! (is-some patient-data) ERR-NO-PATIENT)
    (asserts! (or (is-eq tx-sender patient-id)
                  (is-some (contract-call? .MediVault get-doctor-info tx-sender))) ERR-NOT-AUTHORIZED)
    
    (let ((base-score u75)
          (chronic-conditions u2)
          (calculated-score (- base-score (* chronic-conditions u5))))
      
      (map-set PatientHealthScore
        { patient-id: patient-id }
        {
          current-score: (min calculated-score u100),
          risk-level: (get-risk-level calculated-score),
          chronic-conditions: chronic-conditions,
          calculated-at: stacks-block-height
        })
      (ok calculated-score)))
)

;; Update condition stats
(define-public (update-condition-stats (condition (string-ascii 64)) (patient-age uint) (treatment (string-ascii 128)))
  (begin
    (asserts! (is-some (contract-call? .MediVault get-doctor-info tx-sender)) ERR-NOT-DOCTOR)
    
    (let ((existing-data (default-to 
                           { total-cases: u0, avg-age: u0, most-common-treatment: "", last-updated: u0 }
                           (map-get? HealthConditions { condition: condition }))))
      
      (let ((new-cases (+ (get total-cases existing-data) u1))
            (new-avg-age (if (> (get total-cases existing-data) u0)
                            (/ (+ (* (get avg-age existing-data) (get total-cases existing-data)) patient-age) new-cases)
                            patient-age)))
        
        (map-set HealthConditions
          { condition: condition }
          {
            total-cases: new-cases,
            avg-age: new-avg-age,
            most-common-treatment: treatment,
            last-updated: stacks-block-height
          })
        (ok true))))
)

;; Update doctor analytics
(define-public (update-doctor-analytics (satisfaction-rating uint) (diagnostic-outcome bool))
  (let ((doctor-data (default-to
                       { total-patients-treated: u0, avg-patient-satisfaction: u0, diagnostic-accuracy: u0, last-updated: u0 }
                       (map-get? DoctorAnalytics { doctor-id: tx-sender }))))
    
    (asserts! (is-some (contract-call? .MediVault get-doctor-info tx-sender)) ERR-NOT-DOCTOR)
    (asserts! (<= satisfaction-rating u10) ERR-INVALID-METRIC)
    
    (let ((treated-count (+ (get total-patients-treated doctor-data) u1))
          (new-satisfaction (if (> (get total-patients-treated doctor-data) u0)
                              (/ (+ (* (get avg-patient-satisfaction doctor-data) (get total-patients-treated doctor-data)) 
                                   satisfaction-rating) treated-count)
                              satisfaction-rating))
          (accuracy-boost (if diagnostic-outcome u100 u0))
          (current-accuracy (get diagnostic-accuracy doctor-data))
          (new-accuracy (if (> (get total-patients-treated doctor-data) u0)
                          (/ (+ (* current-accuracy (get total-patients-treated doctor-data)) accuracy-boost) treated-count)
                          accuracy-boost)))
      
      (map-set DoctorAnalytics
        { doctor-id: tx-sender }
        {
          total-patients-treated: treated-count,
          avg-patient-satisfaction: new-satisfaction,
          diagnostic-accuracy: new-accuracy,
          last-updated: stacks-block-height
        })
      (ok true)))
)

;; Read-only functions
(define-read-only (get-patient-health-score (patient-id principal))
  (map-get? PatientHealthScore { patient-id: patient-id })
)

(define-read-only (get-condition-stats (condition (string-ascii 64)))
  (map-get? HealthConditions { condition: condition })
)

(define-read-only (get-doctor-analytics (doctor-id principal))
  (map-get? DoctorAnalytics { doctor-id: doctor-id })
)

(define-read-only (get-analytics-summary)
  {
    analytics-generated: (var-get analytics-counter),
    last-calculation: stacks-block-height
  }
)

;; Private helper functions
(define-private (get-risk-level (score uint))
  (if (>= score u80) u1
    (if (>= score u60) u2
      (if (>= score u40) u3 u4)))
)