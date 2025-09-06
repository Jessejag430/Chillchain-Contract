;; Property Neighborhoods Analytics Contract
;; Tracks neighborhood-level metrics, property clustering, and market trends

(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-NEIGHBORHOOD-NOT-FOUND (err u201))
(define-constant ERR-INVALID-COORDINATES (err u202))
(define-constant ERR-PROPERTY-NOT-IN-NEIGHBORHOOD (err u203))

;; Neighborhood data structure  
(define-map neighborhoods
  { neighborhood-id: uint }
  {
    name: (string-ascii 50),
    center-lat: uint,
    center-lng: uint,
    radius: uint,
    established-date: uint,
    total-properties: uint,
    avg-property-value: uint,
    last-updated: uint
  }
)

;; Property neighborhood assignment
(define-map property-neighborhoods
  { property-id: uint }
  {
    neighborhood-id: uint,
    assigned-date: uint,
    latitude: uint,
    longitude: uint
  }
)

;; Neighborhood amenities
(define-map neighborhood-amenities
  { neighborhood-id: uint }
  {
    school-rating: uint,
    crime-score: uint,
    walkability-score: uint,
    public-transport: bool,
    shopping-centers: uint,
    parks-count: uint
  }
)

(define-data-var next-neighborhood-id uint u1)

;; Create neighborhood
(define-public (create-neighborhood
    (name (string-ascii 50))
    (center-lat uint)
    (center-lng uint) 
    (radius uint))
  (let
    ((neighborhood-id (var-get next-neighborhood-id))
     (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))

    (asserts! (get active (contract-call? .Land-Registry is-verifier tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (and (> center-lat u0) (> center-lng u0)) ERR-INVALID-COORDINATES)
    (asserts! (> radius u0) ERR-INVALID-COORDINATES)

    (map-set neighborhoods
      { neighborhood-id: neighborhood-id }
      {
        name: name,
        center-lat: center-lat,
        center-lng: center-lng,
        radius: radius,
        established-date: current-time,
        total-properties: u0,
        avg-property-value: u0,
        last-updated: current-time
      })

    (var-set next-neighborhood-id (+ neighborhood-id u1))
    (ok neighborhood-id)
  )
)

;; Assign property to neighborhood
(define-public (assign-property-to-neighborhood
    (property-id uint)
    (neighborhood-id uint)
    (latitude uint)
    (longitude uint))
  (let
    ((neighborhood (unwrap! (map-get? neighborhoods { neighborhood-id: neighborhood-id }) ERR-NEIGHBORHOOD-NOT-FOUND))
     (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1)))))

    (asserts! (get active (contract-call? .Land-Registry is-verifier tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (and (> latitude u0) (> longitude u0)) ERR-INVALID-COORDINATES)
    (asserts! (is-some (contract-call? .Land-Registry get-property property-id)) ERR-PROPERTY-NOT-IN-NEIGHBORHOOD)

    (map-set property-neighborhoods
      { property-id: property-id }
      {
        neighborhood-id: neighborhood-id,
        assigned-date: current-time,
        latitude: latitude,
        longitude: longitude
      })

    (map-set neighborhoods
      { neighborhood-id: neighborhood-id }
      (merge neighborhood {
        total-properties: (+ (get total-properties neighborhood) u1),
        last-updated: current-time
      }))

    (ok true)
  )
)

;; Update amenities
(define-public (update-neighborhood-amenities
    (neighborhood-id uint)
    (school-rating uint)
    (crime-score uint)
    (walkability-score uint)
    (public-transport bool)
    (shopping-centers uint)
    (parks-count uint))
  (begin
    (asserts! (get active (contract-call? .Land-Registry is-verifier tx-sender)) ERR-NOT-AUTHORIZED)
    (asserts! (<= school-rating u10) ERR-INVALID-COORDINATES)

    (map-set neighborhood-amenities
      { neighborhood-id: neighborhood-id }
      {
        school-rating: school-rating,
        crime-score: crime-score,
        walkability-score: walkability-score,
        public-transport: public-transport,
        shopping-centers: shopping-centers,
        parks-count: parks-count
      })

    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-neighborhood (neighborhood-id uint))
  (map-get? neighborhoods { neighborhood-id: neighborhood-id })
)

(define-read-only (get-property-neighborhood (property-id uint))
  (map-get? property-neighborhoods { property-id: property-id })
)

(define-read-only (get-neighborhood-amenities (neighborhood-id uint))
  (map-get? neighborhood-amenities { neighborhood-id: neighborhood-id })
)

;; Calculate neighborhood desirability score
(define-read-only (calculate-neighborhood-score (neighborhood-id uint))
  (match (get-neighborhood-amenities neighborhood-id)
    amenities (let
      ((school-score (* (get school-rating amenities) u10))
       (safety-score (- u100 (get crime-score amenities)))
       (walk-score (get walkability-score amenities))
       (transport-bonus (if (get public-transport amenities) u20 u0))
       (amenity-score (+ (get shopping-centers amenities) (get parks-count amenities))))

      (ok (+ school-score safety-score walk-score transport-bonus amenity-score))
    )
    (err ERR-NEIGHBORHOOD-NOT-FOUND)
  )
)

;; Compare neighborhoods
(define-read-only (compare-neighborhoods (neighborhood-id-1 uint) (neighborhood-id-2 uint))
  (let
    ((n1 (unwrap! (get-neighborhood neighborhood-id-1) ERR-NEIGHBORHOOD-NOT-FOUND))
     (n2 (unwrap! (get-neighborhood neighborhood-id-2) ERR-NEIGHBORHOOD-NOT-FOUND)))

    (ok {
      value-difference: (to-int (if (> (get avg-property-value n1) (get avg-property-value n2))
                                   (- (get avg-property-value n1) (get avg-property-value n2))
                                   (- (get avg-property-value n2) (get avg-property-value n1)))),
      property-count-diff: (to-int (if (> (get total-properties n1) (get total-properties n2))
                                      (- (get total-properties n1) (get total-properties n2))
                                      (- (get total-properties n2) (get total-properties n1))))
    })
  )
)