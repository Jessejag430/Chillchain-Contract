;; Mental Health Resource Library & Recommendation System
;; Community-curated mental health resources with personalized recommendations

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-NOT-MEMBER (err u201))
(define-constant ERR-RESOURCE-NOT-FOUND (err u202))
(define-constant ERR-ALREADY-RATED (err u203))
(define-constant ERR-INVALID-RATING (err u204))
(define-constant ERR-INVALID-CATEGORY (err u205))
(define-constant ERR-ALREADY-FAVORITED (err u206))
(define-constant ERR-NOT-FAVORITED (err u207))

;; Data variables
(define-data-var next-resource-id uint u1)
(define-data-var next-recommendation-id uint u1)

;; Resource categories
(define-constant CATEGORY-ANXIETY "anxiety")
(define-constant CATEGORY-DEPRESSION "depression")
(define-constant CATEGORY-STRESS "stress")
(define-constant CATEGORY-MINDFULNESS "mindfulness")
(define-constant CATEGORY-CRISIS "crisis")
(define-constant CATEGORY-GENERAL "general")

;; Resource types
(define-constant TYPE-ARTICLE "article")
(define-constant TYPE-VIDEO "video")
(define-constant TYPE-EXERCISE "exercise")
(define-constant TYPE-TOOL "tool")
(define-constant TYPE-BOOK "book")

;; Mental health resources
(define-map mental-health-resources
  { resource-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    url: (string-ascii 200),
    resource-type: (string-ascii 20),
    category: (string-ascii 20),
    difficulty-level: uint, ;; 1=beginner, 5=advanced
    estimated-duration: uint, ;; in minutes
    total-ratings: uint,
    rating-sum: uint,
    view-count: uint,
    added-by: principal,
    created-at: uint,
    is-active: bool,
    helpful-count: uint
  }
)

;; Resource ratings by members
(define-map resource-ratings
  { resource-id: uint, rater: principal }
  {
    rating: uint,
    helpful: bool,
    comments: (string-ascii 200),
    created-at: uint
  }
)

;; Member favorites
(define-map member-favorites
  { member: principal, resource-id: uint }
  {
    added-at: uint,
    notes: (string-ascii 100)
  }
)

;; Member resource preferences based on interactions
(define-map member-preferences
  { member: principal }
  {
    preferred-categories: (string-ascii 100), ;; comma-separated
    preferred-types: (string-ascii 100), ;; comma-separated
    difficulty-preference: uint,
    total-views: uint,
    total-ratings: uint,
    last-activity: uint
  }
)

;; Personalized recommendations
(define-map personalized-recommendations
  { recommendation-id: uint }
  {
    member: principal,
    resource-id: uint,
    recommendation-type: (string-ascii 30), ;; "category-match", "rating-based", "peer-suggested"
    confidence-score: uint, ;; 1-10
    reason: (string-ascii 150),
    created-at: uint,
    viewed: bool,
    acted-upon: bool
  }
)

;; Resource usage tracking
(define-map resource-usage
  { resource-id: uint, member: principal }
  {
    view-count: uint,
    first-viewed: uint,
    last-viewed: uint,
    completion-status: (string-ascii 20), ;; "started", "completed", "abandoned"
    effectiveness-rating: (optional uint)
  }
)

;; Add a new mental health resource
(define-public (add-resource 
  (title (string-ascii 100))
  (description (string-ascii 300))
  (url (string-ascii 200))
  (resource-type (string-ascii 20))
  (category (string-ascii 20))
  (difficulty-level uint)
  (estimated-duration uint)
)
  (let
    (
      (resource-id (var-get next-resource-id))
      (caller tx-sender)
    )
    ;; Validate inputs
    (asserts! (> difficulty-level u0) ERR-INVALID-RATING)
    (asserts! (<= difficulty-level u5) ERR-INVALID-RATING)
    (asserts! (or 
      (is-eq category CATEGORY-ANXIETY)
      (is-eq category CATEGORY-DEPRESSION)
      (is-eq category CATEGORY-STRESS)
      (is-eq category CATEGORY-MINDFULNESS)
      (is-eq category CATEGORY-CRISIS)
      (is-eq category CATEGORY-GENERAL)
    ) ERR-INVALID-CATEGORY)
    
    ;; Create resource
    (map-set mental-health-resources
      { resource-id: resource-id }
      {
        title: title,
        description: description,
        url: url,
        resource-type: resource-type,
        category: category,
        difficulty-level: difficulty-level,
        estimated-duration: estimated-duration,
        total-ratings: u0,
        rating-sum: u0,
        view-count: u0,
        added-by: caller,
        created-at: stacks-block-height,
        is-active: true,
        helpful-count: u0
      }
    )
    
    (var-set next-resource-id (+ resource-id u1))
    (ok resource-id)
  )
)

;; Rate a resource and provide feedback
(define-public (rate-resource 
  (resource-id uint)
  (rating uint)
  (helpful bool)
  (comments (string-ascii 200))
)
  (let
    (
      (resource (unwrap! (map-get? mental-health-resources { resource-id: resource-id }) ERR-RESOURCE-NOT-FOUND))
      (caller tx-sender)
    )
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
    (asserts! (is-none (map-get? resource-ratings { resource-id: resource-id, rater: caller })) ERR-ALREADY-RATED)
    
    ;; Add rating
    (map-set resource-ratings
      { resource-id: resource-id, rater: caller }
      {
        rating: rating,
        helpful: helpful,
        comments: comments,
        created-at: stacks-block-height
      }
    )
    
    ;; Update resource stats
    (map-set mental-health-resources
      { resource-id: resource-id }
      (merge resource {
        total-ratings: (+ (get total-ratings resource) u1),
        rating-sum: (+ (get rating-sum resource) rating),
        helpful-count: (if helpful (+ (get helpful-count resource) u1) (get helpful-count resource))
      })
    )
    
    ;; Update member preferences
    (try! (update-member-preferences caller category resource-type))
    
    (ok true)
  )
)

;; View a resource (tracks usage)
(define-public (view-resource (resource-id uint))
  (let
    (
      (resource (unwrap! (map-get? mental-health-resources { resource-id: resource-id }) ERR-RESOURCE-NOT-FOUND))
      (caller tx-sender)
      (current-usage (map-get? resource-usage { resource-id: resource-id, member: caller }))
    )
    ;; Update view count for resource
    (map-set mental-health-resources
      { resource-id: resource-id }
      (merge resource { view-count: (+ (get view-count resource) u1) })
    )
    
    ;; Update member usage tracking
    (match current-usage
      usage (map-set resource-usage
        { resource-id: resource-id, member: caller }
        (merge usage {
          view-count: (+ (get view-count usage) u1),
          last-viewed: stacks-block-height
        })
      )
      (map-set resource-usage
        { resource-id: resource-id, member: caller }
        {
          view-count: u1,
          first-viewed: stacks-block-height,
          last-viewed: stacks-block-height,
          completion-status: "started",
          effectiveness-rating: none
        }
      )
    )
    
    ;; Update member preferences for recommendation engine
    (try! (update-member-preferences caller (get category resource) (get resource-type resource)))
    
    (ok true)
  )
)

;; Add resource to favorites
(define-public (add-to-favorites (resource-id uint) (notes (string-ascii 100)))
  (let
    (
      (resource (unwrap! (map-get? mental-health-resources { resource-id: resource-id }) ERR-RESOURCE-NOT-FOUND))
      (caller tx-sender)
    )
    (asserts! (is-none (map-get? member-favorites { member: caller, resource-id: resource-id })) ERR-ALREADY-FAVORITED)
    
    (map-set member-favorites
      { member: caller, resource-id: resource-id }
      {
        added-at: stacks-block-height,
        notes: notes
      }
    )
    (ok true)
  )
)

;; Remove from favorites
(define-public (remove-from-favorites (resource-id uint))
  (let
    (
      (caller tx-sender)
    )
    (asserts! (is-some (map-get? member-favorites { member: caller, resource-id: resource-id })) ERR-NOT-FAVORITED)
    
    (map-delete member-favorites { member: caller, resource-id: resource-id })
    (ok true)
  )
)

;; Generate personalized recommendations
(define-public (generate-recommendations (member principal) (count uint))
  (let
    (
      (member-prefs (map-get? member-preferences { member: member }))
      (recommendation-id (var-get next-recommendation-id))
    )
    (match member-prefs
      prefs (begin
        ;; Simple recommendation: suggest highly rated resources in preferred categories
        (map-set personalized-recommendations
          { recommendation-id: recommendation-id }
          {
            member: member,
            resource-id: u1, ;; Would use algorithm to find best match
            recommendation-type: "category-match",
            confidence-score: u8,
            reason: "Based on your viewing history and preferences",
            created-at: stacks-block-height,
            viewed: false,
            acted-upon: false
          }
        )
        (var-set next-recommendation-id (+ recommendation-id u1))
        (ok recommendation-id)
      )
      ;; Default recommendation for new members
      (begin
        (map-set personalized-recommendations
          { recommendation-id: recommendation-id }
          {
            member: member,
            resource-id: u1,
            recommendation-type: "general",
            confidence-score: u5,
            reason: "Popular resource for new members",
            created-at: stacks-block-height,
            viewed: false,
            acted-upon: false
          }
        )
        (var-set next-recommendation-id (+ recommendation-id u1))
        (ok recommendation-id)
      )
    )
  )
)

;; Mark completion status for a resource
(define-public (mark-completion-status (resource-id uint) (status (string-ascii 20)) (effectiveness-rating (optional uint)))
  (let
    (
      (caller tx-sender)
      (current-usage (map-get? resource-usage { resource-id: resource-id, member: caller }))
    )
    (match current-usage
      usage (begin
        (map-set resource-usage
          { resource-id: resource-id, member: caller }
          (merge usage {
            completion-status: status,
            effectiveness-rating: effectiveness-rating
          })
        )
        (ok true)
      )
      ERR-RESOURCE-NOT-FOUND
    )
  )
)

;; Read-only functions

(define-read-only (get-resource (resource-id uint))
  (map-get? mental-health-resources { resource-id: resource-id })
)

(define-read-only (get-resource-rating (resource-id uint) (rater principal))
  (map-get? resource-ratings { resource-id: resource-id, rater: rater })
)

(define-read-only (get-member-preferences (member principal))
  (map-get? member-preferences { member: member })
)

(define-read-only (get-member-favorites (member principal) (resource-id uint))
  (map-get? member-favorites { member: member, resource-id: resource-id })
)

(define-read-only (get-resource-usage (resource-id uint) (member principal))
  (map-get? resource-usage { resource-id: resource-id, member: member })
)

(define-read-only (get-recommendation (recommendation-id uint))
  (map-get? personalized-recommendations { recommendation-id: recommendation-id })
)

(define-read-only (get-resource-stats (resource-id uint))
  (match (map-get? mental-health-resources { resource-id: resource-id })
    resource (some {
      average-rating: (if (> (get total-ratings resource) u0)
        (/ (get rating-sum resource) (get total-ratings resource))
        u0),
      total-ratings: (get total-ratings resource),
      view-count: (get view-count resource),
      helpful-percentage: (if (> (get total-ratings resource) u0)
        (/ (* (get helpful-count resource) u100) (get total-ratings resource))
        u0),
      category: (get category resource),
      difficulty-level: (get difficulty-level resource)
    })
    none
  )
)

(define-read-only (get-library-stats)
  {
    total-resources: (- (var-get next-resource-id) u1),
    total-recommendations: (- (var-get next-recommendation-id) u1)
  }
)

;; Private helper functions

(define-private (update-member-preferences (member principal) (category (string-ascii 20)) (resource-type (string-ascii 20)))
  (let
    (
      (current-prefs (map-get? member-preferences { member: member }))
    )
    (match current-prefs
      prefs (map-set member-preferences
        { member: member }
        (merge prefs {
          total-views: (+ (get total-views prefs) u1),
          last-activity: stacks-block-height
        })
      )
      (map-set member-preferences
        { member: member }
        {
          preferred-categories: category,
          preferred-types: resource-type,
          difficulty-preference: u3, ;; default to medium
          total-views: u1,
          total-ratings: u0,
          last-activity: stacks-block-height
        }
      )
    )
    (ok true)
  )
)
