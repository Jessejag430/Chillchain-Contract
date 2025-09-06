;; MeetingRecords - Meeting documentation and minutes management for Meetdao

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u300))
(define-constant ERR-MEETING-NOT-FOUND (err u301))
(define-constant ERR-RECORD-NOT-FOUND (err u302))
(define-constant ERR-INVALID-INPUT (err u303))
(define-constant ERR-RECORD-ALREADY-EXISTS (err u304))
(define-constant ERR-MEETING-NOT-COMPLETED (err u305))
(define-constant ERR-INVALID-STATUS (err u306))

(define-constant RECORD-STATUS-DRAFT u1)
(define-constant RECORD-STATUS-PUBLISHED u2)
(define-constant RECORD-STATUS-ARCHIVED u3)

(define-constant DOCUMENT-TYPE-MINUTES u1)
(define-constant DOCUMENT-TYPE-DECISION u2)
(define-constant DOCUMENT-TYPE-ACTION-ITEM u3)
(define-constant DOCUMENT-TYPE-SUMMARY u4)

;; Data variables
(define-data-var record-counter uint u0)
(define-data-var document-counter uint u0)

;; Meeting records storage
(define-map MeetingRecords uint {
    id: uint,
    meeting-id: uint,
    recorder: principal,
    title: (string-utf8 100),
    summary: (string-utf8 500),
    attendees-count: uint,
    decisions-made: uint,
    action-items: uint,
    duration-minutes: uint,
    status: uint,
    created-at: uint,
    updated-at: uint
})

;; Meeting documents storage
(define-map MeetingDocuments uint {
    id: uint,
    record-id: uint,
    document-type: uint,
    title: (string-utf8 100),
    content: (string-utf8 1000),
    author: principal,
    created-at: uint,
    priority: uint
})

;; Meeting decisions tracking
(define-map MeetingDecisions uint {
    id: uint,
    record-id: uint,
    decision-title: (string-utf8 100),
    decision-details: (string-utf8 500),
    decided-by: principal,
    implementation-deadline: uint,
    assigned-to: (optional principal),
    status: uint,
    created-at: uint
})

;; Action items tracking
(define-map ActionItems uint {
    id: uint,
    record-id: uint,
    task-description: (string-utf8 200),
    assigned-to: principal,
    due-date: uint,
    priority: uint,
    completion-status: uint,
    created-at: uint,
    completed-at: (optional uint)
})

;; Meeting attendance records
(define-map AttendanceRecords { record-id: uint, attendee: principal } {
    role: (string-utf8 30),
    participation-score: uint,
    contributions: uint,
    recorded-at: uint
})

;; Create meeting record
(define-public (create-meeting-record (meeting-id uint) (title (string-utf8 100)) (summary (string-utf8 500)) (attendees-count uint) (duration-minutes uint))
    (let ((record-id (+ (var-get record-counter) u1))
          (current-block stacks-block-height)
          (meeting-data (contract-call? .Meetdao get-meeting meeting-id)))
        
        (asserts! (is-some meeting-data) ERR-MEETING-NOT-FOUND)
        (asserts! (> (len title) u0) ERR-INVALID-INPUT)
        (asserts! (> attendees-count u0) ERR-INVALID-INPUT)
        (asserts! (is-none (get-meeting-record-by-meeting-id meeting-id)) ERR-RECORD-ALREADY-EXISTS)
        
        (map-set MeetingRecords record-id {
            id: record-id,
            meeting-id: meeting-id,
            recorder: tx-sender,
            title: title,
            summary: summary,
            attendees-count: attendees-count,
            decisions-made: u0,
            action-items: u0,
            duration-minutes: duration-minutes,
            status: RECORD-STATUS-DRAFT,
            created-at: current-block,
            updated-at: current-block
        })
        
        (var-set record-counter record-id)
        (ok record-id))
)

;; Add meeting document
(define-public (add-meeting-document (record-id uint) (document-type uint) (title (string-utf8 100)) (content (string-utf8 1000)) (priority uint))
    (let ((document-id (+ (var-get document-counter) u1))
          (current-block stacks-block-height)
          (record (unwrap! (map-get? MeetingRecords record-id) ERR-RECORD-NOT-FOUND)))
        
        (asserts! (or (is-eq tx-sender (get recorder record))
                      (is-eq tx-sender (unwrap! (contract-call? .Meetdao get-contract-info) (err u0)))) ERR-NOT-AUTHORIZED)
        (asserts! (and (>= document-type u1) (<= document-type u4)) ERR-INVALID-INPUT)
        (asserts! (> (len title) u0) ERR-INVALID-INPUT)
        (asserts! (> (len content) u0) ERR-INVALID-INPUT)
        (asserts! (<= priority u3) ERR-INVALID-INPUT)
        
        (map-set MeetingDocuments document-id {
            id: document-id,
            record-id: record-id,
            document-type: document-type,
            title: title,
            content: content,
            author: tx-sender,
            created-at: current-block,
            priority: priority
        })
        
        (var-set document-counter document-id)
        (ok document-id))
)

;; Record meeting decision
(define-public (record-decision (record-id uint) (decision-title (string-utf8 100)) (decision-details (string-utf8 500)) (implementation-deadline uint) (assigned-to (optional principal)))
    (let ((current-block stacks-block-height)
          (record (unwrap! (map-get? MeetingRecords record-id) ERR-RECORD-NOT-FOUND))
          (decision-id (+ (get decisions-made record) u1)))
        
        (asserts! (or (is-eq tx-sender (get recorder record))
                      (is-some (contract-call? .Meetdao get-meeting-attendee (get meeting-id record) tx-sender))) ERR-NOT-AUTHORIZED)
        (asserts! (> (len decision-title) u0) ERR-INVALID-INPUT)
        (asserts! (> implementation-deadline current-block) ERR-INVALID-INPUT)
        
        (map-set MeetingDecisions decision-id {
            id: decision-id,
            record-id: record-id,
            decision-title: decision-title,
            decision-details: decision-details,
            decided-by: tx-sender,
            implementation-deadline: implementation-deadline,
            assigned-to: assigned-to,
            status: u1,
            created-at: current-block
        })
        
        (map-set MeetingRecords record-id (merge record {
            decisions-made: (+ (get decisions-made record) u1),
            updated-at: current-block
        }))
        
        (ok decision-id))
)

;; Add action item
(define-public (add-action-item (record-id uint) (task-description (string-utf8 200)) (assigned-to principal) (due-date uint) (priority uint))
    (let ((current-block stacks-block-height)
          (record (unwrap! (map-get? MeetingRecords record-id) ERR-RECORD-NOT-FOUND))
          (action-id (+ (get action-items record) u1)))
        
        (asserts! (or (is-eq tx-sender (get recorder record))
                      (is-some (contract-call? .Meetdao get-meeting-attendee (get meeting-id record) tx-sender))) ERR-NOT-AUTHORIZED)
        (asserts! (> (len task-description) u0) ERR-INVALID-INPUT)
        (asserts! (> due-date current-block) ERR-INVALID-INPUT)
        (asserts! (<= priority u3) ERR-INVALID-INPUT)
        
        (map-set ActionItems action-id {
            id: action-id,
            record-id: record-id,
            task-description: task-description,
            assigned-to: assigned-to,
            due-date: due-date,
            priority: priority,
            completion-status: u0,
            created-at: current-block,
            completed-at: none
        })
        
        (map-set MeetingRecords record-id (merge record {
            action-items: (+ (get action-items record) u1),
            updated-at: current-block
        }))
        
        (ok action-id))
)

;; Complete action item
(define-public (complete-action-item (action-id uint))
    (let ((current-block stacks-block-height)
          (action-item (unwrap! (map-get? ActionItems action-id) ERR-RECORD-NOT-FOUND)))
        
        (asserts! (or (is-eq tx-sender (get assigned-to action-item))
                      (is-some (map-get? MeetingRecords (get record-id action-item)))) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get completion-status action-item) u0) ERR-INVALID-STATUS)
        
        (map-set ActionItems action-id (merge action-item {
            completion-status: u1,
            completed-at: (some current-block)
        }))
        
        (ok true))
)

;; Publish meeting record
(define-public (publish-meeting-record (record-id uint))
    (let ((current-block stacks-block-height)
          (record (unwrap! (map-get? MeetingRecords record-id) ERR-RECORD-NOT-FOUND)))
        
        (asserts! (is-eq tx-sender (get recorder record)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status record) RECORD-STATUS-DRAFT) ERR-INVALID-STATUS)
        
        (map-set MeetingRecords record-id (merge record {
            status: RECORD-STATUS-PUBLISHED,
            updated-at: current-block
        }))
        
        (ok true))
)

;; Record attendance
(define-public (record-attendance (record-id uint) (attendee principal) (role (string-utf8 30)) (participation-score uint))
    (let ((current-block stacks-block-height)
          (record (unwrap! (map-get? MeetingRecords record-id) ERR-RECORD-NOT-FOUND)))
        
        (asserts! (is-eq tx-sender (get recorder record)) ERR-NOT-AUTHORIZED)
        (asserts! (<= participation-score u10) ERR-INVALID-INPUT)
        (asserts! (> (len role) u0) ERR-INVALID-INPUT)
        
        (map-set AttendanceRecords { record-id: record-id, attendee: attendee } {
            role: role,
            participation-score: participation-score,
            contributions: u1,
            recorded-at: current-block
        })
        
        (ok true))
)

;; Read-only functions
(define-read-only (get-meeting-record (record-id uint))
    (map-get? MeetingRecords record-id)
)

(define-read-only (get-meeting-document (document-id uint))
    (map-get? MeetingDocuments document-id)
)

(define-read-only (get-meeting-decision (decision-id uint))
    (map-get? MeetingDecisions decision-id)
)

(define-read-only (get-action-item (action-id uint))
    (map-get? ActionItems action-id)
)

(define-read-only (get-attendance-record (record-id uint) (attendee principal))
    (map-get? AttendanceRecords { record-id: record-id, attendee: attendee })
)

(define-read-only (get-records-summary)
    {
        total-records: (var-get record-counter),
        total-documents: (var-get document-counter),
        last-updated: stacks-block-height
    }
)

(define-read-only (get-record-statistics (record-id uint))
    (match (map-get? MeetingRecords record-id)
        record (ok {
            meeting-id: (get meeting-id record),
            total-documents: (count-documents-by-record record-id),
            decisions-made: (get decisions-made record),
            action-items-total: (get action-items record),
            action-items-completed: (count-completed-actions record-id),
            status: (get status record),
            last-updated: (get updated-at record)
        })
        ERR-RECORD-NOT-FOUND)
)

;; Private helper functions
(define-private (get-meeting-record-by-meeting-id (meeting-id uint))
    (fold check-meeting-record (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) none)
)

(define-private (check-meeting-record (record-id uint) (current-match (optional uint)))
    (if (is-some current-match)
        current-match
        (match (map-get? MeetingRecords record-id)
            record (if (is-eq (get meeting-id record) record-id) (some record-id) none)
            none))
)

(define-private (count-documents-by-record (record-id uint))
    (fold count-record-documents (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) u0)
)

(define-private (count-record-documents (document-id uint) (acc uint))
    (match (map-get? MeetingDocuments document-id)
        document (if (is-eq (get record-id document) document-id) (+ acc u1) acc)
        acc)
)

(define-private (count-completed-actions (record-id uint))
    (fold count-completed-action-items (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) u0)
)

(define-private (count-completed-action-items (action-id uint) (acc uint))
    (match (map-get? ActionItems action-id)
        action (if (and (is-eq (get record-id action) record-id) (is-eq (get completion-status action) u1))
                  (+ acc u1) 
                  acc)
        acc)
)
