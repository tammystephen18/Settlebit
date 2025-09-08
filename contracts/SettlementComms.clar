;; Settlement Communication System
;; Secure messaging between parties in legal settlements

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u200))
(define-constant err-settlement-not-found (err u201))
(define-constant err-message-not-found (err u202))
(define-constant err-invalid-recipient (err u203))
(define-constant err-message-too-long (err u204))
(define-constant err-invalid-settlement-id (err u205))

;; Data variables
(define-data-var message-counter uint u0)

;; Message storage
(define-map settlement-messages
  { settlement-id: uint, message-id: uint }
  {
    sender: principal,
    recipient: principal,
    subject: (string-ascii 100),
    content-hash: (string-ascii 64), ;; Encrypted content hash
    timestamp: uint,
    read-status: bool,
    message-type: (string-ascii 20), ;; "negotiation", "evidence", "update"
    priority: uint ;; 1=low, 2=normal, 3=high, 4=urgent
  }
)

;; Message metadata for each settlement
(define-map settlement-message-metadata
  { settlement-id: uint }
  {
    total-messages: uint,
    last-message-id: uint,
    last-activity: uint
  }
)

;; Party message counters
(define-map party-message-counts
  { settlement-id: uint, party: principal }
  {
    sent-count: uint,
    received-count: uint,
    unread-count: uint
  }
)

;; Message thread tracking
(define-map message-threads
  { settlement-id: uint, thread-id: uint }
  {
    title: (string-ascii 100),
    started-by: principal,
    started-at: uint,
    last-reply: uint,
    message-count: uint,
    status: (string-ascii 20) ;; "active", "resolved", "archived"
  }
)

(define-data-var thread-counter uint u0)

;; Send a message between settlement parties
(define-public (send-message (settlement-id uint) (recipient principal) (subject (string-ascii 100)) (content-hash (string-ascii 64)) (message-type (string-ascii 20)) (priority uint))
  (let
    (
      (message-id (+ (var-get message-counter) u1))
      (current-block stacks-block-height)
      (metadata (default-to 
        { total-messages: u0, last-message-id: u0, last-activity: u0 }
        (map-get? settlement-message-metadata { settlement-id: settlement-id })))
      (sender-counts (default-to 
        { sent-count: u0, received-count: u0, unread-count: u0 }
        (map-get? party-message-counts { settlement-id: settlement-id, party: tx-sender })))
      (recipient-counts (default-to 
        { sent-count: u0, received-count: u0, unread-count: u0 }
        (map-get? party-message-counts { settlement-id: settlement-id, party: recipient })))
    )
    (asserts! (> settlement-id u0) err-invalid-settlement-id)
    (asserts! (not (is-eq tx-sender recipient)) err-invalid-recipient)
    (asserts! (and (>= priority u1) (<= priority u4)) err-unauthorized)
    (asserts! (> (len subject) u0) err-message-too-long)
    
    ;; Store the message
    (map-set settlement-messages
      { settlement-id: settlement-id, message-id: message-id }
      {
        sender: tx-sender,
        recipient: recipient,
        subject: subject,
        content-hash: content-hash,
        timestamp: current-block,
        read-status: false,
        message-type: message-type,
        priority: priority
      }
    )
    
    ;; Update settlement metadata
    (map-set settlement-message-metadata
      { settlement-id: settlement-id }
      {
        total-messages: (+ (get total-messages metadata) u1),
        last-message-id: message-id,
        last-activity: current-block
      }
    )
    
    ;; Update sender counts
    (map-set party-message-counts
      { settlement-id: settlement-id, party: tx-sender }
      (merge sender-counts { sent-count: (+ (get sent-count sender-counts) u1) })
    )
    
    ;; Update recipient counts
    (map-set party-message-counts
      { settlement-id: settlement-id, party: recipient }
      (merge recipient-counts {
        received-count: (+ (get received-count recipient-counts) u1),
        unread-count: (+ (get unread-count recipient-counts) u1)
      })
    )
    
    (var-set message-counter message-id)
    (ok message-id)
  )
)

;; Mark message as read
(define-public (mark-message-read (settlement-id uint) (message-id uint))
  (let
    (
      (message (unwrap! (map-get? settlement-messages { settlement-id: settlement-id, message-id: message-id }) err-message-not-found))
      (recipient-counts (unwrap! (map-get? party-message-counts { settlement-id: settlement-id, party: (get recipient message) }) err-unauthorized))
    )
    (asserts! (is-eq tx-sender (get recipient message)) err-unauthorized)
    (asserts! (not (get read-status message)) err-message-not-found)
    
    ;; Mark message as read
    (map-set settlement-messages
      { settlement-id: settlement-id, message-id: message-id }
      (merge message { read-status: true })
    )
    
    ;; Update recipient unread count
    (map-set party-message-counts
      { settlement-id: settlement-id, party: tx-sender }
      (merge recipient-counts { unread-count: (- (get unread-count recipient-counts) u1) })
    )
    
    (ok true)
  )
)

;; Start a new message thread
(define-public (start-thread (settlement-id uint) (title (string-ascii 100)))
  (let
    (
      (thread-id (+ (var-get thread-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (> settlement-id u0) err-invalid-settlement-id)
    (asserts! (> (len title) u0) err-message-too-long)
    
    (map-set message-threads
      { settlement-id: settlement-id, thread-id: thread-id }
      {
        title: title,
        started-by: tx-sender,
        started-at: current-block,
        last-reply: current-block,
        message-count: u0,
        status: "active"
      }
    )
    
    (var-set thread-counter thread-id)
    (ok thread-id)
  )
)

;; Close a message thread
(define-public (close-thread (settlement-id uint) (thread-id uint))
  (let
    (
      (thread (unwrap! (map-get? message-threads { settlement-id: settlement-id, thread-id: thread-id }) err-message-not-found))
    )
    (asserts! (is-eq tx-sender (get started-by thread)) err-unauthorized)
    
    (map-set message-threads
      { settlement-id: settlement-id, thread-id: thread-id }
      (merge thread { status: "resolved" })
    )
    
    (ok true)
  )
)

;; Send message to arbitrator for urgent matters
(define-public (notify-arbitrator (settlement-id uint) (subject (string-ascii 100)) (content-hash (string-ascii 64)) (priority uint))
  (let
    (
      (current-block stacks-block-height)
    )
    (asserts! (> settlement-id u0) err-invalid-settlement-id)
    (asserts! (and (>= priority u1) (<= priority u4)) err-unauthorized)
    (asserts! (> (len subject) u0) err-message-too-long)
    
    ;; This would send a notification to the arbitrator
    ;; Implementation would depend on integration with main contract
    (ok "arbitrator-notified")
  )
)

;; Read-only functions

(define-read-only (get-message (settlement-id uint) (message-id uint))
  (map-get? settlement-messages { settlement-id: settlement-id, message-id: message-id })
)

(define-read-only (get-settlement-messages-metadata (settlement-id uint))
  (map-get? settlement-message-metadata { settlement-id: settlement-id })
)

(define-read-only (get-party-message-counts (settlement-id uint) (party principal))
  (map-get? party-message-counts { settlement-id: settlement-id, party: party })
)

(define-read-only (get-message-thread (settlement-id uint) (thread-id uint))
  (map-get? message-threads { settlement-id: settlement-id, thread-id: thread-id })
)

(define-read-only (get-unread-message-count (settlement-id uint) (party principal))
  (match (map-get? party-message-counts { settlement-id: settlement-id, party: party })
    counts (get unread-count counts)
    u0
  )
)

(define-read-only (get-settlement-communication-summary (settlement-id uint))
  (match (map-get? settlement-message-metadata { settlement-id: settlement-id })
    metadata {
      total-messages: (get total-messages metadata),
      last-message-id: (get last-message-id metadata),
      last-activity: (get last-activity metadata),
      active-threads: (count-active-threads settlement-id)
    }
    {
      total-messages: u0,
      last-message-id: u0,
      last-activity: u0,
      active-threads: u0
    }
  )
)

;; Count active threads for a settlement
(define-private (count-active-threads (settlement-id uint))
  ;; Simplified implementation - returns 0 for now
  ;; In a full implementation, this would iterate through threads
  u0
)

;; Check if user has unread priority messages
(define-read-only (has-priority-messages (settlement-id uint) (party principal))
  ;; Simplified check - in full implementation would scan recent messages
  (> (get-unread-message-count settlement-id party) u0)
)
