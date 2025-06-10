;; title: Settlebit
;; version: 1.0.0
;; summary: Escrow for Legal Settlements - Conditional fund release for case resolution
;; description: A smart contract that manages escrow funds for legal settlements with conditional release mechanisms

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_SETTLEMENT_NOT_FOUND (err u101))
(define-constant ERR_SETTLEMENT_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_SETTLEMENT_ALREADY_RESOLVED (err u104))
(define-constant ERR_SETTLEMENT_NOT_ACTIVE (err u105))
(define-constant ERR_INVALID_PARTY (err u106))
(define-constant ERR_DEADLINE_PASSED (err u107))
(define-constant ERR_DEADLINE_NOT_PASSED (err u108))
(define-constant ERR_ALREADY_AGREED (err u109))
(define-constant ERR_INVALID_AMOUNT (err u110))

(define-data-var settlement-counter uint u0)

(define-map settlements
  { settlement-id: uint }
  {
    plaintiff: principal,
    defendant: principal,
    arbitrator: principal,
    amount: uint,
    deadline: uint,
    status: (string-ascii 20),
    plaintiff-agreed: bool,
    defendant-agreed: bool,
    arbitrator-decision: (optional bool),
    created-at: uint,
    resolved-at: (optional uint)
  }
)

(define-map settlement-funds
  { settlement-id: uint }
  { deposited-amount: uint }
)

(define-map user-settlements
  { user: principal }
  { settlement-ids: (list 100 uint) }
)

(define-public (create-settlement (defendant principal) (arbitrator principal) (amount uint) (deadline uint))
  (let
    (
      (settlement-id (+ (var-get settlement-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> deadline current-block) ERR_DEADLINE_PASSED)
    (asserts! (and (not (is-eq tx-sender defendant)) (not (is-eq tx-sender arbitrator))) ERR_INVALID_PARTY)
    
    (map-set settlements
      { settlement-id: settlement-id }
      {
        plaintiff: tx-sender,
        defendant: defendant,
        arbitrator: arbitrator,
        amount: amount,
        deadline: deadline,
        status: "pending",
        plaintiff-agreed: false,
        defendant-agreed: false,
        arbitrator-decision: none,
        created-at: current-block,
        resolved-at: none
      }
    )
    
    (map-set settlement-funds
      { settlement-id: settlement-id }
      { deposited-amount: u0 }
    )
    
    (var-set settlement-counter settlement-id)
    (ok settlement-id)
  )
)

(define-public (deposit-funds (settlement-id uint))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (current-funds (default-to { deposited-amount: u0 } (map-get? settlement-funds { settlement-id: settlement-id })))
      (required-amount (get amount settlement))
    )
    (asserts! (is-eq (get status settlement) "pending") ERR_SETTLEMENT_NOT_ACTIVE)
    (asserts! (< (get deposited-amount current-funds) required-amount) ERR_SETTLEMENT_ALREADY_RESOLVED)
    
    (let
      (
        (deposit-amount (- required-amount (get deposited-amount current-funds)))
      )
      (try! (stx-transfer? deposit-amount tx-sender (as-contract tx-sender)))
      
      (map-set settlement-funds
        { settlement-id: settlement-id }
        { deposited-amount: required-amount }
      )
      
      (map-set settlements
        { settlement-id: settlement-id }
        (merge settlement { status: "funded" })
      )
      
      (ok deposit-amount)
    )
  )
)

(define-public (agree-to-settlement (settlement-id uint))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq (get status settlement) "funded") ERR_SETTLEMENT_NOT_ACTIVE)
    (asserts! (< current-block (get deadline settlement)) ERR_DEADLINE_PASSED)
    
    (if (is-eq tx-sender (get plaintiff settlement))
      (begin
        (asserts! (not (get plaintiff-agreed settlement)) ERR_ALREADY_AGREED)
        (map-set settlements
          { settlement-id: settlement-id }
          (merge settlement { plaintiff-agreed: true })
        )
        (ok "plaintiff-agreed")
      )
      (if (is-eq tx-sender (get defendant settlement))
        (begin
          (asserts! (not (get defendant-agreed settlement)) ERR_ALREADY_AGREED)
          (let
            (
              (updated-settlement (merge settlement { defendant-agreed: true }))
            )
            (map-set settlements
              { settlement-id: settlement-id }
              updated-settlement
            )
            (if (get plaintiff-agreed updated-settlement)
              (begin
                (try! (resolve-settlement settlement-id true))
                (ok "settlement-resolved")
              )
              (ok "defendant-agreed")
            )
          )
        )
        ERR_UNAUTHORIZED
      )
    )
  )
)

(define-public (arbitrator-decision (settlement-id uint) (decision bool))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get arbitrator settlement)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status settlement) "funded") ERR_SETTLEMENT_NOT_ACTIVE)
    (asserts! (>= current-block (get deadline settlement)) ERR_DEADLINE_NOT_PASSED)
    
    (map-set settlements
      { settlement-id: settlement-id }
      (merge settlement { arbitrator-decision: (some decision) })
    )
    
    (try! (resolve-settlement settlement-id decision))
    (ok decision)
  )
)

(define-public (emergency-refund (settlement-id uint))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (current-block stacks-block-height)
      (funds (unwrap! (map-get? settlement-funds { settlement-id: settlement-id }) ERR_INSUFFICIENT_FUNDS))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> (get deposited-amount funds) u0) ERR_INSUFFICIENT_FUNDS)
    (asserts! (not (is-eq (get status settlement) "resolved")) ERR_SETTLEMENT_ALREADY_RESOLVED)
    
    (try! (as-contract (stx-transfer? (get deposited-amount funds) tx-sender (get plaintiff settlement))))
    
    (map-set settlements
      { settlement-id: settlement-id }
      (merge settlement { 
        status: "refunded",
        resolved-at: (some current-block)
      })
    )
    
    (map-set settlement-funds
      { settlement-id: settlement-id }
      { deposited-amount: u0 }
    )
    
    (ok (get deposited-amount funds))
  )
)

(define-private (resolve-settlement (settlement-id uint) (in-favor-of-plaintiff bool))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (funds (unwrap! (map-get? settlement-funds { settlement-id: settlement-id }) ERR_INSUFFICIENT_FUNDS))
      (current-block stacks-block-height)
      (recipient (if in-favor-of-plaintiff (get defendant settlement) (get plaintiff settlement)))
    )
    (try! (as-contract (stx-transfer? (get deposited-amount funds) tx-sender recipient)))
    
    (map-set settlements
      { settlement-id: settlement-id }
      (merge settlement { 
        status: "resolved",
        resolved-at: (some current-block)
      })
    )
    
    (map-set settlement-funds
      { settlement-id: settlement-id }
      { deposited-amount: u0 }
    )
    
    (ok recipient)
  )
)

(define-read-only (get-settlement (settlement-id uint))
  (map-get? settlements { settlement-id: settlement-id })
)

(define-read-only (get-settlement-funds (settlement-id uint))
  (map-get? settlement-funds { settlement-id: settlement-id })
)

(define-read-only (get-settlement-counter)
  (var-get settlement-counter)
)

(define-read-only (is-settlement-expired (settlement-id uint))
  (match (map-get? settlements { settlement-id: settlement-id })
    settlement (> stacks-block-height (get deadline settlement))
    false
  )
)

(define-read-only (can-arbitrator-decide (settlement-id uint))
  (match (map-get? settlements { settlement-id: settlement-id })
    settlement (and 
      (is-eq (get status settlement) "funded")
      (>= stacks-block-height (get deadline settlement))
      (is-none (get arbitrator-decision settlement))
    )
    false
  )
)

(define-read-only (get-settlement-status (settlement-id uint))
  (match (map-get? settlements { settlement-id: settlement-id })
    settlement (get status settlement)
    "not-found"
  )
)