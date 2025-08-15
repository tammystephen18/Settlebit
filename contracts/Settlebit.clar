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
(define-constant ERR_MILESTONE_NOT_FOUND (err u111))
(define-constant ERR_MILESTONE_ALREADY_COMPLETED (err u112))
(define-constant ERR_MILESTONE_ALREADY_EXISTS (err u113))
(define-constant ERR_INVALID_MILESTONE_INDEX (err u114))
(define-constant ERR_PREVIOUS_MILESTONE_NOT_COMPLETE (err u115))
(define-constant ERR_MILESTONE_PERCENTAGE_INVALID (err u116))
(define-constant ERR_TOTAL_PERCENTAGE_INVALID (err u117))
(define-constant ERR_PARTY_NOT_FOUND (err u118))
(define-constant ERR_PARTY_ALREADY_EXISTS (err u119))
(define-constant ERR_INVALID_WEIGHT (err u120))
(define-constant ERR_INSUFFICIENT_APPROVAL_WEIGHT (err u121))
(define-constant ERR_INVALID_DISTRIBUTION_PERCENTAGE (err u122))
(define-constant ERR_MULTIPARTY_NOT_ENABLED (err u123))
(define-constant ERR_PARTY_LIMIT_EXCEEDED (err u124))

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

(define-map settlement-milestones
  { settlement-id: uint, milestone-index: uint }
  {
    description: (string-ascii 200),
    percentage: uint,
    completed: bool,
    completed-at: (optional uint),
    completed-by: (optional principal),
    evidence-hash: (optional (string-ascii 64))
  }
)

(define-map milestone-metadata
  { settlement-id: uint }
  {
    total-milestones: uint,
    completed-milestones: uint,
    total-percentage: uint,
    released-amount: uint
  }
)

;; Multi-party settlement support
(define-map multiparty-settlements
  { settlement-id: uint }
  {
    enabled: bool,
    total-parties: uint,
    total-weight: uint,
    approval-threshold: uint,
    current-approval-weight: uint,
    distribution-finalized: bool
  }
)

(define-map settlement-parties
  { settlement-id: uint, party: principal }
  {
    role: (string-ascii 20), ;; "plaintiff", "defendant", "stakeholder"
    weight: uint,
    distribution-percentage: uint,
    has-approved: bool,
    approved-at: (optional uint)
  }
)

(define-map party-lists
  { settlement-id: uint }
  { parties: (list 50 principal) }
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

(define-public (create-milestone (settlement-id uint) (description (string-ascii 200)) (percentage uint))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (metadata (default-to 
        { total-milestones: u0, completed-milestones: u0, total-percentage: u0, released-amount: u0 }
        (map-get? milestone-metadata { settlement-id: settlement-id })))
      (milestone-index (+ (get total-milestones metadata) u1))
      (new-total-percentage (+ (get total-percentage metadata) percentage))
    )
    (asserts! (or (is-eq tx-sender (get plaintiff settlement)) (is-eq tx-sender (get defendant settlement))) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status settlement) "pending") ERR_SETTLEMENT_NOT_ACTIVE)
    (asserts! (> percentage u0) ERR_MILESTONE_PERCENTAGE_INVALID)
    (asserts! (<= percentage u100) ERR_MILESTONE_PERCENTAGE_INVALID)
    (asserts! (<= new-total-percentage u100) ERR_TOTAL_PERCENTAGE_INVALID)
    (asserts! (is-none (map-get? settlement-milestones { settlement-id: settlement-id, milestone-index: milestone-index })) ERR_MILESTONE_ALREADY_EXISTS)
    
    (map-set settlement-milestones
      { settlement-id: settlement-id, milestone-index: milestone-index }
      {
        description: description,
        percentage: percentage,
        completed: false,
        completed-at: none,
        completed-by: none,
        evidence-hash: none
      }
    )
    
    (map-set milestone-metadata
      { settlement-id: settlement-id }
      (merge metadata {
        total-milestones: milestone-index,
        total-percentage: new-total-percentage
      })
    )
    
    (ok milestone-index)
  )
)

(define-public (complete-milestone (settlement-id uint) (milestone-index uint) (evidence-hash (optional (string-ascii 64))))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (milestone (unwrap! (map-get? settlement-milestones { settlement-id: settlement-id, milestone-index: milestone-index }) ERR_MILESTONE_NOT_FOUND))
      (metadata (unwrap! (map-get? milestone-metadata { settlement-id: settlement-id }) ERR_MILESTONE_NOT_FOUND))
      (current-block stacks-block-height)
      (funds (unwrap! (map-get? settlement-funds { settlement-id: settlement-id }) ERR_INSUFFICIENT_FUNDS))
    )
    (asserts! (is-eq (get status settlement) "funded") ERR_SETTLEMENT_NOT_ACTIVE)
    (asserts! (not (get completed milestone)) ERR_MILESTONE_ALREADY_COMPLETED)
    (asserts! (or (is-eq tx-sender (get plaintiff settlement)) (is-eq tx-sender (get defendant settlement)) (is-eq tx-sender (get arbitrator settlement))) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq milestone-index u1) (is-milestone-previous-completed settlement-id (- milestone-index u1))) ERR_PREVIOUS_MILESTONE_NOT_COMPLETE)
    
    (let
      (
        (payment-amount (/ (* (get deposited-amount funds) (get percentage milestone)) u100))
        (recipient (get defendant settlement))
        (new-completed-milestones (+ (get completed-milestones metadata) u1))
        (new-released-amount (+ (get released-amount metadata) payment-amount))
      )
      (try! (as-contract (stx-transfer? payment-amount tx-sender recipient)))
      
      (map-set settlement-milestones
        { settlement-id: settlement-id, milestone-index: milestone-index }
        (merge milestone {
          completed: true,
          completed-at: (some current-block),
          completed-by: (some tx-sender),
          evidence-hash: evidence-hash
        })
      )
      
      (map-set milestone-metadata
        { settlement-id: settlement-id }
        (merge metadata {
          completed-milestones: new-completed-milestones,
          released-amount: new-released-amount
        })
      )
      
      (map-set settlement-funds
        { settlement-id: settlement-id }
        { deposited-amount: (- (get deposited-amount funds) payment-amount) }
      )
      
      (if (is-eq new-completed-milestones (get total-milestones metadata))
        (begin
          (map-set settlements
            { settlement-id: settlement-id }
            (merge settlement {
              status: "resolved",
              resolved-at: (some current-block)
            })
          )
          (ok "settlement-fully-resolved")
        )
        (ok "milestone-completed")
      )
    )
  )
)

(define-public (dispute-milestone (settlement-id uint) (milestone-index uint) (reason (string-ascii 200)))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (milestone (unwrap! (map-get? settlement-milestones { settlement-id: settlement-id, milestone-index: milestone-index }) ERR_MILESTONE_NOT_FOUND))
    )
    (asserts! (is-eq (get status settlement) "funded") ERR_SETTLEMENT_NOT_ACTIVE)
    (asserts! (not (get completed milestone)) ERR_MILESTONE_ALREADY_COMPLETED)
    (asserts! (or (is-eq tx-sender (get plaintiff settlement)) (is-eq tx-sender (get defendant settlement))) ERR_UNAUTHORIZED)
    
    (map-set settlements
      { settlement-id: settlement-id }
      (merge settlement { status: "disputed" })
    )
    
    (ok reason)
  )
)

(define-public (arbitrator-resolve-milestone (settlement-id uint) (milestone-index uint) (approved bool) (evidence-hash (optional (string-ascii 64))))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (milestone (unwrap! (map-get? settlement-milestones { settlement-id: settlement-id, milestone-index: milestone-index }) ERR_MILESTONE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get arbitrator settlement)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status settlement) "disputed") ERR_SETTLEMENT_NOT_ACTIVE)
    (asserts! (not (get completed milestone)) ERR_MILESTONE_ALREADY_COMPLETED)
    
    (map-set settlements
      { settlement-id: settlement-id }
      (merge settlement { status: "funded" })
    )
    
    (if approved
      (complete-milestone settlement-id milestone-index evidence-hash)
      (ok "milestone-rejected")
    )
  )
)

(define-public (withdraw-remaining-funds (settlement-id uint))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (metadata (unwrap! (map-get? milestone-metadata { settlement-id: settlement-id }) ERR_MILESTONE_NOT_FOUND))
      (funds (unwrap! (map-get? settlement-funds { settlement-id: settlement-id }) ERR_INSUFFICIENT_FUNDS))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq (get status settlement) "resolved") ERR_SETTLEMENT_NOT_ACTIVE)
    (asserts! (> (get deposited-amount funds) u0) ERR_INSUFFICIENT_FUNDS)
    (asserts! (or (is-eq tx-sender (get plaintiff settlement)) (is-eq tx-sender (get defendant settlement))) ERR_UNAUTHORIZED)
    
    (let
      (
        (remaining-amount (get deposited-amount funds))
        (recipient (get plaintiff settlement))
      )
      (try! (as-contract (stx-transfer? remaining-amount tx-sender recipient)))
      
      (map-set settlement-funds
        { settlement-id: settlement-id }
        { deposited-amount: u0 }
      )
      
      (ok remaining-amount)
    )
  )
)

(define-read-only (get-milestone (settlement-id uint) (milestone-index uint))
  (map-get? settlement-milestones { settlement-id: settlement-id, milestone-index: milestone-index })
)

(define-read-only (get-milestone-metadata (settlement-id uint))
  (map-get? milestone-metadata { settlement-id: settlement-id })
)

(define-read-only (get-milestone-progress (settlement-id uint))
  (match (map-get? milestone-metadata { settlement-id: settlement-id })
    metadata (if (> (get total-milestones metadata) u0)
      (/ (* (get completed-milestones metadata) u100) (get total-milestones metadata))
      u0
    )
    u0
  )
)

(define-read-only (is-milestone-previous-completed (settlement-id uint) (milestone-index uint))
  (if (is-eq milestone-index u0)
    true
    (match (map-get? settlement-milestones { settlement-id: settlement-id, milestone-index: milestone-index })
      milestone (get completed milestone)
      false
    )
  )
)

(define-read-only (calculate-milestone-payout (settlement-id uint) (milestone-index uint))
  (match (map-get? settlement-milestones { settlement-id: settlement-id, milestone-index: milestone-index })
    milestone (match (map-get? settlement-funds { settlement-id: settlement-id })
      funds (/ (* (get deposited-amount funds) (get percentage milestone)) u100)
      u0
    )
    u0
  )
)

(define-read-only (get-settlement-milestones-summary (settlement-id uint))
  (match (map-get? milestone-metadata { settlement-id: settlement-id })
    metadata {
      total-milestones: (get total-milestones metadata),
      completed-milestones: (get completed-milestones metadata),
      progress-percentage: (get-milestone-progress settlement-id),
      total-percentage: (get total-percentage metadata),
      released-amount: (get released-amount metadata)
    }
    {
      total-milestones: u0,
      completed-milestones: u0,
      progress-percentage: u0,
      total-percentage: u0,
      released-amount: u0
    }
  )
)

;; Enable multi-party support for a settlement
(define-public (enable-multiparty-settlement (settlement-id uint) (approval-threshold uint))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get plaintiff settlement)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status settlement) "pending") ERR_SETTLEMENT_NOT_ACTIVE)
    (asserts! (> approval-threshold u0) ERR_INVALID_WEIGHT)
    (asserts! (<= approval-threshold u100) ERR_INVALID_WEIGHT)
    
    (map-set multiparty-settlements
      { settlement-id: settlement-id }
      {
        enabled: true,
        total-parties: u0,
        total-weight: u0,
        approval-threshold: approval-threshold,
        current-approval-weight: u0,
        distribution-finalized: false
      }
    )
    
    (ok settlement-id)
  )
)

;; Add a party to multi-party settlement
(define-public (add-settlement-party (settlement-id uint) (party principal) (role (string-ascii 20)) (weight uint) (distribution-percentage uint))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (multiparty (unwrap! (map-get? multiparty-settlements { settlement-id: settlement-id }) ERR_MULTIPARTY_NOT_ENABLED))
      (party-list (default-to { parties: (list) } (map-get? party-lists { settlement-id: settlement-id })))
    )
    (asserts! (get enabled multiparty) ERR_MULTIPARTY_NOT_ENABLED)
    (asserts! (is-eq tx-sender (get plaintiff settlement)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status settlement) "pending") ERR_SETTLEMENT_NOT_ACTIVE)
    (asserts! (> weight u0) ERR_INVALID_WEIGHT)
    (asserts! (<= distribution-percentage u100) ERR_INVALID_DISTRIBUTION_PERCENTAGE)
    (asserts! (< (get total-parties multiparty) u50) ERR_PARTY_LIMIT_EXCEEDED)
    (asserts! (is-none (map-get? settlement-parties { settlement-id: settlement-id, party: party })) ERR_PARTY_ALREADY_EXISTS)
    
    (let
      (
        (new-total-parties (+ (get total-parties multiparty) u1))
        (new-total-weight (+ (get total-weight multiparty) weight))
        (updated-parties (unwrap! (as-max-len? (append (get parties party-list) party) u50) ERR_PARTY_LIMIT_EXCEEDED))
      )
      
      (map-set settlement-parties
        { settlement-id: settlement-id, party: party }
        {
          role: role,
          weight: weight,
          distribution-percentage: distribution-percentage,
          has-approved: false,
          approved-at: none
        }
      )
      
      (map-set multiparty-settlements
        { settlement-id: settlement-id }
        (merge multiparty {
          total-parties: new-total-parties,
          total-weight: new-total-weight
        })
      )
      
      (map-set party-lists
        { settlement-id: settlement-id }
        { parties: updated-parties }
      )
      
      (ok party)
    )
  )
)

;; Multi-party approval system
(define-public (multiparty-approve (settlement-id uint))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (multiparty (unwrap! (map-get? multiparty-settlements { settlement-id: settlement-id }) ERR_MULTIPARTY_NOT_ENABLED))
      (party-info (unwrap! (map-get? settlement-parties { settlement-id: settlement-id, party: tx-sender }) ERR_PARTY_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (get enabled multiparty) ERR_MULTIPARTY_NOT_ENABLED)
    (asserts! (is-eq (get status settlement) "funded") ERR_SETTLEMENT_NOT_ACTIVE)
    (asserts! (not (get has-approved party-info)) ERR_ALREADY_AGREED)
    
    (let
      (
        (new-approval-weight (+ (get current-approval-weight multiparty) (get weight party-info)))
        (threshold-met (>= new-approval-weight (get approval-threshold multiparty)))
      )
      
      (map-set settlement-parties
        { settlement-id: settlement-id, party: tx-sender }
        (merge party-info {
          has-approved: true,
          approved-at: (some current-block)
        })
      )
      
      (map-set multiparty-settlements
        { settlement-id: settlement-id }
        (merge multiparty { current-approval-weight: new-approval-weight })
      )
      
      (if threshold-met
        (begin
          (try! (finalize-multiparty-distribution settlement-id))
          (ok "settlement-approved")
        )
        (ok "vote-recorded")
      )
    )
  )
)

;; Finalize multi-party distribution
(define-private (finalize-multiparty-distribution (settlement-id uint))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (multiparty (unwrap! (map-get? multiparty-settlements { settlement-id: settlement-id }) ERR_MULTIPARTY_NOT_ENABLED))
      (funds (unwrap! (map-get? settlement-funds { settlement-id: settlement-id }) ERR_INSUFFICIENT_FUNDS))
      (current-block stacks-block-height)
    )
    
    (map-set settlements
      { settlement-id: settlement-id }
      (merge settlement {
        status: "resolved",
        resolved-at: (some current-block)
      })
    )
    
    (map-set multiparty-settlements
      { settlement-id: settlement-id }
      (merge multiparty { distribution-finalized: true })
    )
    
    (ok settlement-id)
  )
)

;; Claim funds for multi-party settlement
(define-public (claim-multiparty-funds (settlement-id uint))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (multiparty (unwrap! (map-get? multiparty-settlements { settlement-id: settlement-id }) ERR_MULTIPARTY_NOT_ENABLED))
      (party-info (unwrap! (map-get? settlement-parties { settlement-id: settlement-id, party: tx-sender }) ERR_PARTY_NOT_FOUND))
      (funds (unwrap! (map-get? settlement-funds { settlement-id: settlement-id }) ERR_INSUFFICIENT_FUNDS))
    )
    (asserts! (get enabled multiparty) ERR_MULTIPARTY_NOT_ENABLED)
    (asserts! (is-eq (get status settlement) "resolved") ERR_SETTLEMENT_NOT_ACTIVE)
    (asserts! (get distribution-finalized multiparty) ERR_SETTLEMENT_NOT_ACTIVE)
    (asserts! (get has-approved party-info) ERR_UNAUTHORIZED)
    
    (let
      (
        (claim-amount (/ (* (get deposited-amount funds) (get distribution-percentage party-info)) u100))
      )
      (asserts! (> claim-amount u0) ERR_INSUFFICIENT_FUNDS)
      
      (try! (as-contract (stx-transfer? claim-amount tx-sender tx-sender)))
      
      (map-set settlement-parties
        { settlement-id: settlement-id, party: tx-sender }
        (merge party-info { distribution-percentage: u0 })
      )
      
      (ok claim-amount)
    )
  )
)

;; Remove party from settlement (before funding)
(define-public (remove-settlement-party (settlement-id uint) (party principal))
  (let
    (
      (settlement (unwrap! (map-get? settlements { settlement-id: settlement-id }) ERR_SETTLEMENT_NOT_FOUND))
      (multiparty (unwrap! (map-get? multiparty-settlements { settlement-id: settlement-id }) ERR_MULTIPARTY_NOT_ENABLED))
      (party-info (unwrap! (map-get? settlement-parties { settlement-id: settlement-id, party: party }) ERR_PARTY_NOT_FOUND))
    )
    (asserts! (get enabled multiparty) ERR_MULTIPARTY_NOT_ENABLED)
    (asserts! (is-eq tx-sender (get plaintiff settlement)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status settlement) "pending") ERR_SETTLEMENT_NOT_ACTIVE)
    
    (let
      (
        (new-total-parties (- (get total-parties multiparty) u1))
        (new-total-weight (- (get total-weight multiparty) (get weight party-info)))
      )
      
      (map-delete settlement-parties { settlement-id: settlement-id, party: party })
      
      (map-set multiparty-settlements
        { settlement-id: settlement-id }
        (merge multiparty {
          total-parties: new-total-parties,
          total-weight: new-total-weight
        })
      )
      
      (ok party)
    )
  )
)

;; Helper function for filtering parties (simplified approach)
(define-private (is-not-target-party (party-to-check principal))
  true
)

;; Read-only functions for multi-party settlements
(define-read-only (get-multiparty-settlement (settlement-id uint))
  (map-get? multiparty-settlements { settlement-id: settlement-id })
)

(define-read-only (get-settlement-party (settlement-id uint) (party principal))
  (map-get? settlement-parties { settlement-id: settlement-id, party: party })
)

(define-read-only (get-settlement-parties (settlement-id uint))
  (map-get? party-lists { settlement-id: settlement-id })
)

(define-read-only (calculate-approval-progress (settlement-id uint))
  (match (map-get? multiparty-settlements { settlement-id: settlement-id })
    multiparty (if (> (get approval-threshold multiparty) u0)
      (/ (* (get current-approval-weight multiparty) u100) (get approval-threshold multiparty))
      u0
    )
    u0
  )
)

(define-read-only (is-multiparty-enabled (settlement-id uint))
  (match (map-get? multiparty-settlements { settlement-id: settlement-id })
    multiparty (get enabled multiparty)
    false
  )
)

(define-read-only (get-party-claim-amount (settlement-id uint) (party principal))
  (match (map-get? settlement-parties { settlement-id: settlement-id, party: party })
    party-info (match (map-get? settlement-funds { settlement-id: settlement-id })
      funds (/ (* (get deposited-amount funds) (get distribution-percentage party-info)) u100)
      u0
    )
    u0
  )
)



