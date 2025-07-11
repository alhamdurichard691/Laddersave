(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_LADDER_NOT_FOUND (err u103))
(define-constant ERR_MILESTONE_NOT_REACHED (err u104))
(define-constant ERR_ALREADY_CLAIMED (err u105))
(define-constant ERR_LADDER_COMPLETED (err u106))
(define-constant ERR_INVALID_MILESTONE (err u107))
(define-constant ERR_GROUP_NOT_FOUND (err u108))
(define-constant ERR_NOT_GROUP_MEMBER (err u109))
(define-constant ERR_GROUP_FULL (err u110))
(define-constant ERR_ALREADY_GROUP_MEMBER (err u111))
(define-constant ERR_GROUP_COMPLETED (err u112))
(define-constant ERR_MINIMUM_CONTRIBUTION (err u113))
(define-constant ERR_INVALID_GROUP_SIZE (err u114))
(define-constant ERR_CANNOT_LEAVE_GROUP (err u115))

(define-data-var next-ladder-id uint u1)
(define-data-var total-rewards-pool uint u0)
(define-data-var next-group-id uint u1)

(define-map savings-ladders
  { ladder-id: uint }
  {
    owner: principal,
    target-amount: uint,
    current-amount: uint,
    milestone-count: uint,
    milestone-amount: uint,
    milestones-claimed: uint,
    created-at: uint,
    completed: bool,
    reward-multiplier: uint
  }
)

(define-map milestone-claims
  { ladder-id: uint, milestone: uint }
  { claimed: bool, claimed-at: uint }
)

(define-map user-ladder-count
  { user: principal }
  { count: uint }
)

(define-map savings-groups
  { group-id: uint }
  {
    creator: principal,
    name: (string-ascii 50),
    target-amount: uint,
    current-amount: uint,
    max-members: uint,
    current-members: uint,
    min-contribution: uint,
    created-at: uint,
    completed: bool,
    reward-share-percentage: uint
  }
)

(define-map group-members
  { group-id: uint, member: principal }
  {
    total-contributed: uint,
    contribution-count: uint,
    rewards-earned: uint,
    joined-at: uint
  }
)

(define-map group-contributions
  { group-id: uint, contribution-id: uint }
  {
    contributor: principal,
    amount: uint,
    contributed-at: uint
  }
)

(define-map group-contribution-counter
  { group-id: uint }
  { next-contribution-id: uint }
)

(define-public (create-savings-ladder (target-amount uint) (milestone-count uint))
  (let
    (
      (ladder-id (var-get next-ladder-id))
      (milestone-amount (/ target-amount milestone-count))
      (reward-multiplier (calculate-reward-multiplier milestone-count))
    )
    (asserts! (> target-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (and (>= milestone-count u3) (<= milestone-count u20)) ERR_INVALID_MILESTONE)
    (asserts! (> milestone-amount u0) ERR_INVALID_AMOUNT)
    
    (map-set savings-ladders
      { ladder-id: ladder-id }
      {
        owner: tx-sender,
        target-amount: target-amount,
        current-amount: u0,
        milestone-count: milestone-count,
        milestone-amount: milestone-amount,
        milestones-claimed: u0,
        created-at: stacks-block-height,
        completed: false,
        reward-multiplier: reward-multiplier
      }
    )
    
    (map-set user-ladder-count
      { user: tx-sender }
      { count: (+ (get-user-ladder-count tx-sender) u1) }
    )
    
    (var-set next-ladder-id (+ ladder-id u1))
    (ok ladder-id)
  )
)

(define-public (deposit-to-ladder (ladder-id uint) (amount uint))
  (let
    (
      (ladder (unwrap! (map-get? savings-ladders { ladder-id: ladder-id }) ERR_LADDER_NOT_FOUND))
      (new-amount (+ (get current-amount ladder) amount))
    )
    (asserts! (is-eq tx-sender (get owner ladder)) ERR_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (not (get completed ladder)) ERR_LADDER_COMPLETED)
    (asserts! (<= new-amount (get target-amount ladder)) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set savings-ladders
      { ladder-id: ladder-id }
      (merge ladder { 
        current-amount: new-amount,
        completed: (is-eq new-amount (get target-amount ladder))
      })
    )
    
    (ok new-amount)
  )
)

(define-public (claim-milestone-reward (ladder-id uint) (milestone uint))
  (let
    (
      (ladder (unwrap! (map-get? savings-ladders { ladder-id: ladder-id }) ERR_LADDER_NOT_FOUND))
      (milestone-threshold (* (get milestone-amount ladder) milestone))
      (reward-amount (calculate-milestone-reward ladder milestone))
      (claim-key { ladder-id: ladder-id, milestone: milestone })
    )
    (asserts! (is-eq tx-sender (get owner ladder)) ERR_NOT_AUTHORIZED)
    (asserts! (and (> milestone u0) (<= milestone (get milestone-count ladder))) ERR_INVALID_MILESTONE)
    (asserts! (>= (get current-amount ladder) milestone-threshold) ERR_MILESTONE_NOT_REACHED)
    (asserts! (is-none (map-get? milestone-claims claim-key)) ERR_ALREADY_CLAIMED)
    
    (map-set milestone-claims
      claim-key
      { claimed: true, claimed-at: stacks-block-height }
    )
    
    (map-set savings-ladders
      { ladder-id: ladder-id }
      (merge ladder { milestones-claimed: (+ (get milestones-claimed ladder) u1) })
    )
    
    (as-contract (stx-transfer? reward-amount tx-sender (get owner ladder)))
  )
)

(define-public (withdraw-completed-ladder (ladder-id uint))
  (let
    (
      (ladder (unwrap! (map-get? savings-ladders { ladder-id: ladder-id }) ERR_LADDER_NOT_FOUND))
      (completion-bonus (calculate-completion-bonus ladder))
      (total-withdrawal (+ (get current-amount ladder) completion-bonus))
    )
    (asserts! (is-eq tx-sender (get owner ladder)) ERR_NOT_AUTHORIZED)
    (asserts! (get completed ladder) ERR_MILESTONE_NOT_REACHED)
    
    (try! (as-contract (stx-transfer? total-withdrawal tx-sender (get owner ladder))))
    
    (map-delete savings-ladders { ladder-id: ladder-id })
    
    (ok total-withdrawal)
  )
)

(define-public (fund-rewards-pool (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set total-rewards-pool (+ (var-get total-rewards-pool) amount))
    (ok (var-get total-rewards-pool))
  )
)

(define-public (emergency-withdraw (ladder-id uint))
  (let
    (
      (ladder (unwrap! (map-get? savings-ladders { ladder-id: ladder-id }) ERR_LADDER_NOT_FOUND))
      (penalty-rate u10)
      (penalty-amount (/ (* (get current-amount ladder) penalty-rate) u100))
      (withdrawal-amount (- (get current-amount ladder) penalty-amount))
    )
    (asserts! (is-eq tx-sender (get owner ladder)) ERR_NOT_AUTHORIZED)
    (asserts! (> (get current-amount ladder) u0) ERR_INSUFFICIENT_BALANCE)
    
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender (get owner ladder))))
    (var-set total-rewards-pool (+ (var-get total-rewards-pool) penalty-amount))
    
    (map-delete savings-ladders { ladder-id: ladder-id })
    
    (ok withdrawal-amount)
  )
)

(define-read-only (get-ladder-details (ladder-id uint))
  (map-get? savings-ladders { ladder-id: ladder-id })
)

(define-read-only (get-milestone-status (ladder-id uint) (milestone uint))
  (map-get? milestone-claims { ladder-id: ladder-id, milestone: milestone })
)

(define-read-only (get-user-ladder-count (user principal))
  (default-to u0 (get count (map-get? user-ladder-count { user: user })))
)

(define-read-only (get-ladder-progress (ladder-id uint))
  (match (map-get? savings-ladders { ladder-id: ladder-id })
    ladder (some {
      progress-percentage: (/ (* (get current-amount ladder) u100) (get target-amount ladder)),
      milestones-available: (/ (get current-amount ladder) (get milestone-amount ladder)),
      milestones-claimed: (get milestones-claimed ladder),
      next-milestone-threshold: (* (get milestone-amount ladder) (+ (get milestones-claimed ladder) u1))
    })
    none
  )
)

(define-read-only (calculate-milestone-reward (ladder (tuple (owner principal) (target-amount uint) (current-amount uint) (milestone-count uint) (milestone-amount uint) (milestones-claimed uint) (created-at uint) (completed bool) (reward-multiplier uint))) (milestone uint))
  (let
    (
      (base-reward (/ (get milestone-amount ladder) u20))
      (multiplier (get reward-multiplier ladder))
      (milestone-bonus (/ (* base-reward milestone) u10))
    )
    (+ base-reward (* base-reward multiplier) milestone-bonus)
  )
)

(define-read-only (calculate-completion-bonus (ladder (tuple (owner principal) (target-amount uint) (current-amount uint) (milestone-count uint) (milestone-amount uint) (milestones-claimed uint) (created-at uint) (completed bool) (reward-multiplier uint))))
  (let
    (
      (base-bonus (/ (get target-amount ladder) u50))
      (milestone-bonus (* (get milestone-count ladder) u1000))
      (multiplier-bonus (* base-bonus (get reward-multiplier ladder)))
    )
    (+ base-bonus milestone-bonus multiplier-bonus)
  )
)

(define-read-only (calculate-reward-multiplier (milestone-count uint))
  (if (<= milestone-count u5)
    u1
    (if (<= milestone-count u10)
      u2
      (if (<= milestone-count u15)
        u3
        u4
      )
    )
  )
)

(define-read-only (get-total-rewards-pool)
  (var-get total-rewards-pool)
)

(define-read-only (get-next-ladder-id)
  (var-get next-ladder-id)
)

(define-read-only (estimate-total-rewards (target-amount uint) (milestone-count uint))
  (let
    (
      (milestone-amount (/ target-amount milestone-count))
      (reward-multiplier (calculate-reward-multiplier milestone-count))
      (mock-ladder {
        owner: tx-sender,
        target-amount: target-amount,
        current-amount: target-amount,
        milestone-count: milestone-count,
        milestone-amount: milestone-amount,
        milestones-claimed: u0,
        created-at: stacks-block-height,
        completed: true,
        reward-multiplier: reward-multiplier
      })
      (total-milestone-rewards (fold + (map uint-to-milestone-reward (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20)) u0))
      (completion-bonus (calculate-completion-bonus mock-ladder))
    )
    (+ total-milestone-rewards completion-bonus)
  )
)

(define-read-only (uint-to-milestone-reward (milestone uint))
  u1000
)

(define-public (create-savings-group (name (string-ascii 50)) (target-amount uint) (max-members uint) (min-contribution uint) (reward-share-percentage uint))
  (let
    (
      (group-id (var-get next-group-id))
    )
    (asserts! (> target-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (and (>= max-members u2) (<= max-members u20)) ERR_INVALID_GROUP_SIZE)
    (asserts! (> min-contribution u0) ERR_MINIMUM_CONTRIBUTION)
    (asserts! (and (>= reward-share-percentage u1) (<= reward-share-percentage u50)) ERR_INVALID_AMOUNT)
    
    (map-set savings-groups
      { group-id: group-id }
      {
        creator: tx-sender,
        name: name,
        target-amount: target-amount,
        current-amount: u0,
        max-members: max-members,
        current-members: u1,
        min-contribution: min-contribution,
        created-at: stacks-block-height,
        completed: false,
        reward-share-percentage: reward-share-percentage
      }
    )
    
    (map-set group-members
      { group-id: group-id, member: tx-sender }
      {
        total-contributed: u0,
        contribution-count: u0,
        rewards-earned: u0,
        joined-at: stacks-block-height
      }
    )
    
    (map-set group-contribution-counter
      { group-id: group-id }
      { next-contribution-id: u1 }
    )
    
    (var-set next-group-id (+ group-id u1))
    (ok group-id)
  )
)

(define-public (join-savings-group (group-id uint))
  (let
    (
      (group (unwrap! (map-get? savings-groups { group-id: group-id }) ERR_GROUP_NOT_FOUND))
      (member-key { group-id: group-id, member: tx-sender })
    )
    (asserts! (is-none (map-get? group-members member-key)) ERR_ALREADY_GROUP_MEMBER)
    (asserts! (< (get current-members group) (get max-members group)) ERR_GROUP_FULL)
    (asserts! (not (get completed group)) ERR_GROUP_COMPLETED)
    
    (map-set group-members
      member-key
      {
        total-contributed: u0,
        contribution-count: u0,
        rewards-earned: u0,
        joined-at: stacks-block-height
      }
    )
    
    (map-set savings-groups
      { group-id: group-id }
      (merge group { current-members: (+ (get current-members group) u1) })
    )
    
    (ok true)
  )
)

(define-public (contribute-to-group (group-id uint) (amount uint))
  (let
    (
      (group (unwrap! (map-get? savings-groups { group-id: group-id }) ERR_GROUP_NOT_FOUND))
      (member-key { group-id: group-id, member: tx-sender })
      (member (unwrap! (map-get? group-members member-key) ERR_NOT_GROUP_MEMBER))
      (contribution-counter (unwrap! (map-get? group-contribution-counter { group-id: group-id }) ERR_GROUP_NOT_FOUND))
      (contribution-id (get next-contribution-id contribution-counter))
      (new-group-amount (+ (get current-amount group) amount))
    )
    (asserts! (>= amount (get min-contribution group)) ERR_MINIMUM_CONTRIBUTION)
    (asserts! (not (get completed group)) ERR_GROUP_COMPLETED)
    (asserts! (<= new-group-amount (get target-amount group)) ERR_INVALID_AMOUNT)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set group-contributions
      { group-id: group-id, contribution-id: contribution-id }
      {
        contributor: tx-sender,
        amount: amount,
        contributed-at: stacks-block-height
      }
    )
    
    (map-set group-contribution-counter
      { group-id: group-id }
      { next-contribution-id: (+ contribution-id u1) }
    )
    
    (map-set group-members
      member-key
      (merge member {
        total-contributed: (+ (get total-contributed member) amount),
        contribution-count: (+ (get contribution-count member) u1)
      })
    )
    
    (map-set savings-groups
      { group-id: group-id }
      (merge group {
        current-amount: new-group-amount,
        completed: (is-eq new-group-amount (get target-amount group))
      })
    )
    
    (ok new-group-amount)
  )
)

(define-public (distribute-group-rewards (group-id uint))
  (let
    (
      (group (unwrap! (map-get? savings-groups { group-id: group-id }) ERR_GROUP_NOT_FOUND))
      (total-reward-pool (/ (* (get target-amount group) (get reward-share-percentage group)) u100))
    )
    (asserts! (get completed group) ERR_MILESTONE_NOT_REACHED)
    (asserts! (>= (var-get total-rewards-pool) total-reward-pool) ERR_INSUFFICIENT_BALANCE)
    
    (var-set total-rewards-pool (- (var-get total-rewards-pool) total-reward-pool))
    (ok total-reward-pool)
  )
)

(define-public (claim-group-reward (group-id uint))
  (let
    (
      (group (unwrap! (map-get? savings-groups { group-id: group-id }) ERR_GROUP_NOT_FOUND))
      (member-key { group-id: group-id, member: tx-sender })
      (member (unwrap! (map-get? group-members member-key) ERR_NOT_GROUP_MEMBER))
      (contribution-percentage (calculate-member-contribution-percentage group-id tx-sender))
      (total-reward-pool (/ (* (get target-amount group) (get reward-share-percentage group)) u100))
      (member-reward (/ (* total-reward-pool contribution-percentage) u100))
    )
    (asserts! (get completed group) ERR_MILESTONE_NOT_REACHED)
    (asserts! (is-eq (get rewards-earned member) u0) ERR_ALREADY_CLAIMED)
    
    (map-set group-members
      member-key
      (merge member { rewards-earned: member-reward })
    )
    
    (as-contract (stx-transfer? member-reward tx-sender tx-sender))
  )
)

(define-public (leave-savings-group (group-id uint))
  (let
    (
      (group (unwrap! (map-get? savings-groups { group-id: group-id }) ERR_GROUP_NOT_FOUND))
      (member-key { group-id: group-id, member: tx-sender })
      (member (unwrap! (map-get? group-members member-key) ERR_NOT_GROUP_MEMBER))
      (refund-amount (/ (* (get total-contributed member) u90) u100))
    )
    (asserts! (not (get completed group)) ERR_CANNOT_LEAVE_GROUP)
    (asserts! (> (get total-contributed member) u0) ERR_INSUFFICIENT_BALANCE)
    
    (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
    
    (map-delete group-members member-key)
    
    (map-set savings-groups
      { group-id: group-id }
      (merge group {
        current-members: (- (get current-members group) u1),
        current-amount: (- (get current-amount group) (get total-contributed member))
      })
    )
    
    (ok refund-amount)
  )
)

(define-read-only (get-group-details (group-id uint))
  (map-get? savings-groups { group-id: group-id })
)

(define-read-only (get-group-member-details (group-id uint) (member principal))
  (map-get? group-members { group-id: group-id, member: member })
)

(define-read-only (get-group-contribution (group-id uint) (contribution-id uint))
  (map-get? group-contributions { group-id: group-id, contribution-id: contribution-id })
)

(define-read-only (calculate-member-contribution-percentage (group-id uint) (member principal))
  (match (get-group-member-details group-id member)
    member-data (match (get-group-details group-id)
      group-data (if (is-eq (get current-amount group-data) u0)
        u0
        (/ (* (get total-contributed member-data) u100) (get current-amount group-data))
      )
      u0
    )
    u0
  )
)

(define-read-only (get-group-progress (group-id uint))
  (match (get-group-details group-id)
    group (some {
      progress-percentage: (if (is-eq (get target-amount group) u0)
        u0
        (/ (* (get current-amount group) u100) (get target-amount group))
      ),
      members-joined: (get current-members group),
      slots-available: (- (get max-members group) (get current-members group)),
      amount-remaining: (- (get target-amount group) (get current-amount group))
    })
    none
  )
)

(define-read-only (get-next-group-id)
  (var-get next-group-id)
)