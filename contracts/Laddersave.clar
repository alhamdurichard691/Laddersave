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
(define-constant ERR_PLAN_NOT_FOUND (err u116))
(define-constant ERR_PLAN_PAUSED (err u117))
(define-constant ERR_PLAN_NOT_DUE (err u118))
(define-constant ERR_INVALID_FREQUENCY (err u119))
(define-constant ERR_INVALID_TARGET (err u120))
(define-constant ERR_PLAN_ALREADY_EXISTS (err u121))
(define-constant ERR_CHALLENGE_NOT_FOUND (err u122))
(define-constant ERR_CHALLENGE_ENDED (err u123))
(define-constant ERR_CHALLENGE_NOT_ENDED (err u124))
(define-constant ERR_ALREADY_JOINED_CHALLENGE (err u125))
(define-constant ERR_CHALLENGE_FULL (err u126))
(define-constant ERR_NOT_CHALLENGE_PARTICIPANT (err u127))
(define-constant ERR_INVALID_DURATION (err u128))

(define-data-var next-ladder-id uint u1)
(define-data-var total-rewards-pool uint u0)
(define-data-var next-group-id uint u1)
(define-data-var next-plan-id uint u1)
(define-data-var next-challenge-id uint u1)

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

(define-map automated-savings-plans
  { plan-id: uint }
  {
    owner: principal,
    target-type: (string-ascii 10),
    target-id: uint,
    amount: uint,
    frequency-blocks: uint,
    last-executed: uint,
    next-execution: uint,
    is-active: bool,
    created-at: uint,
    total-executions: uint,
    streak-count: uint,
    automation-bonus: uint
  }
)

(define-map plan-execution-history
  { plan-id: uint, execution-id: uint }
  {
    executed-at: uint,
    amount: uint,
    block-height: uint,
    bonus-earned: uint
  }
)

(define-map plan-execution-counter
  { plan-id: uint }
  { next-execution-id: uint }
)

(define-map user-plan-count
  { user: principal }
  { active-plans: uint, total-plans: uint }
)

(define-map savings-challenges
  { challenge-id: uint }
  {
    creator: principal,
    title: (string-ascii 50),
    description: (string-ascii 200),
    entry-fee: uint,
    prize-pool: uint,
    max-participants: uint,
    current-participants: uint,
    start-block: uint,
    end-block: uint,
    winner: (optional principal),
    status: (string-ascii 20),
    challenge-type: (string-ascii 20)
  }
)

(define-map challenge-participants
  { challenge-id: uint, participant: principal }
  {
    amount-saved: uint,
    ladder-id: (optional uint),
    group-id: (optional uint),
    joined-at: uint,
    prize-claimed: bool,
    final-rank: uint
  }
)

(define-map challenge-leaderboard
  { challenge-id: uint, rank: uint }
  {
    participant: principal,
    amount-saved: uint,
    prize-percentage: uint
  }
)

(define-map user-challenge-stats
  { user: principal }
  {
    challenges-joined: uint,
    challenges-won: uint,
    total-prizes-won: uint,
    best-rank: uint
  }
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

(define-public (create-automated-plan (target-type (string-ascii 10)) (target-id uint) (amount uint) (frequency-blocks uint))
  (let
    (
      (plan-id (var-get next-plan-id))
      (user-count (default-to { active-plans: u0, total-plans: u0 } (map-get? user-plan-count { user: tx-sender })))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (and (>= frequency-blocks u144) (<= frequency-blocks u4320)) ERR_INVALID_FREQUENCY)
    (asserts! (or (is-eq target-type "ladder") (is-eq target-type "group")) ERR_INVALID_TARGET)
    
    (if (is-eq target-type "ladder")
      (asserts! (is-some (map-get? savings-ladders { ladder-id: target-id })) ERR_LADDER_NOT_FOUND)
      (asserts! (is-some (map-get? savings-groups { group-id: target-id })) ERR_GROUP_NOT_FOUND)
    )
    
    (map-set automated-savings-plans
      { plan-id: plan-id }
      {
        owner: tx-sender,
        target-type: target-type,
        target-id: target-id,
        amount: amount,
        frequency-blocks: frequency-blocks,
        last-executed: u0,
        next-execution: (+ stacks-block-height frequency-blocks),
        is-active: true,
        created-at: stacks-block-height,
        total-executions: u0,
        streak-count: u0,
        automation-bonus: u0
      }
    )
    
    (map-set plan-execution-counter
      { plan-id: plan-id }
      { next-execution-id: u1 }
    )
    
    (map-set user-plan-count
      { user: tx-sender }
      {
        active-plans: (+ (get active-plans user-count) u1),
        total-plans: (+ (get total-plans user-count) u1)
      }
    )
    
    (var-set next-plan-id (+ plan-id u1))
    (ok plan-id)
  )
)

(define-public (execute-automated-plan (plan-id uint))
  (let
    (
      (plan (unwrap! (map-get? automated-savings-plans { plan-id: plan-id }) ERR_PLAN_NOT_FOUND))
      (execution-counter (unwrap! (map-get? plan-execution-counter { plan-id: plan-id }) ERR_PLAN_NOT_FOUND))
      (execution-id (get next-execution-id execution-counter))
      (streak-bonus (calculate-automation-bonus (get streak-count plan)))
      (total-amount (+ (get amount plan) streak-bonus))
    )
    (asserts! (get is-active plan) ERR_PLAN_PAUSED)
    (asserts! (>= stacks-block-height (get next-execution plan)) ERR_PLAN_NOT_DUE)
    
    (try! (stx-transfer? total-amount tx-sender (as-contract tx-sender)))
    
    (if (is-eq (get target-type plan) "ladder")
      (try! (deposit-to-ladder (get target-id plan) total-amount))
      (try! (contribute-to-group (get target-id plan) total-amount))
    )
    
    (map-set plan-execution-history
      { plan-id: plan-id, execution-id: execution-id }
      {
        executed-at: stacks-block-height,
        amount: total-amount,
        block-height: stacks-block-height,
        bonus-earned: streak-bonus
      }
    )
    
    (map-set plan-execution-counter
      { plan-id: plan-id }
      { next-execution-id: (+ execution-id u1) }
    )
    
    (map-set automated-savings-plans
      { plan-id: plan-id }
      (merge plan {
        last-executed: stacks-block-height,
        next-execution: (+ stacks-block-height (get frequency-blocks plan)),
        total-executions: (+ (get total-executions plan) u1),
        streak-count: (+ (get streak-count plan) u1),
        automation-bonus: (+ (get automation-bonus plan) streak-bonus)
      })
    )
    
    (ok total-amount)
  )
)

(define-public (pause-automated-plan (plan-id uint))
  (let
    (
      (plan (unwrap! (map-get? automated-savings-plans { plan-id: plan-id }) ERR_PLAN_NOT_FOUND))
      (user-count (default-to { active-plans: u0, total-plans: u0 } (map-get? user-plan-count { user: tx-sender })))
    )
    (asserts! (is-eq tx-sender (get owner plan)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active plan) ERR_PLAN_PAUSED)
    
    (map-set automated-savings-plans
      { plan-id: plan-id }
      (merge plan { is-active: false })
    )
    
    (map-set user-plan-count
      { user: tx-sender }
      (merge user-count { active-plans: (- (get active-plans user-count) u1) })
    )
    
    (ok true)
  )
)

(define-public (resume-automated-plan (plan-id uint))
  (let
    (
      (plan (unwrap! (map-get? automated-savings-plans { plan-id: plan-id }) ERR_PLAN_NOT_FOUND))
      (user-count (default-to { active-plans: u0, total-plans: u0 } (map-get? user-plan-count { user: tx-sender })))
    )
    (asserts! (is-eq tx-sender (get owner plan)) ERR_NOT_AUTHORIZED)
    (asserts! (not (get is-active plan)) ERR_PLAN_ALREADY_EXISTS)
    
    (map-set automated-savings-plans
      { plan-id: plan-id }
      (merge plan {
        is-active: true,
        next-execution: (+ stacks-block-height (get frequency-blocks plan)),
        streak-count: u0
      })
    )
    
    (map-set user-plan-count
      { user: tx-sender }
      (merge user-count { active-plans: (+ (get active-plans user-count) u1) })
    )
    
    (ok true)
  )
)

(define-public (update-plan-amount (plan-id uint) (new-amount uint))
  (let
    (
      (plan (unwrap! (map-get? automated-savings-plans { plan-id: plan-id }) ERR_PLAN_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get owner plan)) ERR_NOT_AUTHORIZED)
    (asserts! (> new-amount u0) ERR_INVALID_AMOUNT)
    
    (map-set automated-savings-plans
      { plan-id: plan-id }
      (merge plan { amount: new-amount })
    )
    
    (ok new-amount)
  )
)

(define-public (delete-automated-plan (plan-id uint))
  (let
    (
      (plan (unwrap! (map-get? automated-savings-plans { plan-id: plan-id }) ERR_PLAN_NOT_FOUND))
      (user-count (default-to { active-plans: u0, total-plans: u0 } (map-get? user-plan-count { user: tx-sender })))
    )
    (asserts! (is-eq tx-sender (get owner plan)) ERR_NOT_AUTHORIZED)
    
    (map-delete automated-savings-plans { plan-id: plan-id })
    
    (if (get is-active plan)
      (map-set user-plan-count
        { user: tx-sender }
        (merge user-count { active-plans: (- (get active-plans user-count) u1) })
      )
      true
    )
    
    (ok true)
  )
)

(define-read-only (get-automated-plan (plan-id uint))
  (map-get? automated-savings-plans { plan-id: plan-id })
)

(define-read-only (get-plan-execution (plan-id uint) (execution-id uint))
  (map-get? plan-execution-history { plan-id: plan-id, execution-id: execution-id })
)

(define-read-only (get-user-plan-stats (user principal))
  (default-to { active-plans: u0, total-plans: u0 } (map-get? user-plan-count { user: user }))
)

(define-read-only (is-plan-ready-for-execution (plan-id uint))
  (match (get-automated-plan plan-id)
    plan (and 
      (get is-active plan) 
      (>= stacks-block-height (get next-execution plan))
    )
    false
  )
)

(define-read-only (calculate-automation-bonus (streak-count uint))
  (if (<= streak-count u5)
    u0
    (if (<= streak-count u10)
      u1000
      (if (<= streak-count u20)
        u2500
        u5000
      )
    )
  )
)

(define-read-only (get-plan-status (plan-id uint))
  (match (get-automated-plan plan-id)
    plan (some {
      is-active: (get is-active plan),
      blocks-until-next: (if (>= stacks-block-height (get next-execution plan))
        u0
        (- (get next-execution plan) stacks-block-height)
      ),
      total-saved: (* (get total-executions plan) (get amount plan)),
      bonus-earned: (get automation-bonus plan),
      current-streak: (get streak-count plan)
    })
    none
  )
)

(define-read-only (get-next-plan-id)
  (var-get next-plan-id)
)

(define-public (create-savings-challenge (title (string-ascii 50)) (description (string-ascii 200)) (entry-fee uint) (max-participants uint) (duration-blocks uint) (challenge-type (string-ascii 20)))
  (let
    (
      (challenge-id (var-get next-challenge-id))
      (start-block (+ stacks-block-height u144))
      (end-block (+ start-block duration-blocks))
    )
    (asserts! (> entry-fee u0) ERR_INVALID_AMOUNT)
    (asserts! (and (>= max-participants u2) (<= max-participants u50)) ERR_INVALID_GROUP_SIZE)
    (asserts! (and (>= duration-blocks u1440) (<= duration-blocks u43200)) ERR_INVALID_DURATION)
    (asserts! (or (is-eq challenge-type "total-saved") (is-eq challenge-type "consistency")) ERR_INVALID_TARGET)
    
    (try! (stx-transfer? entry-fee tx-sender (as-contract tx-sender)))
    
    (map-set savings-challenges
      { challenge-id: challenge-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        entry-fee: entry-fee,
        prize-pool: entry-fee,
        max-participants: max-participants,
        current-participants: u1,
        start-block: start-block,
        end-block: end-block,
        winner: none,
        status: "open",
        challenge-type: challenge-type
      }
    )
    
    (map-set challenge-participants
      { challenge-id: challenge-id, participant: tx-sender }
      {
        amount-saved: u0,
        ladder-id: none,
        group-id: none,
        joined-at: stacks-block-height,
        prize-claimed: false,
        final-rank: u0
      }
    )
    
    (var-set next-challenge-id (+ challenge-id u1))
    (ok challenge-id)
  )
)

(define-public (join-savings-challenge (challenge-id uint) (target-type (string-ascii 10)) (target-id uint))
  (let
    (
      (challenge (unwrap! (map-get? savings-challenges { challenge-id: challenge-id }) ERR_CHALLENGE_NOT_FOUND))
      (participant-key { challenge-id: challenge-id, participant: tx-sender })
      (user-stats (default-to { challenges-joined: u0, challenges-won: u0, total-prizes-won: u0, best-rank: u999 } (map-get? user-challenge-stats { user: tx-sender })))
    )
    (asserts! (is-none (map-get? challenge-participants participant-key)) ERR_ALREADY_JOINED_CHALLENGE)
    (asserts! (is-eq (get status challenge) "open") ERR_CHALLENGE_ENDED)
    (asserts! (< (get current-participants challenge) (get max-participants challenge)) ERR_CHALLENGE_FULL)
    (asserts! (< stacks-block-height (get start-block challenge)) ERR_CHALLENGE_ENDED)
    
    (if (is-eq target-type "ladder")
      (asserts! (is-some (map-get? savings-ladders { ladder-id: target-id })) ERR_LADDER_NOT_FOUND)
      (if (is-eq target-type "group")
        (asserts! (is-some (map-get? savings-groups { group-id: target-id })) ERR_GROUP_NOT_FOUND)
        (asserts! false ERR_INVALID_TARGET)
      )
    )
    
    (try! (stx-transfer? (get entry-fee challenge) tx-sender (as-contract tx-sender)))
    
    (map-set challenge-participants
      participant-key
      {
        amount-saved: u0,
        ladder-id: (if (is-eq target-type "ladder") (some target-id) none),
        group-id: (if (is-eq target-type "group") (some target-id) none),
        joined-at: stacks-block-height,
        prize-claimed: false,
        final-rank: u0
      }
    )
    
    (map-set savings-challenges
      { challenge-id: challenge-id }
      (merge challenge {
        current-participants: (+ (get current-participants challenge) u1),
        prize-pool: (+ (get prize-pool challenge) (get entry-fee challenge))
      })
    )
    
    (map-set user-challenge-stats
      { user: tx-sender }
      (merge user-stats { challenges-joined: (+ (get challenges-joined user-stats) u1) })
    )
    
    (ok true)
  )
)

(define-public (update-challenge-progress (challenge-id uint) (new-amount uint))
  (let
    (
      (challenge (unwrap! (map-get? savings-challenges { challenge-id: challenge-id }) ERR_CHALLENGE_NOT_FOUND))
      (participant-key { challenge-id: challenge-id, participant: tx-sender })
      (participant (unwrap! (map-get? challenge-participants participant-key) ERR_NOT_CHALLENGE_PARTICIPANT))
    )
    (asserts! (is-eq (get status challenge) "active") ERR_CHALLENGE_ENDED)
    (asserts! (and (>= stacks-block-height (get start-block challenge)) (< stacks-block-height (get end-block challenge))) ERR_CHALLENGE_ENDED)
    (asserts! (> new-amount (get amount-saved participant)) ERR_INVALID_AMOUNT)
    
    (map-set challenge-participants
      participant-key
      (merge participant { amount-saved: new-amount })
    )
    
    (ok new-amount)
  )
)

(define-public (start-challenge (challenge-id uint))
  (let
    (
      (challenge (unwrap! (map-get? savings-challenges { challenge-id: challenge-id }) ERR_CHALLENGE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator challenge)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status challenge) "open") ERR_CHALLENGE_ENDED)
    (asserts! (>= stacks-block-height (get start-block challenge)) ERR_CHALLENGE_NOT_ENDED)
    
    (map-set savings-challenges
      { challenge-id: challenge-id }
      (merge challenge { status: "active" })
    )
    
    (ok true)
  )
)

(define-public (end-challenge (challenge-id uint))
  (let
    (
      (challenge (unwrap! (map-get? savings-challenges { challenge-id: challenge-id }) ERR_CHALLENGE_NOT_FOUND))
      (winner (get-challenge-winner challenge-id))
    )
    (asserts! (or (is-eq tx-sender (get creator challenge)) (>= stacks-block-height (get end-block challenge))) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status challenge) "active") ERR_CHALLENGE_ENDED)
    
    (map-set savings-challenges
      { challenge-id: challenge-id }
      (merge challenge {
        status: "ended",
        winner: winner
      })
    )
    
    (ok winner)
  )
)

(define-public (claim-challenge-prize (challenge-id uint))
  (let
    (
      (challenge (unwrap! (map-get? savings-challenges { challenge-id: challenge-id }) ERR_CHALLENGE_NOT_FOUND))
      (participant-key { challenge-id: challenge-id, participant: tx-sender })
      (participant (unwrap! (map-get? challenge-participants participant-key) ERR_NOT_CHALLENGE_PARTICIPANT))
      (prize-amount (calculate-prize-amount challenge-id tx-sender))
      (user-stats (default-to { challenges-joined: u0, challenges-won: u0, total-prizes-won: u0, best-rank: u999 } (map-get? user-challenge-stats { user: tx-sender })))
    )
    (asserts! (is-eq (get status challenge) "ended") ERR_CHALLENGE_NOT_ENDED)
    (asserts! (not (get prize-claimed participant)) ERR_ALREADY_CLAIMED)
    (asserts! (> prize-amount u0) ERR_INSUFFICIENT_BALANCE)
    
    (try! (as-contract (stx-transfer? prize-amount tx-sender tx-sender)))
    
    (map-set challenge-participants
      participant-key
      (merge participant { prize-claimed: true })
    )
    
    (map-set user-challenge-stats
      { user: tx-sender }
      (merge user-stats {
        total-prizes-won: (+ (get total-prizes-won user-stats) prize-amount),
        challenges-won: (if (is-eq (some tx-sender) (get winner challenge))
          (+ (get challenges-won user-stats) u1)
          (get challenges-won user-stats)
        )
      })
    )
    
    (ok prize-amount)
  )
)

(define-read-only (get-challenge-details (challenge-id uint))
  (map-get? savings-challenges { challenge-id: challenge-id })
)

(define-read-only (get-challenge-participant (challenge-id uint) (participant principal))
  (map-get? challenge-participants { challenge-id: challenge-id, participant: participant })
)

(define-read-only (get-user-challenge-stats (user principal))
  (default-to { challenges-joined: u0, challenges-won: u0, total-prizes-won: u0, best-rank: u999 } (map-get? user-challenge-stats { user: user }))
)

(define-read-only (get-challenge-winner (challenge-id uint))
  (let
    (
      (challenge (unwrap! (map-get? savings-challenges { challenge-id: challenge-id }) none))
      (max-saved u0)
      (winner none)
    )
    (if (is-eq (get status challenge) "ended")
      (get winner challenge)
      none
    )
  )
)

(define-read-only (calculate-prize-amount (challenge-id uint) (participant principal))
  (match (get-challenge-details challenge-id)
    challenge (match (get-challenge-participant challenge-id participant)
      participant-data (let
        (
          (total-pool (get prize-pool challenge))
          (is-winner (is-eq (some participant) (get winner challenge)))
          (winner-prize (/ (* total-pool u70) u100))
          (runner-up-prize (/ (* total-pool u20) u100))
          (third-prize (/ (* total-pool u10) u100))
        )
        (if is-winner
          winner-prize
          (if (is-eq (get final-rank participant-data) u2)
            runner-up-prize
            (if (is-eq (get final-rank participant-data) u3)
              third-prize
              u0
            )
          )
        )
      )
      u0
    )
    u0
  )
)

(define-read-only (get-challenge-status (challenge-id uint))
  (match (get-challenge-details challenge-id)
    challenge (some {
      is-active: (is-eq (get status challenge) "active"),
      blocks-remaining: (if (> (get end-block challenge) stacks-block-height)
        (- (get end-block challenge) stacks-block-height)
        u0
      ),
      participants-count: (get current-participants challenge),
      slots-available: (- (get max-participants challenge) (get current-participants challenge)),
      prize-pool: (get prize-pool challenge)
    })
    none
  )
)

(define-read-only (get-next-challenge-id)
  (var-get next-challenge-id)
)



