;; Milestone Reminder System
;; Provides automated reminders and achievement tracking for savings milestones

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u400))
(define-constant ERR_REMINDER_NOT_FOUND (err u401))
(define-constant ERR_INVALID_FREQUENCY (err u402))
(define-constant ERR_BADGE_NOT_FOUND (err u403))
(define-constant ERR_ALREADY_CLAIMED (err u404))
(define-constant ERR_REQUIREMENTS_NOT_MET (err u405))
(define-constant ERR_INVALID_MESSAGE (err u406))

;; Data variables
(define-data-var next-reminder-id uint u1)
(define-data-var next-badge-id uint u1)
(define-data-var total-reminders uint u0)

;; User reminder settings
(define-map user-reminders
  uint
  {
    user: principal,
    ladder-id: uint,
    message: (string-ascii 140),
    frequency-blocks: uint,
    next-reminder: uint,
    is-active: bool,
    created-at: uint,
    total-sent: uint,
    reminder-type: (string-ascii 20)
  }
)

;; Achievement badges
(define-map achievement-badges
  uint
  {
    name: (string-ascii 50),
    description: (string-ascii 120),
    requirement-type: (string-ascii 30),
    requirement-value: uint,
    badge-icon: (string-ascii 10),
    rarity: (string-ascii 10),
    reward-amount: uint
  }
)

;; User earned badges
(define-map user-badges
  { user: principal, badge-id: uint }
  {
    earned-at: uint,
    ladder-id: (optional uint),
    milestone-reached: uint
  }
)

;; User progress tracking
(define-map user-progress
  principal
  {
    current-streak: uint,
    longest-streak: uint,
    last-activity: uint,
    total-milestones: uint,
    weekly-savings: uint,
    monthly-savings: uint,
    badges-earned: uint,
    reminder-response-rate: uint
  }
)

;; Reminder history
(define-map reminder-history
  { user: principal, reminder-date: uint }
  {
    reminder-id: uint,
    message-sent: (string-ascii 140),
    user-responded: bool,
    response-block: (optional uint)
  }
)

;; Weekly progress snapshots
(define-map weekly-snapshots
  { user: principal, week-number: uint }
  {
    savings-amount: uint,
    milestones-reached: uint,
    active-days: uint,
    snapshot-date: uint
  }
)

;; Set up a milestone reminder
(define-public (create-milestone-reminder 
  (ladder-id uint)
  (message (string-ascii 140))
  (frequency-blocks uint)
  (reminder-type (string-ascii 20)))
  (let (
    (reminder-id (var-get next-reminder-id))
    (current-block stacks-block-height)
  )
    ;; Validations
    (asserts! (> (len message) u0) ERR_INVALID_MESSAGE)
    (asserts! (and (>= frequency-blocks u144) (<= frequency-blocks u4320)) ERR_INVALID_FREQUENCY)
    
    ;; Create reminder
    (map-set user-reminders reminder-id
      {
        user: tx-sender,
        ladder-id: ladder-id,
        message: message,
        frequency-blocks: frequency-blocks,
        next-reminder: (+ current-block frequency-blocks),
        is-active: true,
        created-at: current-block,
        total-sent: u0,
        reminder-type: reminder-type
      })
    
    ;; Update counters
    (var-set next-reminder-id (+ reminder-id u1))
    (var-set total-reminders (+ (var-get total-reminders) u1))
    
    (ok reminder-id)
  ))

;; Check and trigger reminder if due
(define-public (check-reminder (reminder-id uint))
  (let (
    (reminder (unwrap! (map-get? user-reminders reminder-id) ERR_REMINDER_NOT_FOUND))
    (current-block stacks-block-height)
  )
    (asserts! (is-eq tx-sender (get user reminder)) ERR_NOT_AUTHORIZED)
    (asserts! (get is-active reminder) ERR_REMINDER_NOT_FOUND)
    
    (if (>= current-block (get next-reminder reminder))
      (begin
        ;; Update reminder for next time
        (map-set user-reminders reminder-id
          (merge reminder {
            next-reminder: (+ current-block (get frequency-blocks reminder)),
            total-sent: (+ (get total-sent reminder) u1)
          }))
        
        ;; Record in history
        (map-set reminder-history
          { user: tx-sender, reminder-date: current-block }
          {
            reminder-id: reminder-id,
            message-sent: (get message reminder),
            user-responded: false,
            response-block: none
          })
        
        (ok {reminder-due: true, message: (get message reminder)})
      )
      (ok {reminder-due: false, message: ""})
    )
  ))

;; Mark reminder as responded to
(define-public (respond-to-reminder (reminder-date uint))
  (let (
    (history-key { user: tx-sender, reminder-date: reminder-date })
    (history (unwrap! (map-get? reminder-history history-key) ERR_REMINDER_NOT_FOUND))
    (current-progress (default-to 
      {current-streak: u0, longest-streak: u0, last-activity: u0, total-milestones: u0,
       weekly-savings: u0, monthly-savings: u0, badges-earned: u0, reminder-response-rate: u0}
      (map-get? user-progress tx-sender)))
  )
    ;; Update response
    (map-set reminder-history history-key
      (merge history {
        user-responded: true,
        response-block: (some stacks-block-height)
      }))
    
    ;; Update user progress
    (map-set user-progress tx-sender
      (merge current-progress {
        last-activity: stacks-block-height,
        reminder-response-rate: (+ (get reminder-response-rate current-progress) u1)
      }))
    
    (ok true)
  ))

;; Update daily progress (called when user makes deposits)
(define-public (update-daily-progress (amount-saved uint) (milestone-reached bool))
  (let (
    (current-progress (default-to 
      {current-streak: u0, longest-streak: u0, last-activity: u0, total-milestones: u0,
       weekly-savings: u0, monthly-savings: u0, badges-earned: u0, reminder-response-rate: u0}
      (map-get? user-progress tx-sender)))
    (days-since-last (if (> (get last-activity current-progress) u0)
                       (- stacks-block-height (get last-activity current-progress))
                       u999999))
    (streak-continues (< days-since-last u200)) ;; Less than ~1.4 days
    (new-streak (if streak-continues (+ (get current-streak current-progress) u1) u1))
    (new-longest (if (> new-streak (get longest-streak current-progress))
                    new-streak
                    (get longest-streak current-progress)))
    (new-milestones (if milestone-reached 
                       (+ (get total-milestones current-progress) u1)
                       (get total-milestones current-progress)))
  )
    
    ;; Update progress
    (map-set user-progress tx-sender
      {
        current-streak: new-streak,
        longest-streak: new-longest,
        last-activity: stacks-block-height,
        total-milestones: new-milestones,
        weekly-savings: (+ (get weekly-savings current-progress) amount-saved),
        monthly-savings: (+ (get monthly-savings current-progress) amount-saved),
        badges-earned: (get badges-earned current-progress),
        reminder-response-rate: (get reminder-response-rate current-progress)
      })
    
    ;; Check for badge eligibility
    (try! (check-badge-eligibility))
    
    (ok new-streak)
  ))

;; Initialize default badges
(define-public (initialize-badges)
  (begin
    ;; First Milestone Badge
    (map-set achievement-badges u1
      {
        name: "First Steps",
        description: "Reached your first savings milestone",
        requirement-type: "milestones",
        requirement-value: u1,
        badge-icon: "TARGET",
        rarity: "common",
        reward-amount: u1000
      })
    
    ;; Streak Badges
    (map-set achievement-badges u2
      {
        name: "Week Warrior",
        description: "Maintained 7-day savings streak",
        requirement-type: "streak",
        requirement-value: u7,
        badge-icon: "FIRE",
        rarity: "uncommon",
        reward-amount: u2500
      })
    
    (map-set achievement-badges u3
      {
        name: "Milestone Master",
        description: "Reached 10 milestones",
        requirement-type: "milestones",
        requirement-value: u10,
        badge-icon: "TROPHY",
        rarity: "rare",
        reward-amount: u5000
      })
    
    (var-set next-badge-id u4)
    (ok true)
  ))

;; Check if user is eligible for new badges
(define-private (check-badge-eligibility)
  (let (
    (progress (default-to 
      {current-streak: u0, longest-streak: u0, last-activity: u0, total-milestones: u0,
       weekly-savings: u0, monthly-savings: u0, badges-earned: u0, reminder-response-rate: u0}
      (map-get? user-progress tx-sender)))
  )
    ;; Check milestone badges
    (if (and (>= (get total-milestones progress) u1)
             (is-none (map-get? user-badges { user: tx-sender, badge-id: u1 })))
      (begin (try! (award-badge u1)) true)
      true)
    
    ;; Check streak badges
    (if (and (>= (get longest-streak progress) u7)
             (is-none (map-get? user-badges { user: tx-sender, badge-id: u2 })))
      (begin (try! (award-badge u2)) true)
      true)
    
    ;; Check milestone master badge
    (if (and (>= (get total-milestones progress) u10)
             (is-none (map-get? user-badges { user: tx-sender, badge-id: u3 })))
      (begin (try! (award-badge u3)) true)
      true)
    
    (ok true)
  ))

;; Award a badge to user
(define-private (award-badge (badge-id uint))
  (let (
    (badge (unwrap! (map-get? achievement-badges badge-id) ERR_BADGE_NOT_FOUND))
    (current-progress (default-to 
      {current-streak: u0, longest-streak: u0, last-activity: u0, total-milestones: u0,
       weekly-savings: u0, monthly-savings: u0, badges-earned: u0, reminder-response-rate: u0}
      (map-get? user-progress tx-sender)))
  )
    ;; Record badge earned
    (map-set user-badges 
      { user: tx-sender, badge-id: badge-id }
      {
        earned-at: stacks-block-height,
        ladder-id: none,
        milestone-reached: (get total-milestones current-progress)
      })
    
    ;; Update badge count
    (map-set user-progress tx-sender
      (merge current-progress {
        badges-earned: (+ (get badges-earned current-progress) u1)
      }))
    
    ;; Award reward (simplified - in production this would use contract balance)
    (ok (get reward-amount badge))
  ))

;; Toggle reminder active/inactive
(define-public (toggle-reminder (reminder-id uint))
  (let (
    (reminder (unwrap! (map-get? user-reminders reminder-id) ERR_REMINDER_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get user reminder)) ERR_NOT_AUTHORIZED)
    
    (map-set user-reminders reminder-id
      (merge reminder {is-active: (not (get is-active reminder))}))
    
    (ok (not (get is-active reminder)))
  ))

;; Read-only functions

;; Get user's reminders
(define-read-only (get-user-reminder (reminder-id uint))
  (map-get? user-reminders reminder-id))

;; Get user progress summary
(define-read-only (get-user-progress (user principal))
  (map-get? user-progress user))

;; Get user badges
(define-read-only (get-user-badge (user principal) (badge-id uint))
  (map-get? user-badges { user: user, badge-id: badge-id }))

;; Get badge information
(define-read-only (get-badge-info (badge-id uint))
  (map-get? achievement-badges badge-id))

;; Get reminder history
(define-read-only (get-reminder-history (user principal) (reminder-date uint))
  (map-get? reminder-history { user: user, reminder-date: reminder-date }))

;; Check if reminder is due
(define-read-only (is-reminder-due (reminder-id uint))
  (match (map-get? user-reminders reminder-id)
    reminder (and 
               (get is-active reminder)
               (>= stacks-block-height (get next-reminder reminder)))
    false))

;; Get motivation message based on progress
(define-read-only (get-motivation-message (user principal))
  (match (map-get? user-progress user)
    progress (let ((streak (get current-streak progress))
                   (milestones (get total-milestones progress)))
              (if (> streak u10)
                "Amazing streak! You're unstoppable!"
                (if (> streak u5)
                  "Great momentum! Keep it going!"
                  (if (> milestones u5)
                    "Milestone master in the making!"
                    "Every step counts! Stay consistent!"))))
    "Welcome to your savings journey!"))

;; Calculate progress percentage for motivation
(define-read-only (get-progress-insight (user principal))
  (match (map-get? user-progress user)
    progress {
      streak-level: (if (> (get current-streak progress) u20) "legendary"
                      (if (> (get current-streak progress) u10) "excellent"
                        (if (> (get current-streak progress) u5) "good"
                          "building"))),
      milestone-tier: (if (> (get total-milestones progress) u20) "master"
                        (if (> (get total-milestones progress) u10) "expert"
                          (if (> (get total-milestones progress) u5) "advanced"
                            "beginner"))),
      badges-earned: (get badges-earned progress),
      days-since-activity: (if (> (get last-activity progress) u0)
                             (- stacks-block-height (get last-activity progress))
                             u0)
    }
    {
      streak-level: "new",
      milestone-tier: "new", 
      badges-earned: u0,
      days-since-activity: u999999
    }))
