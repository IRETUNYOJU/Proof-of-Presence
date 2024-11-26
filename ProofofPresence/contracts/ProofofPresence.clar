;; Proof of Presence (PoP) Smart Contract with Adaptive Rewards
;; A system for verifying and logging event attendance via blockchain with advanced reward mechanisms

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-event (err u101))
(define-constant err-event-inactive (err u102))
(define-constant err-already-claimed (err u103))
(define-constant err-invalid-signature (err u104))
(define-constant err-participant-limit (err u105))
(define-constant err-invalid-reward (err u106))
(define-constant err-insufficient-points (err u107))

;; Event storage (extended with reward-related fields)
(define-map events 
  { event-id: uint }
  {
    name: (string-ascii 100),
    start-time: uint,
    end-time: uint,
    is-active: bool,
    max-participants: uint,
    current-participants: uint,
    reward-tiers: (list 3 uint),  ;; Points for gold, silver, bronze
    skill-badges: (list 3 (string-ascii 50)),  ;; Skill badge names
    reward-points-base: uint  ;; Base points for attendance
  }
)

;; Participant reward tracking
(define-map participant-rewards 
  principal 
  {
    total-points: uint,
    claimed-badges: (list 5 (string-ascii 50)),
    tier-levels: {
      gold-events: uint,
      silver-events: uint,
      bronze-events: uint
    }
  }
)

;; Reward marketplace items
(define-map reward-marketplace
  { item-id: uint }
  {
    name: (string-ascii 100),
    description: (string-ascii 200),
    points-required: uint,
    available-quantity: uint
  }
)

;; Tracking event participation with reward details
(define-map event-participants 
  { event-id: uint, participant: principal }
  { 
    claimed: bool,
    points-earned: uint,
    tier-achieved: (string-ascii 20)
  }
)

;; Unique proof tracking to prevent replay attacks
(define-map used-proofs 
  { proof-hash: (buff 32) }
  { used: bool }
)

;; Event ID and Reward Item ID counters
(define-data-var next-event-id uint u0)
(define-data-var next-reward-item-id uint u0)

;; Redeem reward from marketplace
(define-public (redeem-marketplace-item
  (item-id uint)
  (participant principal)
)
  (let (
    (marketplace-item (unwrap! 
      (map-get? reward-marketplace { item-id: item-id }) 
      err-invalid-reward
    ))
    (participant-reward (unwrap! 
      (map-get? participant-rewards participant) 
      err-invalid-reward
    ))
  )
    ;; Check if participant has enough points
    (asserts! 
      (>= (get total-points participant-reward) (get points-required marketplace-item)) 
      err-insufficient-points
    )
    
    ;; Check item availability
    (asserts! 
      (> (get available-quantity marketplace-item) u0) 
      err-invalid-reward
    )
    
    ;; Deduct points
    (map-set participant-rewards 
      participant
      {
        total-points: (- (get total-points participant-reward) (get points-required marketplace-item)),
        claimed-badges: (get claimed-badges participant-reward),
        tier-levels: (get tier-levels participant-reward)
      }
    )
    
    ;; Update marketplace item quantity
    (map-set reward-marketplace
      { item-id: item-id }
      (merge marketplace-item {
        available-quantity: (- (get available-quantity marketplace-item) u1)
      })
    )
    
    (ok true)
  )
)

;; Rest of the contract remains the same as in the previous submission...
(define-public (create-event 
  (name (string-ascii 100))
  (start-time uint)
  (end-time uint)
  (max-participants uint)
  (reward-points-base uint)
  (reward-tiers (list 3 uint))  ;; [gold, silver, bronze] points
  (skill-badges (list 3 (string-ascii 50)))
)
  (begin
    ;; Only contract owner can create events
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Validate event parameters
    (asserts! (< start-time end-time) err-invalid-event)
    (asserts! (> max-participants u0) err-invalid-event)
    (asserts! (> (len reward-tiers) u0) err-invalid-reward)
    
    ;; Increment event ID
    (var-set next-event-id (+ (var-get next-event-id) u1))
    
    ;; Store event details with rewards
    (map-set events 
      { event-id: (var-get next-event-id) }
      {
        name: name,
        start-time: start-time,
        end-time: end-time,
        is-active: true,
        max-participants: max-participants,
        current-participants: u0,
        reward-tiers: reward-tiers,
        skill-badges: skill-badges,
        reward-points-base: reward-points-base
      }
    )

    ;; Return the new event ID
    (ok (var-get next-event-id))
  )
)

;; Add reward marketplace item
(define-public (add-marketplace-item
  (name (string-ascii 100))
  (description (string-ascii 200))
  (points-required uint)
  (available-quantity uint)
)
  (begin
    ;; Only contract owner can add marketplace items
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Increment reward item ID
    (var-set next-reward-item-id (+ (var-get next-reward-item-id) u1))
    
    ;; Store marketplace item
    (map-set reward-marketplace
      { item-id: (var-get next-reward-item-id) }
      {
        name: name,
        description: description,
        points-required: points-required,
        available-quantity: available-quantity
      }
    )
    
    (ok (var-get next-reward-item-id))
  )
)

;; Claim proof of presence with advanced reward logic
(define-public (claim-proof
  (event-id uint)
  (participant principal)
  (signature (buff 65))
  (timestamp uint)
)
  (let 
    (
      ;; Retrieve event details
      (event (unwrap! 
        (map-get? events { event-id: event-id }) 
        err-invalid-event
      ))
    )
    (let 
      (
        ;; Generate unique proof hash
        (proof-hash (sha256 
          (concat 
            (concat 
              (unwrap-panic (to-consensus-buff? event-id))
              (unwrap-panic (to-consensus-buff? participant))
            )
            (unwrap-panic (to-consensus-buff? timestamp))
          )
        ))
        
        ;; Determine tier and points
        (base-points (get reward-points-base event))
        (reward-tiers (get reward-tiers event))
      )
      (let 
        (
          ;; Determine tier level using nested if statements
          (tier-level 
            (if (< (get current-participants event) (/ (get max-participants event) u3))
              "gold"
              (if (< (get current-participants event) (/ (* (get max-participants event) u2) u3))
                "silver"
                "bronze"
              )
            )
          )
          
          ;; Calculate earned points based on tier using nested if statements
          (earned-points 
            (if (is-eq tier-level "gold")
              (unwrap-panic (element-at reward-tiers u0))
              (if (is-eq tier-level "silver")
                (unwrap-panic (element-at reward-tiers u1))
                (unwrap-panic (element-at reward-tiers u2))
              )
            )
          )
          
          ;; Retrieve or create default participant rewards
          (current-rewards (default-to 
            {
              total-points: u0, 
              claimed-badges: (list), 
              tier-levels: { 
                gold-events: u0, 
                silver-events: u0, 
                bronze-events: u0 
              }
            } 
            (map-get? participant-rewards participant)))
        )
        ;; Validate event conditions
        (asserts! (get is-active event) err-event-inactive)
        (asserts! 
          (and 
            (<= timestamp (get end-time event)) 
            (>= timestamp (get start-time event))
          ) 
          err-invalid-event
        )
        
        ;; Check participant limit
        (asserts! 
          (< (get current-participants event) (get max-participants event)) 
          err-participant-limit
        )
        
        ;; Prevent duplicate claims
        (asserts! 
          (not (default-to false 
            (get claimed (map-get? event-participants { 
              event-id: event-id, 
              participant: participant 
            }))
          )) 
          err-already-claimed
        )
        
        ;; Prevent proof replay attacks
        (asserts! 
          (not (default-to false 
            (get used (map-get? used-proofs { proof-hash: proof-hash }))
          )) 
          err-invalid-signature
        )
        
        ;; Verify signature (placeholder)
        (asserts! 
          (validate-signature participant signature proof-hash) 
          err-invalid-signature
        )
        
        ;; Mark proof as used
        (map-set used-proofs 
          { proof-hash: proof-hash } 
          { used: true }
        )
        
        ;; Record participant claim with tier and points
        (map-set event-participants 
          { event-id: event-id, participant: participant }
          { 
            claimed: true,
            points-earned: earned-points,
            tier-achieved: tier-level
          }
        )
        
        ;; Update event participants count
        (let (
          (updated-event (merge event { 
            current-participants: (+ (get current-participants event) u1) 
          }))
        )
          (map-set events 
            { event-id: event-id }
            updated-event
          )
        )
        
        ;; Update participant rewards
        (let (
          (updated-rewards {
            total-points: (+ (get total-points current-rewards) earned-points),
            claimed-badges: (get claimed-badges current-rewards),
            tier-levels: (merge (get tier-levels current-rewards) {
              gold-events: (if (is-eq tier-level "gold")
                             (+ (get gold-events (get tier-levels current-rewards)) u1)
                             (get gold-events (get tier-levels current-rewards))),
              silver-events: (if (is-eq tier-level "silver")
                               (+ (get silver-events (get tier-levels current-rewards)) u1)
                               (get silver-events (get tier-levels current-rewards))),
              bronze-events: (if (is-eq tier-level "bronze")
                               (+ (get bronze-events (get tier-levels current-rewards)) u1)
                               (get bronze-events (get tier-levels current-rewards)))
            })
          })
        )
          (map-set participant-rewards 
            participant
            updated-rewards)
        )
        
        (ok true)
      )
    )
  )
)

;; Validate signature (placeholder function)
(define-private (validate-signature 
  (participant principal)
  (signature (buff 65))
  (hash (buff 32))
) 
  ;; Actual implementation would depend on specific signature verification logic
  ;; This is a placeholder that always returns true for demonstration
  true
)

;; Read-only functions for retrieving information
(define-read-only (get-event-details (event-id uint))
  (map-get? events { event-id: event-id })
)

(define-read-only (get-participant-rewards (participant principal))
  (map-get? participant-rewards participant)
)

(define-read-only (get-marketplace-item (item-id uint))
  (map-get? reward-marketplace { item-id: item-id })
)