;; Proof of Presence (PoP) Smart Contract
;; A system for verifying and logging event attendance via blockchain

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-event (err u101))
(define-constant err-event-inactive (err u102))
(define-constant err-already-claimed (err u103))
(define-constant err-invalid-signature (err u104))
(define-constant err-participant-limit (err u105))

;; Event storage
(define-map events 
  { event-id: uint }
  {
    name: (string-ascii 100),
    start-time: uint,
    end-time: uint,
    is-active: bool,
    max-participants: uint,
    current-participants: uint
  }
)

;; Tracking event participation
(define-map event-participants 
  { event-id: uint, participant: principal }
  { claimed: bool }
)

;; Unique proof tracking to prevent replay attacks
(define-map used-proofs 
  { proof-hash: (buff 32) }
  { used: bool }
)

;; Event ID counter
(define-data-var next-event-id uint u0)

;; Create a new event
(define-public (create-event 
  (name (string-ascii 100))
  (start-time uint)
  (end-time uint)
  (max-participants uint)
)
  (begin
    ;; Only contract owner can create events
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Validate event times
    (asserts! (< start-time end-time) err-invalid-event)
    (asserts! (> max-participants u0) err-invalid-event)

    ;; Increment event ID
    (var-set next-event-id (+ (var-get next-event-id) u1))
    
    ;; Store event details
    (map-set events 
      { event-id: (var-get next-event-id) }
      {
        name: name,
        start-time: start-time,
        end-time: end-time,
        is-active: true,
        max-participants: max-participants,
        current-participants: u0
      }
    )

    ;; Return the new event ID
    (ok (var-get next-event-id))
  )
)

;; Toggle event active status
(define-public (set-event-status 
  (event-id uint)
  (is-active bool)
)
  ;; Only contract owner can modify event status
  (let ((existing-event (map-get? events { event-id: event-id })))
    (if (is-eq tx-sender contract-owner)
      (match existing-event
        event-details
        (begin
          ;; Update event status
          (map-set events 
            { event-id: event-id }
            (merge event-details { is-active: is-active })
          )
          (ok is-active)
        )
        (err err-invalid-event)
      )
      (err err-owner-only)
    )
  )
)

;; Claim proof of presence
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
    
    ;; Verify signature (placeholder - actual implementation depends on signature verification method)
    (asserts! 
      (validate-signature participant signature proof-hash) 
      err-invalid-signature
    )
    
    ;; Mark proof as used
    (map-set used-proofs 
      { proof-hash: proof-hash } 
      { used: true }
    )
    
    ;; Record participant claim
    (map-set event-participants 
      { event-id: event-id, participant: participant }
      { claimed: true }
    )
    
    ;; Update participant count
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
    
    (ok true)
  )
)

;; Retrieve event details
(define-read-only (get-event-details (event-id uint))
  (map-get? events { event-id: event-id })
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

;; Read-only function to check if a participant has claimed a proof
(define-read-only (has-claimed-proof 
  (event-id uint)
  (participant principal)
)
  (default-to false 
    (get claimed (map-get? event-participants { 
      event-id: event-id, 
      participant: participant 
    }))
  )
)