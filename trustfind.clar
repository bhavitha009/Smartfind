;; SmartFound - Decentralized Lost & Found System
;; A Clarity smart contract for registering lost/found items and managing rewards

;; Contract Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ITEM-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-FUNDS (err u102))
(define-constant ERR-ALREADY-CLAIMED (err u103))
(define-constant ERR-INVALID-MATCH (err u104))
(define-constant ERR-ITEM-ALREADY-EXISTS (err u105))
(define-constant ERR-INVALID-REWARD (err u106))

;; Data Variables
(define-data-var next-item-id uint u1)
(define-data-var total-items uint u0)
(define-data-var total-matches uint u0)
(define-data-var contract-fee uint u50) ;; 0.5% fee in basis points

;; Data Maps
(define-map lost-items
    { item-id: uint }
    {
        owner: principal,
        item-name: (string-ascii 100),
        description: (string-ascii 500),
        location: (string-ascii 200),
        contact-hash: (buff 32), ;; Encrypted contact info
        reward-amount: uint,
        date-lost: uint, ;; Unix timestamp
        date-registered: uint,
        is-active: bool,
        is-claimed: bool
    }
)

(define-map found-items
    { item-id: uint }
    {
        finder: principal,
        item-name: (string-ascii 100),
        description: (string-ascii 500),
        location: (string-ascii 200),
        contact-hash: (buff 32),
        date-found: uint,
        date-registered: uint,
        notes: (string-ascii 300),
        is-active: bool
    }
)

(define-map item-matches
    { match-id: uint }
    {
        lost-item-id: uint,
        found-item-id: uint,
        confidence-score: uint, ;; 0-100
        match-date: uint,
        is-confirmed: bool,
        reward-claimed: bool
    }
)

(define-map user-stats
    { user: principal }
    {
        items-lost: uint,
        items-found: uint,
        rewards-earned: uint,
        rewards-paid: uint,
        reputation-score: uint
    }
)

;; Private Functions

;; Generate hash for contact information (privacy protection)
(define-private (hash-contact-info (contact (string-ascii 200)))
    (sha256 (unwrap-panic (to-consensus-buff? contact)))
)

;; Calculate matching confidence between two items
(define-private (calculate-match-confidence 
    (lost-name (string-ascii 100)) 
    (found-name (string-ascii 100))
    (lost-desc (string-ascii 500))
    (found-desc (string-ascii 500))
    (lost-location (string-ascii 200))
    (found-location (string-ascii 200))
)
    ;; Simplified matching algorithm - in production, this would be more sophisticated
    (let (
        (name-match (if (is-eq lost-name found-name) u40 u0))
        (location-bonus (if (is-eq lost-location found-location) u30 u0))
        (base-score u30) ;; Base confidence for any potential match
    )
        (+ name-match location-bonus base-score)
    )
)

;; Update user statistics
(define-private (update-user-stats (user principal) (stat-type (string-ascii 20)) (amount uint))
    (let (
        (current-stats (default-to 
            { items-lost: u0, items-found: u0, rewards-earned: u0, rewards-paid: u0, reputation-score: u100 }
            (map-get? user-stats { user: user })
        ))
    )
        (if (is-eq stat-type "lost")
            (map-set user-stats { user: user }
                (merge current-stats { items-lost: (+ (get items-lost current-stats) u1) })
            )
            (if (is-eq stat-type "found")
                (map-set user-stats { user: user }
                    (merge current-stats { items-found: (+ (get items-found current-stats) u1) })
                )
                (if (is-eq stat-type "reward-earned")
                    (map-set user-stats { user: user }
                        (merge current-stats { 
                            rewards-earned: (+ (get rewards-earned current-stats) amount),
                            reputation-score: (+ (get reputation-score current-stats) u10)
                        })
                    )
                    (if (is-eq stat-type "reward-paid")
                        (map-set user-stats { user: user }
                            (merge current-stats { rewards-paid: (+ (get rewards-paid current-stats) amount) })
                        )
                        true
                    )
                )
            )
        )
    )
)

;; Public Functions

;; Register a lost item
(define-public (register-lost-item 
    (item-name (string-ascii 100))
    (description (string-ascii 500))
    (location (string-ascii 200))
    (contact-info (string-ascii 200))
    (reward-amount uint)
    (date-lost uint)
)
    (let (
        (item-id (var-get next-item-id))
        (contact-hash (hash-contact-info contact-info))
    )
        ;; Validate reward amount
        (asserts! (>= (stx-get-balance tx-sender) reward-amount) ERR-INSUFFICIENT-FUNDS)
        
        ;; Transfer reward to contract (escrowed)
        (if (> reward-amount u0)
            (try! (stx-transfer? reward-amount tx-sender (as-contract tx-sender)))
            true
        )
        
        ;; Store lost item data
        (map-set lost-items
            { item-id: item-id }
            {
                owner: tx-sender,
                item-name: item-name,
                description: description,
                location: location,
                contact-hash: contact-hash,
                reward-amount: reward-amount,
                date-lost: date-lost,
                date-registered: block-height,
                is-active: true,
                is-claimed: false
            }
        )
        
        ;; Update counters and stats
        (var-set next-item-id (+ item-id u1))
        (var-set total-items (+ (var-get total-items) u1))
        (update-user-stats tx-sender "lost" u0)
        
        (ok item-id)
    )
)

;; Register a found item and check for matches
(define-public (register-found-item
    (item-name (string-ascii 100))
    (description (string-ascii 500))
    (location (string-ascii 200))
    (contact-info (string-ascii 200))
    (date-found uint)
    (notes (string-ascii 300))
)
    (let (
        (item-id (var-get next-item-id))
        (contact-hash (hash-contact-info contact-info))
    )
        ;; Store found item data
        (map-set found-items
            { item-id: item-id }
            {
                finder: tx-sender,
                item-name: item-name,
                description: description,
                location: location,
                contact-hash: contact-hash,
                date-found: date-found,
                date-registered: block-height,
                notes: notes,
                is-active: true
            }
        )
        
        ;; Update counters and stats
        (var-set next-item-id (+ item-id u1))
        (var-set total-items (+ (var-get total-items) u1))
        (update-user-stats tx-sender "found" u0)
        
        ;; Check for potential matches (simplified - would iterate through lost items)
        ;; For now, return the found item ID
        (ok item-id)
    )
)

;; Create a match between lost and found items
(define-public (create-match (lost-item-id uint) (found-item-id uint))
    (let (
        (lost-item (unwrap! (map-get? lost-items { item-id: lost-item-id }) ERR-ITEM-NOT-FOUND))
        (found-item (unwrap! (map-get? found-items { item-id: found-item-id }) ERR-ITEM-NOT-FOUND))
        (match-id (var-get total-matches))
        (confidence (calculate-match-confidence 
            (get item-name lost-item)
            (get item-name found-item)
            (get description lost-item)
            (get description found-item)
            (get location lost-item)
            (get location found-item)
        ))
    )
        ;; Validate items are active
        (asserts! (get is-active lost-item) ERR-INVALID-MATCH)
        (asserts! (get is-active found-item) ERR-INVALID-MATCH)
        (asserts! (not (get is-claimed lost-item)) ERR-ALREADY-CLAIMED)
        
        ;; Create match record
        (map-set item-matches
            { match-id: match-id }
            {
                lost-item-id: lost-item-id,
                found-item-id: found-item-id,
                confidence-score: confidence,
                match-date: block-height,
                is-confirmed: false,
                reward-claimed: false
            }
        )
        
        (var-set total-matches (+ match-id u1))
        (ok { match-id: match-id, confidence: confidence })
    )
)

;; Confirm a match (called by lost item owner)
(define-public (confirm-match (match-id uint))
    (let (
        (match-data (unwrap! (map-get? item-matches { match-id: match-id }) ERR-ITEM-NOT-FOUND))
        (lost-item (unwrap! (map-get? lost-items { item-id: (get lost-item-id match-data) }) ERR-ITEM-NOT-FOUND))
        (found-item (unwrap! (map-get? found-items { item-id: (get found-item-id match-data) }) ERR-ITEM-NOT-FOUND))
        (reward-amount (get reward-amount lost-item))
        (contract-fee-amount (/ (* reward-amount (var-get contract-fee)) u10000))
        (finder-reward (- reward-amount contract-fee-amount))
    )
        ;; Only lost item owner can confirm
        (asserts! (is-eq tx-sender (get owner lost-item)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get is-confirmed match-data)) ERR-ALREADY-CLAIMED)
        
        ;; Update match as confirmed
        (map-set item-matches
            { match-id: match-id }
            (merge match-data { is-confirmed: true, reward-claimed: true })
        )
        
        ;; Mark lost item as claimed
        (map-set lost-items
            { item-id: (get lost-item-id match-data) }
            (merge lost-item { is-claimed: true, is-active: false })
        )
        
        ;; Mark found item as inactive
        (map-set found-items
            { item-id: (get found-item-id match-data) }
            (merge found-item { is-active: false })
        )
        
        ;; Transfer reward to finder (minus contract fee)
        (if (> reward-amount u0)
            (begin
                (try! (as-contract (stx-transfer? finder-reward tx-sender (get finder found-item))))
                (try! (as-contract (stx-transfer? contract-fee-amount tx-sender CONTRACT-OWNER)))
                (update-user-stats (get finder found-item) "reward-earned" finder-reward)
                (update-user-stats tx-sender "reward-paid" reward-amount)
            )
            true
        )
        
        (ok { reward-paid: finder-reward, fee-collected: contract-fee-amount })
    )
)

;; Cancel a lost item registration (refund reward)
(define-public (cancel-lost-item (item-id uint))
    (let (
        (lost-item (unwrap! (map-get? lost-items { item-id: item-id }) ERR-ITEM-NOT-FOUND))
        (reward-amount (get reward-amount lost-item))
    )
        ;; Only owner can cancel
        (asserts! (is-eq tx-sender (get owner lost-item)) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active lost-item) ERR-ALREADY-CLAIMED)
        (asserts! (not (get is-claimed lost-item)) ERR-ALREADY-CLAIMED)
        
        ;; Mark as inactive
        (map-set lost-items
            { item-id: item-id }
            (merge lost-item { is-active: false })
        )
        
        ;; Refund reward
        (if (> reward-amount u0)
            (try! (as-contract (stx-transfer? reward-amount tx-sender (get owner lost-item))))
            true
        )
        
        (ok reward-amount)
    )
)

;; Read-only functions

;; Get lost item details
(define-read-only (get-lost-item (item-id uint))
    (map-get? lost-items { item-id: item-id })
)

;; Get found item details
(define-read-only (get-found-item (item-id uint))
    (map-get? found-items { item-id: item-id })
)

;; Get match details
(define-read-only (get-match (match-id uint))
    (map-get? item-matches { match-id: match-id })
)

;; Get user statistics
(define-read-only (get-user-stats (user principal))
    (default-to 
        { items-lost: u0, items-found: u0, rewards-earned: u0, rewards-paid: u0, reputation-score: u100 }
        (map-get? user-stats { user: user })
    )
)

;; Get contract statistics
(define-read-only (get-contract-stats)
    {
        total-items: (var-get total-items),
        total-matches: (var-get total-matches),
        next-item-id: (var-get next-item-id),
        contract-fee: (var-get contract-fee)
    }
)

;; Get active lost items (simplified - returns count)
(define-read-only (get-active-lost-items-count)
    ;; In a full implementation, this would iterate and count active items
    (var-get total-items)
)

;; Check if user can claim reward for a match
(define-read-only (can-claim-reward (match-id uint) (user principal))
    (match (map-get? item-matches { match-id: match-id })
        match-data (and 
            (get is-confirmed match-data)
            (not (get reward-claimed match-data))
            (match (map-get? found-items { item-id: (get found-item-id match-data) })
                found-item (is-eq user (get finder found-item))
                false
            )
        )
        false
    )
)

;; Admin functions (only contract owner)

;; Update contract fee
(define-public (set-contract-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-fee u1000) ERR-INVALID-REWARD) ;; Max 10% fee
        (var-set contract-fee new-fee)
        (ok new-fee)
    )
)

;; Emergency pause function (for upgrades or issues)
(define-data-var contract-paused bool false)

(define-public (toggle-contract-pause)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set contract-paused (not (var-get contract-paused)))
        (ok (var-get contract-paused))
    )
)

(define-read-only (is-contract-paused)
    (var-get contract-paused)
)
