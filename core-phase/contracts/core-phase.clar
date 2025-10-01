;; Decentralized Disaster Relief Coordination Platform
;; A smart contract for managing disaster relief efforts with transparency and accountability

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-invalid-status (err u105))

;; Data Variables
(define-data-var disaster-nonce uint u0)
(define-data-var relief-org-nonce uint u0)
(define-data-var allocation-nonce uint u0)

;; Disaster Status Types
(define-constant status-pending u1)
(define-constant status-active u2)
(define-constant status-resolved u3)

;; Data Maps
(define-map disasters
    uint
    {
        location: (string-ascii 100),
        severity: uint,
        status: uint,
        timestamp: uint,
        total-funds: uint,
        allocated-funds: uint,
        creator: principal
    }
)

(define-map relief-organizations
    uint
    {
        name: (string-ascii 50),
        wallet: principal,
        reputation-score: uint,
        verified: bool,
        total-delivered: uint
    }
)

(define-map resource-allocations
    uint
    {
        disaster-id: uint,
        org-id: uint,
        amount: uint,
        resource-type: (string-ascii 30),
        status: uint,
        verified-by: (optional principal),
        impact-score: uint
    }
)

(define-map disaster-donations
    {disaster-id: uint, donor: principal}
    uint
)

(define-map org-validators
    principal
    {
        validation-count: uint,
        governance-tokens: uint
    }
)

;; Read-only functions
(define-read-only (get-disaster (disaster-id uint))
    (map-get? disasters disaster-id)
)

(define-read-only (get-relief-org (org-id uint))
    (map-get? relief-organizations org-id)
)

(define-read-only (get-allocation (allocation-id uint))
    (map-get? resource-allocations allocation-id)
)

(define-read-only (get-donation-amount (disaster-id uint) (donor principal))
    (default-to u0 (map-get? disaster-donations {disaster-id: disaster-id, donor: donor}))
)

(define-read-only (get-validator-info (validator principal))
    (map-get? org-validators validator)
)

;; Public functions

;; Register a new disaster
(define-public (register-disaster (location (string-ascii 100)) (severity uint))
    (let
        (
            (new-id (+ (var-get disaster-nonce) u1))
        )
        (map-set disasters new-id
            {
                location: location,
                severity: severity,
                status: status-pending,
                timestamp: block-height,
                total-funds: u0,
                allocated-funds: u0,
                creator: tx-sender
            }
        )
        (var-set disaster-nonce new-id)
        (ok new-id)
    )
)

;; Register a relief organization
(define-public (register-relief-org (name (string-ascii 50)))
    (let
        (
            (new-id (+ (var-get relief-org-nonce) u1))
        )
        (map-set relief-organizations new-id
            {
                name: name,
                wallet: tx-sender,
                reputation-score: u50,
                verified: false,
                total-delivered: u0
            }
        )
        (var-set relief-org-nonce new-id)
        (ok new-id)
    )
)

;; Donate to a disaster relief fund
(define-public (donate-to-disaster (disaster-id uint) (amount uint))
    (let
        (
            (disaster (unwrap! (map-get? disasters disaster-id) err-not-found))
            (current-donation (get-donation-amount disaster-id tx-sender))
        )
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set disasters disaster-id
            (merge disaster {total-funds: (+ (get total-funds disaster) amount)})
        )
        (map-set disaster-donations
            {disaster-id: disaster-id, donor: tx-sender}
            (+ current-donation amount)
        )
        (ok true)
    )
)

;; Allocate resources to a relief organization
(define-public (allocate-resources 
    (disaster-id uint) 
    (org-id uint) 
    (amount uint) 
    (resource-type (string-ascii 30)))
    (let
        (
            (disaster (unwrap! (map-get? disasters disaster-id) err-not-found))
            (org (unwrap! (map-get? relief-organizations org-id) err-not-found))
            (new-allocation-id (+ (var-get allocation-nonce) u1))
            (available-funds (- (get total-funds disaster) (get allocated-funds disaster)))
        )
        (asserts! (>= available-funds amount) err-insufficient-funds)
        (asserts! (get verified org) err-unauthorized)
        
        (map-set resource-allocations new-allocation-id
            {
                disaster-id: disaster-id,
                org-id: org-id,
                amount: amount,
                resource-type: resource-type,
                status: status-pending,
                verified-by: none,
                impact-score: u0
            }
        )
        
        (map-set disasters disaster-id
            (merge disaster {allocated-funds: (+ (get allocated-funds disaster) amount)})
        )
        
        (var-set allocation-nonce new-allocation-id)
        (ok new-allocation-id)
    )
)

;; Verify impact and release funds
(define-public (verify-impact (allocation-id uint) (impact-score uint))
    (let
        (
            (allocation (unwrap! (map-get? resource-allocations allocation-id) err-not-found))
            (org (unwrap! (map-get? relief-organizations (get org-id allocation)) err-not-found))
            (validator-info (default-to 
                {validation-count: u0, governance-tokens: u0}
                (map-get? org-validators tx-sender)
            ))
        )
        (asserts! (is-eq (get status allocation) status-pending) err-invalid-status)
        
        ;; Transfer funds to organization
        (try! (as-contract (stx-transfer? (get amount allocation) tx-sender (get wallet org))))
        
        ;; Update allocation status
        (map-set resource-allocations allocation-id
            (merge allocation {
                status: status-active,
                verified-by: (some tx-sender),
                impact-score: impact-score
            })
        )
        
        ;; Update organization reputation
        (map-set relief-organizations (get org-id allocation)
            (merge org {
                reputation-score: (+ (get reputation-score org) impact-score),
                total-delivered: (+ (get total-delivered org) (get amount allocation))
            })
        )
        
        ;; Reward validator with governance tokens
        (map-set org-validators tx-sender
            {
                validation-count: (+ (get validation-count validator-info) u1),
                governance-tokens: (+ (get governance-tokens validator-info) u10)
            }
        )
        
        (ok true)
    )
)

;; Verify relief organization (owner only)
(define-public (verify-organization (org-id uint))
    (let
        (
            (org (unwrap! (map-get? relief-organizations org-id) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set relief-organizations org-id
            (merge org {verified: true})
        )
        (ok true)
    )
)

;; Update disaster status (creator or owner only)
(define-public (update-disaster-status (disaster-id uint) (new-status uint))
    (let
        (
            (disaster (unwrap! (map-get? disasters disaster-id) err-not-found))
        )
        (asserts! (or 
            (is-eq tx-sender (get creator disaster))
            (is-eq tx-sender contract-owner)
        ) err-unauthorized)
        (map-set disasters disaster-id
            (merge disaster {status: new-status})
        )
        (ok true)
    )
)