;; (impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

(define-non-fungible-token rep-token uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-already-endorsed (err u101))
(define-constant err-invalid-score (err u102))
(define-constant err-worker-not-found (err u103))

(define-map worker-profiles
  principal
  {
    total-score: uint,
    endorsement-count: uint,
    jobs-completed: uint
  }
)

(define-map endorsements
  { endorser: principal, worker: principal }
  { endorsed: bool }
)

(define-map token-metadata
  uint
  {
    worker: principal,
    score: uint,
    timestamp: uint
  }
)

(define-data-var token-id-nonce uint u0)

(define-public (register-worker)
  (begin
    (map-set worker-profiles
      tx-sender
      {
        total-score: u0,
        endorsement-count: u0,
        jobs-completed: u0
      }
    )
    (ok true)
  )
)

(define-public (complete-job (worker principal))
  (let (
    (profile (unwrap! (map-get? worker-profiles worker) err-worker-not-found))
  )
    (map-set worker-profiles
      worker
      (merge profile {
        jobs-completed: (+ (get jobs-completed profile) u1)
      })
    )
    (ok true)
  )
)

(define-public (endorse-worker (worker principal) (score uint))
  (let (
    (profile (unwrap! (map-get? worker-profiles worker) err-worker-not-found))
    (endorsement-key { endorser: tx-sender, worker: worker })
  )
    (asserts! (< score u6) err-invalid-score)
    (asserts! (not (get endorsed (default-to { endorsed: false } (map-get? endorsements endorsement-key)))) err-already-endorsed)
    
    (map-set endorsements
      endorsement-key
      { endorsed: true }
    )
    
    (map-set worker-profiles
      worker
      (merge profile {
        total-score: (+ (get total-score profile) score),
        endorsement-count: (+ (get endorsement-count profile) u1)
      })
    )
    
    (mint-reputation worker score)
  )
)

(define-private (mint-reputation (worker principal) (score uint))
  (let (
    (token-id (var-get token-id-nonce))
  )
    (try! (nft-mint? rep-token token-id worker))
    (map-set token-metadata
      token-id
      {
        worker: worker,
        score: score,
        timestamp: stacks-block-height
      }
    )
    (var-set token-id-nonce (+ token-id u1))
    (ok token-id)
  )
)

(define-read-only (get-worker-profile (worker principal))
  (map-get? worker-profiles worker)
)

(define-read-only (get-reputation-score (worker principal))
  (match (map-get? worker-profiles worker)
    profile (let (
      (total (get total-score profile))
      (count (get endorsement-count profile))
    )
      (if (is-eq count u0)
        (ok u0)
        (ok (/ total count))
      )
    )
    (err err-worker-not-found)
  )
)

(define-read-only (get-token-data (token-id uint))
  (map-get? token-metadata token-id)
)

(define-read-only (get-owner (token-id uint))
  (nft-get-owner? rep-token token-id)
)

(define-read-only (get-last-token-id)
  (ok (var-get token-id-nonce))
)



(define-map worker-skills 
    principal 
    (list 10 (string-ascii 24)))

(define-public (add-worker-skills (skills (list 10 (string-ascii 24))))
    (let (
        (profile (unwrap! (map-get? worker-profiles tx-sender) err-worker-not-found))
    )
        (map-set worker-skills tx-sender skills)
        (ok true)
    ))

(define-read-only (get-worker-skills (worker principal))
    (map-get? worker-skills worker))


(define-constant dispute-window u144)
(define-constant err-dispute-expired (err u104))

(define-map endorsement-disputes
    uint
    {
        disputer: principal,
        worker: principal,
        reason: (string-ascii 50),
        resolved: bool
    })

(define-public (dispute-endorsement (token-id uint) (reason (string-ascii 50)))
    (let (
        (token-data (unwrap! (map-get? token-metadata token-id) err-worker-not-found))
        (current-height stacks-block-height)
    )
        (asserts! (< (- current-height (get timestamp token-data)) dispute-window) err-dispute-expired)
        (map-set endorsement-disputes
            token-id
            {
                disputer: tx-sender,
                worker: (get worker token-data),
                reason: reason,
                resolved: false
            })
        (ok true)
    ))

(define-read-only (get-dispute (token-id uint))
    (map-get? endorsement-disputes token-id))


