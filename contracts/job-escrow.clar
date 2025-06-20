(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u200))
(define-constant err-job-not-found (err u201))
(define-constant err-insufficient-funds (err u202))
(define-constant err-job-already-assigned (err u203))
(define-constant err-job-not-assigned (err u204))
(define-constant err-invalid-status (err u205))

(define-data-var job-id-nonce uint u0)

(define-map jobs
  uint
  {
    client: principal,
    worker: (optional principal),
    title: (string-ascii 50),
    description: (string-ascii 200),
    payment: uint,
    status: (string-ascii 20),
    created-at: uint,
    completed-at: (optional uint)
  }
)

(define-map escrow-funds
  uint
  { amount: uint }
)

(define-public (create-job (title (string-ascii 50)) (description (string-ascii 200)) (payment uint))
  (let (
    (job-id (var-get job-id-nonce))
  )
    (try! (stx-transfer? payment tx-sender (as-contract tx-sender)))
    
    (map-set jobs
      job-id
      {
        client: tx-sender,
        worker: none,
        title: title,
        description: description,
        payment: payment,
        status: "open",
        created-at: stacks-block-height,
        completed-at: none
      }
    )
    
    (map-set escrow-funds
      job-id
      { amount: payment }
    )
    
    (var-set job-id-nonce (+ job-id u1))
    (ok job-id)
  )
)

(define-public (assign-job (job-id uint) (worker principal))
  (let (
    (job (unwrap! (map-get? jobs job-id) err-job-not-found))
  )
    (asserts! (is-eq tx-sender (get client job)) err-not-authorized)
    (asserts! (is-eq (get status job) "open") err-job-already-assigned)
    
    (map-set jobs
      job-id
      (merge job {
        worker: (some worker),
        status: "assigned"
      })
    )
    (ok true)
  )
)

(define-public (complete-job (job-id uint))
  (let (
    (job (unwrap! (map-get? jobs job-id) err-job-not-found))
    (escrow (unwrap! (map-get? escrow-funds job-id) err-job-not-found))
    (worker (unwrap! (get worker job) err-job-not-assigned))
  )
    (asserts! (is-eq tx-sender (get client job)) err-not-authorized)
    (asserts! (is-eq (get status job) "assigned") err-invalid-status)
    
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender worker)))
    
    (map-set jobs
      job-id
      (merge job {
        status: "completed",
        completed-at: (some stacks-block-height)
      })
    )
    
    (map-delete escrow-funds job-id)
    (ok true)
  )
)

(define-public (cancel-job (job-id uint))
  (let (
    (job (unwrap! (map-get? jobs job-id) err-job-not-found))
    (escrow (unwrap! (map-get? escrow-funds job-id) err-job-not-found))
  )
    (asserts! (is-eq tx-sender (get client job)) err-not-authorized)
    (asserts! (is-eq (get status job) "open") err-invalid-status)
    
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get client job))))
    
    (map-set jobs
      job-id
      (merge job { status: "cancelled" })
    )
    
    (map-delete escrow-funds job-id)
    (ok true)
  )
)

(define-read-only (get-job (job-id uint))
  (map-get? jobs job-id)
)

(define-read-only (get-escrow-amount (job-id uint))
  (map-get? escrow-funds job-id)
)

(define-read-only (get-jobs-by-status (status (string-ascii 20)))
  (ok status)
)

(define-read-only (get-total-jobs)
  (ok (var-get job-id-nonce))
)