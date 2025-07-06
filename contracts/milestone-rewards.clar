(define-non-fungible-token milestone-badge uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u300))
(define-constant err-milestone-not-reached (err u301))
(define-constant err-badge-already-claimed (err u302))
(define-constant err-worker-not-found (err u303))

(define-data-var badge-id-nonce uint u0)

(define-map milestone-definitions
  uint
  {
    name: (string-ascii 30),
    jobs-required: uint,
    score-required: uint,
    badge-type: (string-ascii 20)
  }
)

(define-map worker-milestones
  { worker: principal, milestone-id: uint }
  {
    achieved: bool,
    achieved-at: uint,
    badge-id: (optional uint)
  }
)

(define-map badge-metadata
  uint
  {
    worker: principal,
    milestone-id: uint,
    milestone-name: (string-ascii 30),
    badge-type: (string-ascii 20),
    minted-at: uint
  }
)

(define-private (init-milestones)
  (begin
    (map-set milestone-definitions
      u1
      {
        name: "First Steps",
        jobs-required: u5,
        score-required: u15,
        badge-type: "bronze"
      }
    )
    (map-set milestone-definitions
      u2
      {
        name: "Rising Star",
        jobs-required: u20,
        score-required: u60,
        badge-type: "silver"
      }
    )
    (map-set milestone-definitions
      u3
      {
        name: "Expert Worker",
        jobs-required: u50,
        score-required: u200,
        badge-type: "gold"
      }
    )
    (map-set milestone-definitions
      u4
      {
        name: "Master Professional",
        jobs-required: u100,
        score-required: u400,
        badge-type: "platinum"
      }
    )
    (ok true)
  )
)

(define-public (check-and-award-milestones (worker principal))
  (let (
    (worker-profile (unwrap! (contract-call? .rep-gig get-worker-profile worker) err-worker-not-found))
    (jobs-completed (get jobs-completed worker-profile))
    (total-score (get total-score worker-profile))
  )
    (try! (check-milestone worker u1 jobs-completed total-score))
    (try! (check-milestone worker u2 jobs-completed total-score))
    (try! (check-milestone worker u3 jobs-completed total-score))
    (try! (check-milestone worker u4 jobs-completed total-score))
    (ok true)
  )
)

(define-private (check-milestone (worker principal) (milestone-id uint) (jobs-completed uint) (total-score uint))
  (let (
    (milestone (unwrap! (map-get? milestone-definitions milestone-id) (ok false)))
    (milestone-key { worker: worker, milestone-id: milestone-id })
    (existing-milestone (map-get? worker-milestones milestone-key))
  )
    (if (and
          (>= jobs-completed (get jobs-required milestone))
          (>= total-score (get score-required milestone))
          (is-none existing-milestone))
        (begin
          (try! (mint-milestone-badge worker milestone-id milestone))
          (ok true)
        )
        (ok false)
    )
  )
)

(define-private (mint-milestone-badge (worker principal) (milestone-id uint) (milestone {name: (string-ascii 30), jobs-required: uint, score-required: uint, badge-type: (string-ascii 20)}))
  (let (
    (badge-id (var-get badge-id-nonce))
    (milestone-key { worker: worker, milestone-id: milestone-id })
  )
    (try! (nft-mint? milestone-badge badge-id worker))
    
    (map-set worker-milestones
      milestone-key
      {
        achieved: true,
        achieved-at: stacks-block-height,
        badge-id: (some badge-id)
      }
    )
    
    (map-set badge-metadata
      badge-id
      {
        worker: worker,
        milestone-id: milestone-id,
        milestone-name: (get name milestone),
        badge-type: (get badge-type milestone),
        minted-at: stacks-block-height
      }
    )
    
    (var-set badge-id-nonce (+ badge-id u1))
    (ok badge-id)
  )
)

(define-read-only (get-milestone-definition (milestone-id uint))
  (map-get? milestone-definitions milestone-id)
)

(define-read-only (get-worker-milestone (worker principal) (milestone-id uint))
  (map-get? worker-milestones { worker: worker, milestone-id: milestone-id })
)

(define-read-only (get-badge-metadata (badge-id uint))
  (map-get? badge-metadata badge-id)
)

(define-read-only (get-worker-progress (worker principal))
  (match (contract-call? .rep-gig get-worker-profile worker)
    profile (let (
      (jobs-completed (get jobs-completed profile))
      (total-score (get total-score profile))
      (next-milestone (get-next-milestone-for-worker jobs-completed total-score))
    )
      (ok {
        jobs-completed: jobs-completed,
        total-score: total-score,
        next-milestone: next-milestone
      })
    )
    err-worker-not-found
  )
)

(define-read-only (get-next-milestone-for-worker (jobs-completed uint) (total-score uint))
  (if (< jobs-completed u5)
    (some u1)
    (if (< jobs-completed u20)
      (some u2)
      (if (< jobs-completed u50)
        (some u3)
        (if (< jobs-completed u100)
          (some u4)
          none
        )
      )
    )
  )
)

(define-read-only (get-badge-owner (badge-id uint))
  (nft-get-owner? milestone-badge badge-id)
)

(define-read-only (get-total-badges)
  (ok (var-get badge-id-nonce))
)
