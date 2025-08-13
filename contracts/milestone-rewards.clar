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

;; Professional Skill Certification Authority System
;; Enables trusted authorities to issue verifiable skill certificates to workers

(define-non-fungible-token skill-certificate uint)

(define-constant err-not-certified-authority (err u400))
(define-constant err-certificate-not-found (err u401))
(define-constant err-certificate-expired (err u402))
(define-constant err-authority-already-exists (err u403))
(define-constant err-invalid-validity-period (err u404))
(define-constant err-certificate-already-revoked (err u405))

(define-data-var certificate-id-nonce uint u0)
(define-data-var max-validity-blocks uint u52560) ;; ~1 year in blocks

;; Trusted certification authorities registry
(define-map certification-authorities
  principal
  {
    name: (string-ascii 50),
    domain: (string-ascii 30),
    authorized: bool,
    authorized-at: uint
  }
)

;; Certificate data storage
(define-map certificates
  uint
  {
    worker: principal,
    authority: principal,
    skill-category: (string-ascii 30),
    skill-name: (string-ascii 50),
    proficiency-level: uint, ;; 1-10 scale
    issued-at: uint,
    expires-at: uint,
    revoked: bool,
    verification-hash: (string-ascii 64)
  }
)

;; Worker's active certificates index
(define-map worker-certificates
  { worker: principal, skill-category: (string-ascii 30) }
  (list 20 uint)
)

;; Authority's issued certificates count
(define-map authority-stats
  principal
  {
    total-issued: uint,
    active-certificates: uint
  }
)

;; Certificate validation requirements by skill category
(define-map skill-requirements
  (string-ascii 30)
  {
    min-proficiency: uint,
    max-validity-blocks: uint,
    requires-renewal: bool
  }
)

;; Register a new certification authority (only contract owner)
(define-public (register-authority (authority principal) (name (string-ascii 50)) (domain (string-ascii 30)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (is-none (map-get? certification-authorities authority)) err-authority-already-exists)
    
    (map-set certification-authorities
      authority
      {
        name: name,
        domain: domain,
        authorized: true,
        authorized-at: stacks-block-height
      }
    )
    
    (map-set authority-stats
      authority
      {
        total-issued: u0,
        active-certificates: u0
      }
    )
    (ok true)
  )
)

;; Issue a skill certificate to a worker
(define-public (issue-certificate 
  (worker principal) 
  (skill-category (string-ascii 30)) 
  (skill-name (string-ascii 50))
  (proficiency-level uint)
  (validity-blocks uint)
  (verification-hash (string-ascii 64)))
  (let (
    (certificate-id (var-get certificate-id-nonce))
    (authority-data (unwrap! (map-get? certification-authorities tx-sender) err-not-certified-authority))
    (expires-at (+ stacks-block-height validity-blocks))
    (max-validity (var-get max-validity-blocks))
  )
    ;; Validate authority and parameters
    (asserts! (get authorized authority-data) err-not-certified-authority)
    (asserts! (and (>= proficiency-level u1) (<= proficiency-level u10)) err-invalid-validity-period)
    (asserts! (<= validity-blocks max-validity) err-invalid-validity-period)
    
    ;; Mint certificate NFT
    (try! (nft-mint? skill-certificate certificate-id worker))
    
    ;; Store certificate data
    (map-set certificates
      certificate-id
      {
        worker: worker,
        authority: tx-sender,
        skill-category: skill-category,
        skill-name: skill-name,
        proficiency-level: proficiency-level,
        issued-at: stacks-block-height,
        expires-at: expires-at,
        revoked: false,
        verification-hash: verification-hash
      }
    )
    
    ;; Update worker's certificate index
    (let (
      (worker-cert-key { worker: worker, skill-category: skill-category })
      (existing-certs (default-to (list) (map-get? worker-certificates worker-cert-key)))
    )
      (map-set worker-certificates
        worker-cert-key
        (unwrap! (as-max-len? (append existing-certs certificate-id) u20) (ok certificate-id))
      )
    )
    
    ;; Update authority statistics
    (let (
      (stats (unwrap! (map-get? authority-stats tx-sender) err-not-certified-authority))
    )
      (map-set authority-stats
        tx-sender
        {
          total-issued: (+ (get total-issued stats) u1),
          active-certificates: (+ (get active-certificates stats) u1)
        }
      )
    )
    
    (var-set certificate-id-nonce (+ certificate-id u1))
    (ok certificate-id)
  )
)

;; Revoke a certificate (only issuing authority)
(define-public (revoke-certificate (certificate-id uint))
  (let (
    (certificate (unwrap! (map-get? certificates certificate-id) err-certificate-not-found))
  )
    (asserts! (is-eq tx-sender (get authority certificate)) err-not-authorized)
    (asserts! (not (get revoked certificate)) err-certificate-already-revoked)
    
    ;; Mark certificate as revoked
    (map-set certificates
      certificate-id
      (merge certificate { revoked: true })
    )
    
    ;; Update authority statistics
    (let (
      (stats (unwrap! (map-get? authority-stats tx-sender) err-not-certified-authority))
    )
      (map-set authority-stats
        tx-sender
        (merge stats { 
          active-certificates: (- (get active-certificates stats) u1)
        })
      )
    )
    (ok true)
  )
)

;; Verify if a certificate is valid and current
(define-read-only (verify-certificate (certificate-id uint))
  (match (map-get? certificates certificate-id)
    certificate (let (
      (current-block stacks-block-height)
      (is-expired (> current-block (get expires-at certificate)))
      (is-revoked (get revoked certificate))
    )
      (ok {
        valid: (and (not is-expired) (not is-revoked)),
        expired: is-expired,
        revoked: is-revoked,
        proficiency-level: (get proficiency-level certificate),
        skill-name: (get skill-name certificate)
      })
    )
    err-certificate-not-found
  )
)

;; Get worker's certificates in a specific skill category
(define-read-only (get-worker-skill-certificates (worker principal) (skill-category (string-ascii 30)))
  (let (
    (cert-ids (default-to (list) (map-get? worker-certificates { worker: worker, skill-category: skill-category })))
  )
    (ok {
      certificates: cert-ids,
      count: (len cert-ids)
    })
  )
)

;; Calculate worker's verified skill score in a category
(define-read-only (get-verified-skill-score (worker principal) (skill-category (string-ascii 30)))
  (let (
    (cert-ids (default-to (list) (map-get? worker-certificates { worker: worker, skill-category: skill-category })))
    (valid-scores (filter-valid-certificates cert-ids))
  )
    (if (> (len valid-scores) u0)
      (ok (calculate-average-score valid-scores))
      (ok u0)
    )
  )
)

;; Helper function to filter valid certificates and extract scores
(define-private (filter-valid-certificates (cert-ids (list 20 uint)))
  (fold check-certificate-validity cert-ids (list))
)

(define-private (check-certificate-validity (cert-id uint) (acc (list 20 uint)))
  (match (map-get? certificates cert-id)
    certificate (let (
      (current-block stacks-block-height)
      (is-valid (and 
        (not (get revoked certificate))
        (<= current-block (get expires-at certificate))
      ))
    )
      (if is-valid
        (unwrap! (as-max-len? (append acc (get proficiency-level certificate)) u20) acc)
        acc
      )
    )
    acc
  )
)

;; Calculate average proficiency score
(define-private (calculate-average-score (scores (list 20 uint)))
  (let (
    (total (fold + scores u0))
    (count (len scores))
  )
    (if (> count u0)
      (/ total count)
      u0
    )
  )
)

;; Get certificate details
(define-read-only (get-certificate (certificate-id uint))
  (map-get? certificates certificate-id)
)

;; Get authority information
(define-read-only (get-authority-info (authority principal))
  (map-get? certification-authorities authority)
)

;; Get authority statistics
(define-read-only (get-authority-stats (authority principal))
  (map-get? authority-stats authority)
)

;; Check if principal is authorized authority
(define-read-only (is-authorized-authority (authority principal))
  (match (map-get? certification-authorities authority)
    auth-data (get authorized auth-data)
    false
  )
)

;; Get total certificates issued
(define-read-only (get-total-certificates)
  (ok (var-get certificate-id-nonce))
)

;; Get certificate owner
(define-read-only (get-certificate-owner (certificate-id uint))
  (nft-get-owner? skill-certificate certificate-id)
)


