;; Worker Performance Analytics Contract
;; Provides advanced analytics and insights for gig workers

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u500))
(define-constant err-worker-not-found (err u501))
(define-constant err-invalid-period (err u502))
(define-constant err-no-data-available (err u503))

;; Performance snapshot data (weekly snapshots)
(define-map performance-snapshots
  { worker: principal, period: uint }
  {
    jobs-completed: uint,
    total-earnings: uint,
    avg-score: uint,
    endorsements-received: uint,
    snapshot-height: uint
  }
)

;; Worker activity streaks
(define-map worker-streaks
  principal
  {
    current-job-streak: uint,
    max-job-streak: uint,
    current-endorsement-streak: uint,
    max-endorsement-streak: uint,
    last-activity-period: uint
  }
)

;; Market position tracking (simplified to avoid circular dependencies)
(define-map skill-leaderboards
  (string-ascii 30)
  (list 10 { worker: principal, score: uint })
)

;; Performance efficiency metrics
(define-map worker-efficiency
  principal
  {
    avg-completion-time: uint,
    jobs-per-period: uint,
    earnings-per-job: uint,
    peak-performance-period: uint,
    efficiency-score: uint
  }
)

;; Record performance snapshot for a worker
(define-public (record-performance-snapshot (worker principal))
  (let (
    (worker-profile (unwrap! (contract-call? .rep-gig get-worker-profile worker) err-worker-not-found))
    (current-period (/ stacks-block-height u1008)) ;; Weekly periods (~1 week in blocks)
    (jobs-completed (get jobs-completed worker-profile))
    (total-score (get total-score worker-profile))
    (endorsement-count (get endorsement-count worker-profile))
    (avg-score (if (> endorsement-count u0) (/ total-score endorsement-count) u0))
  )
    ;; Store performance snapshot
    (map-set performance-snapshots
      { worker: worker, period: current-period }
      {
        jobs-completed: jobs-completed,
        total-earnings: u0, ;; Would integrate with escrow contract for real earnings
        avg-score: avg-score,
        endorsements-received: endorsement-count,
        snapshot-height: stacks-block-height
      }
    )
    
    ;; Update worker streaks
    (unwrap! (update-worker-streaks worker current-period jobs-completed endorsement-count) err-worker-not-found)
    
    ;; Update efficiency metrics
    (unwrap! (update-efficiency-metrics worker current-period jobs-completed) err-worker-not-found)
    
    (ok true)
  )
)

;; Update worker activity streaks
(define-private (update-worker-streaks (worker principal) (current-period uint) (jobs-completed uint) (endorsements uint))
  (let (
    (existing-streaks (default-to 
      { current-job-streak: u0, max-job-streak: u0, current-endorsement-streak: u0, max-endorsement-streak: u0, last-activity-period: u0 }
      (map-get? worker-streaks worker)
    ))
    (last-period (get last-activity-period existing-streaks))
    (is-consecutive (is-eq (- current-period last-period) u1))
    (has-activity (or (> jobs-completed u0) (> endorsements u0)))
    
    (new-job-streak (if (and is-consecutive (> jobs-completed u0))
      (+ (get current-job-streak existing-streaks) u1)
      (if (> jobs-completed u0) u1 u0)))
    
    (new-endorsement-streak (if (and is-consecutive (> endorsements u0))
      (+ (get current-endorsement-streak existing-streaks) u1)
      (if (> endorsements u0) u1 u0)))
  )
    (if has-activity
      (map-set worker-streaks
        worker
        {
          current-job-streak: new-job-streak,
          max-job-streak: (if (> new-job-streak (get max-job-streak existing-streaks)) new-job-streak (get max-job-streak existing-streaks)),
          current-endorsement-streak: new-endorsement-streak,
          max-endorsement-streak: (if (> new-endorsement-streak (get max-endorsement-streak existing-streaks)) new-endorsement-streak (get max-endorsement-streak existing-streaks)),
          last-activity-period: current-period
        }
      )
      false
    )
    (ok true)
  )
)

;; Update efficiency metrics for a worker
(define-private (update-efficiency-metrics (worker principal) (period uint) (jobs-completed uint))
  (let (
    (existing-efficiency (default-to
      { avg-completion-time: u0, jobs-per-period: u0, earnings-per-job: u0, peak-performance-period: u0, efficiency-score: u0 }
      (map-get? worker-efficiency worker)
    ))
    (jobs-this-period (calculate-jobs-in-period worker period))
    (efficiency-score (calculate-efficiency-score jobs-this-period))
  )
    (map-set worker-efficiency
      worker
      {
        avg-completion-time: u7, ;; Simplified: assume 1 week average
        jobs-per-period: jobs-this-period,
        earnings-per-job: u1000000, ;; Simplified: 1 STX per job average
        peak-performance-period: (if (> jobs-this-period (get jobs-per-period existing-efficiency)) period (get peak-performance-period existing-efficiency)),
        efficiency-score: efficiency-score
      }
    )
    (ok true)
  )
)

;; Calculate jobs completed in current period
(define-private (calculate-jobs-in-period (worker principal) (period uint))
  (match (map-get? performance-snapshots { worker: worker, period: (- period u1) })
    prev-snapshot (let (
      (current-profile (unwrap! (contract-call? .rep-gig get-worker-profile worker) u0))
      (current-jobs (get jobs-completed current-profile))
      (prev-jobs (get jobs-completed prev-snapshot))
    )
      (if (>= current-jobs prev-jobs) (- current-jobs prev-jobs) u0)
    )
    u0 ;; No previous data, assume current total as period activity
  )
)

;; Calculate efficiency score based on various metrics
(define-private (calculate-efficiency-score (jobs-per-period uint))
  (let (
    (base-score (* jobs-per-period u10)) ;; 10 points per job
    (bonus-score (if (> jobs-per-period u5) u50 u0)) ;; Bonus for high activity
  )
    (+ base-score bonus-score)
  )
)

;; Get worker performance trends (simplified to avoid recursion)
(define-read-only (get-performance-trends (worker principal) (periods uint))
  (let (
    (current-period (/ stacks-block-height u1008))
    (current-snapshot (map-get? performance-snapshots { worker: worker, period: current-period }))
    (prev-snapshot (map-get? performance-snapshots { worker: worker, period: (- current-period u1) }))
  )
    (ok {
      worker: worker,
      periods-analyzed: periods,
      current-snapshot: current-snapshot,
      previous-snapshot: prev-snapshot
    })
  )
)

;; Get worker's current streaks and achievements
(define-read-only (get-worker-streaks (worker principal))
  (ok (map-get? worker-streaks worker))
)

;; Get efficiency metrics for a worker
(define-read-only (get-efficiency-metrics (worker principal))
  (ok (map-get? worker-efficiency worker))
)

;; Update skill category leaderboard (simplified)
(define-public (update-skill-leaderboard (skill-category (string-ascii 30)) (worker principal))
  (let (
    (worker-score (unwrap! (contract-call? .rep-gig get-reputation-score worker) err-worker-not-found))
    (current-leaderboard (default-to (list) (map-get? skill-leaderboards skill-category)))
    (new-entry { worker: worker, score: worker-score })
  )
    (match (as-max-len? (append current-leaderboard new-entry) u10)
      updated-leaderboard (begin
        (map-set skill-leaderboards skill-category updated-leaderboard)
        (ok true)
      )
      (ok false)
    )
  )
)

;; Get comprehensive worker analytics dashboard
(define-read-only (get-worker-dashboard (worker principal))
  (let (
    (current-period (/ stacks-block-height u1008))
    (profile (unwrap! (contract-call? .rep-gig get-worker-profile worker) err-worker-not-found))
    (streaks (map-get? worker-streaks worker))
    (efficiency (map-get? worker-efficiency worker))
  )
    (ok {
      basic-stats: profile,
      streaks: streaks,
      efficiency: efficiency,
      analysis-period: current-period
    })
  )
)

;; Calculate worker's performance score based on multiple factors
(define-read-only (calculate-performance-score (worker principal))
  (let (
    (reputation-score (unwrap! (contract-call? .rep-gig get-reputation-score worker) err-worker-not-found))
    (streaks (default-to 
      { current-job-streak: u0, max-job-streak: u0, current-endorsement-streak: u0, max-endorsement-streak: u0, last-activity-period: u0 }
      (map-get? worker-streaks worker)
    ))
    (efficiency (default-to
      { avg-completion-time: u0, jobs-per-period: u0, earnings-per-job: u0, peak-performance-period: u0, efficiency-score: u0 }
      (map-get? worker-efficiency worker)
    ))
    
    ;; Scoring components
    (reputation-component (* reputation-score u20)) ;; Base reputation weight
    (streak-component (* (get current-job-streak streaks) u5)) ;; Streak bonus
    (efficiency-component (get efficiency-score efficiency)) ;; Efficiency bonus
    (consistency-component (if (> (get max-job-streak streaks) u10) u25 u0)) ;; Consistency bonus
  )
    (ok (+ reputation-component streak-component efficiency-component consistency-component))
  )
)

;; Get performance insights and recommendations
(define-read-only (get-performance-insights (worker principal))
  (let (
    (performance-score (unwrap! (calculate-performance-score worker) err-worker-not-found))
    (efficiency (map-get? worker-efficiency worker))
    (streaks (map-get? worker-streaks worker))
  )
    (ok {
      overall-score: performance-score,
      performance-tier: (get-performance-tier performance-score),
      insights: (generate-insights performance-score efficiency streaks),
      recommendations: (generate-recommendations performance-score)
    })
  )
)

;; Determine performance tier based on score
(define-private (get-performance-tier (score uint))
  (if (>= score u200)
    "elite"
    (if (>= score u150)
      "advanced"
      (if (>= score u100)
        "intermediate"
        (if (>= score u50)
          "developing"
          "newcomer"
        )
      )
    )
  )
)

;; Generate performance insights
(define-private (generate-insights (score uint) (efficiency (optional { avg-completion-time: uint, jobs-per-period: uint, earnings-per-job: uint, peak-performance-period: uint, efficiency-score: uint })) (streaks (optional { current-job-streak: uint, max-job-streak: uint, current-endorsement-streak: uint, max-endorsement-streak: uint, last-activity-period: uint })))
  (let (
    (eff-data (default-to { avg-completion-time: u0, jobs-per-period: u0, earnings-per-job: u0, peak-performance-period: u0, efficiency-score: u0 } efficiency))
    (streak-data (default-to { current-job-streak: u0, max-job-streak: u0, current-endorsement-streak: u0, max-endorsement-streak: u0, last-activity-period: u0 } streaks))
  )
    {
      high-performer: (> score u150),
      consistent-worker: (> (get max-job-streak streak-data) u10),
      efficient: (> (get efficiency-score eff-data) u50),
      active-streak: (> (get current-job-streak streak-data) u3)
    }
  )
)

;; Generate personalized recommendations
(define-private (generate-recommendations (score uint))
  {
    focus-area: (if (< score u100) "reputation" "efficiency"),
    target-jobs: (if (< score u50) u5 u10),
    improvement-potential: (- u200 score)
  }
)

;; Get top performers in a skill category
(define-read-only (get-top-performers (skill-category (string-ascii 30)) (limit uint))
  (let (
    (leaderboard (default-to (list) (map-get? skill-leaderboards skill-category)))
    (top-entries (take-top-n leaderboard limit))
  )
    (ok top-entries)
  )
)

;; Helper to take top N entries from leaderboard
(define-private (take-top-n (leaderboard (list 10 { worker: principal, score: uint })) (n uint))
  (if (and (> n u0) (> (len leaderboard) u0))
    (let ((max-take (if (< n (len leaderboard)) n (len leaderboard))))
      (unwrap! (slice? leaderboard u0 max-take) (list))
    )
    (list)
  )
)

;; Get worker's performance comparison with market average
(define-read-only (get-market-comparison (worker principal) (skill-category (string-ascii 30)))
  (let (
    (worker-score (unwrap! (contract-call? .rep-gig get-reputation-score worker) err-worker-not-found))
    (leaderboard (default-to (list) (map-get? skill-leaderboards skill-category)))
    (market-avg (calculate-market-average leaderboard))
  )
    (ok {
      worker-score: worker-score,
      market-average: market-avg,
      above-average: (> worker-score market-avg),
      total-competitors: (len leaderboard)
    })
  )
)

;; Calculate market average score
(define-private (calculate-market-average (leaderboard (list 10 { worker: principal, score: uint })))
  (let (
    (total-scores (fold sum-scores leaderboard u0))
    (count (len leaderboard))
  )
    (if (> count u0) (/ total-scores count) u0)
  )
)

;; Helper to sum scores
(define-private (sum-scores (entry { worker: principal, score: uint }) (acc uint))
  (+ acc (get score entry))
)

;; Get performance snapshot for specific period
(define-read-only (get-performance-snapshot (worker principal) (period uint))
  (map-get? performance-snapshots { worker: worker, period: period })
)

;; Get current performance metrics summary
(define-read-only (get-current-metrics (worker principal))
  (let (
    (current-period (/ stacks-block-height u1008))
    (snapshot (map-get? performance-snapshots { worker: worker, period: current-period }))
    (efficiency (map-get? worker-efficiency worker))
    (streaks (map-get? worker-streaks worker))
  )
    (ok {
      current-period: current-period,
      snapshot: snapshot,
      efficiency: efficiency,
      streaks: streaks
    })
  )
)
