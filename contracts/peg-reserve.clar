;; -------------------------------------------------------------
;; Contract: peg-reserve.clar
;; Description:
;; Fixed-peg stable token backed 1:1 with STX reserve.
;; Users mint PEG by depositing STX. Redeem PEG for STX anytime.
;; Owner can update peg rate and withdraw only excess reserve.
;; -------------------------------------------------------------

(define-constant ERR-NOT-OWNER u100)
(define-constant ERR-ZERO u101)
(define-constant ERR-INCOHERENT u102)
(define-constant ERR-INSUFFICIENT-RESERVE u103)

;; ----- Owner Storage -----
(define-data-var owner (optional principal) none)

;; ----- Peg price -----
;; peg-rate means: 1 PEG = peg-rate STX (in micro-STX)
(define-data-var peg-rate uint u1000000) ;; Default: 1 PEG = 1 STX

;; ----- Reserve Tracking -----
(define-data-var stx-reserve uint u0)
(define-data-var peg-supply uint u0)

;; ----- Token balances -----
(define-map peg-balances
  {account: principal}
  {amount: uint}
)

;; -------------------------------------------------------------
;; Admin setup
;; -------------------------------------------------------------

(define-public (initialize (admin principal))
  (if (is-some (var-get owner))
      (err ERR-INCOHERENT) ;; already initialized
      (if (not (is-eq admin tx-sender))
          (err ERR-NOT-OWNER)
          (begin
            (var-set owner (some admin))
            (ok true)
          )
      )
  )
)

(define-read-only (get-owner) (ok (var-get owner)))

;; -------------------------------------------------------------
;; Internal utilities
;; -------------------------------------------------------------

(define-private (mint-peg (user principal) (amount uint))
  (let ((prev (default-to u0 (get amount (map-get? peg-balances { account: user })))))
    (map-set peg-balances { account: user } { amount: (+ prev amount) })
    (var-set peg-supply (+ (var-get peg-supply) amount))
    (ok true)
  )
)

(define-private (burn-peg (user principal) (amount uint))
  (match (map-get? peg-balances { account: user })
    some-b
      (let ((prev (get amount some-b)))
        (if (< prev amount)
            (err ERR-INCOHERENT)
            (begin
              (map-set peg-balances { account: user } { amount: (- prev amount) })
              (var-set peg-supply (- (var-get peg-supply) amount))
              (ok true)
            )
        )
      )
    (err ERR-INCOHERENT)
  )
)

;; -------------------------------------------------------------
;; User Mint: send STX to mint PEG
;; amount(PEG) = STX sent / peg-rate
;; -------------------------------------------------------------
(define-public (mint (stx-amount uint))
  (let ((rate (var-get peg-rate))
        (sender tx-sender))
    (if (<= stx-amount u0)
        (err ERR-ZERO)
        (let ((peg-amount (/ stx-amount rate)))
          (if (<= peg-amount u0)
              (err ERR-INCOHERENT)
              (begin
                (try! (stx-transfer? stx-amount sender (as-contract tx-sender)))
                (var-set stx-reserve (+ (var-get stx-reserve) stx-amount))
                (mint-peg sender peg-amount)
              )
          )
        )
    )
  )
)

;; -------------------------------------------------------------
;; User Redeem: burn PEG for STX
;; STX out = amount(PEG) * peg-rate
;; -------------------------------------------------------------
(define-public (redeem (amount uint))
  (let ((rate (var-get peg-rate))
        (sender tx-sender))
    (if (<= amount u0)
        (err ERR-ZERO)
        (let ((out (* amount rate)))
          (if (> out (var-get stx-reserve))
              (err ERR-INSUFFICIENT-RESERVE)
              (begin
                (try! (burn-peg sender amount))
                (var-set stx-reserve (- (var-get stx-reserve) out))
                (stx-transfer? out (as-contract tx-sender) sender)
              )
          )
        )
    )
  )
)

;; -------------------------------------------------------------
;; Owner Controls
;; -------------------------------------------------------------
(define-public (set-peg-rate (new-rate uint))
  (let ((owner-opt (var-get owner)))
    (if (is-none owner-opt)
        (err ERR-NOT-OWNER)
        (let ((owner-val (default-to tx-sender owner-opt)))
          (if (not (is-eq owner-val tx-sender))
              (err ERR-NOT-OWNER)
              (if (> new-rate u0)
                  (begin
                    (var-set peg-rate new-rate)
                    (ok new-rate)
                  )
                  (err ERR-ZERO)
              )
          )
        )
    )
  )
)

;; Owner can withdraw only surplus reserve (never collateral share)
(define-public (owner-withdraw (amount uint))
  (let ((owner-opt (var-get owner)))
    (if (is-none owner-opt)
        (err ERR-NOT-OWNER)
        (let ((owner-val (default-to tx-sender owner-opt)))
          (if (not (is-eq tx-sender owner-val))
              (err ERR-NOT-OWNER)
              (let ((min-back (* (var-get peg-supply) (var-get peg-rate)))
                    (reserve (var-get stx-reserve)))
                (if (> amount (- reserve min-back))
                    (err ERR-INSUFFICIENT-RESERVE)
                    (begin
                      (var-set stx-reserve (- reserve amount))
                      (stx-transfer? amount (as-contract tx-sender) owner-val)
                    )
                )
              )
          )
        )
    )
  )
)

;; -------------------------------------------------------------
;; View functions
;; -------------------------------------------------------------
(define-read-only (get-peg-rate) (ok (var-get peg-rate)))
(define-read-only (get-stx-reserve) (ok (var-get stx-reserve)))
(define-read-only (get-peg-supply) (ok (var-get peg-supply)))

(define-read-only (get-balance (user principal))
  (ok (default-to u0 (get amount (map-get? peg-balances { account: user }))))
)
