;; Collective Treasury Vault Smart Contract

;; Error Constants
(define-constant ERR-ACCESS-DENIED (err u1))
(define-constant ERR-FUNDS-UNAVAILABLE (err u2))
(define-constant ERR-REQUEST-INVALID (err u3))
(define-constant ERR-GUARDIAN-EXISTS (err u4))
(define-constant ERR-GUARDIAN-MISSING (err u5))
(define-constant ERR-REQUEST-MISSING (err u6))
(define-constant ERR-APPROVAL-EXISTS (err u7))
(define-constant ERR-APPROVAL-LIMIT-HIGH (err u8))
(define-constant ERR-RECIPIENT-INVALID (err u9))
(define-constant ERR-VALUE-INVALID (err u10))

;; Storage Maps and State Variables
(define-map guardian-registry principal bool)
(define-map approval-tracker { request-id: uint, guardian: principal } bool)
(define-map withdrawal-requests 
    { request-id: uint }
    {
        recipient: principal,
        value: uint,
        processed: bool,
        approval-tally: uint
    }
)

;; Vault Administrator (Deployer)
(define-data-var vault-admin principal tx-sender)

;; Approval requirements
(define-data-var minimum-approvals uint u2)
(define-data-var current-request-id uint u0)

;; Input validation functions
(define-private (is-valid-recipient (target principal))
    (and 
        (not (is-eq target tx-sender))  ;; Prevent sending to contract address
        (not (is-eq target (as-contract tx-sender)))  ;; Double-check contract address
    )
)

(define-private (is-valid-value (amount uint))
    (> amount u0)  ;; Ensure non-zero amount
)

;; Setup function for initial configuration
(define-public (setup-vault (guardian-list (list 10 principal)) (approval-threshold uint))
    (begin
        ;; Ensure only vault admin can setup
        (asserts! (is-eq tx-sender (var-get vault-admin)) ERR-ACCESS-DENIED)
        
        ;; Ensure threshold is valid
        (asserts! (< approval-threshold (len guardian-list)) ERR-APPROVAL-LIMIT-HIGH)
        
        ;; Set minimum approvals
        (var-set minimum-approvals approval-threshold)
        
        ;; Register guardians
        (fold register-guardian-in-map guardian-list true)
        
        (ok true)
    )
)

;; Register a guardian in the map
(define-private (register-guardian-in-map (guardian principal) (operation-success bool))
    (begin
        ;; Only proceed if previous operations were successful
        (if operation-success
            (begin
                ;; Check if guardian already registered
                (if (is-none (map-get? guardian-registry guardian))
                    (begin
                        (map-set guardian-registry guardian true)
                        true
                    )
                    false
                )
            )
            false
        )
    )
)

;; Register a new guardian (manual registration)
(define-public (register-guardian (new-guardian principal))
    (begin
        ;; Only vault admin can register guardians
        (asserts! (is-eq tx-sender (var-get vault-admin)) ERR-ACCESS-DENIED)
        
        ;; Check if guardian already registered
        (asserts! (is-none (map-get? guardian-registry new-guardian)) ERR-GUARDIAN-EXISTS)
        
        ;; Register new guardian
        (map-set guardian-registry new-guardian true)
        
        (ok true)
    )
)

;; Unregister a guardian
(define-public (unregister-guardian (guardian principal))
    (begin
        ;; Only vault admin can unregister guardians
        (asserts! (is-eq tx-sender (var-get vault-admin)) ERR-ACCESS-DENIED)
        
        ;; Check if guardian exists
        (asserts! (is-some (map-get? guardian-registry guardian)) ERR-GUARDIAN-MISSING)
        
        ;; Unregister guardian
        (map-delete guardian-registry guardian)
        
        (ok true)
    )
)

;; Submit a new withdrawal request
(define-public (submit-withdrawal-request (recipient principal) (value uint))
    (let 
        (
            (request-id (var-get current-request-id))
        )
        ;; Validate inputs
        (asserts! (is-valid-recipient recipient) ERR-RECIPIENT-INVALID)
        (asserts! (is-valid-value value) ERR-VALUE-INVALID)
        
        ;; Ensure sender is a guardian
        (asserts! (is-some (map-get? guardian-registry tx-sender)) ERR-ACCESS-DENIED)
        
        ;; Store withdrawal request with validated inputs
        (map-set withdrawal-requests { request-id: request-id }
            {
                recipient: recipient,
                value: value,
                processed: false,
                approval-tally: u0
            }
        )
        
        ;; Increment request ID
        (var-set current-request-id (+ request-id u1))
        
        ;; Automatically approve by submitter
        (try! (approve-withdrawal-request request-id))
        
        (ok request-id)
    )
)

;; Approve a withdrawal request
(define-public (approve-withdrawal-request (request-id uint))
    (let 
        (
            (request (unwrap! (map-get? withdrawal-requests { request-id: request-id }) ERR-REQUEST-MISSING))
            (approver tx-sender)
        )
        ;; Ensure sender is a guardian
        (asserts! (is-some (map-get? guardian-registry approver)) ERR-ACCESS-DENIED)
        
        ;; Prevent duplicate approvals
        (asserts! 
            (is-none (map-get? approval-tracker { request-id: request-id, guardian: approver })) 
            ERR-APPROVAL-EXISTS
        )
        
        ;; Mark approval
        (map-set approval-tracker 
            { 
                request-id: request-id, 
                guardian: approver 
            } 
            true
        )
        
        ;; Update approval tally
        (let 
            (
                (current-tally (get approval-tally request))
                (updated-request 
                    (merge request 
                        { 
                            approval-tally: (+ current-tally u1) 
                        }
                    )
                )
            )
            ;; Update request with new approval count
            (map-set withdrawal-requests 
                { request-id: request-id } 
                updated-request
            )
            
            ;; Process if threshold met
            (if (>= (get approval-tally updated-request) (var-get minimum-approvals))
                (process-withdrawal request-id)
                (ok false)
            )
        )
    )
)

;; Process an approved withdrawal request
(define-private (process-withdrawal (request-id uint))
    (let 
        (
            (request (unwrap! (map-get? withdrawal-requests { request-id: request-id }) ERR-REQUEST-MISSING))
        )
        ;; Prevent re-processing
        (asserts! (not (get processed request)) ERR-REQUEST-INVALID)
        
        ;; Transfer funds
        (try! 
            (stx-transfer? 
                (get value request) 
                (as-contract tx-sender) 
                (get recipient request)
            )
        )
        
        ;; Mark as processed
        (map-set withdrawal-requests { request-id: request-id }
            (merge request { processed: true })
        )
        
        (ok true)
    )
)

;; Get withdrawal request details (read-only)
(define-read-only (get-withdrawal-details (request-id uint))
    (map-get? withdrawal-requests { request-id: request-id })
)

;; Check if an address is a guardian
(define-read-only (is-guardian (address principal))
    (map-get? guardian-registry address)
)

;; Get current minimum approvals required
(define-read-only (get-minimum-approvals)
    (var-get minimum-approvals)
)