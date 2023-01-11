#lang typed/racket


(require typed/rackunit
         racket/runtime-path)

(require/typed srfi/19
               [#:opaque Date date?]
               [string->date (String String -> Date)]
               [date->string (Date String -> String)])

(define-runtime-path here ".")
(define config-file-path (build-path here "config.rktd"))
(define config-hash : (HashTable Symbol Any)
  (cond [(file-exists? config-file-path)
         (cast (file->value config-file-path)
               (HashTable Symbol Any))]
        [else
         (error 'parse-qif "expected to find config.rktd file at ~v"
                config-file-path)]))
(define category-mapping-filename
  (assert (hash-ref config-hash 'category-mapping-file) string?))

;; oh dear... it occurs to me that representing a record as a
;; hash table might actually make
;; the job of the type checker easier, here...

;; incomplete?

;; text from wikipedia:
; D 	Date. Leading zeroes on month and day can be skipped. Year can be either
;        4 digits or 2 digits or '6 (=2006). 	All 	D25 December 2006
; T 	Amount of the item. For payments, a leading minus sign is
;        required. For deposits, either no sign or a leading plus sign
;        is accepted. Do not include currency symbols ; ($, £, ¥, etc.).
;        Comma separators between thousands are allowed. 	All 	T-1,234.50
; M 	Memo—any text you want to record about the item. 	All 	Mgasoline for my car
; C 	Cleared status. Values are blank (not cleared), "*" or "c" (cleared) and "X" or "R" (reconciled). 	All 	CR
; N 	Number of the check. Can also be "Deposit", "Transfer", "Print", "ATM", "EFT". 	Banking, Splits 	N1001
; P 	Payee. Or a description for deposits, transfers, etc. 	Banking, Investment 	PStandard Oil, Inc.
; A 	Address of Payee. Up to 5 address lines are allowed. A 6th address line is a message that prints on the check.
;        1st line is normally the same as the Payee line—the name of the Payee. 	Banking, Splits 	A101 Main St.
; L 	Category or Transfer and (optionally) Class. The literal values are those defined in the Quicken Category list.
;        SubCategories can be indicated by a colon (":") followed by
;        the subcategory literal. If the Quicken file uses Classes, this can be indicated by a slash
;        ("/") followed by the class literal. For Investments, MiscIncX
;        or MiscExpX actions, Category/class or transfer/class. 	Banking, Splits 	LFuel:car
; F 	Flag this transaction as a reimbursable business expense. 	Banking 	F???

;; NOTE: looks like Paypal is using "S" to denote splits, and Gnucash is buying it ...
;; specifically, it looks like each "S" field is immediately followed by a "$" field indicating the total of the split.
;; oh boy...

;; additional fields for investment transactions:

;; Q   Quantity: number of shares purchased or sold
;; Y   Instrument: used as the sub-account in which to add the shares.
;;       E.G.: if the specified top-level account is Assets:Investments,
;;       and the Y field is "IBM", then the shares would go in
;;       Assets:Investments:IBM

(define-type QifLetter (U 'P 'L 'A 'C 'M 'N 'F 'Y))

;; before the S and $ lines are joined...
(define-type QifRecordPreElt
  (U QifRecordElt
     (List 'S String)
     (List '$ Real)))

;; represents a QIF field
(define-type QifRecordElt
  (U
   (List 'U Real)
   (List 'T Real)
   (List 'Q Real)
   (List 'D Date)
   ;; the transaction splits, listing descriptions and amounts
   (List 'Split (Listof (List String Real)))
   (List QifLetter (U String Real))))
(define-predicate qif-element? QifRecordElt)
(define-type QifRecord (cons (U 'std 'investment) (Listof QifRecordElt)))
(define-type PreCategoryAssoc (Listof (Pair String (Pair String Any))))
(define-type CategoryAssoc (Listof (List String String)))

;; represents whether or not a transaction's payee or note matched a pattern
(define-type MStatus (U 'matched 'unmatched))

(define-predicate category-assoc? CategoryAssoc)
(define-predicate pre-category-assoc? PreCategoryAssoc)
(define-predicate qif-letter? QifLetter)
(define-predicate qif-record-elts? (Listof QifRecordElt))
(define-predicate qif-record? QifRecord)

(provide
 read-records
 write-records
 dollar-string->number
 parse-record
 only-outflows
 remove-category
 remove-cleared
 no-checks
 discard-some-ms
 trim-category-names
 apply-categories
 group-by-payee
 sort-by-has-category
 display-payees-of-unmatched
 QifRecord
 QifRecordElt
 qif-record-elts?
 qif-record?)

;; given a filename and whether to remove leading hashes and
;; whether to split lines such as "^C*" into two lines,
;; and whether to replace blank lines with carets (sigh),
;; read the records in the file.
(: read-records (Path-String Boolean Boolean Boolean -> (Listof QifRecord)))
(define (read-records file remove-leading-hash? break-caret-lines?
                      blanks-to-carets?)
  (define lines
    (call-with-input-file file
      (λ ([port : Input-Port])
        (discard-bom port)
        (port->lines port))))
  (define lines2 (cond [break-caret-lines? (break-caret-lines lines)]
                       [else lines]))
  (define lines3 (cond [blanks-to-carets?
                        (map (λ ([s : String])
                               (match s
                                 ["" "^"]
                                 [other other]))
                             lines2)]
                       [else lines2]))
  (lines->records lines3 remove-leading-hash?))

;; strip a leading byte order mark from an input port
(: discard-bom (Input-Port -> Void))
(define (discard-bom p)
  (void (regexp-try-match #rx"^\uFEFF" p)))

;; convert a list of lines into a list of records
(: lines->records ((Listof String) Boolean -> (Listof QifRecord)))
(define (lines->records lines remove-leading-hash?)
  (define non-blank-lines (filter not-whitespace
                                  (filter not-bang lines)))
  (define record-string-lists
    (filter (lambda (x) (not (empty? x)))
            (split-at-elt "^" non-blank-lines)))
  ;; detect parsing errors by finding files that are all one record
  (when (= (length record-string-lists) 1)
    (fprintf (current-error-port)
             "WARNING: file contains only one record, probably needs runtime flag\n"))
  ;; detect parsing errors by finding very long records
  (define very-long-record-count
    (count very-long-record? record-string-lists))
  (when (< 0 very-long-record-count)
    (error 'lines->records "expected not to find records longer\
 than ~a lines, got: ~e"
           very-long-record-len
           (findf very-long-record? record-string-lists)))
  (map (parse-record remove-leading-hash?) record-string-lists))

;; a record with this many lines is probably a parsing bug
(define very-long-record-len 40)
(define (very-long-record? [r : (Listof String)]) : Boolean
  (<= very-long-record-len (length r)))

;; given whether to remove leading hashes, parse each line of a record
(: parse-record (Boolean -> ((Listof String) -> QifRecord)))
(define ((parse-record remove-leading-hash?) transaction-lines)
  (define pre-elts
    (map (parse-line remove-leading-hash?) transaction-lines))
  (define-values (non-split-elts split-elts)
    (partition qif-element? pre-elts))
  (cons 'std
        (eliminate-trivial-split
         (ensure-no-duplicates
          (append
           non-split-elts
           (list (ann (list 'Split
                            (filter
                             non-empty-split?
                             (join-split-pairs split-elts)))
                      QifRecordElt)))))))

;; given a transaction, drop the Split element if it contains only
;; one split whose description matches the record's description or
;; memo
(define (eliminate-trivial-split [record : (Listof QifRecordElt)])
  : (Listof QifRecordElt)
  (define amount (record-numfield 'T record))
  (match (assoc 'Split record)
    [(list 'Split splits)
     ;; can't quite figure out how to make the types help me here:
     (define splitsy (cast splits (Listof (List String Real))))
     (cond [(and
             (= (length splitsy) 1)
             (= (second (first splitsy))
                amount)
             (member (first (first splitsy))
                     (list
                      (record-strfield 'M record)
                      (record-strfield 'P record)
                      (record-strfield 'L record))))
            ;; drop the split, it contains no new information:
            (filter
             (λ ([r : QifRecordElt])
               (not (equal? (first r) 'Split)))
             record)]
           [else
            record])]
    [other record]))

;; look up a "number" field
(define (record-numfield [tag : Symbol] [r : (Listof QifRecordElt)]) : Real
  (define lookup (assoc tag r))
  (if lookup
      (assert (second lookup) real?)
      0.0))

;; look up a "string" field
(define (record-strfield [tag : Symbol] [r : (Listof QifRecordElt)])
  : String
  (define lookup (assoc tag r))
  (if lookup
      (assert (second lookup) string?)
      ""))

;; drop the empty splits
(define (non-empty-split? (split : (List String Real))) : Boolean
  (not (= (second split) 0)))

;; given a Record, signal an error if the record
;; contains duplicate fields, return it if not
(: ensure-no-duplicates ((Listof QifRecordElt) -> (Listof QifRecordElt)))
(define (ensure-no-duplicates r)
  (when (check-duplicates
         (map (ann first (QifRecordElt -> Any)) (rest r)))
    (raise-argument-error 'ensure-no-duplicates
                          "record without duplicate fields"
                          0 r))
  r)

;; join together pairs of "S" and "$" lines into split lines
(define (join-split-pairs [lines : (Listof QifRecordPreElt)])
  : (Listof (List String Real))
  ;; basically we need a little state machine here:
  ;; - init, and
  ;; - just-saw-S
  (let loop ([state : (U 'init (List 'just-saw-S String)) 'init]
             [remaining : (Listof QifRecordPreElt) lines])
    (match state
      ['init
       (cond
         [(empty? remaining) '()]
         [else
          (define f (first remaining))
          ;; would have used match, but it blew TR's mind.
          (cond
            [(qif-element? f)
             (error 'join-split-pairs
                    "qif elements should already be split out: ~e"
                    f)]
            [(equal? 'S (first f))
             (loop (list 'just-saw-S (second f))
                   (rest remaining))]
            [(equal? '$ (first f))
             (error 'join-splits
                    "found $ not immediately after S")])])]
      [(list 'just-saw-S label)
       (cond
         [(empty? remaining)
          (error 'join-splits
                 "ran out of lines right after 'S'")]
         [else
          (match (first remaining)
            [`($ ,amt) (cons (list label amt)
                             (loop 'init (rest remaining)))]
            [other (error 'join-splits
                          "expected $ to follow S, got: ~e"
                          other)])])])))

;; parse a single line
(: parse-line (Boolean -> (String -> QifRecordPreElt)))
(define ((parse-line remove-leading-hash?) s)
  (match (regexp-match #px"^([$[:alpha:]])(.*)$" s)
    [(list _ "D" (? string? date-str)) 
     `(D ,(or (with-handlers ([exn:fail? (lambda (exn) #f)])
                (string->date date-str "~m/~d/~Y"))
              (with-handlers ([exn:fail? (lambda (exn) #f)])
                (string->date date-str "~m/~d'~Y"))
              (raise-argument-error 'parse-line
                                    "parsable date"
                                    0 s)))]
    [(list _ "U" (? string? num)) `(U ,(string->number/checked num))]
    [(list _ "T" (? string? num)) 
     `(T ,(dollar-string->number num))]
    [(list _ "P" (? string? payee))
     (cond [remove-leading-hash?
            (cond [(regexp-match #px"^#" payee) 
                   (list 'P (substring payee 1))]
                  [(list 'P payee)])]
           [else (list 'P payee)])]
    [(list _ "S" (? string? split-label))
     `(S ,split-label)]
    [(list _ "$" (? string? num)) 
     `($ ,(dollar-string->number num))]
    [(list _ (? string? other) (? string? str))
     (match (string->symbol other)
       [(? qif-letter? s) (list s str)]
       [else (raise-argument-error 'parse-line
                                   "legal qif-starting letter"
                                   0 s)])]
    [#f (error 'parse-line "unparseable line: ~e" s)]))

;; break any line beginning with a caret into two lines
(: break-caret-lines ((Listof String) -> (Listof String)))
(define (break-caret-lines strs)
  (apply
   append
   (for/list : (Listof (Listof String))
     ([str (in-list strs)])
     (cond [(regexp-match #px"^\\^." str)
            (list (substring str 0 1) (substring str 1))]
           [else (list str)]))))

;; convert a a dollar string (may have commas) to a number
(: dollar-string->number (String -> Real))
(define (dollar-string->number str)
  (string->number/checked 
   (apply string 
          (filter (lambda (c) (not (eq? c #\,))) 
                  (string->list (string-trim str))))))

;; convert a string to a number, ensuring not #f
(: string->number/checked (String -> Real))
(define (string->number/checked s)
  (match (string->number s)
    [(? real? n) n]
    [else (raise-argument-error 'string->number/checked
                                "string representing a real number"
                                0 s)]))

;; filter a list by returning only those that are outflows
(: only-outflows ((Listof QifRecord) -> (Listof QifRecord)))
(define (only-outflows records)
  (printf "rejecting non-outflows:\n")
  (filter (lambda ([this : QifRecord])
            (match (assq 'T (rest this))
              [(list 'T (? real? n))
               (if (< n 0)
                   #t
                   (begin 
                     (printf "rejected: ~v\n" this)
                     #f))]
              [other
               (error 'only-outflows "no T element in record: ~a" this)]))
          records))

(: no-checks ((Listof QifRecord) -> (Listof QifRecord)))
(define (no-checks records)
  (printf "rejecting checks:\n")
  (foldr (lambda ([this : QifRecord] [rest : (Listof QifRecord)])
           (if (string=? (payee-or-memo this) "CHECK")
               (begin (printf "rejected: ~v\n" this)
                      rest)
               (cons this rest)))
         null
         records))

(: trim-category-names (QifRecord -> QifRecord))
(define (trim-category-names record) ; the chase statement appends whitespace and a category name to the payee
  (cons (first record)
        (for/list : (Listof QifRecordElt) ([field (in-list (rest record))])
          (match field
            [`(P ,(? string? payee))
             `(P ,(match (regexp-match "(.*[^ ]) *?"
                                       (substring payee 0 (min 26 (string-length payee))))
                    [`(,whole-thing ,(? string? sub-match)) sub-match]))]
            [other other]))))

;; return the payee for a record. Could be a P or an M field.
(: payee-or-memo (QifRecord -> String))
(define (payee-or-memo record)
  (match (assq 'P (rest record))
    [(list 'P (? string? payee))
     payee]
    [other
     (match (assq 'M (rest record))
       [(list 'M (? string? memo))
        memo]
       [other
        (error 'payee
               "no M or P field for record: ~v"
               record)])]))

;; display a missing payee in a format suitable for insertion
;; into category-mapping.rktd (unless it's a vapid string)
(: display-missing-payee (String -> Void))
(define (display-missing-payee payee)
  (when (not (member payee vapid-payees))
    (write (list (string-append "^" (regexp-quote payee)) "")) (newline)))

(define vapid-payees
  '("" "WITHDRAW" "TRANSFER" "FEE" "PURCHASE" "DEPOSIT" "DIVIDEND"))

;; return payee and memo of transactions with no matching category.
(define (payees-of-unmatched [r : QifRecord]) : (Listof String)
  (match (assq 'L (rest r))
    [(list 'L (? string? r))
     (raise-argument-error 'display-payee-of-unmatched
                           "record without L field"
                           0 r)]
    [#f 'dontcare])
  (append
   (match (assq 'P (rest r))
     [(list 'P (? string? payee))
      (list payee)]
     [#f '()])
   (match (assq 'M (rest r))
     [(list 'M (? string? memo))
      (list memo)]
     [#f '()])))

(define (display-payees-of-unmatched [rs : (Listof QifRecord)]) : Void
  (define groups
    ((inst sort (Listof QifRecord) Index)
     (group-by payees-of-unmatched rs)
     (ann > (Index Index -> Boolean))
     #:key (inst length Any)))
  ;; first, show the amounts and dates...
  (pretty-print groups)
  (for ([g (in-list groups)])
    (when (< 1 (length g))
      (printf ";; ~vx:\n" (length g)))
    (map display-missing-payee (payees-of-unmatched (first g)))
    (when (< 1 (length g))
      (printf "\n")))
  (void))

;; find a category by trying to match P, and then
;; to match M. Return (list 'matched record) if updated
;; successfully with a category, return (list 'unmatched record)
;; if not.
(: apply-categories (QifRecord -> (List MStatus QifRecord)))
(define (apply-categories record)
  (match (assq 'P (rest record))
    [#f
     (match (assq 'M (rest record))
       [#f
        (raise-argument-error 'apply-categories
                              "record with P or M field"
                              0 record)]
       [(list 'M (? string? memo))
        (match (find-category memo)
          [#f
           (list 'unmatched record)]
          [(? string? category)
           (list 'matched
                 (add-category-field category record))])])]
    [(list 'P (? string? payee))
     (match (find-category payee)
       [#f (match (assq 'M (rest record))
             [#f
              (list 'unmatched record)]
             [(list 'M (? string? memo))
              (match (find-category memo)
                [#f
                 (list 'unmatched record)]
                [(? string? category)
                 (list 'matched
                       (add-category-field category record))])])]
       [(? string? category)
        (list 'matched
              (add-category-field category record))])]))

(: find-category (String -> (U False String)))
(define (find-category payee-str)
  (let loop ([category-assoc category-assoc])
    (if (null? category-assoc)
        #f
        (if (regexp-match (caar category-assoc) payee-str)
            (check-not-empty-string?
             (cadar category-assoc)
             payee-str)
            (loop (cdr category-assoc))))))

(define (check-not-empty-string? [s : String]
                                 [payee : String]) : String
  (cond [(equal? s "")
         (error 'check-not-empty-string
                "expected nonempty category string for payee: ~e"
                payee)]
        [else s]))

;; add a category. If one is already present, discard it if it's 
;; empty or signal an error if it's not.
(: add-category-field (String QifRecord -> QifRecord))
(define (add-category-field new-category record)
  (define-values (existing-category others)
    (partition (λ ([f : QifRecordElt]) (eq? (first f) 'L)) (rest record)))
  (match existing-category
    ;; no existing category:
    [(list) (cons (first record) (cons `(L ,new-category) others))]
    ;; existing but blank:
    [(list (list 'L "")) (cons (first record) (cons `(L ,new-category) others))]
    [other (raise-argument-error 'add-category-field
                                 "record with blank or no category field"
                                 1 new-category record)]))

;; does this record have a non-blank category?
(: has-category? (QifRecord -> Boolean))
(define (has-category? record)
  (match (findf (lambda ([field : QifRecordElt])
                  (equal? (car field) 'L)) (rest record))
    [#f #f]
    [(list 'L "") #f]
    [else #t]))

;; remove the category mapping from a record
(: remove-category (QifRecord -> QifRecord))
(define (remove-category r)
  (cons (first r)
        (filter (λ ([field : QifRecordElt]) (not (equal? (car field) 'L)))
                (rest r))))

;; order the records so that those without categories appear first.
(: sort-by-has-category ((Listof QifRecord) -> (Listof QifRecord)))
(define (sort-by-has-category records)
  (define-values (records-with-category records-without-category)
    (partition has-category? records))
  (append records-without-category
          records-with-category))

;; removed the "cleared" status from a record
(: remove-cleared (QifRecord -> QifRecord))
(define (remove-cleared r)
  (cons (first r)
        (filter (λ ([field : QifRecordElt]) (not (equal? (car field) 'C)))
                (rest r))))

;; write a record to a port
(: output-record (QifRecord Output-Port -> Void))
(define (output-record record port)
  (match (first record)
    ['std (void)]
    ['investment (displayln "!Type:Invst" port)])
  (for ([field (in-list (rest record))])
    (display
      (match field
        [`(D ,date) (date->string date "D~m/~d/~Y\n")]
        [`(T ,amt) (~a "T"(cond [(string? amt) amt]
                                [else (/ (exact->inexact (round (* amt 100))) 100)])
                       "\n")]
        ;; splits get serialized in a nasty way.
        [`(Split ,split-list)
         (apply
          string-append
          (for/list : (Listof String) ([split (in-list split-list)])
            (~a "S" (first split) "\n" "$" (second split) "\n")))]
        [`(,id ,content) (format "~a~a\n" id content)])
      port))
  (display "^\n" port))

;; write the records to the given path. In the case of investments,
;; the string provides the name of the base account that the
;; investment accounts extend. Bleah, QIF.
(: write-records ((Listof QifRecord) (U 'bank
                                        (List 'investment
                                              String))
                                     Path-String -> Void))
(define (write-records records kind out-file)
  (call-with-output-file*
   out-file
   (λ ([port : Output-Port])
     (write-records-to-port records kind port))
   #:exists 'truncate))

(define (write-records-to-port [records : (Listof QifRecord)]
                               [kind : (U 'bank
                                          (List 'investment
                                                String))]
                               [port : Output-Port])
  (match kind
       ['bank (displayln "!Type:Bank" port)]
       [(list 'investment account)
        (displayln "!Account" port)
        (displayln (~a "N" account) port)
        (displayln "TInvst" port)
        (displayln "^" port)])
  (for ([record (in-list records)])
    (output-record record port)))

;; replace with plain old 'group-by':
(: group-by-payee ((Listof QifRecord)
                   ->
                   (Listof
                    (List (U String False)
                          (Listof QifRecord)))))
(define (group-by-payee records)
  (define ht
    (for/fold ([ht : (HashTable (U False String) (Listof QifRecord))
                   (hash)])
              ([record (in-list records)])
      (define payee
        (match (assoc 'P (rest record))
          [(list 'P (? string? payee))
           payee]
          [other #f]))
      (hash-set ht payee (cons record (hash-ref ht payee)))))
  (hash-map ht (lambda ([k : (U False String)]
                        [v : (Listof QifRecord)])
                 (list k v))))

(: category-assoc CategoryAssoc)
(define category-assoc
  (match
      (file->value
       (string->path category-mapping-filename))
    [(? pre-category-assoc? ca)
     (map (λ ([los : (Pair String (Pair String Any))]) : (List String String)
            (list (car los) (cadr los)))
          ca)]
    [other (error 'category-assoc
                  "expected category assoc, got ~e"
                  other)]))

;; discard records with these M fields... they involve splits, and shouldn't
;; be added automatically
(: discard-some-ms ((Listof QifRecord) -> (Listof QifRecord)))
(define (discard-some-ms records)
  (filter no-m-match? records))

;; is this record one whose memo does not match any of the patterns below?
(: no-m-match? (QifRecord -> Boolean))
(define (no-m-match? record)
  (cond [(ormap (lambda ([pat : Regexp])
                  (match (assq 'M (rest record))
                    [(list 'M (? string? s))
                     (regexp-match pat s)]
                    [other #f]))
                discard-these-ms)
         (fprintf (current-error-port) "discarding record: ~v\n" record)
         #f]
        [else #t]))

(: discard-these-ms (Listof Regexp))
(define discard-these-ms
  (list #px"^CUESTA COLLEGE/PAYROLL"
        #px"^PAYROLL ST OF CA/CA PAYROLL"
        #px"CO: PAYROLL ST OF CA"
        #px"CO: CAL POLY SAN LUI"
        #px"^DOVENMUEHLE MORT/BILL PAYMT"
        #px"^DOVENMUEHLE MORT/EBILLPAY"
        #px"CO: DOVENMUEHLE MORT ACH ECC WEB ACH Trace 9876543"
        #px"CO: DOVENMUEHLE MORT ACH ECC WEB ACH Trace 1211412"
        #px"CO: DOVENMUEHLE ACH ECC WEB ACH Trace 1211412"
        #px"^TYPE: PAYROLL  ID: 1522018681 CO: CUESTA COLLEGE"
        #px"^WELLS FARGO DEAL/BILL PAYMT"
        #px"^WELLS FARGO DEAL/EBILLPAY"
        #px"CO: WELLS FARGO DEAL ACH ECC WEB ACH "
        )
  )


(: split-at-elt (All (T) (T (Listof T) -> (Listof (Listof T)))))
(define (split-at-elt elt l)
  (let loop ([l l][so-far : (Listof T) '()])
    (match l
      [(list) (list (reverse so-far))]
      [(cons f r) (cond [(equal? f elt)
                         (cons (reverse so-far)
                               (split-at-elt elt r))]
                        [else (loop r (cons f so-far))])])))



(: not-whitespace (String -> Boolean))
(define (not-whitespace s)
  (not (regexp-match #px"^[[:blank:]]*$" s)))

(: not-bang (String -> Boolean))
(define (not-bang s)
  (not (regexp-match #px"^!" s)))

(module+ test
  (define stderrstr (open-output-string))
  (parameterize ([current-error-port stderrstr])
  (check-equal? (discard-some-ms `((std (A "foo1") (M "bar"))
                                   (std (A "foo2") (M "CUESTA COLLEGE/PAYROLL"))
                                   (std (A "foo3") (M "bar"))))
                `((std (A "foo1") (M "bar"))
                  (std (A "foo3") (M "bar")))))
  (check-equal?
   (get-output-string stderrstr)
   "discarding record: '(std (A \"foo2\") (M \"CUESTA COLLEGE/PAYROLL\"))\n")
  
  (check-equal? (split-at-elt "^" '(a b "^" "^" c "^" d "^"))
                '((a b) () (c) (d) ()))
  
  (check-equal? (add-category-field "pringle" '(std (A "bootwash") (T 34.7)))
                '(std (L "pringle") (A "bootwash") (T 34.7)))
  (check-equal? (add-category-field "pringle" '(std (A "bootwash") (L "") (T 34.7)))
                '(std (L "pringle") (A "bootwash") (T 34.7)))

  (check-equal? (has-category? '(std (A "bootwash") (T 34.7))) #f)
  (check-equal? (has-category? '(std (A "bootwash") (L "fungo") (T 34.7))) #t)
  (check-equal? (has-category? '(std (A "bootwash") (L "") (T 34.7))) #f)

  (check-equal? (sort-by-has-category
                 '((std (A "bootwash") (T 34.7) (L "burpy"))
                   (std (A "oouo") (T 37.2))
                   (std (A "zig") (T 3) (L "to"))
                   (std (A "zag") (T 4))))
                '((std (A "oouo") (T 37.2))
                  (std (A "zag") (T 4))
                  (std (A "bootwash") (T 34.7) (L "burpy"))                   
                  (std (A "zig") (T 3) (L "to"))))

  (check-equal? (remove-category
                 '(std (A "bootwash") (T 34.7) (L "burpy") (P "Rootbeer Inc.")))
                '(std (A "bootwash") (T 34.7) (P "Rootbeer Inc.")))

  (check-equal? (remove-cleared
                 '(std (A "bootwash") (C "*") (T 34.7) (P "Rootbeer Inc.")))
                '(std (A "bootwash") (T 34.7) (P "Rootbeer Inc.")))
  
  (check-exn
   #px"expected: record with blank"
   (lambda () (add-category-field "pringle" '(std (A "bootwash") (L "nonempty") (T 34.7)))))

  (check-equal? ((parse-record #f)
                 (regexp-split #px"\n" "D12/14/2015
N24323005347253020010308
PBOO BOO RECORDS SAN LUIS OBISCA
A SAN LUIS OBIS CA 
T-62.60"))
                `(std
                  (D ,(string->date "12/14/2015" "~m/~d/~Y"))
                  (N "24323005347253020010308")
                  (P "BOO BOO RECORDS SAN LUIS OBISCA")
                  (A " SAN LUIS OBIS CA ")
                  (T -62.60)
                  (Split ())))

  (check-equal? ((parse-line #f) "Lwadooda" )
                '(L "wadooda"))

  (check-equal? ((parse-line #f) "SPreApproved Payment Bill User Payment")
                '(S "PreApproved Payment Bill User Payment"))

  (check-equal? ((parse-line #f)  "$-9.99")
                '($ -9.99))
  
  (check-equal? ((parse-line #f) "N24323005347253020010308")
                '(N "24323005347253020010308"))

  (check-equal? (join-split-pairs
                 '((S "def")
                   ($ -3)
                   (S "ghi")
                   ($ 9)))
                '(("def" -3)
                  ("ghi" 9)))

  (check-equal? (break-caret-lines (list "abc" "^def" "ghi" "^^zz" "^"))
                (list "abc" "^" "def" "ghi" "^" "^zz" "^"))

  (check-exn #px"expected: record without duplicate fields"
             (λ ()
               (ensure-no-duplicates (list (list 'A "abc")
                                           (list 'P "def")
                                           (list 'M "oeu")
                                           (list 'P "ddth")))))

  (check-equal? (ensure-no-duplicates (list (list 'A "abc")
                                            (list 'P "def")
                                            (list 'M "oeu")))
                (list (list 'A "abc")
                      (list 'P "def")
                      (list 'M "oeu")))


  (define split-example #<<|
!Type:Cash
T-6.95
LPreApproved Payment Bill User Payment
SPreApproved Payment Bill User Payment
$-6.95
SFee
$0.00
CX
PConsumer Reports
^
|
   )
  
  (check-equal?
   (lines->records (regexp-split #px"\n" split-example) #f)
   (list
    (list
     'std
     '(T -6.95)
     '(L "PreApproved Payment Bill User Payment")
     '(C "X")
     '(P "Consumer Reports")))
   )

  (check-equal?
   (call-with-output-string
    (λ (port)
      (output-record '(std
                       (T -6.95)
                       (L "Explosives")
                       (P "Larry the Lunatic")
                       (Split ()))                     
                     port)))
   "T-6.95
LExplosives
PLarry the Lunatic
^
")
  (check-equal?
   (call-with-output-string
    (λ (port)
      (output-record '(std
                       (T -6.95)
                       (L "Explosives")
                       (P "Larry the Lunatic")
                       (Split (("Shipping" -3.95)
                               ("Goods" -3))))                     
                     port)))
   "T-6.95
LExplosives
PLarry the Lunatic
SShipping
$-3.95
SGoods
$-3
^
" )

  (check-equal?
   (call-with-output-string
    (λ ([port : Output-Port])
      (write-records-to-port
       '((investment
          (Q 0.9)
          (T 500)
          (P "Purchase")
          (N "BuyX")
          (L "[Assets:Investments:Mutual Fund:Cash]")
          (Y "FOO"))
         (investment
          (Q 0.897)
          (T 100)
          (P "Sale")
          (N "SellX")
          (L "[Assets:Investments:Mutual Fund:Cash]")
          (Y "FOO")))
       (list 'investment
             "Assets:Investments:Mutual Fund")
       port)))
   "!Account
NAssets:Investments:Mutual Fund
TInvst
^
!Type:Invst
Q0.9
T500.0
PPurchase
NBuyX
L[Assets:Investments:Mutual Fund:Cash]
YFOO
^
!Type:Invst
Q0.897
T100.0
PSale
NSellX
L[Assets:Investments:Mutual Fund:Cash]
YFOO
^
"))