#lang racket

(require "parse-qif.rkt")

;; keep the inflows?
(define keep-inflows? (make-parameter #f boolean?))
(define remove-leading-hash? (make-parameter #t boolean?))
(define ignore-categories? (make-parameter #f boolean?))
(define break-caret-lines? (make-parameter #f boolean?))
(define blanks-to-carets? (make-parameter #f boolean?))

(command-line #:once-each
              ["--keep" "don't discard the inflows" (keep-inflows? #t)]
              ["--nohash" "remove a leading hash on payees" (remove-leading-hash? #t)]
              ["--ignore-categories" "ignore the existing category annnotations" (ignore-categories? #t)]
              ["--break-caret-lines" "break lines that start with a ^ and then have more characters"
                                     (break-caret-lines? #t)]
              ["--blanks-to-carets" "replace blank lines with lines containing a single caret"
                                    (blanks-to-carets? #t)]
              #:args (src-file target-file)
              (let ()
                (define outflow-filter (cond [(keep-inflows?) (lambda (x) x)]
                                             [else only-outflows]))
                (define records
                  (map remove-cleared (read-records src-file
                                                    (remove-leading-hash?)
                                                    (break-caret-lines?)
                                                    (blanks-to-carets?))))
                (printf "records read: ~v\n" (length records))
                (define cleaned (cond [(ignore-categories?) (map remove-category records)]
                                      [else records]))
                (define records2 (outflow-filter cleaned))
                (printf "records after outflow filtering: ~v\n"
                        (length records2))
                (define records2.5 (discard-some-ms records2))
                (printf "records after complex-split transaction filtering: ~v\n"
                        (length records2.5))
                (define records3 (map apply-categories records2.5))
                (define-values (matched-prs unmatched-prs)
                  (partition (λ (m) (equal? (first m) 'matched)) records3))
                (define unmatched (map second unmatched-prs))
                (define matched (map second matched-prs))
                (printf "~v payees without mappings:\n"
                        (length unmatched))
                (display-payees-of-unmatched unmatched)
                (write-records
                 ;; implicitly sorts by matched status:
                 (append matched unmatched)
                 'bank
                 target-file)))