(define symbol-length
    (lambda (inSym)
        (if (symbol? inSym)
            (string-length (symbol->string inSym))
            0
        )
    )
)

(define sequence?
    (lambda (inSeq)
        (if (null? inSeq)
            #t
            (and (list? inSeq) (eq? (symbol-length (car inSeq)) 1) (sequence? (cdr inSeq)) (symbol? (car inSeq)))
        )
    )
)

(define same-sequence?
    (lambda (inSeq1 inSeq2)
        (if (sequence? inSeq1)
            (if (sequence? inSeq2)
                (if (and (null? inSeq1) (null? inSeq2))
                    #t
                    (if (or (null? inSeq1) (null? inSeq2))
                        #f
                        (and (eq? (car inSeq1) (car inSeq2)) (same-sequence? (cdr inSeq1) (cdr inSeq2)))
                    )
                )
                (error "ERROR305: Second input is not a sequence")
            )
            (error "ERROR305: First input is not a sequence")
        )
    )
)

(define reverse-sequence
    (lambda (inSeq)
        (if (sequence? inSeq)
            (if (null? inSeq)
                '()
                (append 
                (reverse-sequence (cdr inSeq)) (list (car inSeq)))
            )
            (error "ERROR305: Sequence is not a proper sequence")
        )
    )
)

(define palindrome?
    (lambda (inSeq)
        (if (sequence? inSeq)
            (if (null? inSeq)
                #t
                (same-sequence? inSeq (reverse-sequence inSeq))
            )
            (error "ERROR305: Sequence is not a proper sequence")
        )
    )
)

(define member?
    (lambda (inSym inSeq)
        (if (sequence? inSeq)
            (if (symbol? inSym)
                (if (null? inSeq)
                    #f
                    (if (eq? inSym (car inSeq))
                        #t
                        (member? inSym (cdr inSeq))
                    )
                )
                (error "ERROR305: Symbol is not a proper symbol")
            )
            (error "ERROR305: Sequence is not a proper sequence")
        )
    )
)

(define remove-member
    (lambda (inSym inSeq)
        (if (sequence? inSeq)
            (if (symbol? inSym)
                (if (member? inSym inSeq)
                    (if (eq? inSym (car inSeq))
                        (cdr inSeq)
                        (cons (car inSeq) (remove-member inSym (cdr inSeq)))
                    )
                    (error "ERROR305: Symbol is not a member of sequence")
                )
                (error "ERROR305: Symbol is not a proper symbol")
            )
            (error "ERROR305: Sequence is not a proper sequence")
        )
    )
)

(define anagram?
    (lambda (inSeq1 inSeq2)
        (if (sequence? inSeq1)
            (if (sequence? inSeq2)
                (if (and (null? inSeq1) (null? inSeq2)) ; a b c - b a c => 
                    #t
                    (if (or (null? inSeq1) (null? inSeq2))
                        #f
                        (if (member? (car inSeq1) inSeq2)
                            (let ((x (car inSeq1)))
                                (anagram? (remove-member x inSeq1) (remove-member x inSeq2))
                            )
                            #f
                        )
                    )
                )
                (error "ERROR305: Second input is not a sequence")
            )
            (error "ERROR305: First input is not a sequence")
        )
    )
)


(define anapoli?
    (lambda (inSeq1 inSeq2)
        (and (palindrome? inSeq2) (anagram? inSeq1 inSeq2))
    )
)
