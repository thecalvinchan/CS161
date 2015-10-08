;; TREE-CONTAINS takes in 2 arguments
;; - N: number N
;; - TREE: ordered tree that is either a number `n or a list (L m R), where
;;      - L and R are ordered trees
;;      - m is a number
;;      - all numbers appearing in L are smaller than m
;;      - all numbers appearing in R are larger than m
;; TREE-CONTAINS returns T if N exists in TREE, and NIL if N does not exist in T
(defun TREE-CONTAINS (N TREE)
    (cond 
        ((listp TREE)
            (cond
                ((< N (second TREE))
                    (TREE-CONTAINS N (first TREE)))
                ((> N (second TREE))
                    (TREE-CONTAINS N (third TREE)))
                (T
                    (= TREE N))
            ))
        (T
            (= TREE N))
            
    )
)

;; TREE-MAX takes in 1 argument
;; - TREE: ordered tree that is either a number `n or a list (L m R), where
;;      - L and R are ordered trees
;;      - m is a number
;;      - all numbers appearing in L are smaller than m
;;      - all numbers appearing in R are larger than m
;; TREE-MAX returns the largest number appearing in TREE
(defun TREE-MAX (TREE)
    (cond 
        ((listp TREE)
            (TREE-MAX (third TREE)))
        (T
            TREE)
            
    )
)

;; SUB-LIST takes in 3 arguments
;; - L: list L 
;; - START: the start index
;; - LEN: the length of the returned list
;; SUB-LIST returns a sub-list of of L of length LEN starting at index START
(defun SUB-LIST (L START LEN)
    (cond 
        ((> START 0)
            (SUB-LIST (cdr L) (- START 1) LEN))
        (T
            (cond
                ((and (> LEN 0) L)
                    (append 
                        (list (car L)) 
                        (SUB-LIST (cdr L) 0 (- LEN 1))
                    ))
                (T
                    nil)
            ))

    )
)

(defun SPLIT-LIST (L)
    (let* (
        (first-list-length (cond 
            ((evenp (length L))
                (/ (length L) 2))
            (T
                (- (/ (length L) 2) 1))
        ))
        (second-list-length (- (length L) first-list-length))
    )
        (list 
            (SUB-LIST L 0 first-list-length) 
            (SUB-LIST L first-list-length second-list-length)
        )
    )
)
