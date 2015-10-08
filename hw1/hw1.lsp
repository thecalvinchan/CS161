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
