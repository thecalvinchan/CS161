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

;; SPLIT-LIST takes in 1 argument
;; - L: list L 
;; SPLIT-LIST returns a list of two lists L1 and L2
;;      - L is the result of appending L1 and L1
;;      - Length of L2 minus length of L1 is 0 or 1
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

;; BTREE-HEIGHT takes in 1 argument
;; - TREE: binary tree in which each node has 0 or 2 children. 
;;      - A node that has 0 child is called a leaf node. 
;;      - A node that has 2 children is called an internal node. 
;;      - A binary tree can be represented as follows:
;;          - A leaf node N is represented by atom N;
;;          - An internal node N is represented by a list (L R), 
;;            where L represents the left child of N and R
;;            represents the right child of N.
;; BTREE-HEIGHT takes a binary tree, TREE, and returns the height of TREE
(defun BTREE-HEIGHT (TREE)
    (cond
        ((listp TREE)
            (let* (
                (left-height (+ 1 (BTREE-HEIGHT (first TREE))))
                (right-height (+ 1 (BTREE-HEIGHT (second TREE))))
            )
                (cond
                    ((< left-height right-height)
                        right-height)
                    (T
                        left-height)
                )
            ) 
        )
        (T
            0)
    )
)

;; LIST2BTREE takes in 1 argument
;; - LEAVES: a non-empty list of atoms
;; LIST2BTREE returns a binary tree such that
;;      - The tree leaves are the elements of LEAVES;
;;      - For any internal (non-leaf) node in the tree, the number of leaves in its right branch minus the
;;        number of leaves in its left branch is 0 or 1.
(defun LIST2BTREE (LEAVES)
    (cond
        ((= (length LEAVES) 2)
            LEAVES)
        ((= (length LEAVES) 1)
            (first LEAVES))
        (T
            (let* (
                (branches (SPLIT-LIST LEAVES))
            )
                (list (LIST2BTREE (first branches)) (LIST2BTREE (second branches)))
            )
        )
    )
)

;; BTREE2LIST takes in 1 argument
;; - TREE: a binary tree
;; LIST2BTREE returns a list of atoms
(defun BTREE2LIST (TREE)
    (cond
        ((listp TREE)
            (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
        (T
            (list TREE))
    )
)
