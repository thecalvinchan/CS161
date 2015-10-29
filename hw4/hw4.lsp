(defun queens(N)
    (queensHelper `() N 0 0)
)

; queensHelper takes in 4 arguments
; queensList: an existing list of already placed queens
; N: the size of the grid
; column: the current column to attempt to place new queen
; row: the current row to attempt to place new queen
(defun queensHelper(queensList N column row)
    (cond 
        ((>= column N)
            nil
        )
        ((>= row N)
            queensList
        )
        (
            (cond
                ; queen can be placed at column position in this row
                ((checkState queensList 0 column row)
                    (let* (
                        (nqueens (queensHelper (append queensList (list column)) N 0 (+ row 1)))
                    )
                        (cond
                            ; this column position does not lead to a solution
                            ((null nqueens)
                                (queensHelper queensList N (+ column 1) row)
                            )
                            ; this column position leads to a solution
                            (T
                                nqueens
                            )
                        )
                    )
                )
                ; queen cannot be placed at column position in this row
                (T
                    (queensHelper queensList N (+ column 1) row)
                )
            )
        )
    )
)

; checkState checks to see if adding a queen to an old state is valid
; checkState takes in 4 arguments
; queensX is a list of existing queens. The order of the queens in that list
; is sorted by their row index and the value of each atom in the list is the
; corresponding column index of each queen. zero-indexed from top left
(defun checkState (queensX startY newQueenX newQueenY)
    (cond
        ((= (length queensX) 0)
            T
        )
        ((checkQueen (car queensX) startY newQueenX newQueenY)
            ; new queen does not conflict with car of current queensList
            (checkState (cdr queensX) (+ 1 startY) newQueenX newQueenY)
        )
        (T
            ; new queen conflicts with car of current queensList
            nil
        )
    )
)

; checkQueen checks two queens to see if they conflict with each other
(defun checkQueen (queenX queenY newQueenX newQueenY)
    (let* (
        (dx (- queenX newQueenX))
        (dy (- queenY newQueenY))
    )
    (cond
        ((or 
            ; we assume we can't pass in same row
            (= dx 0)            ; same column
            (= (/ dy dx) 1)     ; diagonal
            (= (/ dy dx) -1)    ; diagonal
        )
            nil
        )
        (T
            T
        )
    )
    )
)

; arc-consistency
;(
;(0 1 2 3 4 5 6 7)
;(0 1 2 3 4 5 6 7)
;...
;(0 1 2 3 4 5 6 7)
;)
