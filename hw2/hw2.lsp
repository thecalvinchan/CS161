(defun DFS(tree)
    (cond
        ((listp tree)
            (append
                (DFS (car tree))
                (cond
                    ((> (length tree) 1)
                        (DFS (cdr tree)))
                )
            ))
        (T
            (list tree))
    )
)

(defun LDFS(tree depth)
    (cond
        ((>= depth 0)
            (cond
                ((equal tree nil)
                    `())
                ((listp tree)   
                    (append
                        (LDFS (car tree) (- depth 1))
                        (LDFS (cdr tree) depth)
                    )
                )
                (T
                    (list tree))
            )
        )
    )
)

(defun DFID(tree depth)
    (cond
        ((> depth 0)
            (append
                (DFID tree (- depth 1))
                (LDFS tree depth)
            )
        )
        (T
            `())
    )
)
