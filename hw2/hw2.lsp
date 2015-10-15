(defun DFS(tree)
    (cond
        ((listp tree)
            (append
                (DFS (first tree))
                (cond
                    ((> (length tree) 1)
                        (DFS (cdr tree)))
                )
            ))
        (T
            (list tree))
    )
)
