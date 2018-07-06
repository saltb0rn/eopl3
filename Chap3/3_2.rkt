;; Question: Give an expressed value val ∈ ExpVal for which (num-val (expval->num val)) != val
;; Answer: (expval->num (boo-val n)) is undefined, so val cloud be either (boo-val #t) or (bool-val #f).

