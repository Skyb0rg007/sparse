structure X : VECTOR = Vector
fun vtolist (v : 'a vector): 'a list =
    Vector.foldl op :: [] v
