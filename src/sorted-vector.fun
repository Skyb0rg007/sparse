
functor SortedVectorFn(Elem : SORTED_VECTOR_ELEM) :> SORTED_VECTOR where type elem = Elem.t =
struct
    type elem = Elem.t
    type t = elem Vector.vector

    datatype search_result = Found of int | NotFound of int

    fun bin_search (x, v) =
        let fun midpoint (a, b) = (a + b) div 2
            fun go (lo, hi) =
                if lo >= hi then NotFound lo
                else let val k = midpoint (lo, hi)
                     in  case Elem.compare (Vector.sub (v, k), x) of
                              LESS => go (k + 1, hi)
                            | GREATER => go (lo, k)
                            | EQUAL => Found k
                     end
        in  go (0, Vector.length v)
        end

    fun compare (v1, v2) = Vector.collate Elem.compare (v1, v2)

    fun equals (v1, v2) = compare (v1, v2) = EQUAL

    val empty : t = Vector.fromList []

    fun singleton x = Vector.fromList [x]
    
    fun lookup (x, v) =
        case bin_search (x, v) of
             Found idx => SOME (Vector.sub (v, idx))
           | NotFound _ => NONE

    fun insert (x, v) =
        let val len = Vector.length v
        in  case bin_search (x, v) of
                Found idx => Vector.tabulate (len, fn i =>
                      if i = idx then x else Vector.sub (v, i))
              | NotFound idx => Vector.tabulate (len + 1, fn i =>
                      case Int.compare (i, idx) of
                           LESS => Vector.sub (v, i)
                         | EQUAL => x
                         | GREATER => Vector.sub (v, i - 1))
        end

    fun merge ([], ys) = ys
      | merge (xs, []) = xs
      | merge (x :: xs, y :: ys) =
          case Elem.compare (x, y) of
               LESS    => x :: merge (xs, y :: ys)
             | GREATER => y :: merge (x :: xs, ys)
             | EQUAL   => x :: merge (xs, ys)

    fun toList v = Vector.foldl op :: [] v

    fun union (v1, v2) = Vector.fromList (merge (toList v1, toList v2))

    fun fromList xs = Vector.fromList (Compat.sortUniq Elem.compare xs)

    fun null v = Vector.length v = 0

    fun unions xs = List.foldr union empty xs
end
