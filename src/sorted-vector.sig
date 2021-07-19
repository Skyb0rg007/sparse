
signature SORTED_VECTOR_ELEM =
sig
    type t
    val compare : t * t -> order
end

signature SORTED_VECTOR =
sig
    type t
    type elem

    val empty : t
    val singleton : elem -> t
    val insert : elem * t -> t
    val lookup : elem * t -> elem option
    val union : t * t -> t
    val toList : t -> elem list
    val fromList : elem list -> t
    val compare : t * t -> order
    val equals : t * t -> bool
    val null : t -> bool
    val unions : t list -> t
end

