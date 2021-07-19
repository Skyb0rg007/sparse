
signature SPARSE_EXTRAS =
sig
    include SPARSE

    (* Derived error handling *)
    val failure : ErrorItem.t option * ErrorItemSet.t -> 'a t
    val customFailure : custom_error -> 'a t
    val unexpected : ErrorItem.t -> 'a t
    val registerFailure : ErrorItem.t option * ErrorItemSet.t -> unit t
    val registerCustomFailure : custom_error -> unit t

    (* Derived parser combinators *)
    val single : token -> token t
    val satisfy : (token -> bool) -> token t
    val anySingle : token t
    val anySingleBut : token -> token t
    val oneOf : token list -> token t
    val noneOf : token list -> token t
    val chunk : tokens -> tokens t
    val match : 'a t -> (tokens * 'a) t
    val rest : tokens t
    val atEnd : bool t

    (* Derived monadic combinators *)
    val void : 'a t -> unit t
    val map2 : ('a * 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val <* : 'a t * 'b t -> 'a t
    val *> : 'a t * 'b t -> 'b t
    val optional : 'a t -> 'a option t
    val between : 'a t -> 'b t -> 'c t -> 'c t
    val choice : 'a t list -> 'a t
    val count : int * 'a t -> 'a list t
    val count' : int * int * 'a t -> 'a list t
    val endBy : 'a t -> 'b t -> 'a list t
    val endBy1 : 'a t -> 'b t -> 'a list t
    val sepBy : 'a t -> 'b t -> 'a list t
    val sepBy1 : 'a t -> 'b t -> 'a list t
    val sepEndBy : 'a t -> 'b t -> 'a list t
    val sepEndBy1 : 'a t -> 'b t -> 'a list t
    val many : 'a t -> 'a list t
    val some : 'a t -> 'a list t
    val manyTill : 'a t -> 'b t -> 'a list t
    val someTill : 'a t -> 'b t -> 'a list t
    val option : 'a * 'a t -> 'a t
    val skipMany : 'a t -> unit t
    val skipSome : 'a t -> unit t
    val skipManyTill : 'a t -> 'b t -> 'b t
    val skipSomeTill : 'a t -> 'b t -> 'b t
end
