
signature SPARSE_LEX =
sig
    include SPARSE_EXTRAS

    val space : 'a t * 'a t * 'a t -> unit t
    val lexeme : unit t -> 'a t -> 'a t
    val symbol : unit t -> tokens -> tokens t
    val skipLineComment : (token -> bool) -> tokens -> unit t
    val skipBlockComment : tokens * tokens -> unit t
    val skipBlockCommentNested : tokens * tokens -> unit t
end
