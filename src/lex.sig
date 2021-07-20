(* vim: set ft=sml: *)
signature SPARSE_LEX =
sig
    type 'a t
    type chunk

    val lexeme : unit t -> 'a t -> 'a t
    val symbol : unit t -> chunk -> chunk t

    val space : 'a t * 'a t * 'a t -> unit t
    val skipLineComment : chunk -> unit t
    val skipBlockComment : chunk * chunk -> unit t
    val skipBlockCommentNested : chunk * chunk -> unit t
end
