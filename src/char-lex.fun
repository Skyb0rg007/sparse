(* vim: set ft=sml: *)
functor SparseCharLexFn(P : SPARSE where type Token.t = char) :> SPARSE_LEX
    where type 'a t = 'a P.t 
      and type chunk = P.Chunk.t =
struct
    open P
    type chunk = Chunk.t

    fun space (sp, line, block) =
        skipMany (choice [hidden sp, hidden line, hidden block])

    fun lexeme sp p =
        << (p, sp)

    fun symbol sp s =
        lexeme sp (chunk s)

    fun skipLineComment pre =
        >> (chunk pre, void (takeWhileP (SOME "character") (fn c => c <> #"\n")))

    fun skipBlockComment (s, e) =
        >> (chunk s, void (manyTill anySingle (chunk e)))

    fun skipBlockCommentNested (s, e) = fix (fn self =>
        >> (chunk s, void (manyTill self (chunk e))))
end
