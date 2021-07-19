
functor SparseLexFn(P : SPARSE_EXTRAS): SPARSE_LEX =
struct
    open P

    fun space (sp, line, block) =
        skipMany (choice [hidden sp, hidden line, hidden block])

    fun lexeme sp p =
        <* (p, sp)

    fun symbol sp s =
        lexeme sp (chunk s)

    fun skipLineComment isNewline pre =
        *> (chunk pre, void (takeWhileP (SOME "character") (not o isNewline)))

    fun skipBlockComment (s, e) =
        *> (chunk s, void (manyTill anySingle (chunk e)))

    fun skipBlockCommentNested (s, e) = fix (fn self =>
        *> (chunk s, void (manyTill self (chunk e))))
end
