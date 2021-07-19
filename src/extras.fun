
functor SparseExtrasFn(S : SPARSE)
    : SPARSE_EXTRAS
    (* where type 'a t = 'a S.t *)
    =
struct
    open S

    fun failure (us, ps) =
        >>= (getOffset, fn offset =>
        parseError (Error.Trivial {offset = offset, unexpected = us, expected = ps}))

    fun customFailure e =
        >>= (getOffset, fn offset =>
        parseError (Error.Fancy {offset = offset, error = e}))

    fun unexpected item = failure (SOME item, ErrorItemSet.empty)

    fun registerFailure (us, ps) =
        >>= (getOffset, fn offset =>
        registerError (Error.Trivial {offset = offset, unexpected = us, expected = ps}))

    fun registerCustomFailure e =
        >>= (getOffset, fn offset =>
        registerError (Error.Fancy {offset = offset, error = e}))

    fun single t =
        let fun test x =
                case Token.compare (x, t) of
                     EQUAL => SOME x
                   | _     => NONE
            val expected = ErrorItemSet.singleton (ErrorItem.Tokens [t])
        in  token test expected
        end

    fun satisfy f =
        let fun test x = if f x then SOME x else NONE
        in  token test ErrorItemSet.empty
        end

    val anySingle = satisfy (fn _ => true)

    fun anySingleBut t = satisfy (fn x => Token.compare (x, t) <> EQUAL)

    fun oneOf ts = satisfy (fn x => List.exists (fn t => Token.compare (x, t) = EQUAL) ts)

    fun noneOf ts = satisfy (fn x => List.all (fn t => Token.compare (x, t) <> EQUAL) ts)

    fun chunk ts = tokens (fn (a, b) => Tokens.compare (a, b) = EQUAL) ts

    fun match p =
        >>= (getOffset, fn offset =>
        >>= (getInput, fn input =>
        >>= (p, fn r =>
        >>= (getOffset, fn offset' =>
        pure (#1 (valOf (Input.takeN (offset' - offset, input))), r)))))

    val rest = takeWhileP NONE (fn _ => true)

    val atEnd = <|> (map (fn _ => true) eof, pure false)

    fun void p = map (fn _ => ()) p

    fun map2 f p1 p2 =
        >>= (p1, fn a =>
        >>= (p2, fn b =>
        pure (f (a, b))))

    fun <* (p1, p2) =
        >>= (p1, fn r =>
        >>= (p2, fn _ =>
        pure r))

    fun *> (p1, p2) =
        >>= (p1, fn _ =>
        >>= (p2, fn r =>
        pure r))

    fun optional p = <|> (map SOME p, pure NONE)

    fun between a b c =
        >>= (a, fn _ =>
        >>= (c, fn r =>
        >>= (b, fn _ =>
        pure r)))

    fun choice ps = List.foldl <|> empty ps

    fun count (n, p) =
        let fun go n =
                if n <= 0 then pure []
                else map2 op :: p (go (n - 1))
        in  go n
        end

    fun count' (m, n, p) =
        let fun go (m, n) =
                if n <= 0 orelse m > n then pure []
                else if m > 0 then map2 op :: p (go (m-1, n-1))
                else <|> (map2 op :: p (go (0, n-1)), pure [])
        in  go (m, n)
        end

    fun many p = fix (fn ps =>
        <|> (>>= (p, fn x => >>= (ps, fn xs => pure (x::xs))), pure []))

    fun some p = map2 op :: p (many p)

    fun endBy p sep = many (<* (p, sep))
    fun endBy1 p sep = some (<* (p, sep))

    fun manyTill p e = fix (fn ps =>
        <|> (map (fn _ => []) e,
             map2 op :: p ps))

    fun someTill p e = map2 op :: p (manyTill p e)

    fun option (x, p) = <|> (p, pure x)

    fun sepBy1 p sep = map2 op :: p (many ( *> (sep, p)))

    fun sepBy p sep = <|> (sepBy1 p sep, pure [])

    fun sepEndBy1 p sep = map2 op :: p (<|> ( *> (sep, sepEndBy p sep), pure []))

    and sepEndBy p sep = <|> (sepEndBy1 p sep, pure [])

    fun skipMany p = fix (fn p' => <|> ( *> (p, p'), pure ()))

    fun skipSome p = *> (p, skipMany p)

    fun skipManyTill p e = fix (fn p' => <|> (e, *> (p, p')))

    fun skipSomeTill p e = *> (p, skipManyTill p e)
end
