(* vim: set ft=sml: *)
functor SparsePrimFn(S : SPARSE_STRUCTS) :> SPARSE
        where type Input.t = S.Input.t
          and type Token.t = S.Token.t
          and type Chunk.t = S.Chunk.t
          and type CustomError.t = S.CustomError.t =
struct
    open S

    structure ErrorItem : sig
        datatype t =
            Tokens of Token.t list
          | Label of string
          | EndOfInput

        val compare : t * t -> order
    end = struct
        datatype t =
            Tokens of Token.t list
          | Label of string
          | EndOfInput

        fun compare (Tokens t1, Tokens t2) = List.collate Token.compare (t1, t2)
          | compare (Tokens _, Label _) = LESS
          | compare (Tokens _, EndOfInput) = LESS
          | compare (Label _, Tokens _) = GREATER
          | compare (Label l1, Label l2) = String.compare (l1, l2)
          | compare (Label _, EndOfInput) = LESS
          | compare (EndOfInput, Tokens _) = GREATER
          | compare (EndOfInput, Label _) = GREATER
          | compare (EndOfInput, EndOfInput) = EQUAL
    end

    structure ErrorItemSet = ListSetFn(
        struct
            type ord_key = ErrorItem.t
            val compare = ErrorItem.compare
        end)

    structure Error : sig
        datatype t =
            Fancy of { offset: int, error: CustomError.t }
          | Trivial of { offset: int, unexpected: ErrorItem.t option, expected: ErrorItemSet.set }

        val offset : t -> int
        val merge : t * t -> t
    end = struct
        datatype t =
            Fancy of { offset: int, error: CustomError.t }
          | Trivial of { offset: int, unexpected: ErrorItem.t option, expected: ErrorItemSet.set }

        fun offset (Fancy {offset,...}) = offset
          | offset (Trivial {offset,...}) = offset

        fun optMerge (NONE, y) = y
          | optMerge (x, NONE) = x
          | optMerge (SOME x, SOME y) =
              case ErrorItem.compare (x, y) of
                   LESS => SOME y
                 | _ => SOME x

        fun merge (e1, e2) =
            case Int.compare (offset e1, offset e2) of
                 LESS => e2
               | GREATER => e1
               | EQUAL =>
                   case (e1, e2) of
                        (Trivial {offset=s1, unexpected=u1, expected=p1}, Trivial {unexpected=u2, expected=p2, ...}) =>
                            Trivial {offset=s1,
                                     unexpected=optMerge (u1, u2),
                                     expected=ErrorItemSet.union (p1, p2)}
                      | (Fancy {offset, error=x1}, Fancy {error=x2,...}) =>
                            Fancy {offset=offset, error=CustomError.merge (x1, x2)}
                      | (Fancy _, Trivial _) => e1
                      | (Trivial _, Fancy _) => e2
    end

    structure State : sig
        type t
        val initial : Input.t -> t
        val offset : t -> int
        val input : t -> Input.t
        val errors : t -> Error.t list
        val updateInput : (Input.t -> Input.t) -> t -> t
        val advance : t * Input.t * int -> t
        val registerError : t * Error.t -> t
        val max : t * t -> t
    end = struct
        type t = {
            input: Input.t,
            offset: int,
            errors: Error.t list
        }

        fun offset ({offset = x, ...} : t) = x
        fun input ({input = x, ...} : t) = x
        fun errors ({errors = x, ...} : t) = x

        fun updateInput f {input,offset,errors} =
            {input = f input,
             offset = offset,
             errors = errors}

        fun advance ({input = _, offset,errors}, input, incr) =
            {input = input,
             offset = offset + incr,
             errors = errors}

        fun registerError ({input, offset,errors}, err) =
            {input = input,
             offset = offset,
             errors = err :: errors}

        fun max (s1, s2) = if offset s1 > offset s2 then s1 else s2

        fun initial input = {
                input = input,
                offset = 0,
                errors = []
            }
    end

    structure Hints : sig
        type t
        val empty : t
        val fromError : int * Error.t -> t
        val withHints : ErrorItemSet.set list * (Error.t * State.t -> unit) -> Error.t * State.t -> unit
        val accHints : t * ('a * State.t * t -> unit) -> 'a * State.t * t -> unit
        val refreshLast : ErrorItemSet.set list * ErrorItem.t option -> t
    end = struct
        type t = ErrorItemSet.set list

        val empty = []

        fun fromError (streamOffset, Error.Trivial {offset=errOffset, expected=ps, ...}) =
              if streamOffset = errOffset andalso not (ErrorItemSet.isEmpty ps)
                then [ps]
                else []
          | fromError (streamOffset, Error.Fancy _) = []

        fun withHints (ps', c) = fn (e, s) =>
            case e of
                 Error.Fancy _ => c (e, s)
               | Error.Trivial {offset, unexpected, expected} =>
                   c (Error.Trivial {offset=offset,
                                     unexpected=unexpected,
                                     expected=List.foldl ErrorItemSet.union ErrorItemSet.empty (expected :: ps')}, s)

        fun accHints (hs, k) = fn (x, s, hs') => k (x, s, hs @ hs')

        fun refreshLast ([], _) = []
          | refreshLast (_::xs, NONE) = xs
          | refreshLast (_::xs, SOME m) = ErrorItemSet.singleton m :: xs
    end

    datatype ('e, 'a) result =
        Failure of 'e
      | Success of 'a

    type 'a ok = 'a * State.t * Hints.t -> unit
    type 'a err = Error.t * State.t -> unit
    type 'a t = State.t * 'a ok * 'a err * 'a ok * 'a err -> unit

    fun parse (p, input) = SMLofNJ.Cont.callcc (fn k =>
        let val s = State.initial input
            fun errGt (a, b) = Error.offset a > Error.offset b
            val sortErrs = ListMergeSort.sort errGt
            fun ok (x, s, _) =
                case State.errors s of
                     [] => SMLofNJ.Cont.throw k (Success x)
                   | de => SMLofNJ.Cont.throw k (Failure (sortErrs de))
            fun err (err, s) = SMLofNJ.Cont.throw k (Failure (sortErrs (err :: State.errors s)))
        in  p (s, ok, err, ok, err);
            raise Fail ""
        end)

    fun map f p = fn (s, cok, cerr, eok, eerr) =>
        p (s,
           fn (x, s, h) => cok (f x, s, h),
           cerr,
           fn (x, s, h) => eok (f x, s, h),
           eerr)

    fun pure x = fn (s, _, _, eok, _) => eok (x, s, Hints.empty)

    fun ap m k = fn (s, cok, cerr, eok, eerr) =>
        let fun mcok (x, s', hs) =
                let fun cok' (y, s, hs) = cok (x y, s, hs)
                in  k (s',
                       cok',
                       cerr,
                       Hints.accHints (hs, cok'),
                       Hints.withHints (hs, cerr))
                end
            fun meok (x, s', hs) =
                k (s',
                   fn (y, s, hs) => cok (x y, s, hs),
                   cerr,
                   Hints.accHints (hs, fn (y, s, hs) => eok (x y, s, hs)),
                   Hints.withHints (hs, eerr))
        in  m (s, mcok, cerr, meok, eerr)
        end

    fun >>= (m, f) = fn (s, cok, cerr, eok, eerr) =>
        let fun mcok (x, s', hs) =
                f x (s', cok, cerr, Hints.accHints (hs, cok), Hints.withHints (hs, cerr))
            fun meok (x, s', hs) =
                f x (s', cok, cerr, Hints.accHints (hs, eok), Hints.withHints (hs, eerr))
        in  m (s, mcok, cerr, meok, eerr)
        end

    fun fail err = fn (s, _, _, _, eerr) =>
        eerr (Error.Fancy {offset=State.offset s, error=err}, s)

    fun parseError err = fn (s, _, _, _, eerr) =>
        eerr (err, s)

    fun registerError err = fn (s, _, _, eok, _) =>
        eok ((), State.registerError (s, err), Hints.empty)

    val empty = fn (s, _, _, _, eerr) =>
        eerr (Error.Trivial {offset=State.offset s, unexpected=NONE, expected=ErrorItemSet.empty}, s)

    fun <|> (m, n) = fn (s, cok, cerr, eok, eerr) =>
        let fun meerr (err, ms) =
                n (s,
                   cok,
                   fn (err', s') => cerr (Error.merge (err', err), State.max (ms, s')),
                   fn (x, s', hs) => eok (x, s', Hints.fromError (State.offset s', err) @ hs),
                   fn (err', s') => eerr (Error.merge (err', err), State.max (ms, s')))
        in  m (s, cok, cerr, eok, meerr)
        end

    fun label (l, p) = fn (s, cok, cerr, eok, eerr) =>
        let val el = if String.size l = 0 then NONE else SOME (ErrorItem.Label l)
            fun cok' (x, s', hs) =
                case el of
                     NONE => cok (x, s', Hints.refreshLast (hs, NONE))
                   | SOME _ => cok (x, s', hs)
            fun eok' (x, s', hs) = eok (x, s', Hints.refreshLast (hs, el))
            fun eerr' (Error.Trivial {offset, unexpected, ...}, s') =
                  let val es = case el of
                                    NONE => ErrorItemSet.empty
                                  | SOME x => ErrorItemSet.singleton x
                  in  eerr (Error.Trivial {offset=offset, unexpected=unexpected, expected=es},s')
                  end
              | eerr' (err, s') = eerr (err, s')
        in  p (s, cok', cerr, eok', eerr')
        end

    fun hidden p = label ("", p)

    fun try p = fn (s, cok, _, eok, eerr) =>
        let fun eerr' (err, _) = eerr (err, s)
        in  p (s, cok, eerr', eok, eerr')
        end

    fun lookAhead p = fn (s, _, cerr, eok, eerr) =>
        let fun eok' (x, _, _) = eok (x, s, Hints.empty)
        in  p (s, eok', cerr, eok', eerr)
        end

    fun notFollowedBy p = fn (s, _, _, eok, eerr) =>
        let val what = case Input.take1 (State.input s) of
                            NONE => ErrorItem.EndOfInput
                          | SOME (tok, _) => ErrorItem.Tokens [tok]
            fun unexpect u = Error.Trivial {offset=State.offset s, unexpected=SOME u, expected=ErrorItemSet.empty}
            fun cok' _ = eerr (unexpect what, s)
            fun cerr' _ = eok ((), s, Hints.empty)
            fun eok' _ = eerr (unexpect what, s)
            fun eerr' _ = eok ((), s, Hints.empty)
        in  p (s, cok', cerr', eok', eerr')
        end

    fun withRecovery r p = fn (s, cok, cerr, eok, eerr) =>
        let fun cerr' (err, ms) =
                r err (ms,
                       fn (x, s', _) => cok (x, s', Hints.empty),
                       fn _ => cerr (err, ms),
                       fn (x, s', _) => eok (x, s', Hints.fromError (State.offset s', err)),
                       fn _ => cerr (err, ms))
            fun eerr' (err, ms) =
                r err (ms,
                       fn (x, s', _) => cok (x, s', Hints.fromError (State.offset s', err)),
                       fn _ => eerr (err, ms),
                       fn (x, s', _) => cok (x, s', Hints.fromError (State.offset s', err)),
                       fn _ => eerr (err, ms))
        in  p (s, cok, cerr', eok, eerr')
        end

    fun observing p = fn (s, cok, _, eok, _) =>
        let fun cok' (x, s, hs) = cok (Success x, s, hs)
            fun cerr' (err, s') = cok (Failure err, s', Hints.empty)
            fun eok' (x, s, hs) = eok (Success x, s, hs)
            fun eerr' (err, s') = eok (Failure err, s', Hints.fromError (State.offset s', err))
        in  p (s, cok', cerr', eok', eerr')
        end

    val eof = fn (s, _, _, eok, eerr) =>
        case Input.take1 (State.input s) of
             NONE => eok ((), s, Hints.empty)
           | SOME (x, _) =>
               let val us = SOME (ErrorItem.Tokens [x])
                   val ps = ErrorItemSet.singleton ErrorItem.EndOfInput
               in  eerr (Error.Trivial {offset=State.offset s, unexpected=us, expected=ps}, s)
               end

    fun token test ps = fn (s, cok, _, _, eerr) =>
        case Input.take1 (State.input s) of
             NONE => eerr (Error.Trivial {offset=State.offset s, unexpected=SOME ErrorItem.EndOfInput, expected=ps}, s)
           | SOME (c, cs) =>
               case test c of
                    NONE =>
                      eerr (Error.Trivial {offset=State.offset s, unexpected=SOME (ErrorItem.Tokens [c]), expected=ps}, s)
                  | SOME x =>
                      cok (x, State.advance (s, cs, 1), Hints.empty)

    fun tokens eq tts = fn (s, cok, _, eok, eerr) =>
        let val len = Chunk.length tts
            fun unexpect (pos, u) =
                Error.Trivial {offset=pos, unexpected=SOME u, expected=ErrorItemSet.singleton (ErrorItem.Tokens (Chunk.toTokens tts))}
        in  case Input.takeN (len, State.input s) of
                 NONE => eerr (unexpect (State.offset s, ErrorItem.EndOfInput), s)
               | SOME (tts', input') =>
                   if eq (tts, tts')
                     then let val st = State.advance (s, input', len)
                          in  if Chunk.isEmpty tts
                                then eok (tts', st, Hints.empty)
                                else cok (tts', st, Hints.empty)
                          end
                     else let val ps = ErrorItem.Tokens (Chunk.toTokens tts')
                          in  eerr (unexpect (State.offset s, ps), s)
                          end
        end

    fun takeWhileP ml f = fn (s, cok, _, eok, _) =>
        let val (ts, input') = Input.takeWhile f (State.input s)
            val len = Chunk.length ts
            val hs =
                case ml of
                     NONE => Hints.empty
                   | SOME "" => Hints.empty
                   | SOME l => [ErrorItemSet.singleton (ErrorItem.Label l)]
        in  if Chunk.isEmpty ts
              then eok (ts, State.advance (s, input', len), hs)
              else cok (ts, State.advance (s, input', len), hs)
        end

    fun takeWhile1P ml f = fn (s, cok, _, _, eerr) =>
        let val (ts, input') = Input.takeWhile f (State.input s)
            val len = Chunk.length ts
            val el =
                case ml of
                     NONE => NONE
                   | SOME "" => NONE
                   | SOME l => SOME (ErrorItem.Label l)
            val hs =
                case ml of
                     NONE => Hints.empty
                   | SOME "" => Hints.empty
                   | SOME l => [ErrorItemSet.singleton (ErrorItem.Label l)]
        in  if Chunk.isEmpty ts
              then let val us = case Input.take1 (State.input s) of
                                   NONE => ErrorItem.EndOfInput
                                 | SOME (t, _) => ErrorItem.Tokens [t]
                       val ps = case el of NONE => ErrorItemSet.empty | SOME x => ErrorItemSet.singleton x
                   in  eerr (Error.Trivial {offset=State.offset s, unexpected=SOME us, expected=ps}, s)
                   end
              else cok (ts, State.advance (s, input', len), hs)
        end

    fun takeP ml n = fn (s, cok, _, _, eerr) =>
        let val el =
                case ml of
                     NONE => NONE
                   | SOME "" => NONE
                   | SOME l => SOME (ErrorItem.Label l)
            val ps =
                case el of
                     NONE => ErrorItemSet.empty
                   | SOME x => ErrorItemSet.singleton x
        in  case Input.takeN (n, State.input s) of
                 NONE => eerr (Error.Trivial {offset=State.offset s, unexpected=SOME ErrorItem.EndOfInput, expected=ps}, s)
               | SOME (ts, input') =>
                   let val len = Chunk.length ts
                   in  if len = n
                         then cok (ts, State.advance (s, input', len), Hints.empty)
                         else eerr (Error.Trivial {offset=State.offset s + len, unexpected=SOME ErrorItem.EndOfInput, expected=ps}, s)
                   end
        end

    fun fix f =
        let val r = ref (fn _ => raise Fail "ParseFn.fix")
            fun p args = (!r) args
        in  r := f p;
            p
        end

    val getOffset = fn (s, _, _, eok, _) =>
        eok (State.offset s, s, Hints.empty)

    val getInput = fn (s, _, _, eok, _) =>
        eok (State.input s, s, Hints.empty)

    fun updateInput f = fn (s, _, _, eok, _) =>
        eok ((), State.updateInput f s, Hints.empty)

    (* Derived functions *)

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

    fun anySingle args = satisfy (fn _ => true) args

    fun anySingleBut t = satisfy (fn x => Token.compare (x, t) <> EQUAL)

    fun oneOf ts = satisfy (fn x => List.exists (fn t => Token.compare (x, t) = EQUAL) ts)

    fun noneOf ts = satisfy (fn x => List.all (fn t => Token.compare (x, t) <> EQUAL) ts)

    fun chunk ts = tokens (fn (a, b) => Chunk.compare (a, b) = EQUAL) ts

    fun match p =
        >>= (getOffset, fn offset =>
        >>= (getInput, fn input =>
        >>= (p, fn r =>
        >>= (getOffset, fn offset' =>
        pure (#1 (valOf (Input.takeN (offset' - offset, input))), r)))))

    fun rest args = takeWhileP NONE (fn _ => true) args

    fun atEnd args = <|> (map (fn _ => true) eof, pure false) args

    fun void p = map (fn _ => ()) p

    fun map2 f p1 p2 =
        >>= (p1, fn a =>
        >>= (p2, fn b =>
        pure (f (a, b))))

    fun << (p1, p2) =
        >>= (p1, fn r =>
        >>= (p2, fn _ =>
        pure r))

    fun >> (p1, p2) =
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

    fun endBy p sep = many (<< (p, sep))
    fun endBy1 p sep = some (<< (p, sep))

    fun manyTill p e = fix (fn ps =>
        <|> (map (fn _ => []) e,
             map2 op :: p ps))

    fun someTill p e = map2 op :: p (manyTill p e)

    fun option (x, p) = <|> (p, pure x)

    fun sepBy1 p sep = map2 op :: p (many (>> (sep, p)))

    fun sepBy p sep = <|> (sepBy1 p sep, pure [])

    fun sepEndBy1 p sep = map2 op :: p (<|> (>> (sep, sepEndBy p sep), pure []))

    and sepEndBy p sep = <|> (sepEndBy1 p sep, pure [])

    fun skipMany p = fix (fn p' => <|> ( >> (p, p'), pure ()))

    fun skipSome p = >> (p, skipMany p)

    fun skipManyTill p e = fix (fn p' => <|> (e, >> (p, p')))

    fun skipSomeTill p e = >> (p, skipManyTill p e)

    fun <*> (p1, p2) = ap p1 p2

    fun bind p f = >>= (p, f)

    fun =<< (f, p) = >>= (p, f)

    fun >=> (f, g) x = >>= (f x, g)

    fun <=< (g, f) = >=> (f, g)

    fun <$> (f, p) = map f p

    fun <$ (x, p) = map (fn _ => x) p

    fun $> (p, x) = <$ (x, p)

    fun traverse f xs = List.foldl (fn (x, ys) => map2 op :: (f x) ys) (pure []) xs

    fun sequence xs = List.foldl (fn (x, ys) => map2 op :: x ys) (pure []) xs

    fun join p = >>= (p, fn p' => p')

    fun zipWith f xs ys =
        ListPair.foldlEq
        (fn (x, y, zs) => map2 op :: (f (x, y)) zs)
        (pure [])
        (xs, ys)
end
