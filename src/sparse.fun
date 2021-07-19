
functor SparseFn(S : SPARSE_INPUT)
    :> SPARSE where type input = S.input
                and type token = S.token
                and type tokens = S.tokens
                and type custom_error = S.custom_error =
struct
    type input = S.input
    type token = S.token
    type tokens = S.tokens
    type custom_error = S.custom_error

    structure Token =
    struct
        type t = token
        val compare = S.tokenCompare
    end

    structure Tokens =
    struct
        type t = tokens
        val compare = S.chunkCompare
        val fromTokens = S.tokensToChunk
        val fromToken = S.tokenToChunk
        val toTokens = S.chunkToTokens
        val length = S.chunkLength
        val isEmpty = S.chunkEmpty
    end

    structure Input =
    struct
        type t = input
        val takeN = S.takeN
        val take1 = S.take1
        val takeWhile = S.takeWhile
    end

    structure ErrorItem : sig
        datatype t =
            Tokens of token list
          | Label of string
          | EndOfInput

        val compare : t * t -> order
    end = struct
        datatype t =
            Tokens of token list
          | Label of string
          | EndOfInput

        fun compare (Tokens t1, Tokens t2) = List.collate S.tokenCompare (t1, t2)
          | compare (Tokens _, Label _) = LESS
          | compare (Tokens _, EndOfInput) = LESS
          | compare (Label _, Tokens _) = GREATER
          | compare (Label l1, Label l2) = String.compare (l1, l2)
          | compare (Label _, EndOfInput) = LESS
          | compare (EndOfInput, Tokens _) = GREATER
          | compare (EndOfInput, Label _) = GREATER
          | compare (EndOfInput, EndOfInput) = EQUAL
    end

    structure ErrorItemSet = SortedVectorFn(ErrorItem)

    structure Error : sig
        datatype t =
            Fancy of { offset: int, error: custom_error }
          | Trivial of { offset: int, unexpected: ErrorItem.t option, expected: ErrorItemSet.t }

        val offset : t -> int
        val merge : t * t -> t
    end = struct
        datatype t =
            Fancy of { offset: int, error: custom_error }
          | Trivial of { offset: int, unexpected: ErrorItem.t option, expected: ErrorItemSet.t }

        fun offset (Fancy x) = #offset x
          | offset (Trivial x) = #offset x

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
                            Fancy {offset=offset, error=S.customErrorMerge (x1, x2)}
                      | (Fancy _, Trivial _) => e1
                      | (Trivial _, Fancy _) => e2
    end

    structure State : sig
        type t
        val initial : S.input -> t
        val offset : t -> int
        val input : t -> S.input
        val errors : t -> Error.t list
        val advance : t * S.input * int -> t
        val registerError : t * Error.t -> t
        val max : t * t -> t
    end = struct
        type t = {
            input: S.input,
            offset: int,
            errors: Error.t list
        }

        fun offset ({offset = x, ...} : t) = x
        fun input ({input = x, ...} : t) = x
        fun errors ({errors = x, ...} : t) = x

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
        val withHints : ErrorItemSet.t list * (Error.t * State.t -> unit) -> Error.t * State.t -> unit
        val accHints : t * ('a * State.t * t -> unit) -> 'a * State.t * t -> unit
        val refreshLast : ErrorItemSet.t list * ErrorItem.t option -> t
    end = struct
        type t = ErrorItemSet.t list

        val empty = []

        fun fromError (streamOffset, Error.Trivial {offset=errOffset, expected=ps, ...}) =
              if streamOffset = errOffset andalso not (ErrorItemSet.null ps)
                then [ps]
                else []
          | fromError (streamOffset, Error.Fancy _) = []

        fun withHints (ps', c) = fn (e, s) =>
            case e of
                 Error.Fancy _ => c (e, s)
               | Error.Trivial {offset, unexpected, expected} =>
                   c (Error.Trivial {offset=offset,
                                     unexpected=unexpected,
                                     expected=ErrorItemSet.unions (expected :: ps')}, s)

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

    fun parse (p, input) = Compat.callec (fn k =>
        let val s = State.initial input
            fun errCmp (a, b) = Int.compare (Error.offset a, Error.offset b)
            val sortErrs = Compat.sort errCmp
            fun ok (x, s, _) =
                case State.errors s of
                     [] => k (Success x)
                   | de => k (Failure (sortErrs de))
            fun err (err, s) = k (Failure (sortErrs (err :: State.errors s)))
        in  p (s, ok, err, ok, err)
        end)

    fun map f p = fn (s, cok, cerr, eok, eerr) =>
        p (s,
           fn (x, s, h) => cok (f x, s, h),
           cerr,
           fn (x, s, h) => eok (f x, s, h),
           eerr)

    fun pure x = fn (s, _, _, eok, _) => eok (x, s, Hints.empty)

    fun ap (m, k) = fn (s, cok, cerr, eok, eerr) =>
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
        let val what = case S.take1 (State.input s) of
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
        case S.take1 (State.input s) of
             NONE => eok ((), s, Hints.empty)
           | SOME (x, _) =>
               let val us = SOME (ErrorItem.Tokens [x])
                   val ps = ErrorItemSet.singleton ErrorItem.EndOfInput
               in  eerr (Error.Trivial {offset=State.offset s, unexpected=us, expected=ps}, s)
               end

    fun token test ps = fn (s, cok, _, _, eerr) =>
        case S.take1 (State.input s) of
             NONE => eerr (Error.Trivial {offset=State.offset s, unexpected=SOME ErrorItem.EndOfInput, expected=ps}, s)
           | SOME (c, cs) =>
               case test c of
                    NONE =>
                      eerr (Error.Trivial {offset=State.offset s, unexpected=SOME (ErrorItem.Tokens [c]), expected=ps}, s)
                  | SOME x =>
                      cok (x, State.advance (s, cs, 1), Hints.empty)

    fun tokens eq tts = fn (s, cok, _, eok, eerr) =>
        let val len = S.chunkLength tts
            fun unexpect (pos, u) =
                Error.Trivial {offset=pos, unexpected=SOME u, expected=ErrorItemSet.singleton (ErrorItem.Tokens (S.chunkToTokens tts))}
        in  case S.takeN (len, State.input s) of
                 NONE => eerr (unexpect (State.offset s, ErrorItem.EndOfInput), s)
               | SOME (tts', input') =>
                   if eq (tts, tts')
                     then let val st = State.advance (s, input', len)
                          in  if S.chunkEmpty tts
                                then eok (tts', st, Hints.empty)
                                else cok (tts', st, Hints.empty)
                          end
                     else let val ps = ErrorItem.Tokens (S.chunkToTokens tts')
                          in  eerr (unexpect (State.offset s, ps), s)
                          end
        end

    fun takeWhileP ml f = fn (s, cok, _, eok, _) =>
        let val (ts, input') = S.takeWhile f (State.input s)
            val len = S.chunkLength ts
            val hs =
                case ml of
                     NONE => Hints.empty
                   | SOME "" => Hints.empty
                   | SOME l => [ErrorItemSet.singleton (ErrorItem.Label l)]
        in  if S.chunkEmpty ts
              then eok (ts, State.advance (s, input', len), hs)
              else cok (ts, State.advance (s, input', len), hs)
        end

    fun takeWhile1P ml f = fn (s, cok, _, _, eerr) =>
        let val (ts, input') = S.takeWhile f (State.input s)
            val len = S.chunkLength ts
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
        in  if S.chunkEmpty ts
              then let val us = case S.take1 (State.input s) of
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
        in  case S.takeN (n, State.input s) of
                 NONE => eerr (Error.Trivial {offset=State.offset s, unexpected=SOME ErrorItem.EndOfInput, expected=ps}, s)
               | SOME (ts, input') =>
                   let val len = S.chunkLength ts
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
end
