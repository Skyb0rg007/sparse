
signature SPARSE_STRUCTS =
sig
    (* The atomic unit of a parser
     * The toString function is only used for debug output and can be stubbed
     *)
    structure Token :
    sig
        type t
        val compare : t * t -> order
    end

    (* Represents a group of tokens
     * This is separate from a vector/list of tokens to allow for things like UTF8 support
     * The toString function is only used for debug output and can be stubbed
     *)
    structure Chunk :
    sig
        type t
        val compare : t * t -> order
        val fromToken : Token.t -> t
        val fromTokens : Token.t list -> t
        val toTokens : t -> Token.t list
        val length : t -> int
        val isEmpty : t -> bool
        val toString : t -> string
    end

    (* Where the parser gets its data from
     * This type must be able to backtrack - these functions should be pure
     *)
    structure Input :
    sig
        type t
        val take1 : t -> (Token.t * t) option
        val takeN : int * t -> (Chunk.t * t) option
        val takeWhile : (Token.t -> bool) -> t -> Chunk.t * t
    end

    (* Custom type to allow for custom error reporting *)
    structure CustomError :
    sig
        type t
        val compare : t * t -> order
        val merge : t * t -> t
        val length : t -> int
        val toString : t -> string
    end
end

signature SPARSE =
sig
    include SPARSE_STRUCTS

    (* The main parser type
     * Represents a parser that results in a value of type 'a
     *)
    type 'a t

    (* Represents a part of the input for error reporting
     * Used for unexpected tokens as well as expected tokens
     *)
    structure ErrorItem : sig
        datatype t =
            Tokens of Token.t list
          | Label of string
          | EndOfInput

        val compare : t * t -> order
    end

    structure ErrorItemSet : ORD_SET where type Key.ord_key = ErrorItem.t

    (* Represents a parse error or warning
     * This type is returned after parsing
     *)
    structure Error : sig
        datatype t =
            Fancy of { offset: int, error: CustomError.t }
          | Trivial of { offset: int, unexpected: ErrorItem.t option, expected: ErrorItemSet.set }

        val offset : t -> int
        (* Prefers errors with larger offsets, then Fancy over Trivial *)
        val merge : t * t -> t
        val toString : t -> string
        val textToString : t -> string
    end

    datatype ('e, 'a) result =
        Failure of 'e
      | Success of 'a

    val parse : 'a t * Input.t -> (Error.t list, 'a) result

    (* Monadic combinators *)
    val pure : 'a -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val <$> : ('a -> 'b) * 'a t -> 'b t
    val $> : 'a t * 'b -> 'b t
    val <$ : 'a * 'b t -> 'a t
    val ap : ('a -> 'b) t -> 'a t -> 'b t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val >>= : 'a t * ('a -> 'b t) -> 'b t
    val =<< : ('a -> 'b t) * 'a t -> 'b t
    val >=> : ('a -> 'b t) * ('b -> 'c t) -> 'a -> 'c t
    val <=< : ('b -> 'c t) * ('a -> 'b t) -> 'a -> 'c t
    val empty : 'a t
    val <|> : 'a t * 'a t -> 'a t
    val fix : ('a t -> 'a t) -> 'a t
    val void : 'a t -> unit t
    val map2 : ('a * 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val << : 'a t * 'b t -> 'a t
    val >> : 'a t * 'b t -> 'b t
    val <*> : ('a -> 'b) t * 'a t -> 'b t
    val optional : 'a t -> 'a option t
    val between : 'a t -> 'b t -> 'c t -> 'c t
    val choice : 'a t list -> 'a t
    val count : int * 'a t -> 'a list t
    val count' : int * int * 'a t -> 'a list t
    val many : 'a t -> 'a list t
    val some : 'a t -> 'a list t
    val endBy : 'a t -> 'b t -> 'a list t
    val endBy1 : 'a t -> 'b t -> 'a list t
    val manyTill : 'a t -> 'b t -> 'a list t
    val someTill : 'a t -> 'b t -> 'a list t
    val option : 'a * 'a t -> 'a t
    val sepBy : 'a t -> 'b t -> 'a list t
    val sepBy1 : 'a t -> 'b t -> 'a list t
    val sepEndBy : 'a t -> 'b t -> 'a list t
    val sepEndBy1 : 'a t -> 'b t -> 'a list t
    val skipMany : 'a t -> unit t
    val skipSome : 'a t -> unit t
    val skipManyTill : 'a t -> 'b t -> 'b t
    val skipSomeTill : 'a t -> 'b t -> 'b t
    val traverse : ('a -> 'b t) -> 'a list -> 'b list t
    val sequence : 'a t list -> 'a list t
    val join : 'a t t -> 'a t
    val zipWith : ('a * 'b -> 'c t) -> 'a list -> 'b list -> 'c list t

    (* "Fast" primitives - prefer these if applicable *)
    val tokens : (Chunk.t * Chunk.t -> bool) -> Chunk.t -> Chunk.t t
    val takeWhileP : string option -> (Token.t -> bool) -> Chunk.t t
    val takeWhile1P : string option -> (Token.t -> bool) -> Chunk.t t
    val takeP : string option -> int -> Chunk.t t

    (* Error handling *)
    val parseError : Error.t -> 'a t
    val failure : ErrorItem.t option * ErrorItemSet.set -> 'a t
    val customFailure : CustomError.t -> 'a t
    val unexpected : ErrorItem.t -> 'a t

    val registerError : Error.t -> unit t
    val registerFailure : ErrorItem.t option * ErrorItemSet.set -> unit t
    val registerCustomFailure : CustomError.t -> unit t

    val withRecovery : (Error.t -> 'a t) -> 'a t -> 'a t
    val observing : 'a t -> (Error.t, 'a) result t

    (* Other *)
    val label : string * 'a t -> 'a t
    val hidden : 'a t -> 'a t
    val try : 'a t -> 'a t
    val lookAhead : 'a t -> 'a t
    val notFollowedBy : 'a t -> unit t
    val eof : unit t
    val token : (Token.t -> 'a option) -> ErrorItemSet.set -> 'a t
    val single : Token.t -> Token.t t
    val satisfy : (Token.t -> bool) -> Token.t t
    val anySingle : Token.t t
    val anySingleBut : Token.t -> Token.t t
    val oneOf : Token.t list -> Token.t t
    val noneOf : Token.t list -> Token.t t
    val chunk : Chunk.t -> Chunk.t t
    val match : 'a t -> (Chunk.t * 'a) t
    val rest : Chunk.t t
    val atEnd : bool t

    (* State *)
    val getOffset : int t
    val getInput : Input.t t
    val updateInput : (Input.t -> Input.t) -> unit t
end

