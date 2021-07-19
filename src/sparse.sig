
signature SPARSE_INPUT =
sig
    type token
    type tokens

    val tokenCompare : token * token -> order
    val chunkCompare : tokens * tokens -> order
    val tokenToChunk : token -> tokens
    val tokensToChunk : token list -> tokens
    val chunkToTokens : tokens -> token list
    val chunkLength : tokens -> int
    val chunkEmpty : tokens -> bool

    (* The input type *)
    type input
    val take1 : input -> (token * input) option
    val takeN : int * input -> (tokens * input) option
    val takeWhile : (token -> bool) -> input -> tokens * input

    (* The custom error type *)
    type custom_error
    val customErrorCompare : custom_error * custom_error -> order
    val customErrorMerge : custom_error * custom_error -> custom_error
end

signature SPARSE =
sig
    type 'a t

    type input
    type token
    type tokens
    type custom_error

    structure Token : sig
        type t = token
        val compare : t * t -> order
    end

    structure Tokens : sig
        type t = tokens
        val compare : t * t -> order
        val fromTokens : Token.t list -> t
        val fromToken : Token.t -> t
        val toTokens : t -> Token.t list
        val length : t -> int
        val isEmpty : t -> bool
    end

    structure Input : sig
        type t = input
        val takeN : int * t -> (tokens * t) option
        val take1 : t -> (token * t) option
        val takeWhile : (token -> bool) -> t -> tokens * t
    end

    structure ErrorItem : sig
        datatype t =
            Tokens of token list
          | Label of string
          | EndOfInput

        val compare : t * t -> order
    end

    structure ErrorItemSet : SORTED_VECTOR where type elem = ErrorItem.t

    structure Error : sig
        datatype t =
            Fancy of { offset: int, error: custom_error }
          | Trivial of { offset: int, unexpected: ErrorItem.t option, expected: ErrorItemSet.t }

        val offset : t -> int
        val merge : t * t -> t
    end

    datatype ('e, 'a) result =
        Failure of 'e
      | Success of 'a

    val parse : 'a t * input -> (Error.t list, 'a) result

    (* Signal an error, ending parsing *)
    val parseError : Error.t -> 'a t
    (* Register an error, only visible when parsing is complete *)
    val registerError : Error.t -> unit t

    (* Primitives *)
    val pure : 'a -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val ap : ('a -> 'b) t * 'a t -> 'b t
    val >>= : 'a t * ('a -> 'b t) -> 'b t
    val empty : 'a t
    val <|> : 'a t * 'a t -> 'a t
    val label : string * 'a t -> 'a t
    val try : 'a t -> 'a t
    val lookAhead : 'a t -> 'a t
    val notFollowedBy : 'a t -> unit t
    val withRecovery : (Error.t -> 'a t) -> 'a t -> 'a t
    val observing : 'a t -> (Error.t, 'a) result t
    val eof : unit t
    val token : (token -> 'a option) -> ErrorItemSet.t -> 'a t
    val tokens : (tokens * tokens -> bool) -> tokens -> tokens t
    val takeWhileP : string option -> (token -> bool) -> tokens t
    val takeWhile1P : string option -> (token -> bool) -> tokens t
    val takeP : string option -> int -> tokens t
    val hidden : 'a t -> 'a t
    val fix : ('a t -> 'a t) -> 'a t
    val getOffset : int t
    val getInput : input t
end

