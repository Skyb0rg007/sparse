(* vim: set ft=sml: *)
functor SparseCharFn(P : SPARSE where type Token.t = char)
    : SPARSE_CHAR
        where type 'a t = 'a P.t
          and type chunk = P.Chunk.t =
struct
    open P

    type chunk = Chunk.t

    val newline = single #"\n"
    val crlf = chunk (Chunk.fromTokens [#"\r", #"\n"])
    val eol = label ("end of line", <|> (map Chunk.fromToken newline, crlf))
    val tab = single #"\t"
    val space = void (takeWhileP (SOME "white space") Char.isSpace)
    fun isHSpace c = Char.isSpace c andalso c <> #"\n" andalso c <> #"\r"
    val hspace = void (takeWhileP (SOME "white space") (isHSpace))
    val space1 = void (takeWhile1P (SOME "white space") Char.isSpace)
    val hspace1 = void (takeWhile1P (SOME "white space") isHSpace)

    val controlChar = label ("control character", satisfy Char.isCntrl)
    val spaceChar = label ("white space", satisfy Char.isSpace)
    val upperChar = label ("uppercase letter", satisfy Char.isUpper)
    val lowerChar = label ("lowercase letter", satisfy Char.isLower)
    val alphaChar = label ("letter", satisfy (Char.isAlpha))
    val alphaNumChar = label ("alphanumeric character", satisfy Char.isAlphaNum)
    val printChar = label ("printable character", satisfy Char.isPrint)
    val digitChar = label ("digit", satisfy Char.isDigit)

    fun isBinDigit c = c = #"0" orelse c = #"1"
    val binDigitChar = label ("binary digit", satisfy isBinDigit)
    val hexDigitChar = label ("hex digit", satisfy Char.isHexDigit)
end
