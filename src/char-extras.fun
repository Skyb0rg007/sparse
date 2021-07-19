
functor SparseCharExtrasFn(
    structure P : SPARSE_EXTRAS
    val toChar : P.token -> char
    val fromChar : char -> P.token) =
struct
    open P

    val newline = single (fromChar #"\n")
    val crlf = chunk (Tokens.fromTokens [fromChar #"\r", fromChar #"\n"])
    val eol = label ("end of line", <|> (map Tokens.fromToken newline, crlf))
    val tab = single (fromChar #"\t")
    val space = void (takeWhileP (SOME "white space") (Char.isSpace o toChar))
    fun isHSpace c = Char.isSpace c andalso c <> #"\n" andalso c <> #"\r"
    val hspace = void (takeWhileP (SOME "white space") (isHSpace o toChar))
    val space1 = void (takeWhile1P (SOME "white space") (Char.isSpace o toChar))
    val hspace1 = void (takeWhile1P (SOME "white space") (isHSpace o toChar))

    val controlChar = label ("control character", satisfy (Char.isCntrl o toChar))
    val spaceChar = label ("white space", satisfy (Char.isSpace o toChar))
    val upperChar = label ("uppercase letter", satisfy (Char.isUpper o toChar))
    val lowerChar = label ("lowercase letter", satisfy (Char.isLower o toChar))
    val alphaChar = label ("letter", satisfy (Char.isAlpha o toChar))
    val alphaNumChar = label ("alphanumeric character", satisfy (Char.isAlphaNum o toChar))
    val printChar = label ("printable character", satisfy (Char.isPrint o toChar))
    val digitChar = label ("digit", satisfy (Char.isDigit o toChar))

    fun isBinDigit c = c = #"0" orelse c = #"1"
    val binDigitChar = label ("binary digit", satisfy (isBinDigit o toChar))
    val hexDigitChar = label ("hex digit", satisfy (Char.isHexDigit o toChar))
end
