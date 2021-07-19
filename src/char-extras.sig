signature SPARSE_CHAR_EXTRAS =
sig
    include SPARSE_EXTRAS

    val newline : token t
    val crlf : tokens t
    val eol : tokens t
    val tab : token t
    val space : unit t
    val hspace : unit t
    val space1 : unit t
    val hspace1 : unit t

    val controlChar : token t
    val spaceChar : token t
    val upperChar : token t
    val lowerChar : token t
    val alphaChar : token t
    val alphaNumChar : token t
    val printChar : token t
    val digitChar : token t
    val binDigitChar : token t
    val hexDigitChar : token t
end
