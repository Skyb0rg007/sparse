(* vim: set ft=sml: *)
signature SPARSE_CHAR =
sig
    type 'a t
    type chunk

    val newline : char t
    val crlf : chunk t
    val eol : chunk t
    val tab : char t
    val space : unit t
    val hspace : unit t
    val space1 : unit t
    val hspace1 : unit t

    val controlChar : char t
    val spaceChar : char t
    val upperChar : char t
    val lowerChar : char t
    val alphaChar : char t
    val alphaNumChar : char t
    val printChar : char t
    val digitChar : char t
    val binDigitChar : char t
    val hexDigitChar : char t
end
