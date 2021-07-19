
structure StringInput :> SPARSE_INPUT
    where type input = substring
      and type token = char
      and type tokens = string
      and type custom_error = string list =
struct
    type input = substring
    type token = char
    type tokens = string

    val tokenCompare = Char.compare
    val chunkCompare = String.compare
    val tokenToChunk = String.str
    val tokensToChunk = String.implode
    val chunkToTokens = String.explode
    val chunkLength = String.size
    fun chunkEmpty s = String.size s = 0

    val take1 = Substring.getc
    fun takeN (n, s) =
        let val len = Substring.size s
        in  if len = 0
              then NONE
              else if len < n
                then SOME (Substring.string s, Substring.full "")
                else case Substring.splitAt (s, n) of
                          (s1, s2) => SOME (Substring.string s1, s2)
        end
    fun takeWhile f s =
        case Substring.splitl f s of
             (s1, s2) => (Substring.string s1, s2)

    type custom_error = string list
    val customErrorCompare = List.collate String.compare
    val customErrorMerge = op @
end
