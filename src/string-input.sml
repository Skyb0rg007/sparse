
structure SparseStringInput : SPARSE_STRUCTS =
struct
    structure Token =
    struct
        type t = char
        val compare = Char.compare
        val toString = Char.toString
    end

    structure Chunk =
    struct
        type t = string
        val compare = String.compare
        val toString = String.toString
        val fromToken = String.str
        val fromTokens = String.implode
        val toTokens = String.explode
        val length = String.size
        fun isEmpty s = String.size s = 0
    end

    structure Input =
    struct
        type t = substring
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
    end

    structure CustomError =
    struct
        type t = string list
        val compare = List.collate String.compare
        val merge = List.@
        val toString = String.concatWith "\n"
        val length = fn _ => 1
    end
end

