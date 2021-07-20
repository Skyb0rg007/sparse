(* vim: set ft=sml: *)

structure SourcePos : sig
    type t = {
        name: string,
        line: int,
        column: int
    }

    type 's state = { 
        input: 's,
        offset: int,
        sourcePos: t,
        tabWidth: int,
        linePrefix: string
    }

    val initial : 's * string -> 's state
    val toString : t -> string
end = struct
    type t = {
        name: string,
        line: int,
        column: int
    }

    type 'a state = { 
        input: 'a,
        offset: int,
        sourcePos: t,
        tabWidth: int,
        linePrefix: string
    }

    fun initial (s, name) = {
            input = s,
            offset = 0,
            sourcePos = { name = name, line = 1, column = 1 },
            tabWidth = 4,
            linePrefix = ""
        }

    fun toString {name,line,column} =
        let val lc = Int.toString line ^ ":" ^ Int.toString column
        in  if String.size name = 0 then lc else name ^ ":" ^ lc
        end
end

functor SparseErrorPrettyFn(
    structure P : SPARSE
    val newlineTok : P.Token.t
    val tabTok : P.Token.t
    val tokenToString : P.Token.t -> string
) : sig
    val reachOffset : int * P.Input.t SourcePos.state -> string * P.Input.t SourcePos.state
    val errorPretty : P.Error.t list * string * P.Input.t -> string
end =
struct
    open P

    val reachOffset : int * Input.t SourcePos.state -> string * Input.t SourcePos.state =
        fn (n, {input, offset, sourcePos, tabWidth, linePrefix}) =>
        let 
            fun isNewline c = Token.compare (c, newlineTok) = EQUAL
            fun isTab c = Token.compare (c, tabTok) = EQUAL
            val (pre, post) = valOf (Input.takeN (n - offset, input))
            fun go (ch, ({name,line,column}, s)) =
                if isNewline ch
                  then ({name=name,line=line+1,column=1}, "")
                else if isTab ch
                  then ({name=name,line=line,column=column + tabWidth - (Int.rem (column-1, tabWidth))}, s ^ tokenToString ch)
                else ({name=name,line=line,column=column+1}, s ^ tokenToString ch)
            val (spos, s) = List.foldl go (sourcePos, "") (Chunk.toTokens pre)
            val sameLine = #line spos = #line sourcePos
            fun addPrefix xs = if sameLine then linePrefix ^ xs else xs
            val expandTab = String.translate (fn #"\t" => CharVector.tabulate (tabWidth, fn _ => #" ")
                                               | c => String.str c)
            val line = expandTab (addPrefix (s ^ Chunk.toString (#1 (Input.takeWhile (not o isNewline) post))))
            val line = if String.size line = 0 then "<empty line>" else line
            val prefix = if sameLine then linePrefix ^ s else s
        in  (line, {input=post,offset=Int.max (offset, n),sourcePos=spos,tabWidth=tabWidth,linePrefix=prefix})
        end

    val errorPretty : Error.t list * string * Input.t -> string =
        fn (errors, name, input) =>
        let fun f (error, (out, st)) =
                let val (line, st') = reachOffset (Error.offset error, st)
                    val epos = #sourcePos st'
                    val rpshift = #column epos - 1
                    val lineNumber = Int.toString (#line epos)
                    val padding = CharVector.tabulate (String.size lineNumber + 1, fn _ => #" ")
                    val elen = case error of
                                    Error.Trivial {unexpected=NONE,...} => 1
                                  | Error.Trivial {unexpected=SOME (ErrorItem.Tokens ts),...} => Chunk.length (Chunk.fromTokens ts)
                                  | Error.Trivial {unexpected=SOME _,...} => 1
                                  | Error.Fancy {error=e,...} => CustomError.length e
                    val lineLen = String.size line
                    val pointerLen =
                        if rpshift + elen > lineLen
                          then lineLen - rpshift + 1
                          else elen
                    val pointer = CharVector.tabulate (pointerLen, fn _ => #"^")
                    val rpadding = if pointerLen > 0 then CharVector.tabulate (rpshift, fn _ => #" ") else ""
                    val offendingLine = String.concat
                        [ padding, "|\n", lineNumber, " | ", line
                        , "\n", padding, "| ", rpadding, pointer, "\n"
                        ]
                    val outChunk = "\n" ^ SourcePos.toString epos ^ ":\n" ^ offendingLine ^ Error.textToString error
                in  (out ^ outChunk, st')
                end
            val _ : Error.t * (string * Input.t SourcePos.state) -> string * Input.t SourcePos.state = f
            val (s, _) = List.foldl f ("", SourcePos.initial (input, name)) errors
        in  
            String.substring (s, 1, String.size s - 1)
        end
end

