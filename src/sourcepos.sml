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

