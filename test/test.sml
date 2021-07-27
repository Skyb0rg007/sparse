
structure P = SparseFn(SparseStringInput)
structure C = SparseCharFn(P)
structure L = SparseCharLexFn(P)
structure E = SparseErrorPrettyFn(structure P = P val newlineTok = #"\n" val tabTok = #"\t" val tokenToString = String.str)

val sc = L.space (C.space1, L.skipLineComment "//", L.skipBlockComment ("(*", "*)"))
val symbol = L.symbol sc
fun lexeme p = L.lexeme sc p

fun paren p = P.between (symbol "(") (symbol ")") p

val ident = P.label ("identifier", lexeme (
    P.>>= (C.alphaChar, fn c =>
    P.>>= (P.many C.alphaNumChar, fn cs =>
    P.pure (String.implode (c::cs))))))

val num = P.label ("number", lexeme (P.map String.implode (P.some C.digitChar)))

structure SExp =
struct
    datatype t = List of t list | Atom of string

    val parse = P.fix (fn p =>
        P.choice
        [ P.label ("atom", P.map Atom (P.<|> (num, ident)))
        , P.label ("list", P.map List (paren (P.many p)))
        ])

    fun toString (Atom s) = s
      | toString (List xs) = "[" ^ String.concatWith ", " (List.map toString xs) ^ "]"
end

val input = Substring.full (List.nth (CommandLine.arguments (), 0))
    handle Subscript =>
        (TextIO.print ("Usage: " ^ CommandLine.name () ^ " \"<input>\"\n");
         OS.Process.exit OS.Process.failure)
val () =
    case P.parse (SExp.parse, input) of
         P.Success x => TextIO.print (SExp.toString x ^ "\n")
       | P.Failure errors => TextIO.print (E.errorPretty (errors, "<REPL>", input))

