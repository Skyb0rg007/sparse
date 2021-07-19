structure Compat : COMPAT =
struct
    fun sort cmp xs = ListMergeSort.sort (fn (a, b) => cmp (a, b) = GREATER) xs
    val sortUniq = ListMergeSort.uniqueSort
    fun callec f =
        SMLofNJ.Cont.callcc (fn k =>
            f (SMLofNJ.Cont.throw k);
            raise Fail "Compat.callec")
end
