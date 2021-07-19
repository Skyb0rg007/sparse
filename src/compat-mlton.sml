structure Compat =
struct
    fun sort cmp xs = ListMergeSort.sort (fn (a, b) => cmp (a, b) = GREATER) xs
    val sortUniq = ListMergeSort.uniqueSort
    fun callec f = MLton.Cont.callcc (fn k =>
        f (fn x => MLton.Cont.throw (k, x));
        raise Fail "Compat.callec")
end
