signature COMPAT =
sig
    val sort : ('a * 'a -> order) -> 'a list -> 'a list
    val sortUniq : ('a * 'a -> order) -> 'a list -> 'a list
    val callec : (('a -> unit) -> unit) -> 'a
end
