
type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val add : 'a -> 'a t -> 'a t
val page_forward : 'a t -> ('a option * 'a t)
val page_backward : 'a t -> ('a option * 'a t)
