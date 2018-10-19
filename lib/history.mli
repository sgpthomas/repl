
type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val add : 'a -> 'a t -> 'a t
val page_next : 'a t -> ('a option * 'a t)
val page_prev : 'a t -> ('a option * 'a t)
