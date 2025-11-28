module type S = sig
  type t1 [@@deriving show]
  type t2 [@@deriving show]

  val day : int
  val name : string
  val part1 : Domainslib.Task.pool -> string -> t1
  val part2 : Domainslib.Task.pool -> string -> t2
end

type t = (module S)
