module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module Make(Ord : OrderedType) = struct
  include BatMap.Make(Ord)

  let find_default_fun map key default =
    try find key map with Not_found -> default ()

end;;
