module Str_map = Map.Make (String)

type key = string

type t = {
  types : Type.kind Str_map.t;
  terms : Type.poly Str_map.t;
  constructors_of_type : string list Str_map.t;
}

let empty =
  {
    types = Str_map.empty;
    terms = Str_map.empty;
    constructors_of_type = Str_map.empty;
  }

let insert name ty ctx = { ctx with terms = Str_map.add name ty ctx.terms }
let lookup name ctx = Str_map.find_opt name ctx.terms

let insert_ty name kind ctx =
  { ctx with types = Str_map.add name kind ctx.types }

let lookup_ty name ctx = Str_map.find_opt name ctx.types

let lookup_constructors name ctx =
  Str_map.find_opt name ctx.constructors_of_type

let of_list ~terms ~types =
  {
    types = Str_map.of_seq (List.to_seq types);
    terms = Str_map.of_seq (List.to_seq terms);
    constructors_of_type = Str_map.empty;
  }

let of_terms_list terms_list =
  {
    types = Str_map.empty;
    terms = Str_map.of_seq (List.to_seq terms_list);
    constructors_of_type = Str_map.empty;
  }

let of_types_list types_list =
  {
    terms = Str_map.empty;
    types = Str_map.of_seq (List.to_seq types_list);
    constructors_of_type = Str_map.empty;
  }

let of_constructors_list constructors_list =
  {
    terms = Str_map.empty;
    types = Str_map.empty;
    constructors_of_type = Str_map.of_seq (List.to_seq constructors_list);
  }

let to_terms_list ctx = List.of_seq (Str_map.to_seq ctx.terms)
let to_types_list ctx = List.of_seq (Str_map.to_seq ctx.types)
let tm_equal cmp left right = Str_map.equal cmp left.terms right.terms
let ty_equal cmp left right = Str_map.equal cmp left.types right.types
let union_helper left right = Str_map.fold Str_map.add right left

let union left right =
  {
    types = union_helper left.types right.types;
    terms = union_helper left.terms right.terms;
    constructors_of_type =
      union_helper left.constructors_of_type right.constructors_of_type;
  }
