module StrCtx = Map.Make (String)

type key = string

type t = {
  types : Type.kind StrCtx.t;
  terms : Type.poly StrCtx.t;
  constructors_of_type : string list StrCtx.t;
}

let empty =
  {
    types = StrCtx.empty;
    terms = StrCtx.empty;
    constructors_of_type = StrCtx.empty;
  }

let insert name ty ctx = { ctx with terms = StrCtx.add name ty ctx.terms }
let lookup name ctx = StrCtx.find_opt name ctx.terms

let insert_ty name kind ctx =
  { ctx with types = StrCtx.add name kind ctx.types }

let lookup_ty name ctx = StrCtx.find_opt name ctx.types
let lookup_constructors name ctx = StrCtx.find_opt name ctx.constructors_of_type

let of_list ~terms ~types =
  {
    types = StrCtx.of_seq (List.to_seq types);
    terms = StrCtx.of_seq (List.to_seq terms);
    constructors_of_type = StrCtx.empty;
  }

let of_terms_list terms_list =
  {
    types = StrCtx.empty;
    terms = StrCtx.of_seq (List.to_seq terms_list);
    constructors_of_type = StrCtx.empty;
  }

let of_types_list types_list =
  {
    terms = StrCtx.empty;
    types = StrCtx.of_seq (List.to_seq types_list);
    constructors_of_type = StrCtx.empty;
  }

let of_constructors_list constructors_list =
  {
    terms = StrCtx.empty;
    types = StrCtx.empty;
    constructors_of_type = StrCtx.of_seq (List.to_seq constructors_list);
  }

let to_terms_list ctx = List.of_seq (StrCtx.to_seq ctx.terms)
let to_types_list ctx = List.of_seq (StrCtx.to_seq ctx.types)
let tm_equal cmp left right = StrCtx.equal cmp left.terms right.terms
let ty_equal cmp left right = StrCtx.equal cmp left.types right.types
let union_helper left right = StrCtx.fold StrCtx.add right left

let union left right =
  {
    types = union_helper left.types right.types;
    terms = union_helper left.terms right.terms;
    constructors_of_type =
      union_helper left.constructors_of_type right.constructors_of_type;
  }
