module TmCtx = Ctx.Make (String)
module TyCtx = Solver.Ctx

let tm_ctx =
  TmCtx.of_list
    [
      (* Int arithmetic *)
      ( "add",
        Value.(
          lift2 (fun l r ->
              let x = get_int l in
              let y = get_int r in
              Int (x + y))) );
      ( "sub",
        Value.(
          lift2 (fun l r ->
              let x = get_int l in
              let y = get_int r in
              Int (x - y))) );
      ( "mul",
        Value.(
          lift2 (fun l r ->
              let x = get_int l in
              let y = get_int r in
              Int (x * y))) );
      ( "div",
        Value.(
          lift2 (fun l r ->
              let x = get_int l in
              let y = get_int r in
              (* TODO: Not sure if this is the right semantics *)
              Int (x / y))) );
      (* Int comparisson *)
      ( "eq",
        Value.(
          lift2 (fun l r ->
              let x = get_int l in
              let y = get_int r in
              Bool (x = y))) );
      ( "ne",
        Value.(
          lift2 (fun l r ->
              let x = get_int l in
              let y = get_int r in
              Bool (not (x = y)))) );
      ( "le",
        Value.(
          lift2 (fun l r ->
              let x = get_int l in
              let y = get_int r in
              Bool (x <= y))) );
      ( "ge",
        Value.(
          lift2 (fun l r ->
              let x = get_int l in
              let y = get_int r in
              Bool (x >= y))) );
      ( "lt",
        Value.(
          lift2 (fun l r ->
              let x = get_int l in
              let y = get_int r in
              Bool (x < y))) );
      ( "gt",
        Value.(
          lift2 (fun l r ->
              let x = get_int l in
              let y = get_int r in
              Bool (x > y))) );
      (* Bool operations *)
      ( "not",
        Value.(
          lift (fun b ->
              let b = get_bool b in
              Bool (not b))) );
      ( "and",
        Value.(
          lift2 (fun l r ->
              let x = get_bool l in
              let y = get_bool r in
              Bool (x && y))) );
      ( "or",
        Value.(
          lift2 (fun l r ->
              let x = get_bool l in
              let y = get_bool r in
              Bool (x || y))) );
    ]

let ty_ctx =
  TyCtx.of_terms_list
    [
      (* Int arithmetic *)
      ("add", Type.(Poly ([], Arrow (Int, Arrow (Int, Int)))));
      ("sub", Type.(Poly ([], Arrow (Int, Arrow (Int, Int)))));
      ("mul", Type.(Poly ([], Arrow (Int, Arrow (Int, Int)))));
      ("div", Type.(Poly ([], Arrow (Int, Arrow (Int, Int)))));
      (* Int comparisson *)
      ("eq", Type.(Poly ([], Arrow (Int, Arrow (Int, Bool)))));
      ("ne", Type.(Poly ([], Arrow (Int, Arrow (Int, Bool)))));
      ("le", Type.(Poly ([], Arrow (Int, Arrow (Int, Bool)))));
      ("ge", Type.(Poly ([], Arrow (Int, Arrow (Int, Bool)))));
      ("lt", Type.(Poly ([], Arrow (Int, Arrow (Int, Bool)))));
      ("gt", Type.(Poly ([], Arrow (Int, Arrow (Int, Bool)))));
      (* Bool operations *)
      ("not", Type.(Poly ([], Arrow (Bool, Bool))));
      ("and", Type.(Poly ([], Arrow (Bool, Arrow (Bool, Bool)))));
      ("or", Type.(Poly ([], Arrow (Bool, Arrow (Bool, Bool)))));
    ]