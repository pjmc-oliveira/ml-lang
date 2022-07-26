module TmCtx = Ctx.Make (String)
module TyCtx = Ctx.Make (String)

let tm_ctx =
  TmCtx.of_list
    [
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
      ( "eq",
        Value.(
          lift2 (fun l r ->
              let x = get_int l in
              let y = get_int r in
              Bool (x = y))) );
      ( "not",
        Value.(
          lift (fun b ->
              let b = get_bool b in
              Bool (not b))) );
    ]

let ty_ctx =
  TyCtx.of_list
    [
      ("add", Type.(Poly ([], Arrow (Int, Arrow (Int, Int)))));
      ("sub", Type.(Poly ([], Arrow (Int, Arrow (Int, Int)))));
      ("mul", Type.(Poly ([], Arrow (Int, Arrow (Int, Int)))));
      ("div", Type.(Poly ([], Arrow (Int, Arrow (Int, Int)))));
      ("eq", Type.(Poly ([], Arrow (Int, Arrow (Int, Bool)))));
      ("not", Type.(Poly ([], Arrow (Bool, Bool))));
    ]