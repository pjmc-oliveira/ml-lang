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
    ]

let ty_ctx =
  TyCtx.of_list
    [
      ("add", Type.(Mono (Arrow (Int, Arrow (Int, Int)))));
      ("sub", Type.(Mono (Arrow (Int, Arrow (Int, Int)))));
      ("mul", Type.(Mono (Arrow (Int, Arrow (Int, Int)))));
      ("div", Type.(Mono (Arrow (Int, Arrow (Int, Int)))));
      ("eq", Type.(Mono (Arrow (Int, Arrow (Int, Bool)))));
    ]