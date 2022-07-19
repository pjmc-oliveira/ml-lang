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
      ("add", Type.Arrow { from = Int; to_ = Arrow { from = Int; to_ = Int } });
      ("sub", Type.Arrow { from = Int; to_ = Arrow { from = Int; to_ = Int } });
      ("mul", Type.Arrow { from = Int; to_ = Arrow { from = Int; to_ = Int } });
      ("div", Type.Arrow { from = Int; to_ = Arrow { from = Int; to_ = Int } });
      ("eq", Type.Arrow { from = Int; to_ = Arrow { from = Int; to_ = Bool } });
    ]