open OUnit2
open Ml_lang
open Result.Syntax

let solve_module str ctx =
  let src = Source.of_string str in
  let* tks = Lexer.tokens src in
  let* m = Parser.(parse module_ tks) in
  let* ctx = Solver.(solve_ctx (module_ m) ctx) in
  Ok ctx

let string_of_result r =
  match r with
  | Ok ctx ->
      "Ok ["
      ^ String.concat "\n"
          (List.map
             (fun (name, ty) -> "( \"" ^ name ^ "\", " ^ Type.show ty ^ " )")
             (Solver.TyCtx.to_list ctx))
      ^ "]"
  | Error e -> "Error " ^ String.concat "\n" (List.map Error.to_string e)

let test_solver label str ?initial_ctx expected_ctx =
  let initial_ctx =
    match initial_ctx with
    | None -> Solver.TyCtx.empty
    | Some ctx -> ctx
  in
  label >:: fun _ ->
  assert_equal (Ok expected_ctx)
    (solve_module str initial_ctx)
    ~printer:string_of_result

let suite =
  "Solver"
  >::: [
         test_solver "empty module" "module Hello = {}" Solver.TyCtx.empty;
         test_solver "one binding" "module Hello = { def hello = 1 }"
           (Solver.TyCtx.of_list [ ("hello", Type.Int) ]);
         test_solver "two bindings"
           "module Hello = { def hello = 1 def bye = hello }"
           (Solver.TyCtx.of_list [ ("hello", Type.Int); ("bye", Type.Int) ]);
       ]
