open OUnit2
open Ml_lang
open Result.Syntax

let solve_module str ctx =
  let src = Source.of_string str in
  let* tks = Lexer.tokens src in
  let* m = Parser.(parse module_ tks) in
  let* ctx = Solver.solve_module m ctx in
  Ok ctx

let string_of_result r =
  match r with
  | Ok ctx ->
      "Ok ["
      ^ String.concat "; "
          (List.map
             (fun (name, ty) -> "( \"" ^ name ^ "\", " ^ Type.show ty ^ " )")
             (Solver.TyCtx.to_list ctx))
      ^ "]"
  | Error e -> "Error [" ^ String.concat "\n" (List.map Error.to_string e) ^ "]"

let error_to_lines (e : Error.t) : Error.Line.t list = e.lines
let errors_to_lines es = List.(map error_to_lines es)

let string_of_result_lines (r : (Solver.ty_ctx, Error.Line.t list list) result)
    =
  match r with
  | Ok ctx ->
      "Ok ["
      ^ String.concat "; "
          (List.map
             (fun (name, ty) -> "( \"" ^ name ^ "\", " ^ Type.show ty ^ " )")
             (Solver.TyCtx.to_list ctx))
      ^ "]"
  | Error e ->
      let lines = List.map (List.map Error.Line.show) e in
      let lines = List.map (String.concat "; ") lines in
      "Error [" ^ String.concat "\n" lines ^ "]"

let ty_ctx_equal l r =
  match (l, r) with
  | Ok l, Ok r -> Solver.TyCtx.equal ( = ) l r
  | _, _ -> l = r

let test_solver label str ?(initial_ctx = Solver.TyCtx.empty) expected_ctx =
  label >:: fun _ ->
  assert_equal ~printer:string_of_result ~cmp:ty_ctx_equal (Ok expected_ctx)
    (solve_module str initial_ctx)

let test_failure label str ?(initial_ctx = Solver.TyCtx.empty) expected_lines =
  label >:: fun _ ->
  let result = solve_module str initial_ctx in
  assert_equal ~printer:string_of_result_lines ~cmp:ty_ctx_equal
    (Error expected_lines)
    (Result.map_error errors_to_lines result)

let suite =
  "Solver"
  >::: [
         test_solver "empty module" "module Hello = {}" Solver.TyCtx.empty;
         test_solver "one binding" "module Hello = { def hello = 1 }"
           (Solver.TyCtx.of_list [ ("hello", Type.Int) ]);
         test_solver "two bindings"
           "module Hello = { def hello = 1 def bye = hello }"
           (Solver.TyCtx.of_list [ ("hello", Type.Int); ("bye", Type.Int) ]);
         test_solver "top-level define before use"
           "module Hello = { def hello = bye def bye = 1 }"
           (Solver.TyCtx.of_list [ ("hello", Type.Int); ("bye", Type.Int) ]);
         test_solver "let expression"
           "module Hello = { def hello = let x = 1 in x }"
           (Solver.TyCtx.of_list [ ("hello", Type.Int) ]);
         test_failure "let expression scope"
           "module Hello = { def foo = let x = 1 in x def main = x }"
           [ [ Text "Unbound variable: x" ] ];
       ]
