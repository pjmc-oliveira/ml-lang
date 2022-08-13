open OUnit2
open Ml_lang
open Extensions
open Result.Syntax

module W = Writer_option.Make (struct
  type t = Error.t list

  let empty = []
  let concat = ( @ )
end)

let interpret_module ?(entrypoint = "main") ?(tm_ctx = Env.empty)
    ?(ty_ctx = Ctx.empty) str =
  let m, e =
    let open W.Syntax in
    let src = Source.of_string str in
    let* tks = Lexer.tokens src in
    let* m = Parser.(parse module_ tks) in
    let* m = Solver.module_ m ty_ctx in
    let m = Desugar.module_ m in
    W.pure m
  in
  match m with
  | Some m ->
      let* value = Interpreter.run ~entrypoint ~context:tm_ctx m in
      Ok value
  | None -> Error e

let string_of_result src r =
  match r with
  | Ok value -> "Ok " ^ Value.show value
  | Error e ->
      "Error [" ^ String.concat "\n" (List.map (Error.to_string src) e) ^ "]"

let error_to_lines (e : Error.t) : Error.Line.t list = e.lines
let errors_to_lines es = List.(map error_to_lines es)

let string_of_result_lines (r : (Value.t, Error.Line.t list list) result) =
  match r with
  | Ok value -> "Ok " ^ Value.show value
  | Error e ->
      let lines = List.map (List.map Error.Line.show) e in
      let lines = List.map (String.concat "; ") lines in
      "Error [" ^ String.concat "\n" lines ^ "]"

let test_interpreter label str ?(entrypoint = "main") ?(tm_ctx = Env.empty)
    ?(ty_ctx = Ctx.empty) expected =
  label >:: fun _ ->
  assert_equal
    ~printer:(string_of_result (Source.of_string str))
    (Ok expected)
    (interpret_module ~entrypoint ~tm_ctx ~ty_ctx str)

let test_failure label str ?(entrypoint = "main") ?(tm_ctx = Env.empty)
    ?(ty_ctx = Ctx.empty) (expected : Error.Line.t list list) =
  label >:: fun _ ->
  let result = interpret_module ~entrypoint ~tm_ctx ~ty_ctx str in
  assert_equal ~printer:string_of_result_lines (Error expected)
    (Result.map_error errors_to_lines result)

let suite =
  "Interpreter"
  >::: [
         (* Successes *)
         test_interpreter "one binding" "module Hello = { def main = 1 }"
           (Int 1);
         test_interpreter "boolean literal True"
           "module Hello = { def main = True }" (Bool true);
         test_interpreter "boolean literal False"
           "module Hello = { def main = False }" (Bool false);
         test_interpreter "two bindings"
           "module Hello = { def hello = 1 def main = hello }" (Int 1);
         test_interpreter "top-level define before use"
           "module Hello = { def main = bye def bye = 1 }" (Int 1);
         test_interpreter "let expression"
           "module Hello = { def main = let x = 2 in x }" (Int 2);
         test_interpreter "if expression True"
           "module Hello = { def main = if True then let x = 1 in x else 2 }"
           (Int 1);
         test_interpreter "if expression False"
           "module Hello = { def main = if False then 1 else 2 }" (Int 2);
         test_interpreter "function application"
           "module Hello = {
              def identity = \\x : Int. x
              def main = identity 1
            }"
           (Int 1);
         test_interpreter "annotated expression"
           "module Hello = {
              def main = True : Bool
            }"
           (Bool true);
         test_interpreter "nested lambda"
           "module Hello = {
              def const = \\x : Int. \\y : Int.
                x
              def main = const 1 2
            }"
           (Int 1);
         test_interpreter "nested function application"
           "module Hello = {
              def identity = \\x : Int. x
              def main = identity (identity 1)
            }"
           (Int 1);
         test_interpreter "lambda expression annotated as a whole"
           "module Hello = {
              def identity = (\\x x) : Int -> Int
              def main = (identity 2)
            }"
           (Int 2);
         test_interpreter "different nested function application"
           "module Hello = {
              def id1 = \\x : Int. x
              def id2 = \\y : Int. y
              def main = id1 (id2 1)
            }"
           (Int 1);
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "higher order function"
           "module Hello = {
              def hello : (Int -> Int) -> Int = \\f
                f 1
              def identity : Int -> Int = \\x add x 1
              def main = hello identity
            }"
           (Int 2);
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "built-in functions"
           "module Hello = {
              def my_add = \\x : Int. \\y : Int.
                add x y

              def main = add 1 2
            }"
           (Int 3);
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "recursive function"
           "module Hello = {
              def fact : Int -> Int = \\x
                if eq 0 x then
                  1
                else
                  mul x (fact (sub x 1))

              def main = fact 5
            }"
           (Int 120);
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "recursive let-binding"
           "module Hello = {
              def main =
              let fact : Int -> Int = \\x
                if eq 0 x then
                  1
                else
                  mul x (fact (sub x 1))
              in
                fact 5
            }"
           (Int 120);
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "top-level polymorphic function"
           "module Hello = {
              def identity = \\x x
              def main =
                if identity True then
                  1
                else
                  2
            }"
           (Int 1);
         (let tm_ctx =
            Env.union Built_ins.tm_ctx
              (Env.of_list
                 [
                   ( "exit",
                     ref
                       Value.(
                         lift (fun x ->
                             let x = get_int x in
                             failwith ("Failed with: " ^ string_of_int x))) );
                 ])
          in
          let ty_ctx =
            Ctx.union Built_ins.ty_ctx
              (Ctx.of_terms_list [ ("exit", Type.(mono (Arrow (Int, Int)))) ])
          in
          test_interpreter ~tm_ctx ~ty_ctx "non-strict function application"
            "module Hello = {
              def const : Int -> Int -> Int = \\x \\y
                x

              def main = const 1 (exit 2)
            }"
            (Int 1));
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "custom type"
           "module Hello = {
              type AType =
                | Wibble Int Bool
                | Wobble

              def main = Wibble 1 True
            }"
           Value.(
             Con
               {
                 head = "Wibble";
                 arity = 2;
                 tail = [ ref (Int 1); ref (Bool true) ];
               });
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "wrap constructor in function"
           "module Hello = {
              type AType =
                | Wibble Int Bool
                | Wobble

              def wibble1 = Wibble 1
              def main = wibble1 True
            }"
           Value.(
             Con
               {
                 head = "Wibble";
                 arity = 2;
                 tail = [ ref (Int 1); ref (Bool true) ];
               });
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "match expression"
           "module Hello = {
              type Nat = Zero | Succ Nat
              def two = Succ (Succ Zero)
              def count = \\x
                match x with
                  | Zero -> 0
                  | Succ y -> add 1 (count y)
                end
              def main = count two
            }"
           (Int 2);
         (let tm_ctx =
            Env.union Built_ins.tm_ctx
              (Env.of_list
                 [
                   ( "exit",
                     ref
                       Value.(
                         lift (fun x ->
                             let x = get_int x in
                             failwith ("Failed with: " ^ string_of_int x))) );
                 ])
          in
          let ty_ctx =
            Ctx.union Built_ins.ty_ctx
              (Ctx.of_terms_list [ ("exit", Type.(mono (Arrow (Int, Int)))) ])
          in
          test_interpreter ~tm_ctx ~ty_ctx
            "match expression should only evaluate to weak-head normal form"
            "module Hello = {
              type List = Nil | Cons Int List
              def list = Cons (exit 1) Nil

              def main =
                match list with
                  | Nil -> False
                  | Cons x xs -> True
                end
            }"
            (Bool true));
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "program: fold_right"
           "module Hello = {
              type List a =
                | Nil
                | Cons a (List a)

              def fold_right = \\f \\base \\list
                match list with
                  | Nil -> base
                  | Cons x xs -> f x (fold_right f base xs)
                end

              def main = fold_right add 0 (Cons 1 (Cons 2 (Cons 3 Nil)))
            }"
           (Int 6);
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "program: fib recursive"
           "module Hello = {
              def fib = \\n
                if le n 0 then
                  0
                else if eq n 1 then
                  1
                else
                  add (fib (sub n 1)) (fib (sub n 2))

              def main = fib 6
             }"
           (Int 8);
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "program: fib iterative"
           "module Hello = {
              def fib_helper = \\a \\b \\n
                if le n 1 then
                  b
                else
                  fib_helper b (add a b) (sub n 1)

              def fib = fib_helper 0 1

              def main = fib 6
             }"
           (Int 8);
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "program: List map"
           "module Hello = {
              type List a = Nil | Cons a (List a)

              def map = \\f \\ls
                match ls with
                  | Nil -> Nil
                  | Cons x xs ->
                    let y  = f x in
                    let ys = map f xs in
                    Cons y ys
                end

              def sum = \\ls
                match ls with
                  | Nil -> 0
                  | Cons x xs -> add x (sum xs)
                end

              def main = sum (map (add 1) (Cons 0 (Cons 1 (Cons 2 Nil))))
             }"
           (Int 6);
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "program: List head"
           "module Hello = {
              type List a  = Nil | Cons a (List a)
              type Maybe a = None | Some a

              def head = \\ls
                match ls with
                  | Nil -> None
                  | Cons x xs -> Some x
                end

              def main = head (Cons 1 Nil)
             }"
           Value.(Con { head = "Some"; arity = 1; tail = [ ref (Int 1) ] });
         test_interpreter ~tm_ctx:Built_ins.tm_ctx ~ty_ctx:Built_ins.ty_ctx
           "program: Maybe apply"
           "module Hello = {
              type Maybe a = None | Some a

              def apply = \\tf \\tx
                match tf with
                  | None -> None
                  | Some f ->
                    match tx with
                      | None -> None
                      | Some x -> Some (f x)
                    end
                end

              def main = apply (Some (add 1)) (Some 1)
             }"
           Value.(Con { head = "Some"; arity = 1; tail = [ ref (Int 2) ] });
         (* Failure *)
         test_failure "local scope"
           "module Hello = { def foo = let x = 1 in x def main = x }"
           [
             [
               Text "Unbound variable: x";
               Quote { index = 53; line = 1; column = 54; length = 1 };
             ];
           ];
       ]