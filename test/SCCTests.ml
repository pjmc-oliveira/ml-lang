open OUnit2
open Ml_lang
module StrSCC = SCC.Make (String)
module StrMap = Map.Make (String)

let string_of_scc scc =
  "[\n"
  ^ String.concat ";\n"
      (List.map
         (fun cs ->
           "\t["
           ^ String.concat "; " (List.map (fun c -> "\"" ^ c ^ "\"") cs)
           ^ "]")
         scc)
  ^ "\n]"

let suite =
  "SCC"
  >::: [
         ( "finds strongly connnected components" >:: fun _ ->
           let input =
             StrMap.of_seq
               (List.to_seq
                  [
                    ("e0", [ "e1" ]);
                    ("e1", [ "e2" ]);
                    ("e2", [ "e3"; "e4" ]);
                    ("e3", [ "e0" ]);
                    ("e4", [ "e5" ]);
                    ("e5", [ "e6" ]);
                    ("e6", [ "e4"; "e7" ]);
                    ("e7", []);
                  ])
           in
           let actual = StrSCC.(run (make input)) in
           let expected =
             [ [ "e7" ]; [ "e5"; "e6"; "e4" ]; [ "e1"; "e2"; "e3"; "e0" ] ]
           in
           assert_equal ~printer:string_of_scc expected actual );
       ]
