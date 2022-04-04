(* 
open Stdio
*)

type square = E | O
type board = square Array.t Array.t

let string_of_square = function
  | E -> "[_]"
  | O -> "[o]"

let square_of_string = function
  | "[_]" -> E
  | "[o]" -> O
  | _     -> failwith "Unknown square value"

let string_of_row sep row =
  Array.fold_left
    (fun acc el -> acc ^ el)
    sep 
    (Array.map string_of_square row)

let rec list_of_substr acc s =
  let l = String.length s in
  match l with
    | 0 -> acc
    | _ -> list_of_substr (acc @ [Str.string_before s 3]) (Str.string_after s 3)

let row_of_string s =
  Array.of_list 
    (List.map square_of_string 
              (list_of_substr [] s))

let string_of_board b =
  let s = Array.fold_left
     (fun acc el -> acc ^ el)
     ""
     (Array.map
       (string_of_row "\n")
       b
     ) in
  s ^ "\n"

let board_display b =
  Printf.printf "%s" (string_of_board b)

let board_serialize b filename =
  let oc = open_out filename in
  Printf.fprintf oc "%s" (string_of_board b);
  close_out oc

(*
let board_deserialize filename =
  let lines = In_channel.read_lines filename in
  List.map 
*)

let make_test_board =
  let b : board =
    [|
      [|E;E;E;E;E;E;E;E;E;O|];
      [|E;E;E;O;E;E;E;E;E;E|];
      [|E;E;E;E;E;E;E;E;O;E|];
      [|O;E;O;E;O;E;E;E;O;E|];
      [|O;E;O;E;E;E;O;E;O;E|];
      [|O;E;O;E;E;E;O;E;E;E|];
      [|E;E;O;E;E;E;E;E;E;E|];
      [|E;E;E;E;E;O;E;E;E;E|];
      [|E;E;E;E;E;E;E;E;E;E|];
      [|E;E;E;O;O;E;E;E;O;O|]
    |] in
  b
  
let () =
  Printf.printf "%s\n" (string_of_row "" (row_of_string "[o][_][_]"));

  board_serialize (make_test_board) "test/test_board";

  Printf.printf "\n%s\n" (string_of_square (square_of_string "[o]"));
(*
  let deserialized_test_board : board = board_deserialize "test/test_board" in
  board_display deserialized_test_board
*)
  board_display make_test_board
