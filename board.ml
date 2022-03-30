type square = E | O
type board = square Array.t Array.t

let string_of_square = function
  | E -> "[_]"
  | O -> "[o]"

let string_of_row sep row =
  Array.fold_left
    (fun acc el -> acc ^ el)
    sep 
    (Array.map string_of_square row)

let string_of_board b =
  let s = Array.fold_left
     (fun acc el -> acc ^ el)
     ""
     (Array.map 
       (string_of_row "\n") 
       b
     ) in
  s ^ "\n"
(*
let board_display b =
  Printf.printf "%s" (string_of_board b)
*)

let board_serialize b filename =
    let oc = open_out filename in 
    Printf.fprintf oc "%s" (string_of_board b);
    close_out oc

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

(*
let test_board_display =
  board_display make_test_board
*)

let test_board_serialize =
  board_serialize (make_test_board) "test/test_board"
  
let () =
  test_board_serialize;
   
