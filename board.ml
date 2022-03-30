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

let board_display b =
  Printf.printf "%s" (string_of_board b)

(*
let board_serialize b filename =
    let oc = open_out filename in 
    Array.iter
      (fun row ->
        Array.iter (Printf.fprintf oc "%s" (square_to_str)) row;
        Printf.fprintf oc "s\n")
    close_out oc;
*)

let make_test_board =
  let n = 10 in
  let mat : board = Array.make_matrix n n E in

  (* 4 *)
  mat.(3).(2) <- O;
  mat.(4).(2) <- O;
  mat.(5).(2) <- O;
  mat.(6).(2) <- O;
  
  (* 3 *)
  mat.(2).(8) <- O;
  mat.(3).(8) <- O;
  mat.(4).(8) <- O;

  mat.(3).(0) <- O;
  mat.(4).(0) <- O;
  mat.(5).(0) <- O;
 
  (* 2 *)
  mat.(4).(6) <- O;
  mat.(5).(6) <- O;
  
  mat.(9).(8) <- O;
  mat.(9).(9) <- O;

  mat.(9).(3) <- O;
  mat.(9).(4) <- O;
  
  (* subs *)
  mat.(1).(3) <- O;
  mat.(3).(4) <- O;
  mat.(7).(5) <- O;
  mat.(0).(9) <- O;
  
  mat

let test_board_display =
  board_display make_test_board

let () =
  test_board_display;
   
