type square = E | O
type board = square Array.t Array.t

let square_to_str = function
  | E -> "[_]"
  | O -> "[o]"

let square_print s =
  Printf.printf "%s" (square_to_str s)

let board_display b =
  Array.iter 
    (fun row ->
      Array.iter square_print row;
      Printf.printf "\n") 
    b

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
  let b = make_test_board in
  board_display b

let () =
  test_board_display;
   
