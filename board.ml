open Stdio

type square = E | O
type board = square Array.t Array.t

let string_of_square = function
  | E -> "[_]"
  | O -> "[o]"

let square_of_string = function
  | "[_]" -> E
  | "[o]" -> O
  | _     -> failwith "Unknown square value"

let board_transpose b : board = 
  let rows = Array.length b in
  match rows with
    | 0 -> failwith "Invalid board size"
    | _ -> 
        if Array.for_all
          (fun r -> r = rows)
          (Array.map Array.length b)
        then
          let a = Array.make_matrix rows rows E in
          for i=0 to pred rows do
            for j=0 to pred rows do
              a.(i).(j) <- b.(j).(i);
            done;
          done;
          a
        else
          failwith "Invalid board size"

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

(*
 * TODO: refactor to use Out_channel
 *)
let board_serialize b filename =
  let oc = open_out filename in
  Printf.fprintf oc "%s" (string_of_board b);
  close_out oc

let board_deserialize filename =
  let lines = In_channel.read_lines filename in
  Array.of_list
    (List.map 
      row_of_string
      (List.filter (fun x -> String.length x > 0) lines))

(*
 * Does a ship of length l fit in the middle of the subboard (horizontally)?
 * Use this function to enumerate potential positions for ships
 *)
let fit b l =
  let rows = Array.length b in
  if rows <> 3 
  then 
    false
  else
    let cols = Array.length b.(0) in
    if cols <> l + 2 
    then 
      false
    else
      Array.for_all 
        (fun row -> Array.for_all 
                      (fun el -> el = E) 
                      row) 
        b

(*
let make_small_test_rectangle =
  let b : board =
    [|
      [|E;E;E;E;E;|];
      [|E;E;E;E;E;|];
      [|E;E;E;E;E;|];
    |] in
  b
*)

let make_small_test_board =
  let b : board =
    [|
      [|E;O;O;|];
      [|E;E;O;|];
      [|E;E;E;|];
    |] in
  b

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
  (*
  Printf.printf "Original rectangle";
  let small_r = make_small_test_rectangle in
  board_display small_r;

  Printf.printf "Transpose rectangle";
  board_display (board_transpose small_r);
  *)

  Printf.printf "Original board";
  let small_b = make_small_test_board in
  board_display small_b;

  Printf.printf "Transpose board";
  board_display (board_transpose small_b);

  Printf.printf "Back to original board";
  let t_b = board_transpose small_b in
  board_display (board_transpose t_b);

  let flag = fit small_b 3 in
  match flag with
    | true -> Printf.printf "TRUE\n";
    | false -> Printf.printf "FALSE\n";
  ;

(*
  Printf.printf "%s\n" (string_of_row "" (row_of_string "[o][_][_]"));

  board_display make_test_board;

  Printf.printf "\n%s\n\n\n\n" (string_of_square (square_of_string "[o]"));
*)

  board_serialize (make_test_board) "test/test_board";
  let deserialized_test_board : board = board_deserialize "test/test_board" in
  board_display deserialized_test_board
