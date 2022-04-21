open Stdio

module Board =
  struct
    type square = E | O
    type board = square Array.t Array.t

    let string_of_square = function
      | E -> "[_]"
      | O -> "[o]"
    
    let square_of_string = function
      | "[_]" -> E
      | "[o]" -> O
      | _     -> failwith "Unknown square value"
    
    (*
     *  Valid board size is N x N where N > 0
     *)
    let valid b =
      let rows = Array.length b in
      match rows with
        | 0 -> false
        | _ ->
            Array.for_all
              (fun r -> r = rows)
              (Array.map Array.length b)
    
    (* 
     * Deep copy the board
     *)
    let copy b : board =
      if valid b
      then
        Array.map
          (fun r -> Array.copy r)
          b
      else
        failwith "Invalid board size"
    
    (*
     * Return a transposed copy of the board 
     *)
    let transpose b : board =
      if valid b
      then
        let rows = Array.length b in
        let a = copy b in
        for i=0 to pred rows do
          for j=0 to pred rows do
            a.(i).(j) <- b.(j).(i);
          done;
        done;
        a
      else
        failwith "Invalid board size"
    
    let list_reverse l =
      let rec list_reverse_acc l acc = 
        match l with
          | [] -> acc
          | car :: cdr -> list_reverse_acc cdr (car :: acc) in
      list_reverse_acc l []
    
    let array_reverse a = 
      a |> Array.to_list |> list_reverse |> Array.of_list
    
    let vflip b : board =
        if valid b
        then
          array_reverse b
        else
          failwith "Invalid board size"
    
    let hflip b : board =
        if valid b
        then
          Array.map array_reverse b
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
    
    let display b =
      Printf.printf "%s" (string_of_board b)
    
    (*
     * TODO: refactor to use Out_channel
     *)
    let serialize b filename =
      let oc = open_out filename in
      Printf.fprintf oc "%s" (string_of_board b);
      close_out oc
    
    let deserialize filename =
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
     * Enumerate all possible positions for xxxx
     * canonical: find the two highest density quadrants and make them 1 and 2
     *)
    
    let make_small_test =
      let b : board =
        [|
          [|O;E;E;|];
          [|O;E;E;|];
          [|E;E;E;|];
        |] in
      b
    
    let make_test =
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
  end
