open Board

let () =
  let small_b = Board.make_small_test in

(*
  let flag = Board.fit small_b 3 in
  match flag with
    | true -> Printf.printf "TRUE\n";
    | false -> Printf.printf "FALSE\n";
  ;

  Board.serialize (Board.make_test "test/test_board";
  let deserialized_test : Board.board = Board.deserialize "test/test_board" in
  Board.display deserialized_test;
*)
  let funcs_in_order = [(fun id -> id);
                        Board.vflip; 
                        Board.hflip; 
                        (fun b -> (b |> Board.vflip |> Board.hflip));

                        Board.transpose;
                        (fun b -> (b |> Board.transpose |> Board.vflip));
                        (fun b -> (b |> Board.transpose |> Board.hflip));
                        (fun b -> (b |> Board.transpose |> Board.vflip |> Board.hflip));
    ] in

  let display_small_b =
    List.map (fun f -> f small_b) funcs_in_order in

  List.iter Board.display display_small_b
