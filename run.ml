

let () =
  let small_b = Board.make_small_test_board in

(*
  let flag = Board.fit small_b 3 in
  match flag with
    | true -> Printf.printf "TRUE\n";
    | false -> Printf.printf "FALSE\n";
  ;

  Board.board_serialize (Board.make_test_board) "test/test_board";
  let deserialized_test_board : Board.board = Board.board_deserialize "test/test_board" in
  Board.board_display deserialized_test_board;
*)
  let funcs_in_order = [(fun id -> id);
                        Board.board_vflip; 
                        Board.board_hflip; 
                        (fun b -> (b |> Board.board_vflip |> Board.board_hflip));

                        Board.board_transpose;
                        (fun b -> (b |> Board.board_transpose |> Board.board_vflip));
                        (fun b -> (b |> Board.board_transpose |> Board.board_hflip));
                        (fun b -> (b |> Board.board_transpose |> Board.board_vflip |> Board.board_hflip));
    ] in

  let display_small_b =
    List.map (fun f -> f small_b) funcs_in_order in

  List.iter Board.board_display display_small_b
