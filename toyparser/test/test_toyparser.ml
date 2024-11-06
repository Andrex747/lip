open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9

(* YOUR TESTS HERE *)
let%test "test_eval_2" = parse "3 + 5" |> eval = Ok 8
let%test "test_eval_3" = parse "10 - 4" |> eval = Ok 6
let%test "test_eval_4" = parse "6 * 3" |> eval = Ok 18
let%test "test_eval_5" = parse "20 / 4" |> eval = Ok 5
let%test "test_eval_6" = parse "(1 + 2) * 3" |> eval = Ok 9
let%test "test_eval_7" = parse "(10 - 5) * 2" |> eval = Ok 10
let%test "test_eval_8" = parse "4 * (6 + 2)" |> eval = Ok 32
let%test "test_eval_9" = parse "50 / (2 + 3)" |> eval = Ok 10
let%test "test_eval_10" = parse "(5 + 5) * (3 - 1)" |> eval = Ok 20
let%test "test_eval_11" = parse "(12 / 4) + 6" |> eval = Ok 9
let%test "test_eval_12" = parse "(5 * 2) + 3" |> eval = Ok 13
let%test "test_eval_13" = parse "(8 - 3) * (2 + 1)" |> eval = Ok 15
let%test "test_eval_14" = parse "(9 / 3) - 2" |> eval = Ok 1
let%test "test_eval_15" = parse "3 + 4 * 2" |> eval = Ok 11
let%test "test_eval_16" = parse "10 / 2 + 3" |> eval = Ok 8
let%test "test_eval_17" = parse "(5 + 3) * (2 - 1)" |> eval = Ok 8
let%test "test_eval_18" = parse "100 / (5 + 5)" |> eval = Ok 10
let%test "test_eval_19" = parse "(7 * 2) - 3" |> eval = Ok 11
let%test "test_eval_20" = parse "6 + 2 * 3" |> eval = Ok 12
