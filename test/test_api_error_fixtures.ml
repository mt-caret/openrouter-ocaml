open! Core
open! Async
open Fixture_helpers

(* Error envelope: [code] is a plain int and the body carries a top-level
   [user_id] naming the authenticated account. *)
let%expect_test "api_error_bad_model" =
  let%bind () = parse_api_error_fixture "api_error_bad_model" in
  [%expect
    {|
    ((error
      ((message "totally-not-a-model/foo is not a valid model ID")
       (code ((Int 400))) (metadata ())))
     (user_id (user_2puxUM8KhXV6gcphwhJtSrJUgJK)))
    |}];
  Deferred.unit
;;

(* A request that never authenticated has no account to name, so [user_id] is
   absent rather than null — this is what keeps the field optional. *)
let%expect_test "api_error_unauthenticated" =
  let%bind () = parse_api_error_fixture "api_error_unauthenticated" in
  [%expect {|
    ((error ((message "User not found.") (code ((Int 401))) (metadata ())))
     (user_id ()))
    |}];
  Deferred.unit
;;
