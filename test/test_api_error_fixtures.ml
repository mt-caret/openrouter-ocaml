open! Core
open! Async
open Fixture_helpers

(* Error envelope: [code] is a plain int, the body has an extra [user_id]
   field at top level (handled by [@@jsonaf.allow_extra_fields]). *)
let%expect_test "api_error_bad_model" =
  let%bind () = parse_api_error_fixture "api_error_bad_model" in
  [%expect
    {|
    ((error
      ((message "totally-not-a-model/foo is not a valid model ID")
       (code ((Int 400))) (metadata ()))))
    1970-01-01 00:00:00.000000Z Error "lib/api_error.ml.t_of_jsonaf: extra fields: user_id"
    |}];
  Deferred.unit
;;
